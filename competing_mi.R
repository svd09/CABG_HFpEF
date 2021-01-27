##################################################################
##                      Competing Risks:MI                      ##
##################################################################

# This script is the competing risk model.
# Present unadjusted CIF for MI for the 4 groups
# Get the first MI event and then create the plot.


library(pacman)
p_load('tidyverse','rms','naniar','Hmisc',"haven","cmprsk","etm",
            'MASS', 'tableone','haven',"lubridate", "survival", "mstate",
            "naniar", "survminer", "ggsci", "rstpm2", "splines")




# get the base data here.


hp = read_csv(
  "P:\\ORD_Perez_201602128D\\Deo\\CABG_HF\\coding_paper\\data\\hp2.csv")


# then get the dataset for the readmissions for all my patients .
# this dataset contains all the readmission information for my data.
# have to identify and limit to only MI using ICD codes.

re = 
read_csv("P:/ORD_Perez_201602128D/Deo/CABG_HF/data/hfpef_v_readmits.csv")


# now to see the diagnoses for MI and CHF

icd9_mi = read_sas("P:\\ORD_Perez_201602128D\\Deo\\CABG_HF\\data\\deo_icd9_mi.sas7bdat")

icd10_mi = read_sas("P:\\ORD_Perez_201602128D\\Deo\\CABG_HF\\data\\deo_icd10_mi.sas7bdat")

# see if MI according to ICD9 code 


mi_icd9 = icd9_mi$icd9sid


re$mi_icd9 = with(re, ifelse((principaldiagnosisicd9sid %in% mi_icd9), 1, 0))

re %>% count(mi_icd9)

mi_icd10 = icd10_mi$icd10sid

re$mi_icd10 = with(re, ifelse((principaldiagnosisicd10sid %in% mi_icd10), 1, 0))

re %>% count(mi_icd10)

re$mi_readmit = with(re, ifelse((mi_icd9 == 1|mi_icd10 == 1), 1, 0))

re %>% count(mi_readmit)

# now only limit to mi_readmit

re2 = re %>% filter(mi_readmit == 1)

dim(re2)

glimpse(re2)

length(unique(re2$scrssn)) # now need to only limit to first MI event 

re2 = re2 %>% arrange(scrssn, admitdatetime)

re2$mi_time = (re2$surgdate %--% re2$admitdatetime)/ddays(1)


re2 = re2 %>% arrange(scrssn, mi_time)

re3 = re2[!duplicated(re2$scrssn), ]

dim(re3)

glimpse(re3)

re4 = re3 %>% dplyr::select(scrssn, mi_readmit, mi_time)

# now to join this with the main data hp


hp = read_csv(
  "P:\\ORD_Perez_201602128D\\Deo\\CABG_HF\\coding_paper\\data\\hp2.csv")

hp2 = left_join(hp, re4, by = "scrssn")


# now to do further to

# make NA == 0 for mit_readmit

hp2$mi_readmit[is.na(hp2$mi_readmit)]<- 0

# also set up the time for event 

glimpse(hp2)

hp2$compete_time = with(hp2, ifelse(mi_readmit == 1, mi_time, fupdays))

describe(hp2$compete_time)

hp2$compete_time = hp2$compete_time + 1

hp2$compete_event = with(hp2, ifelse(mi_readmit == 1, 1,
					ifelse(died == 1, 2, 0)))

hp2 %>% count(compete_event)

# now am going to save this dataaset, so that do not need to repeat this till here...

write_csv(hp2,
 "P:\\ORD_Perez_201602128D\\Deo\\CABG_HF\\coding_paper\\data\\hp_mi_comp.csv")



# 10/10/2020 --- continue after getting the packages installed.
# get the data 

hp = 
read_csv("P:\\ORD_Perez_201602128D\\Deo\\CABG_HF\\coding_paper\\data\\hp_mi_comp.csv")


# now to limit to the col that are needed for the analysis.
# convert the scale to years 

hp$compete_time_years = hp$compete_time/365.24

library(etm)

mi_c = etmCIF(Surv(compete_time_years, compete_event != 0) ~ cohort, 
	failcode = 1,
	etype = compete_event,
	data = hp)



mi_res = summary(mi_c)

mi_res

# to get the actual estimates for the paper, redo the model with survival package

mi_surv = survfit(Surv(compete_time_years, compete_event, type = "mstate")
                  ~ cohort, data = hp)


summary(mi_surv, times = c(5,10))





# now to get the columns to create the dataset for the plot.

str(mi_res)

# NEED TO REDO THIS ANALYSIS PART FOR GETTING THE DATA FOR THE PLOTS.
# AM GOING TO CREATE TWO PLOTS. ---- COMP RISK WITH CI / COMBINED SURVIVAL + MI EVENTS CIF.

# get the dataasets for the plots.

mi_normal = mi_res$`cohort=normal`$`CIF 1`
mi_normal = mi_normal %>% tbl_df()

glimpse(mi_normal)


mi_hfmef = mi_res$`cohort=hfmef`$`CIF 1`
mi_hfmef = mi_hfmef %>% tbl_df()

glimpse(mi_hfmef)


mi_hfref = mi_res$`cohort=hfref`$`CIF 1`
mi_hfref = mi_hfref %>% tbl_df()

glimpse(mi_hfref)


mi_hfpef = mi_res$`cohort=hfpef`$`CIF 1`
mi_hfpef = mi_hfpef %>% tbl_df()

glimpse(mi_hfpef)

# t_col function for polygon code

t_col <- function(color, percent = 80, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}




tiff(filename = 
       "P:/ORD_Perez_201602128D/Deo/CABG_HF/coding_paper/figures/fig_mi.tiff",
     height = 5, width = 7, units = "in", res = 1200)


plot(x = mi_normal$time, y = 100*mi_normal$P, type = "s",
xlim = c(0,10), ylim = c(0,15),
	xlab = "Followup Time:Years",
	ylab = "Cumulative Incidence: Myocardial Infarction")

polygon(c(mi_normal$time, rev(mi_normal$time)), 
        c(mi_normal$lower*100, rev(mi_normal$upper*100)),
        col = t_col("gray"), border = NA)

lines(mi_hfpef$time, 100*mi_hfpef$P, col = "blue", lwd = 1.5)


polygon(c(mi_hfpef$time, rev(mi_hfpef$time)), 
        c(mi_hfpef$lower*100, rev(mi_hfpef$upper*100)),
        col = t_col("blue"), border = NA)


lines(mi_hfmef$time, 100*mi_hfmef$P, col = "orange4", lwd = 1.5)


polygon(c(mi_hfmef$time, rev(mi_hfmef$time)), 
        c(mi_hfmef$lower*100, rev(mi_hfmef$upper*100)),
        col = t_col("orange4"), border = NA)



lines(mi_hfref$time, 100*mi_hfref$P, col = "red", lwd = 1.5)


polygon(c(mi_hfref$time, rev(mi_hfref$time)), 
        c(mi_hfref$lower*100, rev(mi_hfref$upper*100)),
        col = t_col("red"), border = NA)

dev.off()


# am going to save the ETM results
# can use that to fill the results sections.

mi_normal = write_csv(mi_normal,
"P:\\ORD_Perez_201602128D\\Deo\\CABG_HF\\coding_paper\\tables\\mi_normal.csv")



mi_hfpef = write_csv(mi_hfpef,
"P:\\ORD_Perez_201602128D\\Deo\\CABG_HF\\coding_paper\\tables\\mi_hfpef.csv")




mi_hfmef = write_csv(mi_hfmef,
"P:\\ORD_Perez_201602128D\\Deo\\CABG_HF\\coding_paper\\tables\\mi_hfmef.csv")




mi_hfref = write_csv(mi_hfref,
"P:\\ORD_Perez_201602128D\\Deo\\CABG_HF\\coding_paper\\tables\\mi_hfref.csv")


# Cause specific hazard for MI.

# for cause specific hazard, we will keep both other events as censored.
# time is going to be the composite time.

# get the cohort in order

hp$cohort_a[hp$cohort == "normal"]<- 0
hp$cohort_a[hp$cohort == "hfpef"]<- 1
hp$cohort_a[hp$cohort == "hfmef"]<- 2
hp$cohort_a[hp$cohort == "hfref"]<- 3

hp$cohort_a = factor(hp$cohort_a)

hp %>% count(cohort_a)

# so a Fine Gray model to get the subdistibution HR
# can be done with the survival package 

hp$compete_event = factor(hp$compete_event, 0:2,
                          labels = c("censor","mi","dead"))

pdata = finegray(Surv(compete_time_years, compete_event) ~ .,
                 data = hp)


fgmi <- coxph(Surv(fgstart, fgstop, fgstatus) ~ cohort_a,
              data = pdata, weight = fgwt)

summary(fgmi)


### this wraps up the MI event analysis ### 


# figure for only N and HFpEF group...



tiff(filename = 
       "P:/ORD_Perez_201602128D/Deo/CABG_HF/coding_paper/figures/fig_mi_2groups.tiff",
     height = 5, width = 7, units = "in", res = 1200)


plot(x = mi_normal$time, y = 100*mi_normal$P, type = "s",
     xlim = c(0,10), ylim = c(0,15),
     xlab = "Followup Time:Years",
     ylab = "Cumulative Incidence: Myocardial Infarction",
     frame.plot = F)

polygon(c(mi_normal$time, rev(mi_normal$time)), 
        c(mi_normal$lower*100, rev(mi_normal$upper*100)),
        col = t_col("gray"), border = NA)

lines(mi_hfpef$time, 100*mi_hfpef$P, col = "blue", lwd = 1.5)


polygon(c(mi_hfpef$time, rev(mi_hfpef$time)), 
        c(mi_hfpef$lower*100, rev(mi_hfpef$upper*100)),
        col = t_col("blue"), border = NA)



dev.off()

