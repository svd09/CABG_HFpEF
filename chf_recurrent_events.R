#################################################################
##                    Recurrent HF Analysis                    ##
#################################################################

# This is the script for analysis of HF events are recurrent events
# Plan to use a penalised frailty model with a terminal event
# or a marginal means/count model
# non-parametric analysis using the mean cumulative count.

# to ensure that we do not consider between hospital transfers as new HF
# admissions, am going to limit the minimum duration between admissions to 
# at least 7 days.

# get the readmit data and then get the main dataset.



library(pacman)

p_load('tidyverse','rms','naniar','Hmisc',"haven","reda","reReg",
            'MASS', 'tableone','haven',"lubridate", "survival", "mstate",
            "naniar", "survminer", "ggsci", "rstpm2", "splines")



hp = read_csv(
"P:\\ORD_Perez_201602128D\\Deo\\CABG_HF\\coding_paper\\data\\hp2.csv")


# this dataset contains all the readmission for my patients.


re = read_csv("P:/ORD_Perez_201602128D/Deo/CABG_HF/data/hfpef_v_readmits.csv")

# now need to only identify all the HF readmits.





# now to get the codes for HF admissions 

icd9_hf = read_sas("P:\\ORD_Perez_201602128D\\Deo\\CABG_HF\\data\\deo_icd9_hf.sas7bdat")

icd10_hf = read_sas("P:\\ORD_Perez_201602128D\\Deo\\CABG_HF\\data\\deo_icd10_hf.sas7bdat")


# icd9

hf_icd9 = icd9_hf$icd9sid # codes for HF

re$hf_icd9 = with(re, ifelse((principaldiagnosisicd9sid %in% hf_icd9), 1, 0))

re %>% count(hf_icd9)

# icd10

hf_icd10 = icd10_hf$icd10sid

re$hf_icd10 = with(re, ifelse((principaldiagnosisicd10sid %in% hf_icd10), 1, 0))

re %>% count(hf_icd10)

re$hf_readmit = with(re, ifelse((hf_icd9 == 1|hf_icd10 == 1), 1, 0))

re %>% count(hf_readmit)

glimpse(re5)

# see readmits for HF

re %>% count(hf_readmit)

# keep only HF readmits here 

re2 = re %>% filter(hf_readmit == 1)

# now this re2 contains only those patients that have 
# see # of patients 

length(unique(re2$scrssn)) 

glimpse(re2)

keep = re2 %>% dplyr::select(scrssn, surgdate, hf_readmit, admitdatetime)

keep = keep %>% rename(hf_admitdate = admitdatetime)

glimpse(keep)

keep$surgdate = as_date(keep$surgdate)
keep$hf_admitdate = as_date(keep$hf_admitdate)

# now to get the days between surgery and readmission.

keep$hf_days = (keep$surgdate %--% keep$hf_admitdate)/ddays(1)

describe(keep$hf_days)

# now need to convert the hf_days to NA if within 7 days of each other.


# first create an row_id for each patient 

keep = keep %>% arrange(scrssn, hf_days)

keep2 = keep %>% group_by(scrssn) %>%
  mutate(rowid = paste0("readmit", row_number(), sep = ""))

keep_wide = keep2 %>% 
  pivot_wider(id = scrssn, values_from = hf_days, names_from = rowid)

glimpse(keep_wide)

# start from the 2nd column and do for each column 
# may end up with less columns with actual data after doing this

keep_wide$hf_t_1 = keep_wide$readmit1

keep_wide$hf_t_2 = with(keep_wide, ifelse(readmit2 - readmit1 < 7, NA, readmit2))

describe(keep_wide$hf_t_2) # this is working , now to do this for each column and then we can use tmerge to combine the dataset into the long format.


keep_wide$hf_t_3 = with(keep_wide, ifelse(readmit3 - readmit2 < 7, NA, readmit3))

keep_wide$hf_t_4 = with(keep_wide, ifelse(readmit4 - readmit3 < 7, NA, readmit4))

keep_wide$hf_t_5 = with(keep_wide, ifelse(readmit5 - readmit4 < 7, NA, readmit5))

keep_wide$hf_t_6 = with(keep_wide, ifelse(readmit6 - readmit5 < 7, NA, readmit6))

keep_wide$hf_t_7 = with(keep_wide, ifelse(readmit7 - readmit6 < 7, NA, readmit7))

keep_wide$hf_t_8 = with(keep_wide, ifelse(readmit8 - readmit7 < 7, NA, readmit8))

keep_wide$hf_t_9 =  with(keep_wide, ifelse(readmit9 - readmit8 < 7, NA, readmit9))

keep_wide$hf_t_10 = with(keep_wide, ifelse(readmit10 - readmit9 < 7, NA, readmit10))

keep_wide$hf_t_11 = with(keep_wide, ifelse(readmit11 - readmit10 < 7, NA, readmit11))

keep_wide$hf_t_12 = with(keep_wide, ifelse(readmit12 - readmit11 < 7, NA, readmit12))

keep_wide$hf_t_13 = with(keep_wide, ifelse(readmit13 - readmit12 < 7, NA, readmit13))

keep_wide$hf_t_14 = with(keep_wide, ifelse(readmit14 - readmit13 < 7, NA, readmit14))

keep_wide$hf_t_15 = with(keep_wide, ifelse(readmit15 - readmit14 < 7, NA, readmit15))

keep_wide$hf_t_16 = with(keep_wide, ifelse(readmit16 - readmit15 < 7, NA, readmit16))

keep_wide$hf_t_17 = with(keep_wide, ifelse(readmit17 - readmit16 < 7, NA, readmit17))

describe(keep_wide$hf_t_17)

# now need to do the tmerge with the main dataset to get the data in long format for the model.

keep_wide2 = keep_wide %>% dplyr::select(scrssn, contains ("hf_t"))

glimpse(keep_wide2)


# now the dr_w.n can be tmerged with the main dataset to do the time gap analysis
# to get the recurrent cumulative mean function,we need to format the data first.
# get the main dataaset and then start tmerge with this wide dataset.


hp = read_csv(
"P:\\ORD_Perez_201602128D\\Deo\\CABG_HF\\coding_paper\\data\\hp2.csv")


# hp now contains 10396 patients, am going to now tmerge the data...

hp$fupdays2 = hp$fupdays + 1

ld1 = tmerge(data1 = hp, data2 = hp, id = scrssn, tstop = fupdays2)

ld2 = tmerge(data1 = ld1, data2 = keep_wide2, id = scrssn, chf = event(hf_t_1))

ld3 = tmerge(data1 = ld2, data2 = keep_wide2, id = scrssn, chf = event(hf_t_2))

dim(ld3)

ld4 = tmerge(data1 = ld3, data2 = keep_wide2, id = scrssn, chf = event(hf_t_3))

ld5 = tmerge(data1 = ld4, data2 = keep_wide2, id = scrssn, chf = event(hf_t_4))

ld6 = tmerge(data1 = ld5, data2 = keep_wide2, id = scrssn, chf = event(hf_t_5))

dim(ld6)

ld7 = tmerge(data1 = ld6, data2 = keep_wide2, id = scrssn, chf = event(hf_t_6))

ld8 = tmerge(data1 = ld7, data2 = keep_wide2, id = scrssn, chf = event(hf_t_7))

ld9 = tmerge(data1 = ld8, data2 = keep_wide2, id = scrssn, chf = event(hf_t_8))

ld10 = tmerge(data1 = ld9, data2 = keep_wide2, id = scrssn, chf = event(hf_t_9))

dim(ld10)

ld11 = tmerge(data1 = ld10, data2 = keep_wide2, id = scrssn, chf = event(hf_t_10))

ld12 = tmerge(data1 = ld11, data2 = keep_wide2, id = scrssn, chf = event(hf_t_11))

ld13 = tmerge(data1 = ld12, data2 = keep_wide2, id = scrssn, chf = event(hf_t_12))

ld14 = tmerge(data1 = ld13, data2 = keep_wide2, id = scrssn, chf = event(hf_t_13))

ld15 = tmerge(data1 = ld14, data2 = keep_wide2, id = scrssn, chf = event(hf_t_14))

dim(ld15)

ld16 =  tmerge(data1 = ld15, data2 = keep_wide2, id = scrssn, chf = event(hf_t_15))

ld17 =  tmerge(data1 = ld16, data2 = keep_wide2, id = scrssn, chf = event(hf_t_16))

dim(ld17)

ld18 = tmerge(data1 = ld17, data2 = keep_wide2, id = scrssn, chf = event(hf_t_17))

dim(ld18)

head(ld18,10) # now the data contains 3 more columns --- id, tstart, tstop & chf

# am going to now save this dataset and can open it again for the models.

write_csv(ld18, 
"P:/ORD_Perez_201602128D/Deo/CABG_HF/data/long_hf_rec.csv")

# now am going to get this dataset open again and going to do the nonparametric MCC for 4 groups.

df = read_csv("P:/ORD_Perez_201602128D/Deo/CABG_HF/data/long_hf_rec.csv")

# dataset df now contains the long data for the MCC and other models.


df_mcf = df %>% dplyr::select(id, tstart, tstop, chf, fupdays2, died, cohort)


df_mcf$cohort_a[df_mcf$cohort == "normal"]<- 0
df_mcf$cohort_a[df_mcf$cohort == "hfpef"]<- 1
df_mcf$cohort_a[df_mcf$cohort == "hfmef"]<- 2
df_mcf$cohort_a[df_mcf$cohort == "hfref"]<- 3


df_mcf$cohort_a <- factor(df_mcf$cohort_a)


df_mcf %>% count(cohort_a)


#- see if we can convert the results to years 

df_mcf$tstart.y = df_mcf$tstart/365.24
df_mcf$tstop.y = df_mcf$tstop/365.24

attach(df_mcf)

g <- Recur(tstart.y %to% tstop.y, id = id, event = chf)

plot(g) # beautiful recurrent event plot.


mcf_overall = mcf(g ~ 1, data = df_mcf,
                  variance = "bootstrap", level = 0.68,
                  )

str(mcf_overall)

plot(mcf_overall)


# am going to extract the information from MCF and then create a plot

res = mcf_overall@MCF

write.csv(res,
"P:/ORD_Perez_201602128D/Deo/CABG_HF/coding_paper/tables/mcc_overall.csv")

# according to group 

mcf_cohort = mcf(g ~ cohort_a, data = df_mcf, 
variance = "bootstrap", level = 0.68)

# am going to save this dataset too; so that can make good graphs.

cohort_res = mcf_cohort@MCF


write.csv(cohort_res,
"P:/ORD_Perez_201602128D/Deo/CABG_HF/coding_paper/tables/mcc_cohort.csv")


# plots for HF MCF and then for each group.
# Get the color function too...


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
## END

#Example - 
#mycol = t_col("blue")




tiff('P:/ORD_Perez_201602128D/Deo/CABG_HF/coding_paper/figures/mcf_overall.tiff',
     height = 7, width = 5, units = "in", res = 1200)


plot(x = res$time, y = 100*res$MCF, type = "s",
     xlab = "Followtime:Years",
     ylab = "Event Rate/100 Patient-Years Followup",
     xlim = c(0,10))
polygon(c(res$time, rev(res$time)), c(100*res$upper, rev(100*res$lower)),
        col = t_col("blue"), border = NA)

dev.off()


# now need to do the same for grouped data.



res_group0 = mcf_cohort@MCF %>% filter(cohort_a == 0)
res_group1 = mcf_cohort@MCF %>% filter(cohort_a == 1)
res_group2 = mcf_cohort@MCF %>% filter(cohort_a == 2)
res_group3 = mcf_cohort@MCF %>% filter(cohort_a == 3)


# plot for each group/cohort --- 


tiff('P:/ORD_Perez_201602128D/Deo/CABG_HF/coding_paper/figures/mcf_cohort.tiff',
     height = 7, width = 5, units = "in", res = 1200)



plot(x = res_group0$time, y = res_group0$MCF*100, 
     type = "s", col = "black", ylim = c(0,120),
     xlab = "Followup Time:Years",
     ylab = "Event Rate/Per 100 Patient-Years Followup",
     xlim = c(0,10))

polygon(c(res_group0$time, rev(res_group0$time)), c(res_group0$lower*100, rev(res_group0$upper*100)),
        col = t_col("black"), border = NA)

lines(x = res_group1$time, y = res_group1$MCF*100, col = "blue", lty = 1)


polygon(c(res_group1$time, rev(res_group1$time)), 
        c(res_group1$lower*100, rev(res_group1$upper*100)),
        col = t_col("blue"), border = NA)

lines(x = res_group2$time, y = res_group2$MCF*100, col = "orange3", lty = 1)

polygon(c(res_group2$time, rev(res_group2$time)), 
        c(res_group2$lower*100, rev(res_group2$upper*100)),
        col = t_col("orange3"), border = NA)



lines(x = res_group3$time, y = res_group3$MCF*100, col = "red", lty = 1)

polygon(c(res_group3$time, rev(res_group3$time)), 
        c(res_group3$lower*100, rev(res_group3$upper*100)),
        col = t_col("red"), border = NA)


dev.off()

# model for recurrent event analysis.
# am going to impute as before with simple imputation


m = df %>% dplyr::select(scrssn, cohort, fupyears, died, 
    bmi, obese, grafts, priorstroke, prior_mi,lvedp,cr,priorhs,
priorpci, anemia, race.mod, GFR, ckd, smoking, afib, age, 
copd, pvd, diabetes, tstart, tstop, id, chf)


glimpse(m)

missing = miss_var_summary(m)

missing

# am going to do simple imputation as before.
# then using frailty model.



m$ckd[is.na(m$ckd)]<- 0
m$obese[is.na(m$obese)]<- 0
m$pvd[is.na(m$pvd)]<- 0
m$anemia[is.na(m$anemia)]<- 0
m$diabetes = factor(m$diabetes)

m$cr[is.na(m$cr)]<- 1.2
m$bmi[is.na(m$bmi)]<- 29.4

m$cohort_a[m$cohort == "normal"]<- 0
m$cohort_a[m$cohort == "hfpef"]<- 1
m$cohort_a[m$cohort == "hfmef"]<- 2
m$cohort_a[m$cohort == "hfref"]<- 3

m$cohort_a <- factor(m$cohort_a)

library(frailtypack)

fp_m = frailtyPenal(Surv(tstart, tstop, chf) ~ 
      cohort_a + cluster(id)+ age + 
        ckd + bmi + pvd + smoking + 
        diabetes + priorstroke + prior_mi + 
        priorpci + anemia + race.mod + afib + copd + terminal(died),
      data = m, n.knots = 12,
      kappa = 1,
      joint = TRUE)


print(fp_m, digits = 4)

summary(fp_m, level = 0.95)

plot(fp_m, type.plot = "Survival")