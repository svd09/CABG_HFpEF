#################################################################
##               HFPEF paper: Tables and results               ##
##                            TRUE                             ##
#################################################################




library(easypackages)
libraries(c('tidyverse','rms','naniar','Hmisc',"broom",
            'MASS', 'tableone','haven',"lubridate", "survival", "mstate",
            "naniar", "survminer", "ggsci", "rstpm2", "splines"))



# get the data df8 

hp = read_csv("P:\\ORD_Perez_201602128D\\Deo\\CABG_HF\\coding_paper\\hfpef1.csv")

# need to remove the duplicates here itself before going any further.

hp2 = hp %>% arrange(scrssn, surgdate)

###### 10585 , but 10396 unique patients #####
# keeping only the 1st events for all.

hp3 = hp2[!duplicated(hp2$scrssn), ]

# convert back to hp

hp = hp3 

# now need to get the variables to make table 1.

# grafts



hp %>% count(cabgda)

hp$grafts[hp$cabgda == 1]<- '1'
hp$grafts[hp$cabgda == 2]<- '2'
hp$grafts[hp$cabgda == 3]<- '3'
hp$grafts[hp$cabgda > 3]<- 'more than 3'

hp %>% count(grafts)


#- prior stroke 

# FOR THESE CODES MAKE HP = DF7 AND THEN CONVERT BACK TO HP #### 

df7 = hp

stroke = as.character(c("V12.54", "Z86.73"))

df7$priorstroke = apply(df7[paste0("icd", 901:910)], 1, function(x) any(x %in% stroke)) %>%
  as.numeric()

df7 %>% count(priorstroke) #- prior stroke or tia 157

#- prior afib

df7 %>% count(prafib) #- 10% missing data 

#- prior mi

df7 %>% count(priormi) #- change to yes , no

df7$prior_mi = with(df7, ifelse(priormi == 0, 0, 1))

df7 %>% count(prior_mi)

#- prior PCI

pci = as.character(c("V45.82", "Z98.61"))

df7$priorpci = apply(df7[paste0("icd", 901:910)], 1, function(x) any(x %in% pci)) %>%
  as.numeric()

df7 %>% count(priorpci) #- prior pci = 262


#- weight and obesity 

describe(df7$wtlbs)

df7$wtlbs = with(df7, ifelse(wtlbs < 100, 100, wtlbs))

describe(df7$htin)

df7$htin = with(df7, ifelse(htin < 60, 60, htin))

df7$bmi = (df7$wtlbs/((df7$htin)^2)) * 703

describe(df7$bmi)

df7$obese = with(df7, ifelse(bmi > 30, 1, 0)) # obesity

df7 %>% count(obese)

# anemia

describe(df7$hgb)

df7$anemia = with(df7, ifelse((hgb < 13 & sex == 0)|(hgb< 12 & sex == 1), 1, 0))


# race

df7$race.mod = with(df7, ifelse(race  %in% c("9"), "black",
                         ifelse(race %in% c("B"), "white", "others")))


df7 %>% count(race.mod)



# need to identify CKD using eGFR


df7$race_n <- with(df7, ifelse(race.mod == 'white', 1, 0))

df7 %>% count(race_n)

describe(df7$cr)

# limit the cr to 5 if > 5

df7$cr = with(df7, ifelse(cr > 5, 5, cr))

df7 %>% count(sex)

describe(df7$gfr)

library(nephro)

df7$sex2 = with(df7, ifelse(sex == 1, 0, 1))

df7$wtkg = df7$wtlbs * 0.45

describe(df7$wtkg)

df7$GFR = with(df7, CG(creatinine = cr, sex = sex2, age = age, wt = wtkg))

describe(df7$GFR)

# using gfr to divide into CKD groups

df7$ckd = with(df7, ifelse(GFR < 60, 1, 0))


df7 %>% count(pvd) #- PVD


df7 %>% count(copd)

df7 %>% count(diabetes)

df7 %>% count(htn)

df7 %>% count(prafib)

df7 %>% count(mitreg)



df7 %>% count(nyha)

df7 %>% count(csmok)

df7$smoking = with(df7, ifelse(csmok == 0, 0, 1))

df7 %>% count(smoking)


#- prior stroke 

stroke = as.character(c("V12.54", "Z86.73"))

df7$priorstroke = apply(df7[paste0("icd", 901:910)], 1, function(x) any(x %in% stroke)) %>%
  as.numeric()


df7 %>% count(priorstroke)


# afib 


afib = as.character(c("427.31","I48.91"))

df7$afib = apply(df7[paste0("icd", 901:910)], 1, function(x) any(x %in% afib)) %>%
  as.numeric()


df7 %>% count(afib)


# NOW CONVERT BACK TO HP 

hp = df7


# now to create table 1 for the whole data 

vars = c("cohortname", "age",  "diabetes", "hgb", "hgba1c",  "mitreg","priorhs", 
"priority", "priormi",  "alb", "sbil",  "cr", 
 "sex",  "cohort", "lvedp",
 "grafts", 
"priorstroke", "prior_mi", "priorpci", "bmi", "obese", "anemia", 
"race.mod",    "GFR", "ckd", "nyha", "smoking", 
"afib")

factors = c("cohortname",  "diabetes",   "mitreg","priorhs", 
"priority", "priormi",  
 "sex",  "cohort",
 "grafts", 
"priorstroke", "prior_mi", "priorpci",  "obese", "anemia", 
"race.mod",    "ckd",  "smoking", 
"afib")


t1 = tableone::CreateTableOne(data = hp, vars = vars, factorVars = factors,strata = c("cohort"))

t1 = print(t1, 
nonnormal = c("age", "bmi", "cr", "alb", "hgb", "hba1c", "gfr", "lvedp"))

write.csv(t1, 
"P:\\ORD_Perez_201602128D\\Deo\\CABG_HF\\coding_paper\\tables\\table1.csv")


write.csv(hp,
"P:\\ORD_Perez_201602128D\\Deo\\CABG_HF\\coding_paper\\data\\hp2.csv")
# 

##########################################################################################################
# get the dataset hp2 from the folder for further analysis.

# going to do some analyses for the results section of the paper.
# start from here ----



hp = read_csv(
"P:\\ORD_Perez_201602128D\\Deo\\CABG_HF\\coding_paper\\data\\hp2.csv")



summary(hp$age)

sd(hp$age)

# followup information 

summary(hp$fupyears)



# make curve for survival 

surv = survfit(Surv(fupyears, died) ~ cohort, data = hp)


summary(surv, times = c(1,5,10,15))

# am going to make the figure 1A for all-cause mortality.
# am going to present it as a failure function.


surv

surv2 = summary(surv)

# get the df o make the figure.

str(surv2)

t = tidy(surv)

# t now contains the data for the graph.
# need to convert to failure function and then separate into 4 df.

t$failure = (1-t$estimate)*100
t$lower = (1-t$conf.high)*100
t$upper = (1-t$conf.low)*100

t_normal = t %>% filter(strata == 'cohort=normal')

t_hfpef = t %>% filter(strata == 'cohort=hfpef')

t_hfmef = t %>% filter(strata == 'cohort=hfmef')

t_hfref = t %>% filter(strata == 'cohort=hfref')

# now to create the plot 


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


# figure 1A - Cumulative Mortality figure.


tiff(filename = 
"P:/ORD_Perez_201602128D/Deo/CABG_HF/coding_paper/figures/fig_allcause_m.tiff",
     height = 6, width = 6, units = "in", res = 1200)


plot(x = t_normal$time, y = t_normal$failure, type = "s",
     xlim = c(0,10),ylim = c(0,80),
     xlab = "Followup Time:Years",
     ylab = "Cumulative All-cause Mortality", lwd = 2, yaxt = "n")

axis(side = 2, at = c(0,20,40,60,80), 
     labels = c("0%", "20%","40%","60%","80%"), las = 2)

polygon(c(t_normal$time, rev(t_normal$time)), 
        c(t_normal$upper, rev(t_normal$lower)),
        col = t_col("gray"), border = NA)

lines(t_hfpef$time, t_hfpef$failure, col = "blue", lwd = 2)


polygon(c(t_hfpef$time, rev(t_hfpef$time)), 
        c(t_hfpef$lower, rev(t_hfpef$upper)),
        col = t_col("blue"), border = NA)


lines(t_hfmef$time, t_hfmef$failure, col = "orange4", lwd = 2)


polygon(c(t_hfmef$time, rev(t_hfmef$time)), 
        c(t_hfmef$lower, rev(t_hfmef$upper)),
        col = t_col("orange4"), border = NA)

lines(t_hfref$time, t_hfref$failure, col = "red", lwd = 2)


polygon(c(t_hfref$time, rev(t_hfref$time)), 
        c(t_hfref$lower, rev(t_hfref$upper)),
        col = t_col("red"), border = NA)


dev.off()


# understand and print survival at 1, 5, 10 and 15 years 

#############################################################################################
# Plan for composite parametric model for the data. 
# Plan for rstmp2 model/PH/3df


# create a smaller dataset for the model.

m = hp %>% dplyr::select(scrssn, cohort, fupyears, died, 
bmi, obese, grafts, priorstroke, prior_mi,lvedp,cr,priorhs,
priorpci, anemia, race.mod, GFR, ckd, smoking, afib, age, copd, pvd, diabetes)


glimpse(m)

missing = miss_var_summary(m)


print(missing)

# amount of missing data 

 > miss_var_summary(m)
# A tibble: 19 x 3
   variable    n_miss pct_miss
   <chr>        <int>    <dbl>
 #1 grafts        2580  24.4   
 #2 GFR             14   0.132 
 #3 ckd             14   0.132 
 #4 anemia          10   0.0945
 #5 bmi              5   0.0472
 #6 obese            5   0.0472
 #7 pvd              5   0.0472


# do a simple imputation for all the variables in the model.
# impute to mode/mean



m$ckd[is.na(m$ckd)]<- 0
m$obese[is.na(m$obese)]<- 0
m$pvd[is.na(m$pvd)]<- 0
m$anemia[is.na(m$anemia)]<- 0
m$diabetes = factor(m$diabetes)


summary(m[, c("cr","bmi")])


m$cr[is.na(m$cr)]<- 1.2
m$bmi[is.na(m$bmi)]<- 29.4


# do not need GFR, BMI, grafts in the model
# grafts has a lot of missing information, so am not going to include that in the model.


m$cohort_n[m$cohort == "normal"]<- 0
m$cohort_n[m$cohort == "hfpef"]<- 1
m$cohort_n[m$cohort == "hfmef"]<- 2
m$cohort_n[m$cohort == "hfref"]<- 3

m$cohort_n = factor(m$cohort_n)

m %>% count(cohort_n)


describe(m$lvedp)

m$wrong = with(m, ifelse(cohort_n == 0 & lvedp > 20, 1, 0))

m$wrong[is.na(m$wrong)]<- 0

m2 = m %>% filter(wrong == 0)

m2 %>% count(wrong)

m2 = m2 %>% filter(priorhs == 0)

dim(m2)



model_cox = coxph(Surv(fupyears, died) ~ cohort_n + age + 
ckd + bmi + pvd + smoking + 
diabetes + priorstroke + prior_mi + 
priorpci + anemia + race.mod + afib + copd,
data = m2) 

model_cox

summary(model_cox)

cox.zph(model_cox)


model = stpm2(Surv(fupyears, died) ~ cohort_n + ns(age,3) + 
ns(bmi,3) + ns(cr,3) + pvd + smoking + diabetes + priorstroke + prior_mi + 
priorpci + anemia + race.mod + afib + copd,
data = m2, df = 3,link.type = "PH") 

model 

summary(model)


# running the model without the normal group

m_2 = m %>% filter(cohort_n %in% c(0,2,3))

dim(m_2)


model_cox2 = coxph(Surv(fupyears, died) ~ cohort_n + ns(age,3) + 
obese + ckd + pvd + smoking + diabetes + priorstroke + prior_mi + 
priorpci + anemia + race.mod + afib + copd,
data = m_2)

summary(model_cox2) 

cox.zph(model_cox2)





##################################################################
##                     MI model and results                     ##
##################################################################
# get the data for MI into the dataset                           #
# identify patients from scrssn that had MI                      #
# going to start the coding from the beginning for MI            #
# get the readmit data and then see admissions for MI            #
##################################################################
# STOPPED HERE. NEED TO GET THE MI DATA NOW.

re = read_sas("P:\\ORD_Perez_201602128D\\Deo\\CABG_HF\\data\\deo_hf_readmits.sas7bdat")

# now am going to first limit this to the patients that are part of my group
# to do that I need to use the crosswalk to join the scrssn and patientsid 

cw = read_sas("P:\\ORD_Perez_201602128D\\Deo\\CABG_HF\\data\\deo_cwalk.sas7bdat")

# now from the cw , limit it to the patients that are in my group

mypat <- dt$scrssn

cw$mine <- with(cw, ifelse(ScrSSN %in% mypat, 1, 0))

cw %>% count(mine)

# keep those patients that are in my main group

cw2 = cw %>% filter(mine == 1)

# now this is the list of patientsid are part of my data.

mypat2 <- cw2$PatientSID 

re$mine <- with(re, ifelse(PatientSID %in% mypat2, 1, 0))

re %>% count(mine)

# now to limit the readmission data to those patients that are in my group

re2 = re %>% filter(mine == 1)

# see unique scrssn in this 

re3 = left_join(re2, cw2, by = "PatientSID")

# keep only the required col

re4 = re3[, c("PatientSID", "AdmitDatetime", "PrincipalDiagnosisICD9SID", 
              "PrincipalDiagnosisICD10SID",  "ScrSSN", "PatientICN")]

# now that I have scrssn, see the actual # of patients that had readmissions.

length(unique(re4$ScrSSN))





# 6531 had readmissions ??? ---- this is wrong, this is because we did not limit to within 
# surgery and last action date

# doing that now.


df = dt %>% dplyr::select(scrssn, ACT_LAST_DT, surgdate)

df$ACT_LAST_DT = as_date(df$ACT_LAST_DT)

df$surgdate = as_date(df$surgdate)

names(re4) = tolower(names(re4))


re5 = left_join(re4, df, by = "scrssn")


re5$admitdatetime = as_date(re5$admitdatetime)


re5$within = with(re5, ifelse((admitdatetime > surgdate & admitdatetime <= ACT_LAST_DT), 1, 0
  ))

re5 %>% count(within)

re6 = re5 %>% filter(within == 1)

# keep only those that have dates correct between surg and last date
# these are the MI events for my study cohort

# now to see the diagnoses for MI and CHF

icd9_mi = read_sas("P:\\ORD_Perez_201602128D\\Deo\\CABG_HF\\data\\deo_icd9_mi.sas7bdat")

icd10_mi = read_sas("P:\\ORD_Perez_201602128D\\Deo\\CABG_HF\\data\\deo_icd10_mi.sas7bdat")

# see if MI according to ICD9 code 


mi_icd9 = icd9_mi$icd9sid


re6$mi_icd9 = with(re6, ifelse((principaldiagnosisicd9sid %in% mi_icd9), 1, 0))

re6 %>% count(mi_icd9)

mi_icd10 = icd10_mi$icd10sid

re6$mi_icd10 = with(re6, ifelse((principaldiagnosisicd10sid %in% mi_icd10), 1, 0))

re6 %>% count(mi_icd10)

re6$mi_readmit = with(re6, ifelse((mi_icd9 == 1|mi_icd10 == 1), 1, 0))

re6 %>% count(mi_readmit)

# limit the dataset to readmits for MI 

re7 = re6 %>% filter(mi_readmit == 1)

re7 = re7 %>% arrange(scrssn)

length(unique(re7$scrssn))

# 2801 patients have MI events.





