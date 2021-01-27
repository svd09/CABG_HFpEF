#################################################################
##              Composite outcome: mortality + HF              ##
#################################################################

# This script is for the composite outcome for mortality and HF: 



library(easypackages)
libraries(c('tidyverse','rms','naniar','Hmisc',"haven",
            'MASS', 'tableone','haven',"lubridate", "survival", "mstate",
            "naniar", "survminer", "ggsci", "rstpm2", "splines"))


# get the base data here:



hp = read_csv(
"P:\\ORD_Perez_201602128D\\Deo\\CABG_HF\\coding_paper\\data\\hp2.csv")


# have to now work on getting the HF readmits for my patients.
# the readmit data is according to patientsid
# so need to get the patientsid first for all my scrssn and then can identify readmits for my patients
# then using icd9/icd10 codes can identify the readmits for HF
# as done earlier, will exclude those admissions that are within 7 days of the prior one, to ensure that we do not 
# wrongly identify transfers as admissions.


# now to get the patientsid list for my patients.

cwalk <- 
read_sas("P:/ORD_Perez_201602128D/Deo/CABG_HF/data/deo_cwalk.sas7bdat")

# understand and confirm the data

dim(cwalk)
dim(hp)


# get only the scrssn from data3 and then merge from cwalk 

scrssn <- hp$scrssn

length(scrssn)

# subset cwalk to only my patients 


names(cwalk) <- tolower(names(cwalk))


glimpse(cwalk)

length(unique(cwalk$scrssn)) # 921128 patients here


# only get the scrssn column from hp

hp2 = hp %>% dplyr::select(scrssn)


hp3 = left_join(hp2, cwalk, by = "scrssn")

dim(hp3)

hp3 = hp3 %>% arrange(scrssn)

head(hp3, 10)

# now hp3 contains the list of patientsid for my patients

length(unique(hp3$scrssn))

# so this hp3 now contain 10396 patients
# that is because the 189 patients died postoperatively
# hence they will not have crosswalk because they do not have followup information
# understand how there at missing patientsid here

glimpse(hp3)

# now to limit the readmits to after the surgery date and before the last date, 
# will need to also get those 2 col into this data

time = hp %>% dplyr::select(scrssn, surgdate, act_last_dt)

hp4 = left_join(hp3, time, by = "scrssn")


# now to get the readmit data and then to identify the HF readmissions.
# to do it two ways ---- 1st HF admission and all HF readmissions.
# to create a composite endpoint of mortality/HF readmission for analysis.

# now to get the readmit file here.
# 

re = 
read_sas("P:/ORD_Perez_201602128D/Deo/CABG_HF/data/deo_hf_readmits.sas7bdat")

glimpse(re)

# now to change to lowercase and only limit to my patients using patientsid

names(re) = tolower(names(re))

patientsid_list = hp3$patientsid

re$keep = with(re, ifelse(patientsid %in% patientsid_list, 1, 0))

re %>% count(keep) # now keep contains the patients that are in my group

re2 = re %>% filter(keep == 1) # re2 now contains the data on my patients being readmitted.
# now for each patient need to keep the readmitted data between surgery date and act_last_dt

# for that am going to join hp4 with re2


re3 = left_join(re2, hp4, by = "patientsid")

glimpse(re3)

re3$admitdatetime = as_date(re3$admitdatetime)

re3$surgdate = as_date(re3$surgdate)
re3$act_last_dt = as_date(re3$act_last_dt)

re3$keep = with(re3, ifelse(admitdatetime > surgdate, 1, 0))

re3 %>% count(keep)

# those that are after, so before == 0

re4 = re3 %>% filter(keep == 1)

# now also keep to limit to my last date of followup in the data

re4$keep2 = with(re4, ifelse(act_last_dt > admitdatetime, 1, 0 ))

re4 %>% count(keep2)

# keep only final == 1, i.e. those that had readmission within my time period

re5 = re4 %>% filter(keep2 == 1)

# now to identify only HF readmissions from this.
# am going to save this re5 dataaset as my readmit dataset.

write_csv(re5, 
          "P:/ORD_Perez_201602128D/Deo/CABG_HF/data/hfpef_v_readmits.csv")





# now to get the codes for HF admissions 

icd9_hf = read_sas("P:\\ORD_Perez_201602128D\\Deo\\CABG_HF\\data\\deo_icd9_hf.sas7bdat")

icd10_hf = read_sas("P:\\ORD_Perez_201602128D\\Deo\\CABG_HF\\data\\deo_icd10_hf.sas7bdat")


# icd9

hf_icd9 = icd9_hf$icd9sid # codes for HF

re5$hf_icd9 = with(re5, ifelse((principaldiagnosisicd9sid %in% hf_icd9), 1, 0))

re5 %>% count(hf_icd9)

# icd10

hf_icd10 = icd10_hf$icd10sid

re5$hf_icd10 = with(re5, ifelse((principaldiagnosisicd10sid %in% hf_icd10), 1, 0))

re5 %>% count(hf_icd10)

re5$hf_readmit = with(re5, ifelse((hf_icd9 == 1|hf_icd10 == 1), 1, 0))

re5 %>% count(hf_readmit)

glimpse(re5)

# see readmits for HF

re5 %>% count(hf_readmit)

# keep only HF readmits here 

re6 = re5 %>% filter(hf_readmit == 1)

# now this re6 contains only those patients that have 
# see # of patients 

length(unique(re6$scrssn)) 

glimpse(re6)

keep = re6 %>% dplyr::select(scrssn, surgdate, act_last_dt, hf_readmit, patientsid,
                             admitdatetime)

# now to create a composite of mortality/HF readmission, we will only keep the first event
# and then get that into the dataset again.


keep = keep %>% arrange(scrssn, admitdatetime)

# to do that first calculate admitdatetime - surgdate


keep$readmit_time = (keep$surgdate %--% keep$admitdatetime)/ddays(1)



describe(keep$readmit_time)

# now to keep the first readmit_time here 

keep = keep %>% arrange(scrssn, readmit_time)

keep2 = keep[!duplicated(keep$scrssn), ]
  
# now keep2 contains the data for readmit time for those readmitted.

keep3 = keep2 %>% dplyr::select(scrssn, readmit_time, hf_readmit)

length(unique(keep3$scrssn))

# now to get this data into the main hp dataset
# get the main dataset here again.




hp = read_csv(
  "P:\\ORD_Perez_201602128D\\Deo\\CABG_HF\\coding_paper\\data\\hp2.csv")


hp2 = left_join(hp, keep3, by = "scrssn")

glimpse(hp2)

hp2$hf_readmit[is.na(hp2$hf_readmit)]<- 0

hp2 %>% count(hf_readmit)

# now to keep only the first event if HF otherwise vital status 

hp2$event = with(hp2, ifelse(hf_readmit == 1, 1,
                             ifelse(died == 1, 1, 0)))

hp2$comp_time = with(hp2, ifelse(hf_readmit == 1, 
                                 readmit_time, fupdays))

describe(hp2$comp_time)

hp2$comp_time = hp2$comp_time + 1

hp2 %>% count(event)
hp2 %>% count(died)

# convert comptime to years

hp2$comp_years = hp2$comp_time/365.24


# save this dataset so that we can then do the remaining analysis 
# later if reqd.

write_csv(hp2,
"P:/ORD_Perez_201602128D/Deo/CABG_HF/data/composite.csv")


# am going to get the composite dataset to make figure for the composite outcome.

comp = read_csv("P:/ORD_Perez_201602128D/Deo/CABG_HF/data/composite.csv")

kmcomp = survfit(Surv(comp_years, event) ~ cohort, data = comp)

compf = tidy(kmcomp)

glimpse(compf)


summary(kmcomp, times = c(1,5,10))


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



compf$failure = (1-compf$estimate)*100
compf$lower = (1-compf$conf.high)*100
compf$upper = (1-compf$conf.low)*100

compf_normal = compf %>% filter(strata == 'cohort=normal')

compf_hfpef = compf %>% filter(strata == 'cohort=hfpef')

compf_hfmef = compf %>% filter(strata == 'cohort=hfmef')

compf_hfref = compf %>% filter(strata == 'cohort=hfref')


tiff(filename = 
       "P:/ORD_Perez_201602128D/Deo/CABG_HF/coding_paper/figures/fig_comp.tiff",
     height = 6, width = 6, units = "in", res = 1200)


plot(x = compf_normal$time, y = compf_normal$failure, type = "s",
     xlim = c(0,10),ylim = c(0,80),
     xlab = "Followup Time:Years",
     ylab = "Composite End-point(Mortality/HF Admission)", lwd = 2, yaxt = "n")

axis(side = 2, at = c(0,20,40,60, 80), 
     labels = c("0%", "20%","40%","60%", "80%"), las = 2)

polygon(c(compf_normal$time, rev(compf_normal$time)), 
        c(compf_normal$upper, rev(compf_normal$lower)),
        col = t_col("gray"), border = NA)

lines(compf_hfpef$time, compf_hfpef$failure, col = "blue", lwd = 2)


polygon(c(compf_hfpef$time, rev(compf_hfpef$time)), 
        c(compf_hfpef$lower, rev(compf_hfpef$upper)),
        col = t_col("blue"), border = NA)


lines(compf_hfmef$time, compf_hfmef$failure, col = "orange4", lwd = 2)


polygon(c(compf_hfmef$time, rev(compf_hfmef$time)), 
        c(compf_hfmef$lower, rev(compf_hfmef$upper)),
        col = t_col("orange4"), border = NA)

lines(compf_hfref$time, compf_hfref$failure, col = "red", lwd = 2)


polygon(c(compf_hfref$time, rev(compf_hfref$time)), 
        c(compf_hfref$lower, rev(compf_hfref$upper)),
        col = t_col("red"), border = NA)


dev.off()


# KM for composite end point




ggsurvplot(kmcomp, censor.size = 0, risk.table = T, conf.int = T,
           xlim = c(0,10), surv.scale = "percent")




mycols = pal_lancet("lanonc")(4)

mycols2 = pal_lancet("lanonc", alpha = 0.2)(4)


# make rms for final plot here.


comp_s = npsurv(Surv(comp_years, event) ~ cohort, data = hp2)


tiff(filename = 
       "P:/ORD_Perez_201602128D/Deo/CABG_HF/coding_paper/compfig.tiff",
     height = 5, width = 7, units = "in", res = 1200)

survplot(comp_s,
         xlim = c(0,10),
         time.inc = 2.5,
         lwd = 2,
         lty = c(1,1,1),
         conf = "bands",
         ylab = "Survival Estimate",
         xlab = "Time:Years",
         col = mycols,
         col.fill = mycols2,         
         n.risk = T,
         label.curves = F,
         adj.n.risk = 0.5,
         levels.only = T)


dev.off()



####################################################################

m = comp %>% dplyr::select(scrssn, cohort, comp_years, event, 
                         bmi, obese, grafts, priorstroke, prior_mi,lvedp,cr,priorhs,
                         priorpci, anemia, race.mod, GFR, ckd, smoking, afib, age, copd, pvd, diabetes)


glimpse(m)

missing = miss_var_summary(m)


print(missing)

# amount of missing data 

# > miss_var_summary(m)
# # A tibble: 19 x 3
# variable    n_miss pct_miss
# <chr>        <int>    <dbl>
#   #1 grafts        2580  24.4   
#   #2 GFR             14   0.132 
#   #3 ckd             14   0.132 
#   #4 anemia          10   0.0945
#   #5 bmi              5   0.0472
#   #6 obese            5   0.0472
#   #7 pvd              5   0.0472
  
  
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

model_cox = coxph(Surv(comp_years, event) ~ cohort_n + age + 
                    ckd + bmi + pvd + smoking + 
                    diabetes + priorstroke + prior_mi + 
                    priorpci + anemia + race.mod + afib + copd,
                  data = m) 

model_cox

summary(model_cox)

cox.zph(model_cox)


# model limiting 10 years 

m$event2 = with(m, ifelse(comp_years> 10, 0, event))


model_cox_10 = coxph(Surv(comp_years, event2) ~ cohort_n + age + 
                    ckd + bmi + pvd + smoking + 
                    diabetes + priorstroke + prior_mi + 
                    priorpci + anemia + race.mod + afib + copd,
                  data = m) 

summary(model_cox_10)


cox.zph(model_cox_10)



# doing segmented Cox @ 5 years for the data. Use survsplit here.


cut_data = survSplit(Surv(comp_years, event) ~ ., data = m,
                     cut = c(5), episode = "timegroup")

glimpse(cut_data)

cut_data %>% count(timegroup)

# timegroup == 1

cut_data1 = cut_data[cut_data$timegroup == 1, ]

cox1 = coxph(Surv(comp_years, event) ~ cohort_n + age + 
               ckd + bmi + pvd + smoking + 
               diabetes + priorstroke + prior_mi + 
               priorpci + anemia + race.mod + afib + copd,
             data = cut_data1)
             
summary(cox1)

cox.zph(cox1, transform = "km")   

# using tt function 

tt1 = coxph(Surv(comp_years, event) ~ cohort_n + tt(cohort_n) + age + 
              ckd + bmi + pvd + smoking + 
              diabetes + priorstroke + prior_mi + 
              priorpci + anemia + race.mod + afib + copd,
            data = cut_data1,
            tt = function(x,t,...){
              mtrx <- model.matrix(~x)[,-1]
              mtrx*t
            }
            )


tt1

summary(tt1)

cox.zph(tt1, transform = function(t)t)







# timegroup == 2



cut_data2 = cut_data[cut_data$timegroup == 2, ]

cox2 = coxph(Surv(comp_years, event) ~ cohort_n + age + 
               ckd + bmi + pvd + smoking + 
               diabetes + priorstroke + prior_mi + 
               priorpci + anemia + race.mod + afib + copd,
             data = cut_data2)


summary(cox2)             

cox.zph(cox2)


# 12/10/2020

# am going to only present time to first HF admission here for each group.

# from hp2 am going to limit data to those with Hf admission.

hp3 <- hp2 %>% filter(hf_readmit == 1)

hp3$hf_admit_years = hp3$readmit_time/365.24

table(hp3$cohort, hp3$hf_readmit)

library(mosaic)

with(hp3, favstats(hf_admit_years ~ cohort))



# cohort        min        Q1   median       Q3      max     mean       sd   n missing
# 1  hfmef 0.02190341 0.3709889 1.954879 5.025463 14.62600 3.143579 3.232068 491       0
# 2  hfpef 0.01095170 0.2696857 2.737926 5.931716 13.30358 3.564665 3.564266  96       0
# 3  hfref 0.01642755 0.4216406 1.626328 4.536743 14.19067 2.867991 3.060806 693       0
# 4 normal 0.02464133 0.6379367 3.685248 6.721608 13.76903 4.195876 3.625361 311       0


# keep only normal and hfpef

df_m <- hp3 %>% filter(cohort %in% c('normal','hfpef'))

wilcox.test(df_m$hf_admit_years ~ df_m$cohort)


# keep only normal and hfmef

df_m2 <- hp3 %>% filter(cohort %in% c('normal','hfmef'))

wilcox.test(df_m2$hf_admit_years ~ df_m2$cohort)

# low ef


df_m3 <- hp3 %>% filter(cohort %in% c('normal','hfref'))

wilcox.test(df_m3$hf_admit_years ~ df_m3$cohort)



# now to do the model for multiple groups.

df = read_csv("P:/ORD_Perez_201602128D/Deo/CABG_HF/data/composite.csv")

df$new_g[df$cohort == "normal"]<- 0
df$new_g[df$hxchf == 1 & df$lvcgrade == 1]<- 1
df$new_g[df$hxchf == 1 & df$lvcgrade == 2]<- 2
df$new_g[df$hxchf == 1 & df$lvcgrade  %in% c(3,4)]<- 3
df$new_g[df$lvcgrade == 4]<- 4
df$new_g[df$lvcgrade == 5]<- 5


df$new_g = factor(df$new_g)

# now to do the same cox model with this group.


cox_newgroup = coxph(Surv(comp_years, event) ~ new_g + age + 
               ckd + bmi + pvd + smoking + 
               diabetes + priorstroke + prior_mi + 
               priorpci + anemia + race.mod + afib + copd,
             data = df)


summary(cox_newgroup)


# new_g1         -0.013830  0.986266  0.072287 -0.191  0.84828    
# new_g2          0.049762  1.051021  0.074157  0.671  0.50220    
# new_g3          0.458752  1.582098  0.093437  4.910 9.12e-07 ***
# new_g4          0.541900  1.719270  0.049383 10.973  < 2e-16 ***
# new_g5          0.670391  1.955001  0.047667 14.064  < 2e-16 ***



#                  exp(coef)  exp(-coef) lower .95  upper .95
# new_g1            0.9863     1.0139    0.8560     1.136
# new_g2            1.0510     0.9515    0.9088     1.215
# new_g3            1.5821     0.6321    1.3173     1.900
# new_g4            1.7193     0.5816    1.5607     1.894
# new_g5            1.9550     0.5115    1.7806     2.146





##################################################################
##                      GDMT - HFpEF study                      ##
##################################################################



# looking at GDMT and then sensitivity analysis using GDMT +ve / GDMT -ve.



### medications :-

# get each medication and then identify
# will need cohort crosswalk too to get patientsid.


comp = read_csv("P:/ORD_Perez_201602128D/Deo/CABG_HF/data/composite.csv")

dt = comp # rename comp so that I do not have change the code.

cw <- read_sas("P:\\ORD_Perez_201602128D\\Deo\\CABG_HF\\data\\deo_cwalk.sas7bdat")

# now to limit to only my patients.

cw$mine <- with(cw, ifelse(ScrSSN %in% dt$scrssn, 1, 0))

cw2 = cw %>% filter(mine == 1)

psid = cw2$PatientSID

# now to get ap data.

ap = read_sas('P:\\ORD_Perez_201602128D\\Deo\\CABG_HF\\data\\aptherapy_hfpef.sas7bdat')

# limit to only my patients.

ap$mine = with(ap, ifelse(PatientSID %in% psid, 1, 0))

ap %>% count(mine)

ap2 = ap %>% filter(mine == 1)

ap2$ActionDateTime = as_date(ap2$ActionDateTime)

dt$surgdate = as_date(dt$surgdate)

dt$disd = as_date(dt$disd)

names(cw2) = tolower(names(cw2))

names(ap2) = tolower(names(ap2))

ap3 = left_join(ap2, cw2, by = "patientsid")

date = dt %>% dplyr::select(scrssn, disd, surgdate)

ap4 = left_join(ap3, date, by = "scrssn")

ap4$after = with(ap4, ifelse(actiondatetime > surgdate, 1, 0))

ap5 = ap4 %>% filter(after == 1)

ap5$before = with(ap5, ifelse(actiondatetime <= disd, 1, 0))

ap6 = ap5 %>% filter(before == 1)

ap_scrssn = ap6$scrssn

# add anti-platelet therapy to the main dataset.

dt$antiplatelet = with(dt, ifelse(scrssn %in% ap_scrssn, 1, 0))

dt %>% count(antiplatelet)

# now to go with statin therapy in the same manner...


st = read_sas('P:\\ORD_Perez_201602128D\\Deo\\CABG_HF\\data\\statintherapy_hfpef.sas7bdat')


st$mine = with(st, ifelse(PatientSID %in% psid, 1, 0))

st %>% count(mine)

st2 = st %>% filter(mine == 1)

st2$ActionDateTime = as_date(st2$ActionDateTime)

dt$surgdate = as_date(dt$surgdate)

dt$disd = as_date(dt$disd)

names(cw2) = tolower(names(cw2))

names(st2) = tolower(names(st2))

st3 = left_join(st2, cw2, by = "patientsid")

date = dt %>% dplyr::select(scrssn, disd, surgdate)

st4 = left_join(st3, date, by = "scrssn")

st4$after = with(st4, ifelse(actiondatetime > surgdate, 1, 0))

st5 = st4 %>% filter(after == 1)

st5$before = with(st5, ifelse(actiondatetime <= disd, 1, 0))

st6 = st5 %>% filter(before == 1)

st_scrssn = st6$scrssn

# add anti-lipid therapy to the main dataset.

dt$antilipid = with(dt, ifelse(scrssn %in% st_scrssn, 1, 0))

dt %>% count(antilipid)


# beta blocker therapy


bb = 
  read_sas('P:\\ORD_Perez_201602128D\\Deo\\CABG_HF\\data\\bbtherapy_hfpef.sas7bdat')


bb$mine = with(bb, ifelse(PatientSID %in% psid, 1, 0))

bb %>% count(mine)

bb2 = bb %>% filter(mine == 1)

bb2$ActionDateTime = as_date(bb2$ActionDateTime)

dt$surgdate = as_date(dt$surgdate)

dt$disd = as_date(dt$disd)

names(cw2) = tolower(names(cw2))

names(bb2) = tolower(names(bb2))

bb3 = left_join(bb2, cw2, by = "patientsid")

date = dt %>% dplyr::select(scrssn, disd, surgdate)

bb4 = left_join(bb3, date, by = "scrssn")

bb4$after = with(bb4, ifelse(actiondatetime > surgdate, 1, 0))

bb5 = bb4 %>% filter(after == 1)

bb5$before = with(bb5, ifelse(actiondatetime <= disd, 1, 0))

bb6 = bb5 %>% filter(before == 1)

bb_scrssn = bb6$scrssn

# add betablocker therapy to the main dataset.

dt$betablocker = with(dt, ifelse(scrssn %in% bb_scrssn, 1, 0))

dt %>% count(betablocker)

# ACE/ARB therapy 



ac = 
  read_sas('P:\\ORD_Perez_201602128D\\Deo\\CABG_HF\\data\\acetherapy_hfpef.sas7bdat')


ac$mine = with(ac, ifelse(PatientSID %in% psid, 1, 0))

ac %>% count(mine)

ac2 = ac %>% filter(mine == 1)

ac2$ActionDateTime = as_date(ac2$ActionDateTime)

dt$surgdate = as_date(dt$surgdate)

dt$disd = as_date(dt$disd)

names(cw2) = tolower(names(cw2))

names(ac2) = tolower(names(ac2))

ac3 = left_join(ac2, cw2, by = "patientsid")

date = dt %>% dplyr::select(scrssn, disd, surgdate)

ac4 = left_join(ac3, date, by = "scrssn")

ac4$after = with(ac4, ifelse(actiondatetime > surgdate, 1, 0))

ac5 = ac4 %>% filter(after == 1)

ac5$before = with(ac5, ifelse(actiondatetime <= disd, 1, 0))

ac6 = ac5 %>% filter(before == 1)

ac_scrssn = ac6$scrssn

# add betablocker therapy to the main dataset.

dt$ace = with(dt, ifelse(scrssn %in% ac_scrssn, 1, 0))

dt %>% count(ace)

# spironolactone



sp = 
  read_sas('P:\\ORD_Perez_201602128D\\Deo\\CABG_HF\\data\\spirotherapy_hfpef.sas7bdat')


sp$mine = with(sp, ifelse(PatientSID %in% psid, 1, 0))

sp %>% count(mine)

sp2 = sp %>% filter(mine == 1)

sp2$ActionDateTime = as_date(sp2$ActionDateTime)

dt$surgdate = as_date(dt$surgdate)

dt$disd = as_date(dt$disd)

names(cw2) = tolower(names(cw2))

names(sp2) = tolower(names(sp2))

sp3 = left_join(sp2, cw2, by = "patientsid")

date = dt %>% dplyr::select(scrssn, disd, surgdate)

sp4 = left_join(sp3, date, by = "scrssn")

sp4$after = with(sp4, ifelse(actiondatetime > surgdate, 1, 0))

sp5 = sp4 %>% filter(after == 1)

sp5$before = with(sp5, ifelse(actiondatetime <= disd, 1, 0))

sp6 = sp5 %>% filter(before == 1)

sp_scrssn = sp6$scrssn

# add betablocker therapy to the main dataset.

dt$spiro = with(dt, ifelse(scrssn %in% sp_scrssn, 1, 0))

dt %>% count(spiro)

# now to save this dataset in the rev1 folder.

write_csv(dt,
          'P:\\ORD_Perez_201602128D\\Deo\\CABG_HF\\data\\comp_meds.csv')

################ done till here. ### now to do the sensitivity analysis
# also to present discharge medications according to group.


# now am going to present the medications in the table 1.

meds = CreateCatTable(vars = c('spiro',"ace","betablocker","antilipid","antiplatelet"),
                      data = dt,
                      strata = c("cohort"))



# > meds
# Stratified by cohort
#                        hfmef        hfpef       hfref        normal       p      test
# n                    2370         667         2619         4740                    
# spiro = 1 (%)         137 ( 5.8)   27 ( 4.0)   225 ( 8.6)    96 ( 2.0)  <0.001     
# ace = 1 (%)           742 (31.3)  183 (27.4)  1018 (38.9)  1245 (26.3)  <0.001     
# betablocker = 1 (%)  2267 (95.7)  643 (96.4)  2511 (95.9)  4584 (96.7)   0.105     
# antilipid = 1 (%)    2013 (84.9)  581 (87.1)  2209 (84.3)  4095 (86.4)   0.050     
# antiplatelet = 1 (%) 2309 (97.4)  659 (98.8)  2574 (98.3)  4675 (98.6)   0.002 


dt$total = dt$ace + dt$spiro + dt$betablocker

dt$gdmt = with(dt, ifelse(total > 1, 1, 0))


meds = CreateCatTable(vars = c('spiro',"ace","betablocker","antilipid","antiplatelet","gdmt"),
                      data = dt,
                      strata = c("cohort"))


# > meds
# Stratified by cohort
#                       hfmef        hfpef       hfref        normal       p      
# n                    2370         667         2619         4740                    
# spiro = 1 (%)         137 ( 5.8)   27 ( 4.0)   225 ( 8.6)    96 ( 2.0)  <0.001     
# ace = 1 (%)           742 (31.3)  183 (27.4)  1018 (38.9)  1245 (26.3)  <0.001     
# betablocker = 1 (%)  2267 (95.7)  643 (96.4)  2511 (95.9)  4584 (96.7)   0.105     
# antilipid = 1 (%)    2013 (84.9)  581 (87.1)  2209 (84.3)  4095 (86.4)   0.050     
# antiplatelet = 1 (%) 2309 (97.4)  659 (98.8)  2574 (98.3)  4675 (98.6)   0.002     
# gdmt = 1 (%)          798 (33.7)  197 (29.5)  1099 (42.0)  1276 (26.9)  <0.001    


# sensitivity analysis.


m = dt %>% dplyr::select(scrssn, cohort, comp_years, event, 
                           bmi, obese, grafts, priorstroke, prior_mi,lvedp,cr,priorhs,
                           priorpci, anemia, race.mod, GFR, ckd, smoking, afib, age, copd, pvd, diabetes,gdmt)


glimpse(m)

missing = miss_var_summary(m)


print(missing)

# amount of missing data 

# > miss_var_summary(m)
# # A tibble: 19 x 3
# variable    n_miss pct_miss
# <chr>        <int>    <dbl>
#   #1 grafts        2580  24.4   
#   #2 GFR             14   0.132 
#   #3 ckd             14   0.132 
#   #4 anemia          10   0.0945
#   #5 bmi              5   0.0472
#   #6 obese            5   0.0472
#   #7 pvd              5   0.0472


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


model_sens = coxph(Surv(comp_years, event) ~ cohort_n + age + 
                       ckd + bmi + pvd + smoking + 
                       diabetes + priorstroke + prior_mi + 
                       priorpci + anemia + race.mod + afib + copd + gdmt,
                     data = m) 

summary(model_sens)

# > summary(model_sens)
# Call:
#   coxph(formula = Surv(comp_years, event) ~ cohort_n + age + ckd + 
#           bmi + pvd + smoking + diabetes + priorstroke + prior_mi + 
#           priorpci + anemia + race.mod + afib + copd + gdmt, data = m)
# 
# n= 10396, number of events= 4709 
# 
# coef  exp(coef)   se(coef)      z Pr(>|z|)    
# cohort_n1      -0.0270578  0.9733050  0.0721077 -0.375 0.707481    
# cohort_n2       0.4263118  1.5315983  0.0390791 10.909  < 2e-16 ***
#   cohort_n3       0.6694758  1.9532133  0.0370669 18.061  < 2e-16 ***
#   age             0.0343936  1.0349919  0.0020052 17.152  < 2e-16 ***
#   ckd             0.3559473  1.4275324  0.0379364  9.383  < 2e-16 ***
#   bmi             0.0002675  1.0002676  0.0030566  0.088 0.930253    
# pvd             0.3162800  1.3720144  0.0315510 10.024  < 2e-16 ***
#   smoking         0.1676448  1.1825165  0.0444950  3.768 0.000165 ***
#   diabetes1       0.1817078  1.1992637  0.0388900  4.672 2.98e-06 ***
#   diabetes2       0.5195410  1.6812558  0.0369123 14.075  < 2e-16 ***
#   priorstroke    -0.2488093  0.7797287  0.1485383 -1.675 0.093924 .  
# prior_mi        0.0920820  1.0964547  0.0306035  3.009 0.002622 ** 
#   priorpci       -0.0604346  0.9413554  0.0826877 -0.731 0.464854    
# anemia          0.3646673  1.4400349  0.0311267 11.716  < 2e-16 ***
#   race.modothers -0.1238546  0.8835083  0.0583660 -2.122 0.033835 *  
#   race.modwhite  -0.0351068  0.9655023  0.0522319 -0.672 0.501499    
# afib            0.1739782  1.1900297  0.0343326  5.067 4.03e-07 ***
#   copd            0.3568431  1.4288117  0.0313535 11.381  < 2e-16 ***
#   gdmt           -0.0027178  0.9972859  0.0305262 -0.089 0.929057    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# exp(coef) exp(-coef) lower .95 upper .95
# cohort_n1         0.9733     1.0274    0.8450    1.1211
# cohort_n2         1.5316     0.6529    1.4187    1.6535
# cohort_n3         1.9532     0.5120    1.8163    2.1004
# age               1.0350     0.9662    1.0309    1.0391
# ckd               1.4275     0.7005    1.3252    1.5377
# bmi               1.0003     0.9997    0.9943    1.0063
# pvd               1.3720     0.7289    1.2897    1.4595
# smoking           1.1825     0.8457    1.0838    1.2903
# diabetes1         1.1993     0.8338    1.1112    1.2942
# diabetes2         1.6813     0.5948    1.5639    1.8074
# priorstroke       0.7797     1.2825    0.5828    1.0432
# prior_mi          1.0965     0.9120    1.0326    1.1642
# priorpci          0.9414     1.0623    0.8005    1.1070
# anemia            1.4400     0.6944    1.3548    1.5306
# race.modothers    0.8835     1.1319    0.7880    0.9906
# race.modwhite     0.9655     1.0357    0.8716    1.0696
# afib              1.1900     0.8403    1.1126    1.2729
# copd              1.4288     0.6999    1.3437    1.5194
# gdmt              0.9973     1.0027    0.9394    1.0588
# 
# Concordance= 0.702  (se = 0.004 )
# Likelihood ratio test= 2188  on 19 df,   p=<2e-16
# Wald test            = 2278  on 19 df,   p=<2e-16
# Score (logrank) test = 2383  on 19 df,   p=<2e-16

# sensitivity analysis - only HFPEF cohort.

model_sens_hfpefonly = coxph(Surv(comp_years, event) ~   age + 
                     ckd + bmi + pvd + smoking + 
                     diabetes + priorstroke + prior_mi + 
                     priorpci + anemia + race.mod + afib + copd + gdmt,
                   data = m[m$cohort_n == 1, ]) 

summary(model_sens_hfpefonly)


# Call:
#   coxph(formula = Surv(comp_years, event) ~ age + ckd + bmi + pvd + 
#           smoking + diabetes + priorstroke + prior_mi + priorpci + 
#           anemia + race.mod + afib + copd + gdmt, data = m[m$cohort_n == 
#                                                              1, ])
# 
# n= 667, number of events= 221 
# 
# coef exp(coef) se(coef)      z Pr(>|z|)    
# age             0.05847   1.06021  0.01018  5.741 9.39e-09 ***
#   ckd             0.44916   1.56700  0.18127  2.478 0.013215 *  
#   bmi             0.01415   1.01425  0.01314  1.077 0.281453    
# pvd             0.47975   1.61566  0.14581  3.290 0.001001 ** 
#   smoking         0.23949   1.27060  0.20292  1.180 0.237909    
# diabetes1       0.32391   1.38252  0.18075  1.792 0.073136 .  
# diabetes2       0.42765   1.53364  0.17431  2.453 0.014152 *  
#   priorstroke    -0.92191   0.39776  1.00783 -0.915 0.360320    
# prior_mi        0.08322   1.08679  0.14433  0.577 0.564198    
# priorpci       -0.47003   0.62498  0.41807 -1.124 0.260886    
# anemia          0.56748   1.76382  0.14708  3.858 0.000114 ***
#   race.modothers  0.28488   1.32960  0.31099  0.916 0.359643    
# race.modwhite   0.24868   1.28234  0.28457  0.874 0.382173    
# afib            0.07750   1.08058  0.15724  0.493 0.622114    
# copd            0.26112   1.29838  0.14934  1.749 0.080370 .  
# gdmt           -0.21433   0.80709  0.15433 -1.389 0.164904    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# exp(coef) exp(-coef) lower .95 upper .95
# age               1.0602     0.9432   1.03926     1.082
# ckd               1.5670     0.6382   1.09843     2.235
# bmi               1.0143     0.9859   0.98847     1.041
# pvd               1.6157     0.6189   1.21406     2.150
# smoking           1.2706     0.7870   0.85366     1.891
# diabetes1         1.3825     0.7233   0.97009     1.970
# diabetes2         1.5336     0.6520   1.08982     2.158
# priorstroke       0.3978     2.5141   0.05518     2.867
# prior_mi          1.0868     0.9201   0.81901     1.442
# priorpci          0.6250     1.6000   0.27543     1.418
# anemia            1.7638     0.5670   1.32209     2.353
# race.modothers    1.3296     0.7521   0.72278     2.446
# race.modwhite     1.2823     0.7798   0.73414     2.240
# afib              1.0806     0.9254   0.79399     1.471
# copd              1.2984     0.7702   0.96892     1.740
# gdmt              0.8071     1.2390   0.59642     1.092
# 
# Concordance= 0.709  (se = 0.019 )
# Likelihood ratio test= 126.3  on 16 df,   p=<2e-16
# Wald test            = 131.6  on 16 df,   p=<2e-16
# Score (logrank) test = 139.1  on 16 df,   p=<2e-16

# HFMEF only 


model_sens_hfmefonly = coxph(Surv(comp_years, event) ~  age + 
                     ckd + bmi + pvd + smoking + 
                     diabetes + priorstroke + prior_mi + 
                     priorpci + anemia + race.mod + afib + copd + gdmt,
                   data = m[m$cohort_n == 2, ]) 

summary(model_sens_hfmefonly)
# 
# 
# Call:
#   coxph(formula = Surv(comp_years, event) ~ age + ckd + bmi + pvd + 
#           smoking + diabetes + priorstroke + prior_mi + priorpci + 
#           anemia + race.mod + afib + copd + gdmt, data = m[m$cohort_n == 
#                                                              2, ])
# 
# n= 2370, number of events= 1221 
# 
# coef exp(coef)  se(coef)      z Pr(>|z|)    
# age             0.043307  1.044258  0.003928 11.026  < 2e-16 ***
#   ckd             0.309050  1.362130  0.073237  4.220 2.44e-05 ***
#   bmi             0.007563  1.007592  0.005681  1.331 0.183033    
# pvd             0.321440  1.379112  0.061461  5.230 1.70e-07 ***
#   smoking         0.225124  1.252478  0.086631  2.599 0.009359 ** 
#   diabetes1       0.291804  1.338841  0.076329  3.823 0.000132 ***
#   diabetes2       0.619654  1.858285  0.071795  8.631  < 2e-16 ***
#   priorstroke    -0.636732  0.529018  0.355218 -1.793 0.073051 .  
# prior_mi        0.071196  1.073791  0.058719  1.212 0.225326    
# priorpci        0.229023  1.257371  0.161986  1.414 0.157407    
# anemia          0.338859  1.403345  0.060746  5.578 2.43e-08 ***
#   race.modothers -0.294056  0.745235  0.116561 -2.523 0.011643 *  
#   race.modwhite  -0.182764  0.832965  0.101842 -1.795 0.072721 .  
# afib            0.279440  1.322389  0.065627  4.258 2.06e-05 ***
#   copd            0.330446  1.391588  0.060119  5.497 3.87e-08 ***
#   gdmt            0.043506  1.044466  0.059533  0.731 0.464909    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# exp(coef) exp(-coef) lower .95 upper .95
# age               1.0443     0.9576    1.0363    1.0523
# ckd               1.3621     0.7341    1.1800    1.5724
# bmi               1.0076     0.9925    0.9964    1.0189
# pvd               1.3791     0.7251    1.2226    1.5557
# smoking           1.2525     0.7984    1.0569    1.4843
# diabetes1         1.3388     0.7469    1.1528    1.5549
# diabetes2         1.8583     0.5381    1.6144    2.1391
# priorstroke       0.5290     1.8903    0.2637    1.0613
# prior_mi          1.0738     0.9313    0.9571    1.2048
# priorpci          1.2574     0.7953    0.9153    1.7272
# anemia            1.4033     0.7126    1.2458    1.5808
# race.modothers    0.7452     1.3419    0.5930    0.9365
# race.modwhite     0.8330     1.2005    0.6822    1.0170
# afib              1.3224     0.7562    1.1628    1.5039
# copd              1.3916     0.7186    1.2369    1.5656
# gdmt              1.0445     0.9574    0.9294    1.1737
# 
# Concordance= 0.68  (se = 0.008 )
# Likelihood ratio test= 490.4  on 16 df,   p=<2e-16
# Wald test            = 499.6  on 16 df,   p=<2e-16
# Score (logrank) test = 516.1  on 16 df,   p=<2e-16


# HFREF only:


model_sens_hfrefonly = coxph(Surv(comp_years, event) ~   age + 
                               ckd + bmi + pvd + smoking + 
                               diabetes + priorstroke + prior_mi + 
                               priorpci + anemia + race.mod + afib + copd + gdmt,
                             data = m[m$cohort_n == 3, ]) 

summary(model_sens_hfrefonly)

