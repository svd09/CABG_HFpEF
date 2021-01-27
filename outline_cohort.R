############################
# Script for HF CABG paper #
############################

# This is the script for the CABG and HF failure paper.

library(easypackages)
libraries(c('tidyverse','rms','naniar','Hmisc','MASS', 'tableone','haven',"lubridate",
"naniar", "survminer"))


#- get the base data to see the dates and other information.

df = read_sas("P:/ORD_Perez_201602128D/Deo/CABG_HF/data/deo_hfcabg2.sas7bdat")

dim(df)



# convert names to lower case.

names(df) = tolower(names(df))

glimpse(df)

summary(df$surgdate) # surgery date 

# see the dates and then decide.


chf = read_sas('P:/ORD_Perez_201602128D/Deo/CABG_HF/data/deo_myfailuregroup.sas7bdat')

glimpse(chf)

names(chf) = tolower(names(chf))

chf2 = distinct(chf, scrssn, .keep_all = TRUE) # keep only disctinct scrssn here 

dim(chf2)


# look at missing information from hxchf

chf2 %>% count(hxchf)

# am going to join both data now.

df2 = left_join(df, chf2, by = "scrssn")

dim(df2)

glimpse(df2)

# now df2 contains joined information regarding chf prior history 

# we need to remove those with h/o chf missing 

df3 = df2[!is.na(df2$hxchf), ]

df3 %>% count(hxchf)

dim(df3) # 42912 patients now in our data with history of chf 

glimpse(df3) # convert to lowercase

names(df3) = tolower(names(df3))

summary(df3$surgdate)

dim(df3)

# still remains with surgery date till 2019 / 42912 patients till now 

df3 %>% count(lvcgrade) #  NA  4210 --- so now need to remove these patients 

df4 = df3[!is.na(df3$lvcgrade), ]

# so now df4 contains patients with no missing lv grade information 

df4$lvef[df4$lvcgrade == 1]<- 1
df4$lvef[df4$lvcgrade %in% c(2,3)]<- 2
df4$lvef[df4$lvcgrade %in% c(4,5,6)]<- 3

# lvef grades 

df4 %>% count(lvef)

# now to see and identify only cabg patients 
#- however, we need to now identify those with only cabg surgery.
#- however, using cabgonly we end up loosing patients after 2014. 
#- hence we will use the icd codes to ensure that the patients did not have another procedure
#- along with CABG.

table(df4$cpt01)

cabg = c(33510:33516, 33533:33536, 33517:33523)

df4$CABG = with(df4, ifelse((cpt01 %in% cabg | cpt02 %in% cabg | cpt03 %in% cabg),
                            1,0))


df4 %>% count(CABG)


valve <- c(33364, 33427, 33426)


df4$VALVE <- with(df4, ifelse((cpt01 %in% valve | cpt02 %in% valve | cpt03 %in% valve),
                              1,0))

df4 %>% count(VALVE)

table(df4$CABG, df4$VALVE)

#- now we have 32,024 patients that have isolated CABG without valve procedures.
# 
#        0     1
#  0  5838   320
#  1 32024   520

# we need to now keep those patients 

df5 = df4 %>% dplyr::filter(CABG == 1 & VALVE == 0)

summary(df5$surgdate)

# now the df5 contains only isolated CABG with no valve surgery done.
# this is going to the main group; now to look at diuretic therapy and then obtain groups now 


# remove patients according to the information above

# - the algorithm for creating the groups is:

# 1. HFpEF = lvef == 1 & h/o CHF AND (diuretic therapy) 

# 2. midrange EF = lvef == 2 & h/o CHF AND (diuretic therapy)

# 3. low EF = lvef == 3 

# For patients with low and midrange we should remove those patients with STEMI and maybe NSTEMI diagnosis

glimpse(df5)

stemi = c(410.1,410.2, 410.5, 410.7, 410.8, 410.9)
  
df5$stemi = with(df5, ifelse(icd901 %in% stemi , 1, 0))

df5 %>% count(stemi)

df5$nstemi = with(df5, ifelse(icd901 == 410.71, 1, 0))

df5 %>% count(nstemi)

#- remove the patients with nstemi 1384. 

df6 = df5 %>% filter(nstemi == 0) # df6 now contains patients without stemi/nstemi

df6 %>% count(curdiur)

df6$high_lvedp = with(df6, ifelse(lvedp > 15, 1, 0))

df6 %>% count(high_lvedp)


# df6 now contains no stemi/nstemi patients.


#################################################################
# remove nyha class 3/4 in normal patients  + mitreg 2/3    #####
# HFPEF = only diuretic therapy                             #####
# remove priority === 3                                     #####
# HFrEF === EF < 40%                                        #####
#################################################################


# to identify hfpef patients 

# HFPEF

df6$hfpef = with(df6, ifelse(lvef == 1 & hxchf == 1 & curdiur == 1,
1, 0))

df6 %>% count(hfpef)

# HFmEF

df6$hfmef = with(df6, ifelse((lvef == 2 & (hxchf == 1|curdiur == 1)), 1, 0))

df6 %>% count(hfmef)

df6$hfref = with(df6, ifelse((lvef == 3), 1, 0))

df6 %>% count(hfref)

# actually am going to limit the data to 2010 onwards

df6$surgdate = as_date(df6$surgdate)

summary(df6$surgdate)

# limit to 2005 onwards

df7 = df6 %>% filter(surgdate > "2005-01-01")

dim(df7)


# WE NOW HAVE 3 GROUPS HFPEF, HFMEF, HFREF.

df7 %>% count(hfpef)
df7 %>% count(hfref)
df7 %>% count(hfmef)

# remove priority == 3

df7 = df7 %>% filter(priority != 3)


df7 %>% count(mitreg)

df7 %>% count(fcc)

df7$nyha[df7$fcc %in% c(0,1)]<- 1
df7$nyha[df7$fcc == 2]<- 2
df7$nyha[df7$fcc == 3]<- 3
df7$nyha[df7$fcc > 3]<- 4
df7$nyha[is.na(df7$fcc)]<- 1


df7 %>% count(nyha)

dim(df7)



# for normal patients, LVEF > 55% and no HF and no diuretic therapy 

df7$normal = with(df7, ifelse((lvef == 1 & hxchf != 1 & curdiur != 1 & (nyha %in% c(1,2))), 1, 0))

df7 %>% count(normal)


table(df7$normal, df7$hfpef)


# also remove mitreg 2/3 in normal 

df7$remove = with(df7, ifelse((normal == 1 & mitreg %in% c(2,3)), 1, 0))

df7 %>% count(remove)

# remove 

df7 = df7 %>% filter(remove != 1)

df7$cohort[df7$hfpef == 1]<- "hfpef"
df7$cohort[df7$hfmef == 1]<- "hfmef"
df7$cohort[df7$hfref == 1]<- "hfref"
df7$cohort[df7$normal == 1]<- "normal"

df7 %>% count(cohort)

# remove those with NA here 

df7 = df7[!is.na(df7$cohort), ]


dim(df7)


# get the folowup information


fup = read_sas("P:\\ORD_Perez_201602128D\\Deo\\CABG_HF\\data\\deo_hf_fup2.sas7bdat")


glimpse(fup)

names(fup) = tolower(names(fup))

# now to join both files 


df8 = left_join(df7, fup, by = "scrssn")

glimpse(df8)

df8$act_last_dt = as_date(df8$act_last_dt)

df8$fupdays = (df8$surgdate %--% df8$act_last_dt)/ddays(1)

describe(df8$fupdays)

df8$fupyears = (df8$fupdays/365.25) + 0.1

describe(df8$fupyears)

# now to convert living to died indicator 

df8$died = with(df8, ifelse(living_ind == 1, 0, 1))

df8 %>% count(died)

# now see the survival 

surv = survfit(Surv(fupyears, died) ~ 1, data = df8)

ggsurvplot(surv, risk.table = T)


survc = survfit(Surv(fupyears, died) ~ cohort, data = df8)

ggsurvplot(survc, risk.table = T, censor.size = 0, conf.int = T, surv.scale = "percent")

# now the df8 is the final data for the further analysis. Am going to save it the code_paper folder.
# 

write_csv(df8, "P:\\ORD_Perez_201602128D\\Deo\\CABG_HF\\coding_paper\\hfpef1.csv")



