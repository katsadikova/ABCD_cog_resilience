#----------------------------------------------------------------------------#
#--- 3_theory_based_clusters.R
#--- Date: 7/27/2023
#----------------------------------------------------------------------------#

library(tidyverse)
library(kableExtra)
library(gtsummary)
library(expss)
library(haven)
library(sjlabelled)
library(readxl)
library(gtools)
library(tableone)
library(mice)
library(HIMA)
library(corrplot)
library(reshape2)
library(mclust)
library(mlogit)
library(tidyLPA)

load("/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/temp_dat/dat.Rda")

names(dat)

#--------------------------------------------------------------------------#
#-- First test 4-cluster option 1: 
# (1) Bottom 40% INR, top 40% total cog 
# (2) Top 40% INR, top 40% total cog 
# (3) Bottom 40% INR, bottom 40% total cog 
# (4) Top 40% INR, bottom 40% total cog 

#-- Exclude rows with missing data (for now)
dat <- na.omit(dat[,c(1,5,8:13)]) %>%
  mutate(fluid_s = as.numeric(scale(nihtbx_fluidcomp_agecorrected)),
         cryst_s = as.numeric(scale(nihtbx_cryst_agecorrected)),
         totcomp_s = as.numeric(scale(nihtbx_totalcomp_agecorrected)),
         inr_quint = cut(inr, unique(quantile(inr, seq(0, 1, 0.2))), labels = FALSE),
         totalcomp_quint = cut(nihtbx_totalcomp_agecorrected, unique(quantile(nihtbx_totalcomp_agecorrected, seq(0, 1, 0.2))), labels = FALSE),
         #-- Identify those who are in bottom 2 quintiles of INR, but top 2 quintiles of total cog
         pov_highcog = ifelse(inr_quint<3 & totalcomp_quint>3, 1, 0),
         #-- Identify those who are in bottom 2 quintiles of INR, and bottom 2 quintiles of total cog
         pov_lowcog = ifelse(inr_quint<3 & totalcomp_quint<3, 1, 0),
         #-- Identify those who are in top 2 quintiles of INR, and top 2 quintiles of total cog
         welloff_highcog = ifelse(inr_quint>3 & totalcomp_quint>3, 1, 0),
         #-- Identify those who are in top 2 quintiles of INR, and bottom 2 quintiles of total cog
         welloff_lowcog = ifelse(inr_quint>3 & totalcomp_quint<3, 1, 0))

dat %>% reframe(quantile = scales::percent(c(0.2,0.4,0.6,0.8,1)),
                inr_quart = quantile(inr, c(0.2,0.4,0.6,0.8,1), na.rm=T),
                totalcomp_quart = quantile(nihtbx_totalcomp_agecorrected, c(0.2,0.4,0.6,0.8,1), na.rm=T))



summary(as.factor(dat$pov_highcog)) #949
summary(as.factor(dat$pov_lowcog)) #2263
summary(as.factor(dat$welloff_highcog)) #1901
summary(as.factor(dat$welloff_lowcog)) #882

d_pov_highcog <- dat %>% filter(pov_highcog==1) %>% 
  mutate(profile="pov_highcog")
d_pov_lowcog <- dat %>% filter(pov_lowcog==1) %>% 
  mutate(profile="pov_lowcog")
d_welloff_highcog <- dat %>% filter(welloff_highcog==1) %>% 
  mutate(profile="welloff_highcog")
d_welloff_lowcog <- dat %>% filter(welloff_lowcog==1) %>% 
  mutate(profile="welloff_lowcog")

d_pov <- rbind(d_pov_highcog,d_pov_lowcog,d_welloff_highcog,d_welloff_lowcog)

#-- Look at distributions
names(d_pov)

vars <- names(d_pov)[c(2,3,5:8)]

## Create Table 1a stratified by poverty
table_by_pov <- CreateTableOne(vars = vars,
                               strata = c("profile"), 
                               includeNA = F, 
                               addOverall = T,
                               data = d_pov)
table_by_pov <- data.frame(print(table_by_pov, missing=F, catDigits = 1, contDigits = 1))
table_by_pov <- tibble::rownames_to_column(table_by_pov, "Characteristic")

names(table_by_pov) <- c("Characteristic", "Overall", "Poverty_High_Cog", "Poverty_Low_Cog","Welloff_High_Cog","Welloff_Low_Cog", "p_value")
table_by_pov


table_by_pov <- data.frame(table_by_pov) %>%
  mutate(
    rownames = case_when(
      Characteristic=="inr (mean (SD))" ~ "Income-to-needs ratio (mean (SD))",
      Characteristic=="p_ed (mean (SD))" ~ "Parental education - highest among parents/caretakers (mean (SD))",
      Characteristic=="reshist_addr1_adi_perc_r (mean (SD))" ~ "Reverse-coded ADI for the primary residence (mean (SD))",
      Characteristic=="nihtbx_fluidcomp_agecorrected (mean (SD))" ~ "NIH toolbox: fluid intelligence composite, age corrected (mean (SD))",
      Characteristic=="nihtbx_cryst_agecorrected (mean (SD))" ~ "NIH toolbox: crystallized intelligence composite, age corrected (mean (SD))",
      Characteristic=="nihtbx_totalcomp_agecorrected (mean (SD))" ~ "NIH toolbox: total intelligence composite, age corrected (mean (SD))",
      T ~ Characteristic
    )
  ) %>%
  dplyr::select(rownames,Poverty_High_Cog,Poverty_Low_Cog,Welloff_High_Cog,Welloff_Low_Cog,Overall,p_value)

table_by_pov
write.csv(table_by_pov, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/table_cog_INR_40%.csv")


#--------------------------------------------------------------------------#
#-- Next test 4-cluster option 2: 
# (1) Resilient group: bottom 25% INR, above average total cog 
# (2) bottom 25% INR, below avergage total cog 
# (3) top 25% INR, above avg total cog 
# (4) top 25% INR, below avg total cog


dat %>% reframe(quantile = scales::percent(c(0.25,0.5,0.75,1)),
                inr_quart = quantile(inr, c(0.25,0.5,0.75,1), na.rm=T),
                totalcomp_quart = quantile(nihtbx_totalcomp_agecorrected, c(0.25,0.5,0.75,1), na.rm=T))
mean(dat$nihtbx_totalcomp_agecorrected, na.rm=T)

names(dat)

#-- Append INR quartiles, above/below avg tot cog, and option 2 for clusters
dat <- dat %>%
  mutate(inr_quart = cut(inr, unique(quantile(inr, seq(0, 1, 0.25))), labels = FALSE),
         totalcomp_avg = ifelse(nihtbx_totalcomp_agecorrected<100.3784,0,1),
         #-- Identify those who are in bottom quartile of INR, but above average on total cog
         pov_highcog2 = ifelse(inr_quart<2 & totalcomp_avg>0, 1, 0),
         #-- Identify those who are in bottom quartile of INR, and below average on total cog
         pov_lowcog2 = ifelse(inr_quart<2 & totalcomp_avg<1, 1, 0),
         #-- Identify those who are in top quartile of INR, and above average on total cog
         welloff_highcog2 = ifelse(inr_quart>3 & totalcomp_avg>0, 1, 0),
         #-- Identify those who are in top 2 quintiles of INR, and below average on total cog
         welloff_lowcog2 = ifelse(inr_quart>3 & totalcomp_avg<1, 1, 0))
names(dat)

summary(as.factor(dat$pov_highcog2)) #673
summary(as.factor(dat$pov_lowcog2)) #1785
summary(as.factor(dat$welloff_highcog2)) #889
summary(as.factor(dat$welloff_lowcog2)) #365

d_pov_highcog <- dat %>% filter(pov_highcog2==1) %>% 
  mutate(profile="pov_highcog")
d_pov_lowcog <- dat %>% filter(pov_lowcog2==1) %>% 
  mutate(profile="pov_lowcog")
d_welloff_highcog <- dat %>% filter(welloff_highcog2==1) %>% 
  mutate(profile="welloff_highcog")
d_welloff_lowcog <- dat %>% filter(welloff_lowcog2==1) %>% 
  mutate(profile="welloff_lowcog")

d_pov <- rbind(d_pov_highcog,d_pov_lowcog,d_welloff_highcog,d_welloff_lowcog)

#-- Look at distributions
names(d_pov)

vars <- names(d_pov)[c(2,3,5:8)]

## Create Table 1a stratified by poverty
table_by_pov <- CreateTableOne(vars = vars,
                               strata = c("profile"), 
                               includeNA = F, 
                               addOverall = T,
                               data = d_pov)
table_by_pov <- data.frame(print(table_by_pov, missing=F, catDigits = 1, contDigits = 1))
table_by_pov <- tibble::rownames_to_column(table_by_pov, "Characteristic")

names(table_by_pov) <- c("Characteristic", "Overall", "Poverty_High_Cog", "Poverty_Low_Cog","Welloff_High_Cog","Welloff_Low_Cog", "p_value")
table_by_pov


table_by_pov <- data.frame(table_by_pov) %>%
  mutate(
    rownames = case_when(
      Characteristic=="inr (mean (SD))" ~ "Income-to-needs ratio (mean (SD))",
      Characteristic=="p_ed (mean (SD))" ~ "Parental education - highest among parents/caretakers (mean (SD))",
      Characteristic=="reshist_addr1_adi_perc_r (mean (SD))" ~ "Reverse-coded ADI for the primary residence (mean (SD))",
      Characteristic=="nihtbx_fluidcomp_agecorrected (mean (SD))" ~ "NIH toolbox: fluid intelligence composite, age corrected (mean (SD))",
      Characteristic=="nihtbx_cryst_agecorrected (mean (SD))" ~ "NIH toolbox: crystallized intelligence composite, age corrected (mean (SD))",
      Characteristic=="nihtbx_totalcomp_agecorrected (mean (SD))" ~ "NIH toolbox: total intelligence composite, age corrected (mean (SD))",
      T ~ Characteristic
    )
  ) %>%
  dplyr::select(rownames,Poverty_High_Cog,Poverty_Low_Cog,Welloff_High_Cog,Welloff_Low_Cog,Overall,p_value)

table_by_pov
write.csv(table_by_pov, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/table_cog_avg_INR_25%.csv")


#--------------------------------------------------------------------------#
#-- Next test 4-cluster option 3:
#-- cog_median_INR_25%

# (1) Resilient group: bottom 25% INR, above MEDIAN total cog 
# (2) bottom 25% INR, below MEDIAN total cog 
# (3) top 25% INR, above MEDIAN total cog 
# (4) top 25% INR, below MEDIAN total cog


#-- Append above/below median tot cog, and option 3 for clusters
dat <- dat %>%
  mutate(totalcomp_med = cut(nihtbx_totalcomp_agecorrected, unique(quantile(nihtbx_totalcomp_agecorrected, seq(0, 1, 0.5))), labels = FALSE),
         #-- Identify those who are in bottom quartile of INR, but above median on total cog
         pov_highcog3 = ifelse(inr_quart==1 & totalcomp_med==2, 1, 0),
         #-- Identify those who are in bottom quartile of INR, and below median on total cog
         pov_lowcog3 = ifelse(inr_quart==1 & totalcomp_med==1, 1, 0),
         #-- Identify those who are in top quartile of INR, and above median on total cog
         welloff_highcog3 = ifelse(inr_quart==4 & totalcomp_med==2, 1, 0),
         #-- Identify those who are in top 2 quintiles of INR, and below median on total cog
         welloff_lowcog3 = ifelse(inr_quart==4 & totalcomp_med==1, 1, 0))
names(dat)

table(dat$totalcomp_avg,dat$totalcomp_med)
summary(as.factor(dat$totalcomp_med))

summary(as.factor(dat$pov_highcog3)) #634
summary(as.factor(dat$pov_lowcog3)) #1822
summary(as.factor(dat$welloff_highcog3)) #858
summary(as.factor(dat$welloff_lowcog3)) #396

d_pov_highcog <- dat %>% filter(pov_highcog3==1) %>% 
  mutate(profile="pov_highcog")
d_pov_lowcog <- dat %>% filter(pov_lowcog3==1) %>% 
  mutate(profile="pov_lowcog")
d_welloff_highcog <- dat %>% filter(welloff_highcog3==1) %>% 
  mutate(profile="welloff_highcog")
d_welloff_lowcog <- dat %>% filter(welloff_lowcog3==1) %>% 
  mutate(profile="welloff_lowcog")

d_pov <- rbind(d_pov_highcog,d_pov_lowcog,d_welloff_highcog,d_welloff_lowcog)

#-- Look at distributions
names(d_pov)

vars <- names(d_pov)[c(2,3,5:8)]

## Create Table 1a stratified by poverty
table_by_pov <- CreateTableOne(vars = vars,
                               strata = c("profile"), 
                               includeNA = F, 
                               addOverall = T,
                               data = d_pov)
table_by_pov <- data.frame(print(table_by_pov, missing=F, catDigits = 1, contDigits = 1))
table_by_pov <- tibble::rownames_to_column(table_by_pov, "Characteristic")

names(table_by_pov) <- c("Characteristic", "Overall", "Poverty_High_Cog", "Poverty_Low_Cog","Welloff_High_Cog","Welloff_Low_Cog", "p_value")
table_by_pov


table_by_pov <- data.frame(table_by_pov) %>%
  mutate(
    rownames = case_when(
      Characteristic=="inr (mean (SD))" ~ "Income-to-needs ratio (mean (SD))",
      Characteristic=="p_ed (mean (SD))" ~ "Parental education - highest among parents/caretakers (mean (SD))",
      Characteristic=="reshist_addr1_adi_perc_r (mean (SD))" ~ "Reverse-coded ADI for the primary residence (mean (SD))",
      Characteristic=="nihtbx_fluidcomp_agecorrected (mean (SD))" ~ "NIH toolbox: fluid intelligence composite, age corrected (mean (SD))",
      Characteristic=="nihtbx_cryst_agecorrected (mean (SD))" ~ "NIH toolbox: crystallized intelligence composite, age corrected (mean (SD))",
      Characteristic=="nihtbx_totalcomp_agecorrected (mean (SD))" ~ "NIH toolbox: total intelligence composite, age corrected (mean (SD))",
      T ~ Characteristic
    )
  ) %>%
  dplyr::select(rownames,Poverty_High_Cog,Poverty_Low_Cog,Welloff_High_Cog,Welloff_Low_Cog,Overall,p_value)

table_by_pov
write.csv(table_by_pov, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/table_cog_median_INR_25%.csv")


#--------------------------------------------------------------------------#
#-- Next test 4-cluster option 4:
#-- cog_median_INR_40%

# (1) Resilient group: bottom 25% INR, above MEDIAN total cog 
# (2) bottom 25% INR, below MEDIAN total cog 
# (3) top 25% INR, above MEDIAN total cog 
# (4) top 25% INR, below MEDIAN total cog


#-- Append above/below median tot cog, and option 3 for clusters
dat <- dat %>%
  mutate(#-- Identify those who are in bottom 40% of INR, but above median on total cog
         pov_highcog4 = ifelse(inr_quint<3 & totalcomp_med==2, 1, 0),
         #-- Identify those who are in bottom 40% of INR, and below median on total cog
         pov_lowcog4 = ifelse(inr_quint<3 & totalcomp_med==1, 1, 0),
         #-- Identify those who are in top 40% of INR, and above median on total cog
         welloff_highcog4 = ifelse(inr_quint>3 & totalcomp_med==2, 1, 0),
         #-- Identify those who are in top 40% of INR, and below median on total cog
         welloff_lowcog4 = ifelse(inr_quint>3 & totalcomp_med==1, 1, 0))
names(dat)

summary(as.factor(dat$pov_highcog4)) #1264
summary(as.factor(dat$pov_lowcog4)) #2663
summary(as.factor(dat$welloff_highcog4)) #2233
summary(as.factor(dat$welloff_lowcog4)) #1284

d_pov_highcog <- dat %>% filter(pov_highcog4==1) %>% 
  mutate(profile="pov_highcog")
d_pov_lowcog <- dat %>% filter(pov_lowcog4==1) %>% 
  mutate(profile="pov_lowcog")
d_welloff_highcog <- dat %>% filter(welloff_highcog4==1) %>% 
  mutate(profile="welloff_highcog")
d_welloff_lowcog <- dat %>% filter(welloff_lowcog4==1) %>% 
  mutate(profile="welloff_lowcog")

d_pov <- rbind(d_pov_highcog,d_pov_lowcog,d_welloff_highcog,d_welloff_lowcog)

#-- Look at distributions
names(d_pov)

vars <- names(d_pov)[c(2,3,5:8)]

## Create Table 1a stratified by poverty
table_by_pov <- CreateTableOne(vars = vars,
                               strata = c("profile"), 
                               includeNA = F, 
                               addOverall = T,
                               data = d_pov)
table_by_pov <- data.frame(print(table_by_pov, missing=F, catDigits = 1, contDigits = 1))
table_by_pov <- tibble::rownames_to_column(table_by_pov, "Characteristic")

names(table_by_pov) <- c("Characteristic", "Overall", "Poverty_High_Cog", "Poverty_Low_Cog","Welloff_High_Cog","Welloff_Low_Cog", "p_value")
table_by_pov


table_by_pov <- data.frame(table_by_pov) %>%
  mutate(
    rownames = case_when(
      Characteristic=="inr (mean (SD))" ~ "Income-to-needs ratio (mean (SD))",
      Characteristic=="p_ed (mean (SD))" ~ "Parental education - highest among parents/caretakers (mean (SD))",
      Characteristic=="reshist_addr1_adi_perc_r (mean (SD))" ~ "Reverse-coded ADI for the primary residence (mean (SD))",
      Characteristic=="nihtbx_fluidcomp_agecorrected (mean (SD))" ~ "NIH toolbox: fluid intelligence composite, age corrected (mean (SD))",
      Characteristic=="nihtbx_cryst_agecorrected (mean (SD))" ~ "NIH toolbox: crystallized intelligence composite, age corrected (mean (SD))",
      Characteristic=="nihtbx_totalcomp_agecorrected (mean (SD))" ~ "NIH toolbox: total intelligence composite, age corrected (mean (SD))",
      T ~ Characteristic
    )
  ) %>%
  dplyr::select(rownames,Poverty_High_Cog,Poverty_Low_Cog,Welloff_High_Cog,Welloff_Low_Cog,Overall,p_value)

table_by_pov
write.csv(table_by_pov, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/table_cog_median_INR_40%.csv")
