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
#-- After call with Kate on 8/30 - group according to fluid/crystallized separately
#-- fluid_cog_avg_INR_25%

# (1) Resilient group: bottom 25% INR, above avg fluid cog 
# (2) bottom 25% INR, below avg fluid cog 
# (3) top 25% INR, above avg fluid cog 
# (4) top 25% INR, below avg fluid cog

#-- Exclude rows with missing data (for now)
dat <- na.omit(dat[,c(1,5,8:13)]) %>%
  mutate(fluid_s = as.numeric(scale(nihtbx_fluidcomp_agecorrected)),
         cryst_s = as.numeric(scale(nihtbx_cryst_agecorrected)),
         totcomp_s = as.numeric(scale(nihtbx_totalcomp_agecorrected))
         ) %>%
  
  # New mutate to get distributions of INR/cog variables in data with missing rows dropped
  mutate(
    inr_quart = factor(Hmisc::cut2(inr, g = 4), labels = c(1:4)),
    
    #-- Clusters based on INR and fluid cog
    #-- Identify those who are in bottom quartile of INR, but above average on fluid cog
    pov_highflcog = ifelse(inr_quart==1 & nihtbx_fluidcomp_agecorrected>mean(nihtbx_fluidcomp_agecorrected,na.rm=T), 1, 0),
    #-- Identify those who are in bottom quartile of INR, and below average on fluid cog
    pov_lowflcog = ifelse(inr_quart==1 & nihtbx_fluidcomp_agecorrected<=mean(nihtbx_fluidcomp_agecorrected,na.rm=T), 1, 0),
    #-- Identify those who are in top quartile of INR, and above average on fluid cog
    welloff_highflcog = ifelse(inr_quart==4 & nihtbx_fluidcomp_agecorrected>mean(nihtbx_fluidcomp_agecorrected,na.rm=T), 1, 0),
    #-- Identify those who are in top quartile of INR, and below average on fluid cog
    welloff_lowflcog = ifelse(inr_quart==4 & nihtbx_fluidcomp_agecorrected<=mean(nihtbx_fluidcomp_agecorrected,na.rm=T), 1, 0),
    
    #-- Clusters based on INR and crystallized cog
    #-- Identify those who are in bottom quartile of INR, but above average on crystallized cog
    pov_highcrcog = ifelse(inr_quart==1 & nihtbx_cryst_agecorrected>mean(nihtbx_cryst_agecorrected,na.rm=T), 1, 0),
    #-- Identify those who are in bottom quartile of INR, and below average on crystallized cog
    pov_lowcrcog = ifelse(inr_quart==1 & nihtbx_cryst_agecorrected<=mean(nihtbx_cryst_agecorrected,na.rm=T), 1, 0),
    #-- Identify those who are in top quartile of INR, and above average on crystallized cog
    welloff_highcrcog = ifelse(inr_quart==4 & nihtbx_cryst_agecorrected>mean(nihtbx_cryst_agecorrected,na.rm=T), 1, 0),
    #-- Identify those who are in top quartile of INR, and below average on crystallized cog
    welloff_lowcrcog = ifelse(inr_quart==4 & nihtbx_cryst_agecorrected<=mean(nihtbx_cryst_agecorrected,na.rm=T), 1, 0)
    
    )

summary(as.factor(dat$pov_highflcog))
summary(as.factor(dat$pov_lowflcog))
summary(as.factor(dat$welloff_highflcog))
summary(as.factor(dat$welloff_lowflcog))

summary(as.factor(dat$pov_highcrcog))
summary(as.factor(dat$pov_lowcrcog))
summary(as.factor(dat$welloff_highcrcog))
summary(as.factor(dat$welloff_lowcrcog))


#-- Distributions of cog & SES variables by FLUID cog clusters

d_pov_highflcog <- dat %>% filter(pov_highflcog==1) %>% 
  mutate(profile="pov_highflcog")
d_pov_lowflcog <- dat %>% filter(pov_lowflcog==1) %>% 
  mutate(profile="pov_lowflcog")
d_welloff_highflcog <- dat %>% filter(welloff_highflcog==1) %>% 
  mutate(profile="welloff_highflcog")
d_welloff_lowflcog <- dat %>% filter(welloff_lowflcog==1) %>% 
  mutate(profile="welloff_lowflcog")

d_flpov <- rbind(d_pov_highflcog,d_pov_lowflcog,d_welloff_highflcog,d_welloff_lowflcog)

#-- Look at distributions
names(d_flpov)

vars <- names(d_flpov)[c(2,3,5:8)]

## Create Table 1a stratified by poverty
table_by_pov <- CreateTableOne(vars = vars,
                               strata = c("profile"), 
                               includeNA = F, 
                               addOverall = T,
                               data = d_flpov)
table_by_pov <- data.frame(print(table_by_pov, missing=F, catDigits = 1, contDigits = 1))
table_by_pov <- tibble::rownames_to_column(table_by_pov, "Characteristic")

names(table_by_pov) <- c("Characteristic", "Overall", "Poverty_High_Fluid_Cog", "Poverty_Low_Fluid_Cog","Welloff_High_Fluid_Cog","Welloff_Low_Fluid_Cog", "p_value")
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
  dplyr::select(rownames,Poverty_High_Fluid_Cog,Poverty_Low_Fluid_Cog,Welloff_High_Fluid_Cog,Welloff_Low_Fluid_Cog,Overall,p_value)

table_by_pov
write.csv(table_by_pov, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/table_mean_FLUID_COG_INR_25%.csv")




#-- Distributions of cog & SES variables by CRYSTALLIZED cog clusters

d_pov_highcrcog <- dat %>% filter(pov_highcrcog==1) %>% 
  mutate(profile="pov_highcrcog")
d_pov_lowcrcog <- dat %>% filter(pov_lowcrcog==1) %>% 
  mutate(profile="pov_lowcrcog")
d_welloff_highcrcog <- dat %>% filter(welloff_highcrcog==1) %>% 
  mutate(profile="welloff_highcrcog")
d_welloff_lowcrcog <- dat %>% filter(welloff_lowcrcog==1) %>% 
  mutate(profile="welloff_lowcrcog")

d_crpov <- rbind(d_pov_highcrcog,d_pov_lowcrcog,d_welloff_highcrcog,d_welloff_lowcrcog)

#-- Look at distributions
names(d_crpov)

vars <- names(d_crpov)[c(2,3,5:8)]

## Create Table 1a stratified by poverty
table_by_pov <- CreateTableOne(vars = vars,
                               strata = c("profile"), 
                               includeNA = F, 
                               addOverall = T,
                               data = d_crpov)
table_by_pov <- data.frame(print(table_by_pov, missing=F, catDigits = 1, contDigits = 1))
table_by_pov <- tibble::rownames_to_column(table_by_pov, "Characteristic")

names(table_by_pov) <- c("Characteristic", "Overall", "Poverty_High_Cryst_Cog", "Poverty_Low_Cryst_Cog","Welloff_High_Cryst_Cog","Welloff_Low_Cryst_Cog", "p_value")
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
  dplyr::select(rownames,Poverty_High_Cryst_Cog,Poverty_Low_Cryst_Cog,Welloff_High_Cryst_Cog,Welloff_Low_Cryst_Cog,Overall,p_value)

table_by_pov
write.csv(table_by_pov, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/table_mean_CRYST_COG_INR_25%.csv")


