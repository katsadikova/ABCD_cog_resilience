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


dat %>% reframe(quantile = scales::percent(c(0.2,0.4,0.6,0.8,1)),
                inr_quint = quantile(inr, c(0.2,0.4,0.6,0.8,1), na.rm=T),
                totalcomp_quint = quantile(nihtbx_totalcomp_agecorrected, c(0.2,0.4,0.6,0.8,1), na.rm=T))


#-- Exclude rows with missing data (for now)
dat <- na.omit(dat[,c(1,5,8:13)]) %>%
  mutate(fluid_s = as.numeric(scale(nihtbx_fluidcomp_agecorrected)),
         cryst_s = as.numeric(scale(nihtbx_cryst_agecorrected)),
         totcomp_s = as.numeric(scale(nihtbx_totalcomp_agecorrected)),
         inr_quint = case_when(
           inr<1.244813 ~ 1,
           inr<2.490040 ~ 2,
           inr<4.445762 ~ 3,
           inr<5.976096 ~ 4,
           T~5),
         totalcomp_quint = case_when(
           nihtbx_totalcomp_agecorrected<85 ~ 1,
           nihtbx_totalcomp_agecorrected<95 ~ 2,
           nihtbx_totalcomp_agecorrected<104 ~ 3,
           nihtbx_totalcomp_agecorrected<115 ~ 4,
           T~5),
         #-- Identify those who are in bottom 2 quintiles of INR, but top 2 quintiles of total cog
         pov_highcog = ifelse(inr_quint<3 & totalcomp_quint>3, 1, 0),
         #-- Identify those who are in bottom 2 quintiles of INR, and bottom 2 quintiles of total cog
         pov_lowcog = ifelse(inr_quint<3 & totalcomp_quint<3, 1, 0),
         #-- Identify those who are in top 2 quintiles of INR, and top 2 quintiles of total cog
         welloff_highcog = ifelse(inr_quint>3 & totalcomp_quint>3, 1, 0),
         #-- Identify those who are in top 2 quintiles of INR, and bottom 2 quintiles of total cog
         welloff_lowcog = ifelse(inr_quint>3 & totalcomp_quint<3, 1, 0))


names(dat)

summary(as.factor(dat$pov_highcog)) #1101
summary(as.factor(dat$pov_lowcog)) #2088
summary(as.factor(dat$welloff_highcog)) #2068
summary(as.factor(dat$welloff_lowcog)) #742

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
write.csv(table_by_pov, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/table_pov_highCog_v_pov_lowCog.csv")


