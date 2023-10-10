#----------------------------------------------------------------------------#
#--- 5_univariate_differences.R
#--- Date: 10/5/2023
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
library(finalfit)
library(caret)
library(pROC) 



#-- Non-missing INR - retain 9572 out of 11868 obs
load("/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/temp_dat/dat_all_inr.Rda")

#-- Complete cases on all considered predictors - only 4809 out of 11868
load("/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/temp_dat/dat_all_inr_complete.Rda")

#-- Load in data dictionary - to be able to label variables
dict <- read.csv("/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/temp_dat/data_dict.csv")

#-------------------------------------------------------------------------------------#
#-- Summarize the predictors - outline the missingness

names(dat_all_inr)

vars <- names(dat_all)[c(2,5:8,31:199)]

## Create Table 1 stratified by delay
table_all <- CreateTableOne(vars = vars,
                            includeNA = TRUE, 
                            addOverall = TRUE,
                            data = dat_all_inr)
table_all_print <- data.frame(print(table_all, explain=F, missing=T, catDigits = 2, contDigits = 2))
table_all_print <- tibble::rownames_to_column(table_all_print, "Characteristic")

# Column labels
names(table_all_print) <- c("Variable", "Overall", "Percent_Missing")
table_all_print_label <- merge(x=table_all_print, y=dict, by="Variable",all.x=T) %>%
  dplyr::select(Label,Variable,Overall,Percent_Missing,Order,Order2)

write.csv(table_all_print_label, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/table_all_missing.csv")

#-------------------------------------------------------------------------------------#
#-- Isolate clusters and summarize the predictors

#-----------------
#-- Poverty, fluid

d_pov_highflcog <- dat_all_inr %>% filter(pov_highflcog==1) %>% 
  mutate(profile="pov_highflcog")
d_pov_lowflcog <- dat_all_inr %>% filter(pov_lowflcog==1) %>% 
  mutate(profile="pov_lowflcog")

d_flpov <- rbind(d_pov_highflcog,d_pov_lowflcog)

#-- Look at distributions
names(d_flpov)

vars <- names(d_flpov)[c(2,5:8,31:199)]

## Create Table 1a stratified by poverty
table_by_pov <- CreateTableOne(vars = vars,
                               strata = c("profile"), 
                               includeNA = F, 
                               addOverall = T,
                               data = d_flpov)
table_by_pov <- data.frame(print(table_by_pov, explain=F, missing=T, catDigits = 2, contDigits = 2))
table_by_pov <- tibble::rownames_to_column(table_by_pov, "Characteristic")

names(table_by_pov) <- c("Variable", "Overall", "Poverty_High_Fluid_Cog", "Poverty_Low_Fluid_Cog","p_value","prop_missing")

table_by_pov_label <- merge(x=table_by_pov, y=dict, by="Variable",all.x=T) %>%
  dplyr::select(Label,Variable,Overall,Poverty_High_Fluid_Cog,Poverty_Low_Fluid_Cog,p_value,prop_missing,Order,Order2)

write.csv(table_by_pov_label, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/table_POV_mean_FLUID_COG_INR_25%.csv")

#------------------
#-- Well-off, fluid
d_pov_highflcog <- dat_all_inr %>% filter(welloff_highflcog==1) %>% 
  mutate(profile="welloff_highflcog")
d_pov_lowflcog <- dat_all_inr %>% filter(welloff_lowflcog==1) %>% 
  mutate(profile="welloff_lowflcog")

d_flpov <- rbind(d_pov_highflcog,d_pov_lowflcog)

#-- Look at distributions
names(d_flpov)

vars <- names(d_flpov)[c(2,5:8,31:199)]

## Create Table 1a stratified by poverty
table_by_pov <- CreateTableOne(vars = vars,
                               strata = c("profile"), 
                               includeNA = F, 
                               addOverall = T,
                               data = d_flpov)
table_by_pov <- data.frame(print(table_by_pov, explain=F, missing=T, catDigits = 2, contDigits = 2))
table_by_pov <- tibble::rownames_to_column(table_by_pov, "Characteristic")

names(table_by_pov) <- c("Variable", "Overall", "Well_off_High_Fluid_Cog", "Well_Off_Low_Fluid_Cog","p_value","prop_missing")

table_by_pov_label <- merge(x=table_by_pov, y=dict, by="Variable",all.x=T) %>%
  dplyr::select(Label,Variable,Overall,Well_off_High_Fluid_Cog,Well_Off_Low_Fluid_Cog,p_value,prop_missing,Order,Order2)

write.csv(table_by_pov_label, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/table_WELLOFF_mean_FLUID_COG_INR_25%.csv")


#------------------------
#-- Poverty, crystallized
names(dat_all)

d_pov_highflcog <- dat_all_inr %>% filter(pov_highcrcog==1) %>% 
  mutate(profile="pov_highcrcog")
d_pov_lowflcog <- dat_all_inr %>% filter(pov_lowcrcog==1) %>% 
  mutate(profile="pov_lowcrcog")

d_flpov <- rbind(d_pov_highflcog,d_pov_lowflcog)

#-- Look at distributions
names(d_flpov)

vars <- names(d_flpov)[c(2,5:8,31:199)]

## Create Table 1a stratified by poverty
table_by_pov <- CreateTableOne(vars = vars,
                               strata = c("profile"), 
                               includeNA = F, 
                               addOverall = T,
                               data = d_flpov)
table_by_pov <- data.frame(print(table_by_pov, explain=F, missing=T, catDigits = 2, contDigits = 2))
table_by_pov <- tibble::rownames_to_column(table_by_pov, "Characteristic")

names(table_by_pov) <- c("Variable", "Overall", "Poverty_High_Cryst_Cog", "Poverty_Low_Cryst_Cog","p_value","prop_missing")

table_by_pov_label <- merge(x=table_by_pov, y=dict, by="Variable",all.x=T) %>%
  dplyr::select(Label,Variable,Overall,Poverty_High_Cryst_Cog,Poverty_Low_Cryst_Cog,p_value,prop_missing,Order,Order2)

write.csv(table_by_pov_label, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/table_POV_mean_CRYST_COG_INR_25%.csv")


#------------------
#-- Well-off, crystallized
d_pov_highflcog <- dat_all_inr %>% filter(welloff_highcrcog==1) %>% 
  mutate(profile="welloff_highcrcog")
d_pov_lowflcog <- dat_all_inr %>% filter(welloff_lowcrcog==1) %>% 
  mutate(profile="welloff_lowcrcog")

d_flpov <- rbind(d_pov_highflcog,d_pov_lowflcog)

#-- Look at distributions
vars <- names(d_flpov)[c(2,5:8,31:199)]

## Create Table 1a stratified by poverty
table_by_pov <- CreateTableOne(vars = vars,
                               strata = c("profile"), 
                               includeNA = F, 
                               addOverall = T,
                               data = d_flpov)
table_by_pov <- data.frame(print(table_by_pov, explain=F, missing=T, catDigits = 2, contDigits = 2))
table_by_pov <- tibble::rownames_to_column(table_by_pov, "Characteristic")

names(table_by_pov) <- c("Variable", "Overall", "Well_off_High_Cryst_Cog", "Well_Off_Low_Cryst_Cog","p_value","prop_missing")

table_by_pov_label <- merge(x=table_by_pov, y=dict, by="Variable",all.x=T) %>%
  dplyr::select(Label,Variable,Overall,Well_off_High_Cryst_Cog,Well_Off_Low_Cryst_Cog,p_value,prop_missing,Order,Order2)

write.csv(table_by_pov_label, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/table_WELLOFF_mean_CRYST_COG_INR_25%.csv")


#-------------------------------------------------------------------------------------#
#-- Isolate clusters based on INR tertiles and summarize the predictors

#-----------------
#-- Poverty, fluid

d_pov_highflcog <- dat_all_inr %>% filter(pov3_highflcog==1) %>% 
  mutate(profile="pov_highflcog")
d_pov_lowflcog <- dat_all_inr %>% filter(pov3_lowflcog==1) %>% 
  mutate(profile="pov_lowflcog")

d_flpov <- rbind(d_pov_highflcog,d_pov_lowflcog)

#-- Look at distributions
names(d_flpov)

vars <- names(d_flpov)[c(2,5:8,31:199)]

## Create Table 1a stratified by poverty
table_by_pov <- CreateTableOne(vars = vars,
                               strata = c("profile"), 
                               includeNA = F, 
                               addOverall = T,
                               data = d_flpov)
table_by_pov <- data.frame(print(table_by_pov, explain=F, missing=T, catDigits = 2, contDigits = 2))
table_by_pov <- tibble::rownames_to_column(table_by_pov, "Characteristic")

names(table_by_pov) <- c("Variable", "Overall", "Poverty_High_Fluid_Cog", "Poverty_Low_Fluid_Cog","p_value","prop_missing")

table_by_pov_label <- merge(x=table_by_pov, y=dict, by="Variable",all.x=T) %>%
  dplyr::select(Label,Variable,Overall,Poverty_High_Fluid_Cog,Poverty_Low_Fluid_Cog,p_value,prop_missing,Order,Order2)

write.csv(table_by_pov_label, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/table_POV_mean_FLUID_COG_INR_33%.csv")

#------------------
#-- Well-off, fluid
d_pov_highflcog <- dat_all_inr %>% filter(welloff3_highflcog==1) %>% 
  mutate(profile="welloff_highflcog")
d_pov_lowflcog <- dat_all_inr %>% filter(welloff3_lowflcog==1) %>% 
  mutate(profile="welloff_lowflcog")

d_flpov <- rbind(d_pov_highflcog,d_pov_lowflcog)

#-- Look at distributions
names(d_flpov)

vars <- names(d_flpov)[c(2,5:8,31:199)]

## Create Table 1a stratified by poverty
table_by_pov <- CreateTableOne(vars = vars,
                               strata = c("profile"), 
                               includeNA = F, 
                               addOverall = T,
                               data = d_flpov)
table_by_pov <- data.frame(print(table_by_pov, explain=F, missing=T, catDigits = 2, contDigits = 2))
table_by_pov <- tibble::rownames_to_column(table_by_pov, "Characteristic")

names(table_by_pov) <- c("Variable", "Overall", "Well_off_High_Fluid_Cog", "Well_Off_Low_Fluid_Cog","p_value","prop_missing")

table_by_pov_label <- merge(x=table_by_pov, y=dict, by="Variable",all.x=T) %>%
  dplyr::select(Label,Variable,Overall,Well_off_High_Fluid_Cog,Well_Off_Low_Fluid_Cog,p_value,prop_missing,Order,Order2)

write.csv(table_by_pov_label, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/table_WELLOFF_mean_FLUID_COG_INR_33%.csv")


#------------------------
#-- Poverty, crystallized
names(dat_all)

d_pov_highflcog <- dat_all_inr %>% filter(pov3_highcrcog==1) %>% 
  mutate(profile="pov_highcrcog")
d_pov_lowflcog <- dat_all_inr %>% filter(pov3_lowcrcog==1) %>% 
  mutate(profile="pov_lowcrcog")

d_flpov <- rbind(d_pov_highflcog,d_pov_lowflcog)

#-- Look at distributions
names(d_flpov)

vars <- names(d_flpov)[c(2,5:8,31:199)]

## Create Table 1a stratified by poverty
table_by_pov <- CreateTableOne(vars = vars,
                               strata = c("profile"), 
                               includeNA = F, 
                               addOverall = T,
                               data = d_flpov)
table_by_pov <- data.frame(print(table_by_pov, explain=F, missing=T, catDigits = 2, contDigits = 2))
table_by_pov <- tibble::rownames_to_column(table_by_pov, "Characteristic")

names(table_by_pov) <- c("Variable", "Overall", "Poverty_High_Cryst_Cog", "Poverty_Low_Cryst_Cog","p_value","prop_missing")

table_by_pov_label <- merge(x=table_by_pov, y=dict, by="Variable",all.x=T) %>%
  dplyr::select(Label,Variable,Overall,Poverty_High_Cryst_Cog,Poverty_Low_Cryst_Cog,p_value,prop_missing,Order,Order2)

write.csv(table_by_pov_label, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/table_POV_mean_CRYST_COG_INR_33%.csv")


#------------------
#-- Well-off, crystallized
d_pov_highflcog <- dat_all_inr %>% filter(welloff3_highcrcog==1) %>% 
  mutate(profile="welloff_highcrcog")
d_pov_lowflcog <- dat_all_inr %>% filter(welloff3_lowcrcog==1) %>% 
  mutate(profile="welloff_lowcrcog")

d_flpov <- rbind(d_pov_highflcog,d_pov_lowflcog)

#-- Look at distributions
vars <- names(d_flpov)[c(2,5:8,31:199)]

## Create Table 1a stratified by poverty
table_by_pov <- CreateTableOne(vars = vars,
                               strata = c("profile"), 
                               includeNA = F, 
                               addOverall = T,
                               data = d_flpov)
table_by_pov <- data.frame(print(table_by_pov, explain=F, missing=T, catDigits = 2, contDigits = 2))
table_by_pov <- tibble::rownames_to_column(table_by_pov, "Characteristic")

names(table_by_pov) <- c("Variable", "Overall", "Well_off_High_Cryst_Cog", "Well_Off_Low_Cryst_Cog","p_value","prop_missing")

table_by_pov_label <- merge(x=table_by_pov, y=dict, by="Variable",all.x=T) %>%
  dplyr::select(Label,Variable,Overall,Well_off_High_Cryst_Cog,Well_Off_Low_Cryst_Cog,p_value,prop_missing,Order,Order2)

write.csv(table_by_pov_label, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/table_WELLOFF_mean_CRYST_COG_INR_33%.csv")

