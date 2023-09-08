#----------------------------------------------------------------------------#
#--- 4_pull_predictors.R
#--- Date: 9/7/2023
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


setwd("/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/release5/core/")


#-- The starting data set
load("/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/temp_dat/dat.Rda")

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

#-- School functioning
ce_y_srpf <- read.csv("./culture-environment/ce_y_srpf.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id, srpf_y_ss_ses)

#-- Adversity - Neighborhood safety and crime
ce_p_nsc <- read.csv("./culture-environment/ce_p_nsc.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id, nsc_p_ss_mean_3_items)

#-- Adversity - Family environment - conflict scale, parent reported
ce_p_fes <- read.csv("./culture-environment/ce_p_fes.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id, fes_p_ss_fc)

#-- Adversity - Family environment - conflict scale, child reported
ce_y_fes <- read.csv("./culture-environment/ce_y_fes.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id, fes_y_ss_fc)

#-- Adversity - Emotional Neglect - Parent & secondary caregiver behavior
ce_y_crpbi <- read.csv("./culture-environment/ce_y_crpbi.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id, crpbi_y_ss_parent,crpbi_y_ss_caregiver)

#-- Adversity - Physical Neglect -Parental monitoring
ce_y_pm <- read.csv("./culture-environment/ce_y_pm.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id, pmq_y_ss_mean)


#-- Temperament/Personality: Prosocial behavior - parent reported
ce_p_psb <- read.csv("./culture-environment/ce_p_psb.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id, psb_p_ss_mean)


#-- Temperament/Personality: Prosocial behavior - self reported
ce_y_psb <- read.csv("./culture-environment/ce_y_psb.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id, psb_y_ss_mean)


#-- COI
led_l_coi <- read.csv("./linked-external-data/led_l_coi.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id,reshist_addr1_coi_ed_attain,
                reshist_addr1_coi_ed_apenr,reshist_addr1_coi_ed_college,
                reshist_addr1_coi_ed_prxece,reshist_addr1_coi_ed_ecenrol,
                reshist_addr1_coi_ed_hsgrad,reshist_addr1_coi_ed_prxhqece,
                reshist_addr1_coi_ed_schpov,reshist_addr1_coi_ed_teachxp,
                reshist_addr1_coi_ed_math,reshist_addr1_coi_ed_reading,
                reshist_addr1_coi_he_green,reshist_addr1_coi_he_food,
                reshist_addr1_coi_he_pm25,reshist_addr1_coi_he_heat,
                reshist_addr1_coi_he_suprfnd,reshist_addr1_coi_he_hlthins,
                reshist_addr1_coi_he_vacancy,reshist_addr1_coi_he_rsei,
                reshist_addr1_coi_he_ozone,reshist_addr1_coi_he_walk,
                reshist_addr1_coi_se_jobprox,reshist_addr1_coi_se_emprat,
                reshist_addr1_coi_se_occ,reshist_addr1_coi_se_home,
                reshist_addr1_coi_se_mhe,reshist_addr1_coi_se_povrate,
                reshist_addr1_coi_se_public,reshist_addr1_coi_se_single)


#-- Urbanization
led_l_denspop <- read.csv("./linked-external-data/led_l_denspop.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id,reshist_addr1_popdensity)

led_l_urban <- read.csv("./linked-external-data/led_l_urban.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id,reshist_addr1_urban_area)

led_l_walk <- read.csv("./linked-external-data/led_l_walk.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id,reshist_addr1_walkindex)

led_l_traffic <- read.csv("./linked-external-data/led_l_traffic.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id,reshist_addr1_traffic_count)

led_l_roadprox <- read.csv("./linked-external-data/led_l_roadprox.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id,reshist_addr1_proxrd)

#-- Neighborhood quality: Social Vulnerability Index (SVI) - overall
led_l_svi <- read.csv("./linked-external-data/led_l_svi.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id,reshist_addr1_svi_tot_20142018)

#-- Neightborhood quality: crime 
led_l_crime <- read.csv("./linked-external-data/led_l_crime.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id,reshist_addr1_p1vlnt)

#-- Neighborhood quality: lead risk
led_l_leadrisk <- read.csv("./linked-external-data/led_l_leadrisk.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id,reshist_addr1_leadrisk)


#-- Natural environment: Natural Space and Satellite, Land Use: Nighttime lights
led_l_urbsat <- read.csv("./linked-external-data/led_l_urbsat.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id,reshist_addr1_urbsat_ntl)

#-- Neighborhood: residential segregation - Dissimilarity index
led_l_dissim <- read.csv("./linked-external-data/led_l_dissim.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id,reshist_addr1_seg_diss_nww_m)


#-- Neighborhood: residential segregation - Exposure / Interaction index
led_l_expint <- read.csv("./linked-external-data/led_l_expint.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id,reshist_addr1_seg_inter_nww_m)


#-- Index of concentration at the extremes
led_l_ice <- read.csv("./linked-external-data/led_l_ice.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id,reshist_addr1_seg_ice_income,
                reshist_addr1_seg_ice_inc_bw)


#-- Neighborhood: residential segregation - Multi-group Entropy Index
led_l_entropy <- read.csv("./linked-external-data/led_l_entropy.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id,reshist_addr1_seg_entropy_t)


#-- Laws and biases: Race bias
led_l_biasrace <- read.csv("./linked-external-data/led_l_biasrace.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id,reshist_state_racism_factor)


#-- Laws and biases: Gender bias
led_l_biasgender <- read.csv("./linked-external-data/led_l_biasgender.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id, reshist_state_so_factor, reshist_state_sexism_factor)


#-- Laws and biases: ACA Medicaid expansion
led_l_aca <- read.csv("./linked-external-data/led_l_aca.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id, reshist_addr1_ACAexpand)

#-- Demo: marital status - fix in tables
abcd_p_demo <- read.csv("./abcd-general/abcd_p_demo.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id, demo_prnt_marital_v2,
                demo_fam_exp1_v2,demo_fam_exp2_v2,
                demo_fam_exp3_v2, demo_fam_exp4_v2,
                demo_fam_exp5_v2,demo_fam_exp6_v2,demo_fam_exp7_v2)

summary(abcd_p_demo)

#-- Adversity - Physical abuse - broad psychopathology, KSADS - post-traumatic stress disorder (indiv. Questions)
mh_p_ksads_ptsd <- read.csv("./mental-health/mh_p_ksads_ptsd.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id, ksads_ptsd_raw_761_p,
                ksads_ptsd_raw_762_p,ksads_ptsd_raw_763_p,
                ksads_ptsd_raw_767_p,ksads_ptsd_raw_768_p,
                ksads_ptsd_raw_769_p)

#-- Bullying
mh_p_ksads_bg <- read.csv("./mental-health/mh_p_ksads_bg.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id, kbi_p_c_bully)


#-- Adversity - Household substance abuse - family history
mh_p_fhx <- read.csv("./mental-health/mh_p_fhx.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id, famhx_ss_fath_prob_alc_p, famhx_ss_parent_dg_p,
                famhx_ss_fath_prob_prf_p, famhx_ss_moth_prob_prf_p,
                famhx_ss_parent_hspd_p)


#-- Adversity - Household substance abuse - Adult Self-report (parent) ASEBA
mh_p_asr <- read.csv("./mental-health/mh_p_asr.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id, asr_q06_p, asr_q90_p, asr_q126_p)



#-- Pubertal development
ph_p_pds <- read.csv("./physical-health/ph_p_pds.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id, pds_p_ss_male_category, pds_p_ss_female_category)

#-- Physical health - development
ph_p_dhx <- read.csv("./physical-health/ph_p_dhx.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id, birth_weight_oz, devhx_3_p)

#-- Physical activity - self-reported
ph_y_yrb <- read.csv("./physical-health/ph_y_yrb.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id, physical_activity1_y,
                physical_activity2_y,physical_activity5_y) 


#-- Sports and organized activities
ph_p_saiq <- read.csv("./physical-health/ph_p_saiq.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id, sai_p_activities___0,sai_p_activities___1,
                sai_p_activities___2,sai_p_activities___3,sai_p_activities___4,
                sai_p_activities___5,sai_p_activities___6,sai_p_activities___7,
                sai_p_activities___8,sai_p_activities___9,sai_p_activities___10,
                sai_p_activities___11,sai_p_activities___12,sai_p_activities___13,
                sai_p_activities___14,sai_p_activities___15,sai_p_activities___16,
                sai_p_activities___17,sai_p_activities___18,sai_p_activities___19,
                sai_p_activities___20,sai_p_activities___21,sai_p_activities___22,
                sai_p_activities___23,sai_p_activities___24,sai_p_activities___25,
                sai_p_activities___26,sai_p_activities___27,
                sai_p_activities___28, sai_p_activities___29) 

#-- Reading & music - self-reported
ph_y_saiq  <- read.csv("./physical-health/ph_y_saiq.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id,sai_read_y,sai_read_hrs_wk_y,
                sai_read_enjoy_y,sai_lmusic_hrs_day_y,
                sai_lmusic_y,sai_lmusic_feel_y,sai_lmusic_studying_y)
                

#-- Sleep
ph_p_sds <- read.csv("./physical-health/ph_p_sds.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id,sds_p_ss_dims,sds_p_ss_sbd,
                sds_p_ss_da,sds_p_ss_swtd,sds_p_ss_does,
                sds_p_ss_shy,sds_p_ss_total)
                

#-- Screen time
nt_p_stq <- read.csv("./novel-technologies/nt_p_stq.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id,screentime1_p_hours, screentime1_p_minutes,
                screentime2_p_hours, screentime2_p_minutes)

#-------------------------------------------------------------------------#
#-- Assemble the data set
data_list <- list()
data_list <- append(data_list, list(dat,ce_y_srpf,ce_p_nsc,
                                   ce_p_fes,ce_y_fes,ce_y_crpbi,
                                   ce_y_pm, ce_p_psb, ce_y_psb,led_l_coi,
                                   led_l_denspop, led_l_urban, led_l_walk,
                                   led_l_traffic, led_l_roadprox, led_l_svi,
                                   led_l_crime, led_l_leadrisk, led_l_urbsat,
                                   led_l_dissim, led_l_expint, led_l_ice,
                                   led_l_entropy, led_l_biasrace, led_l_biasgender,
                                   led_l_aca, abcd_p_demo, mh_p_ksads_ptsd,
                                   mh_p_ksads_bg,mh_p_fhx, mh_p_asr, ph_p_pds,
                                   ph_p_dhx,ph_y_yrb,ph_p_saiq,ph_y_saiq,
                                   ph_p_sds,nt_p_stq))
length(data_list)


dat_all <- Reduce(function(x, y) merge(x, y, by="src_subject_id", all=TRUE), data_list)
names(dat_all)



#-------------------------------------------------------------------------------------#
#-- Summarize the predictors - outline the missingness

## Create a variable list
vars <- names(dat_all)[2:154]

## Create Table 1 stratified by delay
table_all <- CreateTableOne(vars = vars,
                                 includeNA = TRUE, 
                                 addOverall = TRUE,
                                 data = dat_all)
table_all_print <- data.frame(print(table_all, missing=T, catDigits = 1, contDigits = 1))
table_all_print <- tibble::rownames_to_column(table_all_print, "Characteristic")

# Column labels
names(table_all_print) <- c("Characteristic", "Overall", "Percent_Missing")

write.csv(table_all_print, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/table_all_missing.csv")


#-------------------------------------------------------------------------------------#
#-- Isolate clusters and summarize the predictors

#-----------------
#-- Poverty, fluid

d_pov_highflcog <- dat_all %>% filter(pov_highflcog==1) %>% 
  mutate(profile="pov_highflcog")
d_pov_lowflcog <- dat_all %>% filter(pov_lowflcog==1) %>% 
  mutate(profile="pov_lowflcog")

d_flpov <- rbind(d_pov_highflcog,d_pov_lowflcog)

#-- Look at distributions
names(d_flpov)

vars <- names(d_flpov)[c(2,3:4,6:8,12,21:154)]

## Create Table 1a stratified by poverty
table_by_pov <- CreateTableOne(vars = vars,
                               strata = c("profile"), 
                               includeNA = F, 
                               addOverall = T,
                               data = d_flpov)
table_by_pov <- data.frame(print(table_by_pov, missing=T, catDigits = 1, contDigits = 1))
table_by_pov <- tibble::rownames_to_column(table_by_pov, "Characteristic")

names(table_by_pov) <- c("Characteristic", "Overall", "Poverty_High_Fluid_Cog", "Poverty_Low_Fluid_Cog","p_value","prop_missing")
table_by_pov
write.csv(table_by_pov, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/table_POV_mean_FLUID_COG_INR_25%.csv")

#------------------
#-- Well-off, fluid
d_pov_highflcog <- dat_all %>% filter(welloff_highflcog==1) %>% 
  mutate(profile="welloff_highflcog")
d_pov_lowflcog <- dat_all %>% filter(welloff_lowflcog==1) %>% 
  mutate(profile="welloff_lowflcog")

d_flpov <- rbind(d_pov_highflcog,d_pov_lowflcog)

#-- Look at distributions
names(d_flpov)

vars <- names(d_flpov)[c(2,3:4,6:8,12,21:154)]

## Create Table 1a stratified by poverty
table_by_pov <- CreateTableOne(vars = vars,
                               strata = c("profile"), 
                               includeNA = F, 
                               addOverall = T,
                               data = d_flpov)
table_by_pov <- data.frame(print(table_by_pov, missing=T, catDigits = 1, contDigits = 1))
table_by_pov <- tibble::rownames_to_column(table_by_pov, "Characteristic")

names(table_by_pov) <- c("Characteristic", "Overall", "Well_off_High_Fluid_Cog", "Well_Off_Low_Fluid_Cog","p_value","prop_missing")
table_by_pov
write.csv(table_by_pov, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/table_WELLOFF_mean_FLUID_COG_INR_25%.csv")


#------------------------
#-- Poverty, crystallized
names(dat_all)

d_pov_highflcog <- dat_all %>% filter(pov_highcrcog==1) %>% 
  mutate(profile="pov_highcrcog")
d_pov_lowflcog <- dat_all %>% filter(pov_lowcrcog==1) %>% 
  mutate(profile="pov_lowcrcog")

d_flpov <- rbind(d_pov_highflcog,d_pov_lowflcog)

#-- Look at distributions
names(d_flpov)

vars <- names(d_flpov)[c(2,3:4,6:8,12,21:154)]

## Create Table 1a stratified by poverty
table_by_pov <- CreateTableOne(vars = vars,
                               strata = c("profile"), 
                               includeNA = F, 
                               addOverall = T,
                               data = d_flpov)
table_by_pov <- data.frame(print(table_by_pov, missing=T, catDigits = 1, contDigits = 1))
table_by_pov <- tibble::rownames_to_column(table_by_pov, "Characteristic")

names(table_by_pov) <- c("Characteristic", "Overall", "Poverty_High_Cryst_Cog", "Poverty_Low_Cryst_Cog","p_value","prop_missing")
table_by_pov
write.csv(table_by_pov, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/table_POV_mean_CRYST_COG_INR_25%.csv")


#------------------
#-- Well-off, fluid
d_pov_highflcog <- dat_all %>% filter(welloff_highcrcog==1) %>% 
  mutate(profile="welloff_highcrcog")
d_pov_lowflcog <- dat_all %>% filter(welloff_lowcrcog==1) %>% 
  mutate(profile="welloff_lowcrcog")

d_flpov <- rbind(d_pov_highflcog,d_pov_lowflcog)

#-- Look at distributions
vars <- names(d_flpov)[c(2,3:4,6:8,12,21:154)]

## Create Table 1a stratified by poverty
table_by_pov <- CreateTableOne(vars = vars,
                               strata = c("profile"), 
                               includeNA = F, 
                               addOverall = T,
                               data = d_flpov)
table_by_pov <- data.frame(print(table_by_pov, missing=T, catDigits = 1, contDigits = 1))
table_by_pov <- tibble::rownames_to_column(table_by_pov, "Characteristic")

names(table_by_pov) <- c("Characteristic", "Overall", "Well_off_High_Cryst_Cog", "Well_Off_Low_Cryst_Cog","p_value","prop_missing")
table_by_pov
write.csv(table_by_pov, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/table_WELLOFF_mean_CRYST_COG_INR_25%.csv")

