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
## Parallel backend for foreach (also loads foreach and parallel; includes doMC)
library(doParallel)
## Reproducible parallelization
library(doRNG)


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
    #inr_quart = factor(Hmisc::cut2(inr, g = 4), labels = c(1:4)),
    inr_quart = factor(case_when(inr>=5.976 ~ 4,
                          inr>=3.211991 ~ 3,
                          inr>=1.64211 ~ 2,
                          T~1)),
    #inr_tert = factor(Hmisc::cut2(inr, g = 3), labels = c(1:3)),
    inr_tert = factor(case_when(inr>=5.098572 ~ 3,
                         inr>=2.064653 ~ 2,
                         T ~ 1)),
    
    #-- Clusters based on quartiles of INR and fluid cog
    #-- Identify those who are in bottom quartile of INR, but above average on fluid cog
    pov_highflcog = ifelse(inr_quart==1 & nihtbx_fluidcomp_agecorrected>mean(nihtbx_fluidcomp_agecorrected,na.rm=T), 1, 0),
    #-- Identify those who are in bottom quartile of INR, and below average on fluid cog
    pov_lowflcog = ifelse(inr_quart==1 & nihtbx_fluidcomp_agecorrected<=mean(nihtbx_fluidcomp_agecorrected,na.rm=T), 1, 0),
    #-- Identify those who are in top quartile of INR, and above average on fluid cog
    welloff_highflcog = ifelse(inr_quart==4 & nihtbx_fluidcomp_agecorrected>mean(nihtbx_fluidcomp_agecorrected,na.rm=T), 1, 0),
    #-- Identify those who are in top quartile of INR, and below average on fluid cog
    welloff_lowflcog = ifelse(inr_quart==4 & nihtbx_fluidcomp_agecorrected<=mean(nihtbx_fluidcomp_agecorrected,na.rm=T), 1, 0),
    
    #-- Clusters based on quartiles of INR and crystallized cog
    #-- Identify those who are in bottom quartile of INR, but above average on crystallized cog
    pov_highcrcog = ifelse(inr_quart==1 & nihtbx_cryst_agecorrected>mean(nihtbx_cryst_agecorrected,na.rm=T), 1, 0),
    #-- Identify those who are in bottom quartile of INR, and below average on crystallized cog
    pov_lowcrcog = ifelse(inr_quart==1 & nihtbx_cryst_agecorrected<=mean(nihtbx_cryst_agecorrected,na.rm=T), 1, 0),
    #-- Identify those who are in top quartile of INR, and above average on crystallized cog
    welloff_highcrcog = ifelse(inr_quart==4 & nihtbx_cryst_agecorrected>mean(nihtbx_cryst_agecorrected,na.rm=T), 1, 0),
    #-- Identify those who are in top quartile of INR, and below average on crystallized cog
    welloff_lowcrcog = ifelse(inr_quart==4 & nihtbx_cryst_agecorrected<=mean(nihtbx_cryst_agecorrected,na.rm=T), 1, 0),
    
    
    #-- Clusters based on tertiles of INR and fluid cog
    #-- Identify those who are in bottom tertile of INR, but above average on fluid cog
    pov3_highflcog = ifelse(inr_tert==1 & nihtbx_fluidcomp_agecorrected>mean(nihtbx_fluidcomp_agecorrected,na.rm=T), 1, 0),
    #-- Identify those who are in bottom tertile of INR, and below average on fluid cog
    pov3_lowflcog = ifelse(inr_tert==1 & nihtbx_fluidcomp_agecorrected<=mean(nihtbx_fluidcomp_agecorrected,na.rm=T), 1, 0),
    #-- Identify those who are in top tertile of INR, and above average on fluid cog
    welloff3_highflcog = ifelse(inr_tert==3 & nihtbx_fluidcomp_agecorrected>mean(nihtbx_fluidcomp_agecorrected,na.rm=T), 1, 0),
    #-- Identify those who are in top tertile of INR, and below average on fluid cog
    welloff3_lowflcog = ifelse(inr_tert==3 & nihtbx_fluidcomp_agecorrected<=mean(nihtbx_fluidcomp_agecorrected,na.rm=T), 1, 0),
    
    #-- Clusters based on INR and crystallized cog
    #-- Identify those who are in bottom tertile of INR, but above average on crystallized cog
    pov3_highcrcog = ifelse(inr_tert==1 & nihtbx_cryst_agecorrected>mean(nihtbx_cryst_agecorrected,na.rm=T), 1, 0),
    #-- Identify those who are in bottom tertile of INR, and below average on crystallized cog
    pov3_lowcrcog = ifelse(inr_tert==1 & nihtbx_cryst_agecorrected<=mean(nihtbx_cryst_agecorrected,na.rm=T), 1, 0),
    #-- Identify those who are in top tertile of INR, and above average on crystallized cog
    welloff3_highcrcog = ifelse(inr_tert==3 & nihtbx_cryst_agecorrected>mean(nihtbx_cryst_agecorrected,na.rm=T), 1, 0),
    #-- Identify those who are in top tertile of INR, and below average on crystallized cog
    welloff3_lowcrcog = ifelse(inr_tert==3 & nihtbx_cryst_agecorrected<=mean(nihtbx_cryst_agecorrected,na.rm=T), 1, 0)
    
  )

abcd_y_lt <- read.csv("./abcd-general/abcd_y_lt.csv") %>%
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id, rel_family_id)

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
  mutate(demo_mar_livingwith = case_when(
          demo_prnt_marital_v2 %in% c(1,6) ~ 1,
          demo_prnt_marital_v2 == 777 ~ NA_real_,
          T~0),
         across(c(demo_fam_exp1_v2,demo_fam_exp2_v2,
                  demo_fam_exp3_v2, demo_fam_exp4_v2,
                  demo_fam_exp5_v2,demo_fam_exp6_v2,demo_fam_exp7_v2),
                ~ dplyr::recode(.,`0`=0,`1`=1,`777`=NA_real_,.default = NaN))
    ) %>%
  dplyr::select(src_subject_id, demo_mar_livingwith, demo_prnt_ed_v2, demo_prtnr_ed_v2,
                demo_fam_exp1_v2,demo_fam_exp2_v2,
                demo_fam_exp3_v2, demo_fam_exp4_v2,
                demo_fam_exp5_v2,demo_fam_exp6_v2,demo_fam_exp7_v2)
summary(as.factor(abcd_p_demo$demo_fam_exp1_v2))

#-- Adversity - Physical abuse - broad psychopathology, KSADS - post-traumatic stress disorder (indiv. Questions)
mh_p_ksads_ptsd <- read.csv("./mental-health/mh_p_ksads_ptsd.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  mutate(
    ksads_ptsd_accident = case_when(ksads_ptsd_raw_754_p==1 | ksads_ptsd_raw_755_p==1 ~ 1,
                                    T ~ 0),
    ksads_ptsd_wit_viol = case_when(ksads_ptsd_raw_756_p==1 | ksads_ptsd_raw_757_p==1 | ksads_ptsd_raw_758_p==1 | 
                                    ksads_ptsd_raw_759_p==1 | ksads_ptsd_raw_760_p==1 | ksads_ptsd_raw_766_p==1 ~ 1,
                                    T ~ 0),
    ksads_ptsd_thr_viol = case_when(ksads_ptsd_raw_764_p==1 | ksads_ptsd_raw_765_p==1 ~ 1,
                                    T ~ 0),
    ksads_ptsd_phys_abuse = case_when(ksads_ptsd_raw_761_p==1 | ksads_ptsd_raw_762_p==1 | ksads_ptsd_raw_763_p==1 ~ 1,
                                     T ~ 0),
    ksads_ptsd_sexual_abuse = case_when(ksads_ptsd_raw_767_p==1 | ksads_ptsd_raw_768_p==1 | ksads_ptsd_raw_769_p==1 ~ 1,
                                        T ~ 0)
  ) %>%
  dplyr::select(src_subject_id, 
                ksads_ptsd_raw_754_p, ksads_ptsd_raw_755_p,
                ksads_ptsd_raw_756_p, ksads_ptsd_raw_757_p,
                ksads_ptsd_raw_758_p, ksads_ptsd_raw_759_p,
                ksads_ptsd_raw_760_p, ksads_ptsd_raw_761_p,
                ksads_ptsd_raw_762_p, ksads_ptsd_raw_763_p,
                ksads_ptsd_raw_764_p, ksads_ptsd_raw_765_p,
                ksads_ptsd_raw_766_p, ksads_ptsd_raw_767_p, 
                ksads_ptsd_raw_768_p, ksads_ptsd_raw_769_p, 
                ksads_ptsd_raw_770_p,
                ksads_ptsd_accident, ksads_ptsd_wit_viol, ksads_ptsd_thr_viol,
                ksads_ptsd_phys_abuse, ksads_ptsd_sexual_abuse)

#-- Bullying
mh_p_ksads_bg <- read.csv("./mental-health/mh_p_ksads_bg.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  mutate(kbi_p_c_bully = case_when(kbi_p_c_bully==2~0,
                                   T~kbi_p_c_bully)) %>%
  dplyr::select(src_subject_id, kbi_p_c_bully)


#-- Adversity - Household substance abuse - family history
mh_p_fhx <- read.csv("./mental-health/mh_p_fhx.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id, 
                #-- problem with alcohol / drugs endorsed for mother or father
                famhx_ss_fath_prob_alc_p, famhx_ss_moth_prob_alc_p,
                famhx_ss_moth_prob_dg_p, famhx_ss_fath_prob_dg_p,
                #-- mother/father been to counselor due to emotional / mental problem
                famhx_ss_fath_prob_prf_p, famhx_ss_moth_prob_prf_p,
                #-- mother/father hospitalized due to emotional / mental problem
                famhx_ss_moth_prob_hspd_p, famhx_ss_fath_prob_hspd_p)


#-- Adversity - Household substance abuse & parental psychopathology - Adult Self-report (parent) ASEBA
mh_p_asr <- read.csv("./mental-health/mh_p_asr.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id, 
                #Household substance use
                asr_q06_p, asr_q90_p, asr_q126_p,
                #Psychopathology
                asr_scr_anxdep_t, asr_scr_withdrawn_t,
                asr_scr_somatic_t, asr_scr_thought_t,
                asr_scr_attention_t, asr_scr_aggressive_t,
                asr_scr_rulebreak_t, asr_scr_intrusive_t
                )


#-- Pubertal development
ph_p_pds <- read.csv("./physical-health/ph_p_pds.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  mutate(
    pds_p_ss_cat = case_when(pubertal_sex_p==1 ~ pds_p_ss_male_category,
                             pubertal_sex_p==2 ~ pds_p_ss_female_category)
  ) %>%
  dplyr::select(src_subject_id, pubertal_sex_p, pds_p_ss_cat)


#-- Physical health - Development History Questionnaire
ph_p_dhx <- read.csv("./physical-health/ph_p_dhx.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1")  %>%
  mutate(
    birth_weight = birth_weight_lbs+birth_weight_oz/16,
    devhx_4_p = case_when(
      devhx_4_p==332 ~ 32,
      devhx_4_p==389 ~ 39,
      T ~ devhx_4_p
    ),
    preg_tobacco = case_when(
      devhx_8_tobacco==1 | devhx_9_tobacco==1 ~ 1,
      T ~ 0
    ),
    preg_alcohol = case_when(
      devhx_8_alcohol==1 | devhx_9_alcohol==1 ~ 1,
      T ~ 0
    ),
    preg_marijuana = case_when(
      devhx_8_marijuana==1 | devhx_9_marijuana==1 ~ 1,
      T ~ 0
    ),
    preg_other_drug = case_when(
      devhx_8_coc_crack==1 | devhx_8_her_morph==1 | devhx_8_oxycont==1 | devhx_8_other_drugs==1 | devhx_9_coc_crack==1 | devhx_9_her_morph==1 | devhx_9_oxycont==1 | devhx_9_other_drugs==1 ~ 1,
      T ~ 0
    ),
    preg_caffeine = case_when(
      devhx_caffeine_11 == 1 | devhx_caffeine_11 == 2 | devhx_caffeine_11 == 3 ~ 1,
      T ~ 0
    ),
    birth_complications = case_when(
      devhx_14a3_p==1 | devhx_14b3_p==1 | devhx_14c3_p==1 | devhx_14d3_p==1 | devhx_14e3_p==1 | devhx_14f3_p==1 | devhx_14g3_p==1 | devhx_14h3_p==1 ~ 1,
      T ~ 0
    ),
    devhx_16_p = case_when(devhx_16_p==9990 ~ NA_real_,
                           T~devhx_16_p),
    late_motor_dev = case_when(devhx_20_p ==4 | devhx_20_p ==5 ~ 1,
                               T ~ 0),
    late_speech_dev = case_when(devhx_21_p ==4 | devhx_21_p ==5 ~ 1,
                               T ~ 0),
    across(c(devhx_8_prescript_med,devhx_9_prescript_med,devhx_6_p,devhx_10,devhx_12a_p,devhx_22_3_p),
                ~ dplyr::recode(.,`999`=NA_real_,`1`=1,`0`=0, `-1`=NA_real_, .default = NaN))) %>%
  dplyr::select(src_subject_id, birth_weight, devhx_3_p, devhx_4_p,
                devhx_6_p, preg_tobacco, preg_alcohol, preg_marijuana, 
                preg_other_drug, devhx_10, 
                preg_caffeine, birth_complications,
                devhx_13_3_p,devhx_18_p)
                # Dropped premature birth, fever/infection, motor/speech dev, bed-wetting (10/6)
                # devhx_12a_p,devhx_16_p,devhx_17_p,
                # late_motor_dev,late_speech_dev,devhx_22_3_p
                
names(ph_p_pds)
summary(as.factor(ph_p_dhx$devhx_13_3_p))

#-- Physical activity - self-reported
ph_y_yrb <- read.csv("./physical-health/ph_y_yrb.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id, physical_activity1_y,
                physical_activity2_y,physical_activity5_y) 


#-- Sports and organized activities
ph_p_saiq <- read.csv("./physical-health/ph_p_saiq.csv") %>%
  mutate(
    sai_p_perf_arts = case_when(sai_p_activities___0==1 | sai_p_activities___23==1 | sai_p_activities___25==1 ~ 1,
                                T~0),
    sai_p_team_sports = case_when(sai_p_activities___1==1 | sai_p_activities___2==1 |
                                  sai_p_activities___4==1 | sai_p_activities___5==1 |
                                  sai_p_activities___7==1 | sai_p_activities___8==1 |
                                  sai_p_activities___11==1 | sai_p_activities___12==1 |
                                  sai_p_activities___15==1 | sai_p_activities___17==1 | 
                                  sai_p_activities___21==1 ~ 1,
                                  T~0),
    sai_p_non_team_sports = case_when(sai_p_activities___3==1 | sai_p_activities___6==1 |
                                  sai_p_activities___9==1 | sai_p_activities___10==1 |
                                  sai_p_activities___7==1 | sai_p_activities___8==1 |
                                  sai_p_activities___13==1 | sai_p_activities___14==1 |
                                  sai_p_activities___16==1 | sai_p_activities___18==1 | 
                                  sai_p_activities___19==1 | sai_p_activities___20==1 | sai_p_activities___22==1 ~ 1,
                                  T~0),
    sai_p_other = case_when(sai_p_activities___26==1 | sai_p_activities___27 ==1 | sai_p_activities___28==1 | sai_p_activities___29==1 ~ 1,
                            T~0),
    sai_p_any = case_when(sai_p_perf_arts==1 | sai_p_team_sports ==1 | sai_p_non_team_sports==1 | sai_p_other==1 ~ 1,
                          T~0)
  ) %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id, 
                sai_p_activities___24, sai_p_perf_arts,
                sai_p_team_sports,sai_p_non_team_sports,
                sai_p_other,sai_p_any) 


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
  mutate(screentime1_p_hours = screentime1_p_hours + screentime1_p_minutes/60,
         screentime2_p_hours = screentime2_p_hours + screentime2_p_minutes/60) %>%
  dplyr::select(src_subject_id,screentime1_p_hours,
                screentime2_p_hours)


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

#-- Tempoerament/personality: BIS/BAS
mh_y_bisbas <- read.csv("./mental-health/mh_y_bisbas.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id,bis_y_ss_bas_drive,bis_y_ss_bas_fs,
                bis_y_ss_bas_rr,bis_y_ss_bis_sum)

#-- Tempoerament/personality: BIS/BAS
mh_y_upps <- read.csv("./mental-health/mh_y_upps.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id,upps_y_ss_positive_urgency,
                upps_y_ss_negative_urgency, upps_y_ss_lack_of_planning,
                upps_y_ss_lack_of_perseverance, upps_y_ss_sensation_seeking)


#-- Child psychopathology (parent-reported CBCL)
mh_p_cbcl <- read.csv("./mental-health/mh_p_cbcl.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id,
                cbcl_scr_syn_anxdep_r,cbcl_scr_syn_withdep_r,
                cbcl_scr_syn_somatic_r,cbcl_scr_syn_social_r,
                cbcl_scr_syn_thought_r,cbcl_scr_syn_attention_r,
                cbcl_scr_syn_rulebreak_r,cbcl_scr_syn_aggressive_r)

# #-- Child psychopathology (teacher-reported Brief Problem Monitor)
# mh_t_bpm <- read.csv("./mental-health/mh_t_bpm.csv") %>%
#   #-- Select baseline visit
#   filter(eventname=="baseline_year_1_arm_1") %>%
#   dplyr::select(src_subject_id,bpm_t_scr_internal_t,
#                 bpm_t_scr_external_t,bpm_t_scr_attention_t,bpm_t_scr_totalprob_t)


#-- Number of friends / close friends
mh_y_or <- read.csv("./mental-health/mh_y_or.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  mutate(
    num_friends = resiliency5a_y + resiliency6a_y,
    num_close_friends = resiliency5b_y + resiliency6b_y
  ) %>%
  dplyr::select(src_subject_id,num_friends,num_close_friends)


#-- Community risk and protective factors (parent-reported)
su_p_crpf <- read.csv("./substance-use/su_p_crpf.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>% 
  mutate(across(su_risk_p_1:su_risk_p_13, ~ dplyr::recode(.,`0`=0,`1`=1,`2`=2,`3`=3,`4`=NA_real_,.default = NaN))) %>%
  mutate(
    parent_su_comm_risk = rowSums(select(., starts_with("su_risk_p")), na.rm = TRUE)
  ) %>%
  dplyr::select(src_subject_id,parent_su_comm_risk)

#-- Rules around substance use (parent-reported)
su_p_pr <- read.csv("./substance-use/su_p_pr.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  mutate(across(c(parent_rules_q1, parent_rules_q4, parent_rules_q7, parent_rules_q_10),
                ~ dplyr::recode(.,`6`=0,`1`=1,`2`=2,`3`=3,`4`=4, `5`=5,.default = NaN))) %>%
  mutate(
    parent_subst_rules = rowSums(select(., c("parent_rules_q1", "parent_rules_q4", "parent_rules_q7", "parent_rules_q_10")), na.rm = TRUE),
    parent_no_subst_rules = case_when(parent_subst_rules==0 ~ 1 ,
                                      parent_subst_rules>0 ~ 0)
  ) %>%
  dplyr::select(src_subject_id,parent_subst_rules,parent_no_subst_rules)

cor(su_p_pr$parent_subst_rules,su_p_pr$parent_no_subst_rules)

#-- Peer Deviance (youth-reported)
su_y_peerdevia <- read.csv("./substance-use/su_y_peerdevia.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  mutate(
    peer_deviance = rowSums(select(., c("peer_deviance_1_4bbe5d", "peer_deviance_2_dd1457", "peer_deviance_3_e1ec2e",
                                              "peer_deviance_4_b6c588", "peer_deviance_5_bffa44", "peer_deviance_6_69562e", 
                                              "peer_deviance_7_beb683", "peer_deviance_8_35702e", "peer_deviance_9_6dd4ef")), na.rm = TRUE),
    some_peer_deviance = ifelse(peer_deviance>0,1,0)
  ) %>%
  dplyr::select(src_subject_id, peer_deviance, some_peer_deviance)


#-- Intention to use substances (alcohol/tobacco/marijuana) (youth-reported)
su_y_path_intuse <- read.csv("./substance-use/su_y_path_intuse.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  mutate(across(c(path_alc_youth1:path_alc_youth9),
                ~ dplyr::recode(.,`6`=NA_real_,`5`=NA_real_,`1`=3,`2`=2,`3`=1,`4`=0, .default = NaN))) %>%
  mutate(
    curious_subst = rowSums(select(., c("path_alc_youth1", "path_alc_youth2", "path_alc_youth3")), na.rm = TRUE),
    tobacco_intension = rowSums(select(., c("path_alc_youth1","path_alc_youth4","path_alc_youth7")), na.rm=TRUE),
    alc_intension = rowSums(select(., c("path_alc_youth2","path_alc_youth5","path_alc_youth8")), na.rm=TRUE),
    mj_intension = rowSums(select(., c("path_alc_youth3","path_alc_youth6","path_alc_youth9")), na.rm=TRUE)
  ) %>%
  dplyr::select(src_subject_id, curious_subst, tobacco_intension, alc_intension, mj_intension)



#-------------------------------------------------------------------------#
#-- Assemble the data set
data_list <- list()
data_list <- append(data_list, list(dat,abcd_y_lt,ce_y_srpf,ce_p_nsc,
                                   ce_p_fes,ce_y_fes,ce_y_crpbi,
                                   ce_y_pm, ce_p_psb, ce_y_psb,led_l_coi,
                                   led_l_denspop, led_l_urban, led_l_walk,
                                   led_l_traffic, led_l_roadprox, led_l_svi,
                                   led_l_crime, led_l_leadrisk, led_l_urbsat,
                                   led_l_dissim, led_l_expint, led_l_ice,
                                   led_l_entropy, led_l_biasrace, led_l_biasgender,
                                   led_l_aca, abcd_p_demo, mh_p_ksads_ptsd,
                                   mh_p_ksads_bg,mh_p_fhx, mh_p_asr, ph_p_pds,
                                   ph_p_dhx,ph_y_yrb,ph_p_saiq,
                                   ph_p_sds,nt_p_stq,mh_y_bisbas,mh_y_upps,
                                   mh_p_cbcl,mh_y_or,su_p_crpf,
                                   su_p_pr,su_y_peerdevia, su_y_path_intuse))
length(data_list)


dat_all <- Reduce(function(x, y) merge(x, y, by="src_subject_id", all=TRUE), data_list) %>%
  mutate(
    demo_prtnr_ed_v2 = ifelse(is.na(demo_prtnr_ed_v2),demo_prnt_ed_v2,demo_prtnr_ed_v2)
  )
names(dat_all)
save(dat_all,file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/temp_dat/dat_all.Rda")


#-- Non-missing INR - retain 9572 out of 11868 obs
dat_all_inr <- dat_all %>% filter(!is.na(inr))

save(dat_all_inr,file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/temp_dat/dat_all_inr.Rda")

#-- Complete cases on all considered predictors - only 5316 out of 11868
dat_all_inr_complete<-dat_all_inr[complete.cases(dat_all_inr),]

save(dat_all_inr_complete,file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/temp_dat/dat_all_inr_complete.Rda")





#------------------------------------------------------------------------#
#-- Impute missing predictor data among those with non-missing INR/cog --#


#--------------------------------------------------------------------#
#-- Imputation code source: https://rpubs.com/kaz_yos/mice-exclude --#

## Proportion missing
prom_miss <- gather(dat_all_inr, key="src_subject_id") %>%
  group_by(src_subject_id) %>%
  summarize(prop_na = mean(is.na(value))) 

## Configure parallelization
## Detect core count
nCores <- min(parallel::detectCores(), 8)
## Used by parallel::mclapply() as default
options(mc.cores = nCores)
## Used by doParallel as default
options(cores = nCores)
## Register doParallel as the parallel backend with foreach
## http://stackoverflow.com/questions/28989855/the-difference-between-domc-and-doparallel-in-r
doParallel::registerDoParallel(cores = nCores)
## Report multicore use
cat("### Using", foreach::getDoParWorkers(), "cores\n")
cat("### Using", foreach::getDoParName(), "as backend\n")

## Extract all variable names in dataset
allVars <- names(dat_all_inr)
allVars

## names of variables with missingness
missVars <- names(dat_all_inr)[colSums(is.na(dat_all_inr)) > 0]
missVars

predictorMatrix <- matrix(0, ncol = length(allVars), nrow = length(allVars))
rownames(predictorMatrix) <- allVars


colnames(predictorMatrix) <- allVars

names(dat_all_inr)
cat("
###  Specify Variables informing imputation\n")
## These can be either complete variables or variables with missingness.
## Those with missingness must be imputed - need to exclude men_date_12y
## Explicitly specify.
imputerVars <- names(dat_all_inr)[c(3:198)]
imputerVars 
## Keep variables that actually exist in dataset
imputerVars <- intersect(unique(imputerVars), allVars)
imputerVars
imputerMatrix <- predictorMatrix
imputerMatrix[,imputerVars] <- 1
imputerMatrix


cat("
###  Specify variables with missingness to be imputed \n")
## Could specify additional variables that are imputed,
## but does not inform imputation.
imputedOnlyVars <- c()
## Imputers that have missingness must be imputed.
imputedVars <- intersect(unique(c(imputedOnlyVars, imputerVars)), missVars)
imputedVars
imputedMatrix <- predictorMatrix
imputedMatrix[imputedVars,] <- 1
imputedMatrix

cat("
###  Construct a full predictor matrix (rows: imputed variables; cols: imputer variables)\n")
## Keep correct imputer-imputed pairs only
predictorMatrix <- imputerMatrix * imputedMatrix
## Diagonals must be zeros (a variable cannot impute itself)
diag(predictorMatrix) <- 0
predictorMatrix


set.seed(123)

cat("
###  Dry-run mice for imputation methods\n")
dryMice <- mice(data = dat_all_inr, m = 1, predictorMatrix = predictorMatrix, maxit = 0)

## Update predictor matrix
predictorMatrix <- dryMice$predictorMatrix
cat("###   Imputers (non-zero columns of predictorMatrix)\n")
imputerVars <- colnames(predictorMatrix)[colSums(predictorMatrix) > 0]
imputerVars
cat("###   Imputed (non-zero rows of predictorMatrix)\n")
imputedVars <- rownames(predictorMatrix)[rowSums(predictorMatrix) > 0]
imputedVars
cat("###   Imputers that are complete\n")
setdiff(imputerVars, imputedVars)
cat("###   Imputers with missingness\n")
intersect(imputerVars, imputedVars)
cat("###   Imputed-only variables without being imputers\n")
setdiff(imputedVars, imputerVars)
cat("###   Variables with missingness that are not imputed\n")
setdiff(missVars, imputedVars)
cat("###   Relevant part of predictorMatrix\n")
predictorMatrix[rowSums(predictorMatrix) > 0, colSums(predictorMatrix) > 0]


## Empty imputation method to really exclude variables
## http://www.stefvanbuuren.nl/publications/MICE%20in%20R%20-%20Draft.pdf
##
## MICE will automatically skip imputation of variables that are complete.
## One of the problems in previous versions of MICE was that all incomplete
## data needed to be imputed. In MICE 2.0 it is possible to skip imputation
## of selected incomplete variables by specifying the empty method "".
## This works as long as the incomplete variable that is skipped is not being
## used as a predictor for imputing other variables.
## Note: puttting zeros in the predictorMatrix alone is NOT enough!
##
dryMice$method[setdiff(allVars, imputedVars)] <- ""
cat("###   Methods used for imputation\n")
dryMice$method[sapply(dryMice$method, nchar) > 0]

#-- Save Imputed Data!
d_imp <- dryMice
save(d_imp, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/temp_dat/d_imp.Rdata")

dim(complete(d_imp))
