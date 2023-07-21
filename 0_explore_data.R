#----------------------------------------------------------------------------#
#--- 0_explore_data.R
#--- Date: 7/13/2023
#----------------------------------------------------------------------------#

library(tidyverse)
library(quadprog)
library(haven)
library(ggplot2)
library(kableExtra)
library(raters)
library(gmodels)
library(ggpubr)
library(stargazer)
library(gtsummary)
library(tableone)
library(ggplot2)
library(hrbrthemes)
library(viridis)

setwd("/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/release5/core/")

abcd_p_demo <- read.csv("./abcd-general/abcd_p_demo.csv") %>% 
  
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  
  #-- Recode variables for analysis
  mutate(
    #-- Recode household size < 2 to 2; 
    hhsize = ifelse(demo_roster_v2<2,2,demo_roster_v2),
    #-- Set midpoints to household income ranges
    hhinc_mid = case_when(
      demo_comb_income_v2 == 1 ~ 2500,
      demo_comb_income_v2 == 2 ~ 8500,
      demo_comb_income_v2 == 3 ~ 14000,
      demo_comb_income_v2 == 4 ~ 20500,
      demo_comb_income_v2 == 5 ~ 30000,
      demo_comb_income_v2 == 6 ~ 42000,
      demo_comb_income_v2 == 7 ~ 62500,
      demo_comb_income_v2 == 8 ~ 87500,
      demo_comb_income_v2 == 9 ~ 150000,
      demo_comb_income_v2 == 10 ~ 200000,
      demo_comb_income_v2 %in% c(777,999) ~ NA_real_),
    #-- Income-to-needs ratio based on Census 2015 poverty thresholds
    inr = case_when(
      hhsize == 2 ~ hhinc_mid / 16337,
      hhsize == 3 ~ hhinc_mid / 19096,
      hhsize == 4 ~ hhinc_mid / 24120,
      hhsize == 5 ~ hhinc_mid / 27853,
      hhsize == 6 ~ hhinc_mid / 31078,
      hhsize == 7 ~ hhinc_mid / 34077,
      hhsize == 8 ~ hhinc_mid / 38077,
      hhsize >= 9 ~ hhinc_mid / 45822,
      is.na(hhsize) & is.na(hhinc_mid) ~ NA_real_
    ),
    #-- Highest level of parental education - if no partner, just primary parent education
    #-- First recode 777 & 999 to NA_real_
    demo_prnt_ed_v2 = ifelse(demo_prnt_ed_v2==777,NA_real_,demo_prnt_ed_v2),
    demo_prtnr_ed_v2 = ifelse(demo_prtnr_ed_v2 %in% c(777,999),NA_real_,demo_prtnr_ed_v2),
    p_ed = case_when(
      demo_prnt_prtnr_v2 == 1 & !is.na(demo_prtnr_ed_v2) ~ pmax(demo_prnt_ed_v2,demo_prtnr_ed_v2),
      T ~ demo_prnt_ed_v2
    ),
    p_ed_hs_ormore = ifelse(p_ed>=13,1,0),
    p_ed_scol_ormore = ifelse(p_ed>=15,1,0),
    p_ed_ba_ormore = ifelse(p_ed>=18,1,0),
    p_ed_ma_ormore = ifelse(p_ed>=19,1,0)
  ) %>%
  dplyr::select(src_subject_id, inr, p_ed)


#-- Pull in neighborhood SES (ADI at the primary residential address)
led_l_adi <- read.csv("./linked-external-data/led_l_adi.csv") %>%
  dplyr::select(src_subject_id, reshist_addr1_adi_perc)


#-- Pull in cognitive function variables
cog <- read.csv("./neurocognition/nc_y_nihtb.csv") %>%
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id, nihtbx_fluidcomp_agecorrected, nihtbx_cryst_agecorrected, nihtbx_totalcomp_agecorrected)

#~~ Merge the data sets
data_list <- list(abcd_p_demo, led_l_adi, cog)
dat <- Reduce(function(x, y) merge(x, y, by="src_subject_id", all=TRUE),data_list)
save(dat,file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/temp_dat/dat.Rda")

names(dat)


#--------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-- Summarize the data - compare distributions of these few variables by INR quartiles or quintiles (maybe then remove the middle quintile, and compare 1,2 vs 4,5?)

dat %>% 
  reframe(quantile = scales::percent(c(0.2,0.4,0.6,0.8,1)),
          inr_quint = quantile(inr, c(0.2,0.4,0.6,0.8,1)))

dat <- dat %>%
  mutate(
    inr_quint = case_when(
      inr<1.351438 ~ 1,
      inr<2.591211 ~ 2,
      inr<4.826565 ~ 3,
      inr<6.218905 ~ 4,
      T~5),
    poverty = ifelse(inr<=1,1,0)
  )

names(dat)

## Create a variable list
vars <- names(dat)[2:7]

## Create Table 1a stratified by poverty
table_by_pov <- CreateTableOne(vars = vars,
                               strata = c("poverty"), 
                               includeNA = F, 
                               addOverall = T,
                               data = dat)
table_by_pov <- data.frame(print(table_by_pov, missing=F, catDigits = 1, contDigits = 1))
table_by_pov <- tibble::rownames_to_column(table_by_pov, "Characteristic")

# Column labels
names(table_by_pov) <- c("Characteristic", "Overall", "INR_gt_1", "INR_le_1", "p_value","test")

dim(table_by_pov)

# Row labels

table_by_pov <- data.frame(table_by_pov) %>%
  mutate(
    rownames = case_when(
      Characteristic=="inr (mean (SD))" ~ "Income-to-needs ratio (mean (SD))",
      Characteristic=="p_ed (mean (SD))" ~ "Parental educaiton - highest among parents/caretakers (mean (SD))",
      Characteristic=="reshist_addr1_adi_perc (mean (SD))" ~ "ADI for the primary residence (mean (SD))",
      Characteristic=="nihtbx_fluidcomp_agecorrected (mean (SD))" ~ "NIH toolbox: fluid intelligence composite, age corrected (mean (SD))",
      Characteristic=="nihtbx_cryst_agecorrected (mean (SD))" ~ "NIH toolbox: crystallized intelligence composite, age corrected (mean (SD))",
      Characteristic=="nihtbx_totalcomp_agecorrected (mean (SD))" ~ "NIH toolbox: total intelligence composite, age corrected (mean (SD))",
      T ~ Characteristic
    )
  ) %>%
  dplyr::select(rownames,INR_le_1,INR_gt_1,Overall,p_value)

table_by_pov
write.csv(table_by_pov, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/table_by_pov.csv")

#-- Plot distributions of cog outcomes by poverty
fluid <- ggplot(data=dat, aes(x=nihtbx_fluidcomp_agecorrected, group=as.factor(poverty), fill=as.factor(poverty))) +
  geom_histogram(alpha=.6) +
  theme_ipsum()
fluid
crystal <- ggplot(data=dat, aes(x=nihtbx_cryst_agecorrected, group=as.factor(poverty), fill=as.factor(poverty))) +
  geom_histogram(alpha=.6) +
  theme_ipsum()
crystal
total <- ggplot(data=dat, aes(x=nihtbx_totalcomp_agecorrected, group=as.factor(poverty), fill=as.factor(poverty))) +
  geom_histogram(alpha=.6) +
  theme_ipsum()
total

fig1a <- ggarrange(fluid, crystal, total, 
          ncol=3, nrow=1, common.legend = TRUE,
          legend="bottom")
fig1a


#-----------------------------------------------------------------------------------------
#-- Table by INR quintiles
## Create Table 1a stratified by poverty
table_by_pov <- CreateTableOne(vars = vars,
                               strata = c("inr_quint"), 
                               includeNA = F, 
                               addOverall = T,
                               data = dat)
table_by_pov <- data.frame(print(table_by_pov, missing=F, catDigits = 1, contDigits = 1))
table_by_pov <- tibble::rownames_to_column(table_by_pov, "Characteristic")

# Column labels
names(table_by_pov) <- c("Characteristic", "Overall", "INR_q1", "INR_q2", "INR_q3", "INR_q4", "INR_q5", "p_value","test")

dim(table_by_pov)

# Row labels

table_by_pov <- data.frame(table_by_pov) %>%
  mutate(
    rownames = case_when(
      Characteristic=="inr (mean (SD))" ~ "Income-to-needs ratio (mean (SD))",
      Characteristic=="p_ed (mean (SD))" ~ "Parental educaiton - highest among parents/caretakers (mean (SD))",
      Characteristic=="reshist_addr1_adi_perc (mean (SD))" ~ "ADI for the primary residence (mean (SD))",
      Characteristic=="nihtbx_fluidcomp_agecorrected (mean (SD))" ~ "NIH toolbox: fluid intelligence composite, age corrected (mean (SD))",
      Characteristic=="nihtbx_cryst_agecorrected (mean (SD))" ~ "NIH toolbox: crystallized intelligence composite, age corrected (mean (SD))",
      Characteristic=="nihtbx_totalcomp_agecorrected (mean (SD))" ~ "NIH toolbox: total intelligence composite, age corrected (mean (SD))",
      T ~ Characteristic
    )
  ) %>%
  dplyr::select(rownames,INR_q1, INR_q2, INR_q3, INR_q4, INR_q5, Overall, p_value)

table_by_pov
write.csv(table_by_pov, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/table_by_inr_quint.csv")

#-- Plot distributions of cog outcomes by INR quintiles
fluid <- ggplot(data=dat, aes(x=nihtbx_fluidcomp_agecorrected, group=as.factor(inr_quint), fill=as.factor(inr_quint))) +
  geom_histogram(alpha=.6) +
  theme_ipsum()
fluid
crystal <- ggplot(data=dat, aes(x=nihtbx_cryst_agecorrected, group=as.factor(inr_quint), fill=as.factor(inr_quint))) +
  geom_histogram(alpha=.6) +
  theme_ipsum()
crystal
total <- ggplot(data=dat, aes(x=nihtbx_totalcomp_agecorrected, group=as.factor(inr_quint), fill=as.factor(inr_quint))) +
  geom_histogram(alpha=.6) +
  theme_ipsum()
total

fig1b <- ggarrange(fluid, crystal, total, 
                   ncol=3, nrow=1, common.legend = TRUE,
                   legend="bottom")
fig1b

