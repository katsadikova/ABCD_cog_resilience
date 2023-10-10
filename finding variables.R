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

stq <- read.csv("./novel-technologies/nt_p_stq.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1")

names(stq)

psq <- read.csv("./novel-technologies/nt_p_psq.csv")

fitbitslp <- read.csv("./novel-technologies/nt_y_fitb_slp_d.csv")%>%
  #-- Select baseline visit
  filter(eventname=="4_year_follow_up_y_arm_1")

summary(as.factor(fitbitslp$eventname))

length(unique(fitbitslp$src_subject_id))



sub <- read.csv("./substance-use/su_y_sui.csv")

summary(as.factor(sub$su_tlfb_vaped_something___17))

saiq <- data.frame(read.csv("./physical-health/ph_p_saiq.csv"))  %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id, sai_p_activities___0:sai_p_activities___29)

summary(saiq)
