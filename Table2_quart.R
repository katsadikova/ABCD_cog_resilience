library(dplyr)
library(ggplot2)
library(tidyr)

#-- 1x imputed data for 9572 out of 11868 with non-missing INR
load("/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/temp_dat/d_imp.Rdata")

d <- complete(d_imp)

#-- Load in variables selected by LASSO with SHAP values from XGBOOST
load("/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/imp_vars_25_inr_fluid_folds.Rda")
load("/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/imp_vars_25_inr_cryst_folds.Rda")


#-- Drop variables with SHAP=0
imp_vars_cryst <- imp_vars_cryst %>% filter(round(tert_inr_cryst_imp_vars,2)>0)
imp_vars_fluid <- imp_vars_fluid %>% filter(round(tert_inr_fluid_imp_vars,2)>0)


#-- Run logistic regressions with remaining important covariates, store coefficients

#-- Fluid
d_pov_highflcog <- d %>% filter(pov_highflcog==1) %>% 
  mutate(profile="pov_highflcog",
         resilience=1)
d_pov_lowflcog <- d %>% filter(pov_lowflcog==1) %>% 
  mutate(profile="pov_lowflcog",
         resilience=0)
d_flpov <- rbind(d_pov_highflcog,d_pov_lowflcog)


fl <- data.frame(coef(summary(glm(data=d_flpov, formula(paste("resilience ~ ",paste(imp_vars_fluid$Variable, collapse ="+"), collapse = " ")), family="binomial"))))
fl <- tibble::rownames_to_column(fl, "Variable")
names(fl) <- c("Variable","Estimate","Std..Error","z.value","p")

fl <- fl %>%
  mutate(
    OR=round(exp(Estimate),2),
    ci_l = round(exp(Estimate - 1.96*Std..Error),2),
    ci_u = round(exp(Estimate + 1.96*Std..Error),2),
    sig = case_when(
      p < 0.0001 ~ "***",
      p < 0.01 ~ "**",
      p < 0.05 ~ "*",
      T~""
    ),
    fl_or_ci = paste0(OR," (",ci_l, ",", ci_u, ")",sig)
  ) %>%
  dplyr::select(Variable,fl_or_ci)


#-- Crystallized

d_pov_highcrcog <- d %>% filter(pov_highcrcog==1) %>% 
  mutate(profile="pov_highcrcog",
         resilience=1)
d_pov_lowcrcog <- d %>% filter(pov_lowcrcog==1) %>% 
  mutate(profile="pov_lowcrcog",
         resilience=0)
d_crpov <- rbind(d_pov_highcrcog,d_pov_lowcrcog)


cr <- data.frame(coef(summary(glm(data=d_crpov, formula(paste("resilience ~ ",paste(imp_vars_cryst$Variable, collapse ="+"), collapse = " ")), family="binomial"))))
cr <- tibble::rownames_to_column(cr, "Variable")
names(cr) <- c("Variable","Estimate","Std..Error","z.value","p")

cr <- cr %>%
  mutate(
    OR=round(exp(Estimate),2),
    ci_l = round(exp(Estimate - 1.96*Std..Error),2),
    ci_u = round(exp(Estimate + 1.96*Std..Error),2),
    sig = case_when(
      p < 0.0001 ~ "***",
      p < 0.01 ~ "**",
      p < 0.05 ~ "*",
      T~""
    ),
    cr_or_ci = paste0(OR," (",ci_l, ",", ci_u, ")",sig)
  ) %>%
  dplyr::select(Variable,cr_or_ci)


#-- Load in updated data dictionary - with concise variable labels and domains
dict <- read.csv("/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/temp_dat/Predictors_to_merge.csv")
names(dict)


#-- Merge together the odds ratios
table2 <- merge(x=fl,y=cr,by="Variable",all=T)

#-- Add labels
table2_labels <- merge(x=table2,y=dict,by="Variable",all.x=T) %>%
  dplyr::select(Domain,Short_Label,Order,Variable,fl_or_ci,cr_or_ci) 

write.csv(table2_labels, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/drafts/Table2_quart.csv")

names(dict)

#-- Make lists of variables only for fluid, only for cryst, for both - for Figure 3 (parts of which will be created)
#-- in the 7_SHAP_.. scripts 
only_fluid <- table2_labels %>% 
  filter(is.na(table2_labels$cr_or_ci) & Variable != "(Intercept)") 
only_fluid <- merge(x=only_fluid,y=imp_vars_fluid,by="Variable",all.x=T) %>% 
  dplyr::select(Variable, Short_Label, tert_inr_fluid_imp_vars) %>% 
  arrange(desc(tert_inr_fluid_imp_vars))


only_cryst <- table2_labels %>% 
  filter(is.na(fl_or_ci) & Variable != "(Intercept)") 
only_cryst <- merge(x=only_cryst,y=imp_vars_cryst,by="Variable",all.x=T) %>% 
  dplyr::select(Variable, Short_Label, tert_inr_cryst_imp_vars) %>% 
  arrange(desc(tert_inr_cryst_imp_vars))

#-- don't need to order these by SHAP value - different order for cryst/fluid
both <- table2_labels %>% 
  filter(!is.na(fl_or_ci) & !is.na(cr_or_ci) & Variable != "(Intercept)") %>% 
  arrange(Domain) %>%
  dplyr::select(Variable, Short_Label)

save(only_fluid,file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/only_fluid_for_Fig3.Rda")
save(only_cryst,file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/only_cryst_for_Fig3.Rda")
save(both,file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/both_for_Fig3.Rda")



#--------------------------------------------------------------------------------#
#-- Look at simplified logistic regressions - common important predictors only --#

#-- Identify variables that have non-zero coefficients for both fluid/cryst resilience
common <- merge(x=fl,y=cr,by="Variable")[c(2:10),] #-- Exclude intercept
common

coef_fl <- data.frame(coef(summary(glm(data=d_flpov, formula(paste("resilience ~ ",paste(common$Variable, collapse ="+"), collapse = " ")), family="binomial"))))
coef_fl <- tibble::rownames_to_column(coef_fl, "Variable")
names(coef_fl) <- c("Variable","Estimate","Std..Error","z.value","p")

fl <- coef_fl %>%
  mutate(
    OR=round(exp(Estimate),2),
    ci_l = round(exp(Estimate - 1.96*Std..Error),2),
    ci_u = round(exp(Estimate + 1.96*Std..Error),2),
    sig = case_when(
      p < 0.0001 ~ "***",
      p < 0.01 ~ "**",
      p < 0.05 ~ "*",
      T~""
    ),
    fl_or_ci = paste0(OR," (",ci_l, ",", ci_u, ")",sig)
  ) %>%
  dplyr::select(Variable,fl_or_ci)


coef_cr <- data.frame(coef(summary(glm(data=d_crpov, formula(paste("resilience ~ ",paste(common$Variable, collapse ="+"), collapse = " ")), family="binomial"))))
coef_cr <- tibble::rownames_to_column(coef_cr, "Variable")
names(coef_cr) <- c("Variable","Estimate","Std..Error","z.value","p")

cr <- coef_cr %>%
  mutate(
    OR=round(exp(Estimate),2),
    ci_l = round(exp(Estimate - 1.96*Std..Error),2),
    ci_u = round(exp(Estimate + 1.96*Std..Error),2),
    sig = case_when(
      p < 0.0001 ~ "***",
      p < 0.01 ~ "**",
      p < 0.05 ~ "*",
      T~""
    ),
    cr_or_ci = paste0(OR," (",ci_l, ",", ci_u, ")",sig)
  ) %>%
  dplyr::select(Variable,cr_or_ci)

#-- Merge together the odds ratios from simplified models
table2_simp <- merge(x=fl,y=cr,by="Variable",all=T)

#-- Add labels
table2_simp_labels <- merge(x=table2_simp,y=dict,by="Variable",all.x=T) %>%
  dplyr::select(Domain,Short_Label,Order,Variable,fl_or_ci,cr_or_ci) 

write.csv(table2_simp_labels, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/drafts/Table2_simp.csv")

