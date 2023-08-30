#----------------------------------------------------------------------------#
#--- 1_LPA.R
#--- Date: 7/19/2023
#----------------------------------------------------------------------------#

#~~ Resource: 
# https://www.youtube.com/watch?v=h7RVeO-P3zc
# https://willhipson.netlify.app/post/latent-profile/latent-profile/#:~:text=Latent%20Profile%20Analysis%20(LPA)%20tries,of%20responses%20on%20indicator%20items.

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

#-- Exclude rows with missing data (for now)
dat <- na.omit(dat[,c(1,5,8:16)]) %>%
  mutate(fluid_s = as.numeric(scale(nihtbx_fluidcomp_agecorrected)),
         cryst_s = as.numeric(scale(nihtbx_cryst_agecorrected)),
         totcomp_s = as.numeric(scale(nihtbx_totalcomp_agecorrected)))  
names(dat)

summary(as.numeric(dat$adi_s))

#-- Look at simple correlations
vars <- names(dat)[2:7]
cor(dat[,2:7])


#-----------------------------------------------------------------------#
#--- Use tidyLPA to compare estimates from several cluster solutions ---#
#-----------------------------------------------------------------------#

#-- tidyLPA with estimation using mclust can run 4 models:
#--------------------------------------------------------
# 1. Equal variances, and covariances fixed to 0 (model 1)
# 2. Varying variances and covariances fixed to 0 (model 2)
# 3. Equal variances and equal covariances (model 3)
# ONLY MPLUS: 4. Varying means, varying variances, and equal covariances (model 4)
# ONLY MPLUS: 5. Varying means, equal variances, and varying covariances (model 5)
# 6. Varying variances and varying covariances (model 6)

set.seed(123)

allSES <- dat %>% select(fluid_s, cryst_s, totcomp_s, inr_s, p_ed_s, adi_s) %>%
        single_imputation() %>%
        estimate_profiles(2:8,
                          variances = c("equal", "varying"),
                          covariances = c("zero", "varying")) 
save(allSES, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/allSES.Rda")

fit_stats_allSES <- get_fit(allSES)
write.csv(fit_stats_allSES, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/fit_stats_allSES.csv")
compare_fit_allSES <- compare_solutions(allSES,statistics = c("AIC", "BIC"))
compare_fit_allSES

#- Model 6, 8 clusters
plot_profiles(allSES$model_6_class_8, add_line = T, bw=F)
check <- get_data(allSES$model_6_class_8)
summary(as.factor(check$Class))

#- Model 6, 7 clusters
plot_profiles(allSES$model_6_class_7, add_line = T, bw=F)
check <- get_data(allSES$model_6_class_7)
summary(as.factor(check$Class))

#- Model 6, 6 clusters
plot_profiles(allSES$model_6_class_6, add_line = T, bw=F)
check <- get_data(allSES$model_6_class_6)
summary(as.factor(check$Class))

#- Model 6, 5 clusters
plot_profiles(allSES$model_6_class_5, add_line = T, bw=F)
check <- get_data(allSES$model_6_class_5)
summary(as.factor(check$Class))


names(dat)
mclust_BIC_allSES <- mclustBIC(dat[,c(8:13)])
summary(mclust_BIC_allSES) # Maxing out cluster space - not stable

#-------------------------------------------------------------------------------
hhSES <- dat %>% select(fluid_s, cryst_s, totcomp_s, inr_s, p_ed_s) %>%
  single_imputation() %>%
  estimate_profiles(2:8,
                    variances = c("equal", "varying"),
                    covariances = c("zero", "varying")) 
save(hhSES, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/hhSES.Rda")

fit_stats_hhSES <- get_fit(hhSES)
write.csv(fit_stats_hhSES, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/fit_stats_hhSES.csv")
compare_fit_hhSES <- compare_solutions(hhSES,statistics = c("AIC", "BIC"))
compare_fit_hhSES # Model 6 with 7 clusters best, doesn't converge at all with 8

#- Model 6, 7 clusters
plot_profiles(hhSES$model_6_class_7, add_line = T, bw=F)
check <- get_data(hhSES$model_6_class_7)
summary(as.factor(check$Class))

#- Model 6, 6 clusters
plot_profiles(hhSES$model_6_class_6, add_line = T, bw=F)
check <- get_data(hhSES$model_6_class_6)
summary(as.factor(check$Class))

#- Model 6, 5 clusters
plot_profiles(hhSES$model_6_class_5, add_line = T, bw=F)
check <- get_data(hhSES$model_6_class_5)
summary(as.factor(check$Class))

#- Model 6, 4 clusters
plot_profiles(hhSES$model_6_class_4, add_line = T, bw=F)
check <- get_data(hhSES$model_6_class_4)
summary(as.factor(check$Class))

mclust_BIC_hhSES <- mclustBIC(dat[,c(8,9,11:13)])
summary(mclust_BIC_hhSES) # More stable - suggests 8-cluster solution VVE

#-------------------------------------------------------------------------------
inrSES <- dat %>% select(fluid_s, cryst_s, totcomp_s, inr_s) %>%
  single_imputation() %>%
  estimate_profiles(2:8,
                    variances = c("equal", "varying"),
                    covariances = c("zero", "varying")) 
save(inrSES, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/inrSES.Rda")

fit_stats_inrSES <- get_fit(inrSES)
write.csv(fit_stats_inrSES, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/fit_stats_inrSES.csv")
compare_fit_inrSES <- compare_solutions(inrSES,statistics = c("AIC", "BIC"))
compare_fit_inrSES

#- Model 6, 6 clusters
plot_profiles(inrSES$model_6_class_6, add_line = T, bw=F)
check <- get_data(inrSES$model_6_class_6)
summary(as.factor(check$Class))

#- Model 6, 5 clusters
plot_profiles(inrSES$model_6_class_5, add_line = T, bw=F)
check <- get_data(inrSES$model_6_class_5)
summary(as.factor(check$Class))

#- Model 6, 4 clusters
plot_profiles(inrSES$model_6_class_4, add_line = T, bw=F)
check <- get_data(inrSES$model_6_class_4)
summary(as.factor(check$Class))


#-------------------------------------------------------------------------------
#-- Drop Total Cog, include all 3 SES indicators

allSES_notot <- dat %>% select(fluid_s, cryst_s, inr_s, p_ed_s, adi_s) %>%
  single_imputation() %>%
  estimate_profiles(2:8,
                    variances =   c("equal", "varying", "equal", "varying"),
                    covariances = c("zero",  "zero",    "equal", "varying")) 
save(allSES_notot, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/allSES_notot.Rda")

fit_stats_allSES_notot <- get_fit(allSES_notot)
write.csv(fit_stats_allSES_notot, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/fit_stats_allSES_notot.csv")
compare_fit_allSES_notot <- compare_solutions(allSES_notot,statistics = c("AIC", "BIC"))
compare_fit_allSES_notot

#- Model 6, 8 clusters
plot_profiles(allSES_notot$model_6_class_8, add_line = T, bw=F)
check <- get_data(allSES_notot$model_6_class_8)
summary(as.factor(check$Class))
# vars <- names(check)[c(3:7)]
# table_by_pov <- CreateTableOne(vars = vars,
#                                strata = c("Class"), 
#                                includeNA = F, 
#                                addOverall = T,
#                                data = check)
# 
# table_by_pov

#- Model 6, 7 clusters
plot_profiles(allSES_notot$model_6_class_7, add_line = T, bw=F)
check <- get_data(allSES_notot$model_6_class_7)
summary(as.factor(check$Class))

#- Model 6, 6 clusters
plot_profiles(allSES_notot$model_6_class_6, add_line = T, bw=F)
check <- get_data(allSES_notot$model_6_class_6)
summary(as.factor(check$Class))

#-------------------------------------------------------------------------------
#-- Drop Total Cog, include HH SES indicators

hhSES_notot <- dat %>% select(fluid_s, cryst_s, inr_s, p_ed_s) %>%
  single_imputation() %>%
  estimate_profiles(2:8,
                    variances =   c("equal", "varying", "equal", "varying"),
                    covariances = c("zero",  "zero",    "equal", "varying")) 
save(hhSES_notot, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/hhSES_notot.Rda")

fit_stats_hhSES_notot <- get_fit(hhSES_notot)
write.csv(fit_stats_hhSES_notot, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/fit_stats_hhSES_notot.csv")
compare_fit_hhSES_notot <- compare_solutions(hhSES_notot,statistics = c("AIC", "BIC"))
compare_fit_hhlSES_notot

#- Model 6, 8 clusters
plot_profiles(hhSES_notot$model_6_class_8, add_line = T, bw=F)
check <- get_data(hhSES_notot$model_6_class_8)
summary(as.factor(check$Class))

#- Model 6, 6 clusters
plot_profiles(hhSES_notot$model_6_class_6, add_line = T, bw=F)
check <- get_data(hhSES_notot$model_6_class_6)
summary(as.factor(check$Class))

#-------------------------------------------------------------------------------
#-- Drop Total Cog, include all only INR 

inrSES_notot <- dat %>% select(fluid_s, cryst_s, inr_s) %>%
  single_imputation() %>%
  estimate_profiles(2:8,
                    variances =   c("equal", "varying", "equal", "varying"),
                    covariances = c("zero",  "zero",    "equal", "varying")) 
save(inrSES_notot, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/inrSES_notot.Rda")
load(file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/inrSES_notot.Rda")

fit_stats_inrSES_notot <- get_fit(inrSES_notot)
write.csv(fit_stats_inrSES_notot, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/fit_stats_inrSES_notot.csv")
compare_fit_inrSES_notot <- compare_solutions(inrSES_notot,statistics = c("AIC", "BIC"))
compare_fit_inrSES_notot

#- Model 6, 8 clusters
plot_profiles(inrSES_notot$model_6_class_8, add_line = T, bw=F)
check <- get_data(inrSES_notot$model_6_class_8)
summary(as.factor(check$Class))

#- Model 2, 8 clusters
plot_profiles(inrSES_notot$model_2_class_8, add_line = T, bw=F)
check <- get_data(inrSES_notot$model_2_class_8)
summary(as.factor(check$Class))


#-------------------------------------------------------------------------------
#-- Keep Only Total Cog, include all 3 SES indicators

allSES_tot <- dat %>% select(totcomp_s, inr_s, p_ed_s, adi_s) %>%
  single_imputation() %>%
  estimate_profiles(2:8,
                    variances =   c("equal", "varying", "equal", "varying"),
                    covariances = c("zero",  "zero",    "equal", "varying")) 
save(allSES_tot, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/allSES_tot.Rda")

fit_stats_allSES_tot <- get_fit(allSES_tot)
write.csv(fit_stats_allSES_tot, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/fit_stats_allSES_tot.csv")
compare_fit_allSES_tot <- compare_solutions(allSES_tot,statistics = c("AIC", "BIC"))
compare_fit_allSES_tot

#- Model 6, 8 clusters
plot_profiles(allSES_tot$model_6_class_8, add_line = T, bw=F)
check <- get_data(allSES_tot$model_6_class_8)
summary(as.factor(check$Class))

#- Model 6, 7 clusters
plot_profiles(allSES_tot$model_6_class_7, add_line = T, bw=F)
check <- get_data(allSES_tot$model_6_class_7)
summary(as.factor(check$Class))

#- Model 6, 6 clusters
plot_profiles(allSES_tot$model_6_class_6, add_line = T, bw=F)
check <- get_data(allSES_tot$model_6_class_6)
summary(as.factor(check$Class))


#-------------------------------------------------------------------------------
#-- Keep Only Total Cog, include only INR

inrSES_tot <- dat %>% select(totcomp_s, inr_s) %>%
  single_imputation() %>%
  estimate_profiles(2:8,
                    variances =   c("equal", "varying", "equal", "varying"),
                    covariances = c("zero",  "zero",    "equal", "varying")) 
save(inrSES_tot, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/inrSES_tot.Rda")
load(file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/inrSES_tot.Rda")

fit_stats_inrSES_tot <- get_fit(inrSES_tot)
write.csv(fit_stats_inrSES_tot, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/fit_stats_inrSES_tot.csv")
compare_fit_inrSES_tot <- compare_solutions(inrSES_tot,statistics = c("AIC", "BIC"))
compare_fit_inrSES_tot

#- Model 6, 5 clusters
plot_profiles(inrSES_tot$model_6_class_5, add_line = T, bw=F)
check <- get_data(inrSES_tot$model_6_class_5)
summary(as.factor(check$Class))

#- Model 6, 4 clusters
plot_profiles(inrSES_tot$model_6_class_4, add_line = T, bw=F)
check <- get_data(inrSES_tot$model_6_class_4)
summary(as.factor(check$Class))

#- Model 6, 3 clusters
plot_profiles(inrSES_tot$model_6_class_3, add_line = T, bw=F)
check <- get_data(inrSES_tot$model_6_class_3)
summary(as.factor(check$Class))




#-------------------------------------------------------------------------------
noSES <- dat %>% select(fluid_s, cryst_s, totcomp_s) %>%
  single_imputation() %>%
  estimate_profiles(2:8,
                    variances = c("equal", "varying"),
                    covariances = c("zero", "varying")) 
save(noSES, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/noSES_tidyLPA.Rda")
fit_stats_noSES <- get_fit(noSES)
write.csv(fit_stats_noSES, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/fit_stats_noSES.csv")
compare_fit_noSES <- compare_solutions(noSES,statistics = c("AIC", "BIC"))
compare_fit_noSES

#-------------------------------------------------------------------------------
onlySES <- dat %>% select(inr_s, p_ed_s, adi_s) %>%
  single_imputation() %>%
  estimate_profiles(2:8,
                    variances = c("equal", "varying"),
                    covariances = c("zero", "varying")) 
save(onlySES, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/onlySES_tidyLPA.Rda")
fit_stats_onlycog <- get_fit(onlySES)
compare_fit_onlySES <- compare_solutions(onlySES,statistics = c("AIC", "BIC"))
compare_fit_onlySES



#-------------------------------------------------------------------------------------------------#
#--- Compare the BIC statistics from the 14 models (3 dimensions - volume, shape, orientation) ---#
#-------------------------------------------------------------------------------------------------#

names(dat)

set.seed(123)
BIC <- mclustBIC(dat[,c(8,11:13)])
summary(BIC)
plot(BIC)

#---------------------------#
#--- Fit the VVV,9 model ---#
#---------------------------#
mod1 <- Mclust(dat[,c(8,11:13)], modelNames = "VVE", G = 6, x = BIC)

#-------------------------------------------------------------------------------------#
#--- Save the parameter means for VVE,9 for profiles involving both SES & neurocog ---#
#-------------------------------------------------------------------------------------#
mod1$parameters$mean
clusters <- data.frame(mod1$parameters$mean) %>%
  rownames_to_column() %>%
  rename(Phenotype = rowname) 

summary(as.factor(mod1$classification))

write.csv(clusters, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/LPA_clusters_SES_cog.csv")


clusters<-clusters%>%
  pivot_longer(cols = c(X1, X2, X3, X4, X5, X6), names_to = "Profile", values_to = "Mean") %>%
  mutate(Mean = round(Mean, 2)) %>%
  mutate(
    lp_cat = case_when(Profile=="X7"~"Profile 7",
                       Profile=="X6"~"Profile 6",
                       Profile=="X5"~"Profile 5",
                       Profile=="X4"~"Profile 4",
                       Profile=="X3"~"Profile 3",
                       Profile=="X2"~"Profile 2",
                       Profile=="X1"~"Profile 1"),
    phen = case_when(Phenotype=="inr_s" ~ "Income-to-Needs Ratio",
                     Phenotype=="p_ed_s" ~ "Parental Education",
                     Phenotype=="adi_s" ~ "Area Deprivation Index for the primary residence",
                     Phenotype=="fluid_s" ~ "NIH Toolbox: Age-Corrected Fluid Intelligence Composite",
                     Phenotype=="cryst_s" ~ "NIH Toolbox: Age-Corrected Crystallized Intelligence Composite",
                     Phenotype=="totcomp_s" ~ "NIH Toolbox: Age-Corrected Total Intelligence Composite"
    ))


#--- Look at mean phenotype values across imputations by latent profile
clusters %>%
  ggplot(aes(phen, Mean, group = lp_cat, color = lp_cat)) +
  geom_point(size = 2.25) +
  geom_line(size = 1.25) +
  labs(x = NULL, y = "Standardized phenotype mean", color="Latent Profiles") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 70, hjust = 1), 
        legend.position = "top") 

summary(as.factor(clusters$Phenotype))

# The mod1 object classifies each person as belonging to profiles 1,2 or 3
dat$lp <- mod1$classification
print(summary(as.factor(dat$lp)))

d <- d %>%
  mutate(
    # Create dummy variables for the normative, low ToM/SCR, low exec profiles 
    lp_norm = case_when(lp==3 ~ 1, 
                        T ~ 0),
    lp_lowToMscr = case_when(lp==2 ~1, 
                             T ~ 0),
    lp_lowExec = case_when(lp==1 ~1, 
                           T ~ 0),
    lp_cat=case_when(lp==3 ~ "norm",
                     lp==2 ~ "lowToMscr",
                     lp==1 ~ "lowExec")
  ) 

summary(as.factor(d$lp_cat))


#------------------------------------------------#
#--- Compare distributions of variables by LP ---#
#------------------------------------------------#

#~~ Distributions of mediators:

## Create a variable list
names(d)
vars <- names(d[,c(18,19,20,12,13,14,44:47)])
by_lp <- CreateTableOne(vars = vars,
                        strata = c("lp_cat"),
                        addOverall = TRUE,
                        data = d)
by_lp <- data.frame(print(by_lp, missing=TRUE))
by_lp <- tibble::rownames_to_column(by_lp, "Characteristic")

# Column labels
names(by_lp) <- c("Characteristic", "Overall", "Profile1: Below average executive", "Profile 2: Below average ToM, SCR", "Profile 3: reference", "p-value", "% Missing")

# Row labels
table_rows <- c("n",  
                "Age, baseline",
                "Female biological sex",
                "Chronicity of poverty, early childhood",
                "Maternal depression, early childhood",
                "Threat experiences, standardized",
                "Deprivation experiences, standarized",
                "CBCL internalizing t-score",
                "CBCL externalizing t-score",
                "YSR internalizing t-score",
                "YSR externalizing t-score")
by_lp$Characteristic <- table_rows
kbl(by_lp,
    caption = "Distributions of child characteristics, by latent profile") %>% 
  kable_styling()

write.csv(by_lp, file="/Users/Kat/Dropbox/PC/Documents/PHS/Research/Deprivation and Threat/NEW_RESULTS/Distributions_by_LP_hotdeck.csv")


#---------------------------------------------------------#
#--- Joint density of deprivation and threat exposures ---#
#x <- ifelse(d$FIL_THREAT>3,3,d$FIL_THREAT)

summary(d$FIL_DEPRIVATION)
summary(d$FIL_THREAT)

x <- d$FIL_THREAT
y <- d$FIL_DEPRIVATION

plot(x,y)

library(MASS)
den3d <- kde2d(x, y)


# the new part:
library(plotly)

axx <- list(
  title = "Threat"
)

axy <- list(
  title = "Deprivation"
)

axz <- list(
  title = "Joint density"
)

fig <- plot_ly(x=den3d$x, y=den3d$y, z=den3d$z) %>% add_surface()
fig <- fig %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))

fig
