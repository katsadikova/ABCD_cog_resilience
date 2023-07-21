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

load("/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/temp_dat/dat.Rda")

#-- Exclude rows with missing data (for now)
dat <- na.omit(dat)
names(dat)

#-- Look at simple correlations
vars <- names(dat)[2:7]
cor(dat[,2:7])

#-------------------------------------------------------------------------------------------------#
#--- Compare the BIC statistics from the 14 models (3 dimensions - volume, shape, orientation) ---#
#-------------------------------------------------------------------------------------------------#
set.seed(123)
BIC <- mclustBIC(dat[,c(2:7)])
summary(BIC)

#---------------------------#
#--- Fit the VVV,9 model ---#
#---------------------------#
mod1 <- Mclust(dat[,c(2:7)], modelNames = "VVV", G = 9, x = BIC)

#-------------------------------------------------------------------------------------#
#--- Save the parameter means for VVE,9 for profiles involving both SES & neurocog ---#
#-------------------------------------------------------------------------------------#
clusters <- data.frame(mod1$parameters$mean) %>%
  rownames_to_column() %>%
  rename(Phenotype = rowname) 

clusters

write.csv(clusters, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/LPA_clusters_SES_cog.csv")


clusters<-clusters%>%
  pivot_longer(cols = c(X1, X2, X3, X4, X5, X6, X7, X8, X9), names_to = "Profile", values_to = "Mean") %>%
  mutate(Mean = round(Mean, 2)) %>%
  mutate(
    lp_cat = case_when(Profile=="X9"~"Profile 9",
                       Profile=="X8"~"Profile 8",
                       Profile=="X7"~"Profile 7",
                       Profile=="X6"~"Profile 6",
                       Profile=="X5"~"Profile 5",
                       Profile=="X4"~"Profile 4",
                       Profile=="X3"~"Profile 3",
                       Profile=="X2"~"Profile 2",
                       Profile=="X1"~"Profile 1"),
    phen = case_when(Phenotype=="inr" ~ "Income-to-Needs Ratio",
                     Phenotype=="p_ed" ~ "Parental Education",
                     Phenotype=="reshist_addr1_adi_perc" ~ "Area Deprivation Index for the primary residence",
                     Phenotype=="nihtbx_fluidcomp_agecorrected" ~ "NIH Toolbox: Age-Corrected Fluid Intelligence Composite",
                     Phenotype=="nihtbx_cryst_agecorrected" ~ "NIH Toolbox: Age-Corrected Crystallized Intelligence Composite",
                     Phenotype=="nihtbx_totalcomp_agecorrected" ~ "NIH Toolbox: Age-Corrected Total Intelligence Composite"
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
