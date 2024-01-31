packages = c("tidyverse","kableExtra","gtsummary",
             "expss","haven","sjlabelled",
             "readxl","gtools","tableone",
             "mice","HIMA","corrplot",
             "reshape2","mclust","mlogit",
             "tidyLPA","SuperLearner","xgboost",
             "finalfit","caret","pROC",
             "groupdata2","SHAPforxgboost")

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
source("/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/code/ABCD_cog_resilience/shap.R")

#-- Non-missing INR - retain 10313 out of 11868 obs
load("/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/temp_dat/dat_all_inr.Rda")

#-- Complete cases on all considered predictors - only 5439 out of 11868
load("/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/temp_dat/dat_all_inr_complete.Rda")

#-- 1x imputed data for 10313 out of 11868 with non-missing INR
load("/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/temp_dat/d_imp_updated.Rdata")

#-- Load in data dictionary - to be able to label variables
dict <- read.csv("/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/temp_dat/Predictors_to_merge.csv")


#------------------------------------------------#
#-- cross-validated XGBoost in 1x imputed data --#

d <- complete(d_imp)

d_pov_highflcog <- d %>% filter(pov3_highflcog==1) %>% 
  mutate(profile="pov3_highflcog",
         resilience=1)
d_pov_lowflcog <- d %>% filter(pov3_lowflcog==1) %>% 
  mutate(profile="pov3_lowflcog",
         resilience=0)

d_flpov <- rbind(d_pov_highflcog,d_pov_lowflcog)


names(d_flpov)

#----------------------------------------#
#-- cross-validated in 1x imputed data --#

#-- Create folds with kids from the same family kept together
d_flpov$rel_family_id <- as.factor(d_flpov$rel_family_id)

set.seed(123)
d_flpov <- fold(d_flpov,id_col = "rel_family_id",k = 10)
typeof(d_flpov$.folds)
names(d_flpov)


# Prepare data set 
x <- as.matrix(d_flpov[,c(3,28:190)])
y <- factor(d_flpov$resilience)
foldid = as.numeric(d_flpov$.folds)

summary(foldid)

# Run cross-validation

mod_cv <- cv.glmnet(x=x, y=y, foldid=foldid, family='binomial')
mod_cv$lambda.1se


coefs <- tibble::rownames_to_column(data.frame(as.matrix(coef(mod_cv, mod_cv$lambda.1se))), "name") %>%
  filter(s1 != 0 & name != "(Intercept)")
coefs

lasso_pred <- predict(mod_cv,s=mod_cv$lambda.1se,newx=x,type="response")
summary(lasso_pred)
hist(lasso_pred)
roc_test <- roc(y, lasso_pred, algorithm = 2) 
roc_test$auc

glm_cvxgboost = glm(data=d_flpov, formula(paste("resilience ~ ",paste(coefs$name, collapse ="+"), collapse = " ")), family="binomial")

obj<-summary(glm_cvxgboost)$coefficients
write.csv(obj,"/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/INR_tert_fluid_cvLASSO_1ximp_coef_new_folds_updated.csv")


pred <- predict(glm_cvxgboost, newdata=d_flpov, type="response")
roc_test <- roc(y, pred, algorithm = 2) 
roc_test$auc

#-- Run glmer - accounting for clustering within families
datm <- data.frame(d_flpov)
library(lme4)
m <- glmer(resilience ~  inr+interview_age+reshist_addr1_coi_he_vacancy+reshist_addr1_coi_se_povrate+reshist_addr1_coi_se_public+
             reshist_addr1_seg_ice_income+reshist_addr1_seg_ice_inc_bw+demo_prnt_ed_v2+demo_prtnr_ed_v2+pds_p_ss_cat+birth_weight+
             devhx_18_p+physical_activity1_y+sai_p_perf_arts+bis_y_ss_bas_drive+upps_y_ss_positive_urgency+cbcl_scr_syn_attention_r+
             parent_subst_rules+(1 | rel_family_id), data=datm, family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 10)
coef_glmer <- summary(m)$coefficients
write.csv(coef_glmer,"/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/coef_glmer_INR_tert_fluid_cvLASSO_1ximp_coef_new_folds_updated.csv")



#------------------------------#
#-- SHAP VALUES FROM XGBOOST --#
#-- Cross-validated (10 folds)
set.seed(123)

d_flpov <- d_flpov %>%
  dplyr::select(coefs$name, resilience, .folds)

names(d_flpov)

kfolds=length(unique(d_flpov$.folds))

cv_err = rep(0, kfolds)
shap_values = list(shap_score = d_flpov[0,c(1:length(coefs$name))], mean_shap_score=NA_real_)
for(i in 1:kfolds){
  in_data <- filter(d_flpov, .folds!=i) 
  labels = in_data$resilience
  in_data <- as.matrix(in_data[,c(1:length(coefs$name))])
  out_data <- filter(d_flpov, .folds==i)
  out_labels = out_data$resilience
  out_data <- as.matrix(out_data[,c(1:length(coefs$name))])
  
  dtrain <- xgb.DMatrix(data = in_data,label = labels) 
  
  params <- list(booster = "gbtree", objective = "binary:logistic", 
                 eta=0.1, gamma=0.1, max_depth=2, 
                 min_child_weight=1, 
                 subsample=1, colsample_bytree=1)
  
  fit <- xgboost(params = params, data = in_data, label=labels, nrounds = 50, maximize = F, eval="error")
  preds <- predict(fit, newdata=out_data)
  shap_result = shap.score.rank(xgb_model = fit, 
                                X_train =out_data,
                                shap_approx = F)
  
  shap_values$shap_score<-rbind(shap_values$shap_score,shap_result$shap_score)
  shap_values$mean_shap_score <- cbind(shap_values$mean_shap_score,shap_result$mean_shap_score)
  
  err <- out_labels - preds
  mse <- mean(err^2)
  # Record the RMSE
  cv_err[i] <- sqrt(mse)
}
shap_values$mean_shap_score <- rowMeans(cbind(shap_values$mean_shap_score),na.rm=T)
quantile(shap_values$mean_shap_score,c(0.7,0.8,0.9,0.95,1))


## Prepare data for top N variables
shap_long = shap.prep(shap = shap_values,
                      X_train = d_flpov[,c(1:length(coefs$name))], 
                      top_n = length(coefs$name))
imp_vars_fluid <- data.frame(shap_values$mean_shap_score) %>%
  tibble::rownames_to_column(., "Variable")
imp_vars_fluid <- merge(x=imp_vars_fluid, y=dict, by="Variable",all.x=T) %>%
  dplyr::select(Domain, Short_Label,Variable,shap_values.mean_shap_score)

names(imp_vars_fluid) <- c("Domain","Label","Variable","tert_inr_fluid_imp_vars")
save(imp_vars_fluid, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/imp_vars_33_inr_fluid_folds_updated.Rda")


## Plot var importance based on SHAP
var_importance(shap_values, top_n=length(coefs$name))

## Plot shap overall metrics
plot.shap.summary(data_long = shap_long)

## 
xgb.plot.shap(data = as.matrix(d_flpov[,c(1:length(coefs$name))]), # input data
              model=fit,
              features = names(shap_values$mean_shap_score[1:length(coefs$name)]), # only top 10 var
              n_col = 5, # layout option
              plot_loess = T # add red line to plot
)


#-- FIGURE 3 PANELS PERTAINING TO CRYSTALLIZED OUTCOME!!!

#-- Modified plot code to allow for x-axis labels
source("/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/code/ABCD_cog_resilience/xgb_plot_shap_1.R")

#-- Pull in little files with variables created in Table2.R
load("/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/only_fluid_for_Fig3.Rda")
load("/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/both_for_Fig3.Rda")


only_fluid <- only_fluid %>% arrange(Domain,desc(tert_inr_fluid_imp_vars))

colors <- c("#61b6c2", "#f4ad49", "#1e4b79") 

#-- Only crystallized
#-- Child characteristics

png(paste0("/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/drafts/Fig3A_child_",only_fluid$Short_Label[1],".png"), 
    width = 450, height = 150, units="px", pointsize = 12)
xgb.plot.shap.1(data = as.matrix(d_flpov[,c(1:length(coefs$name))]), # input data
                features = c(only_fluid$Variable[1]), 
                model=fit,
                col=colors[1],
                col_loess=colors[1],
                xlab = c(only_fluid$Short_Label[1]),
                n_col = 4, # layout option
                plot_loess = T # add red line to plot
)
dev.off()

#-- Culture and environment
for(a in c(2:4)){
  png(paste0("/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/drafts/Fig3A_neighb_",only_fluid$Short_Label[a],".png"), 
      width = 450, height = 150, units="px", pointsize = 12)
  xgb.plot.shap.1(data = as.matrix(d_flpov[,c(1:length(coefs$name))]), # input data
                  features = c(only_fluid$Variable[a]), 
                  model=fit,
                  col=colors[3],
                  col_loess=colors[3],
                  xlab = c(only_fluid$Short_Label[a]),
                  n_col = 4, # layout option
                  plot_loess = T # add red line to plot
  )
  dev.off()
}


#-- Crystallized and fluid
both <- both %>% arrange(Domain)

#-- Child characteristics
for(a in c(1:6)){
  png(paste0("/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/drafts/Fig3C_fluid_child_",both$Short_Label[a],".png"), 
    width = 450, height = 150, units="px", pointsize = 12)
  xgb.plot.shap.1(data = as.matrix(d_flpov[,c(1:length(coefs$name))]), # input data
                  features = c(both$Variable[a]), 
                  model=fit,
                  col=colors[1],
                  col_loess=colors[1],
                  xlab = c(both$Short_Label[a]),
                  n_col = 1, # layout option
                  plot_loess = T # add red line to plot
  )
  dev.off()
}

#-- Family history
for(a in c(11:14)){
  png(paste0("/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/drafts/Fig3C_fluid_fam_",both$Short_Label[a],".png"), 
    width = 450, height = 150, units="px", pointsize = 12)
  xgb.plot.shap.1(data = as.matrix(d_flpov[,c(1:length(coefs$name))]), # input data
                  features = c(both$Variable[a]), 
                  model=fit,
                  col=colors[2],
                  col_loess=colors[2],
                  xlab = c(both$Short_Label[a]),
                  n_col = 1, # layout option
                  plot_loess = T # add red line to plot
  )
  dev.off()
}

#-- Culture and environment
for(a in c(7:10)){
  png(paste0("/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/drafts/Fig3C_fluid_neighb_",both$Short_Label[a],".png"), 
      width = 450, height = 150, units="px", pointsize = 12)
  xgb.plot.shap.1(data = as.matrix(d_flpov[,c(1:length(coefs$name))]), # input data
                features = c(both$Variable[a]), 
                model=fit,
                col=colors[3],
                col_loess=colors[3],
                xlab = c(both$Short_Label[a]),
                n_col = 1, # layout option
                plot_loess = T # add red line to plot
  )
  dev.off()
}

# `shap_approx` comes from `approxcontrib` from xgboost documentation. 
# Faster but less accurate if true. Read more: help(xgboost)

# 
# #--- GLM with all 2-way interactions
# summary(glm(data=d_flpov, resilience ~ (reshist_addr1_adi_perc_r+reshist_addr1_coi_he_vacancy+reshist_addr1_coi_se_public+
#                                           reshist_addr1_seg_ice_income+reshist_addr1_seg_ice_inc_bw+demo_prnt_ed_v2+birth_weight+
#                                           devhx_18_p+sai_p_perf_arts+bis_y_ss_bas_drive+upps_y_ss_positive_urgency+cbcl_scr_syn_attention_r+parent_subst_rules)^2))
# 

