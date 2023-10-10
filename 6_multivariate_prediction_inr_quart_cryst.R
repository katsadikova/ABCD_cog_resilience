#----------------------------------------------------------------------------#
#--- 6_multivariate_prediction.R
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
library(SuperLearner)
library(xgboost)
library(finalfit)
library(caret)
library(pROC) 



#-- Non-missing INR - retain 9572 out of 11868 obs
load("/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/temp_dat/dat_all_inr.Rda")

#-- Complete cases on all considered predictors - only 4809 out of 11868
load("/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/temp_dat/dat_all_inr_complete.Rda")

#-- 1x imputed data for 9572 out of 11868 with non-missing INR
load("/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/temp_dat/d_imp.Rdata")

#-- Load in data dictionary - to be able to label variables
dict <- read.csv("/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/temp_dat/data_dict.csv")

names(dat_all_inr)
vars <- names(dat_all_inr)[c(2,5:8,31:199)]




#--------------------------------------------------------------------#
#-- XGBoost alone - can handle missing data in predictor variables --#

#---------------------------------------------------#
#-- Poverty (quartiles), fluid cognitive function --#

d_pov_highcog <- dat_all_inr %>% filter(pov_highcrcog==1) %>% 
  mutate(profile="pov_highcrcog",
         resilience=1)
d_pov_lowcog <- dat_all_inr %>% filter(pov_lowcrcog==1) %>% 
  mutate(profile="pov_lowcrcog",
         resilience=0)

d_flpov <- rbind(d_pov_highcog,d_pov_lowcog)


#-------------------------------------------------------------------------------#
#-- 10-fold cross-validated XGBoost in data with missing values in predictors --#

set.seed(123)
labels <- d_flpov$resilience

names(d_flpov)
new_tr <- as.matrix(d_flpov[,c(5,31:199)])

dtrain <- xgb.DMatrix(data = new_tr,label = labels) 

params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.3, gamma=0.1, max_depth=2, 
               min_child_weight=1, 
               subsample=1, colsample_bytree=1)

xgbcv <- xgb.cv(params = params, data = dtrain, nrounds = 500, 
                nfold = 10,print_every_n = 10, 
                prediction=TRUE,
                maximize = F , eval_metric = "error")
xgbpred <- xgbcv$pred

summary(xgbpred)
hist(xgbpred)
roc_test <- roc(labels, xgbpred, algorithm = 2) 
roc_test$auc


#-- Assess variable importance

xgb_util <- xgboost(data = new_tr, label = xgbpred, nrounds = 200, verbose=FALSE,params = params)
importance_matrix_util <- xgb.importance(model = xgb_util)
importance_matrix_util
## Create table comparing distributions of top 20 important variables by any_util
vars <- as.vector(unlist(importance_matrix_util[1:20,1]))
imp <- data.frame(importance_matrix_util)
names(imp) <- c("Variable","Gain","Cover","Frequency")

table_util_imp <- CreateTableOne(vars = vars,
                                 strata = c("resilience"), 
                                 includeNA = F, 
                                 addOverall = F,
                                 data = d_flpov)
table_util_imp <- data.frame(print(table_util_imp, explain=F, missing=T, catDigits = 2, contDigits = 2))
table_util_imp <- tibble::rownames_to_column(table_util_imp, "Characteristic")[,c(1:5)]
table_util_imp

names(table_util_imp) <- c("Variable", "Poverty_Low_Cog", "Poverty_High_Cog","p_value","prop_missing")

table_util_imp_label <- merge(x=table_util_imp, y=dict, by="Variable",all.x=T) %>%
  dplyr::select(Label,Variable,Poverty_Low_Cog,Poverty_High_Cog,p_value,prop_missing,Order,Order2)

table_util_imp_label_gain <- merge(x=table_util_imp_label, y=imp, all.x=T, by="Variable")
table_util_imp_label_gain

write.csv(table_util_imp_label_gain,"/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/INR_quart_cryst_cvXgb_table_util_Missing_label_gain.csv")



#-- Run a logistic regression with the top 20 important predictors selected by xgboost
glm_cvxgboost = glm(data=d_flpov, formula(paste("resilience ~ ",paste(table_util_imp$Variable[2:21], collapse ="+"), collapse = " ")), family="binomial")

summary(glm_cvxgboost)

pred <- predict(glm_cvxgboost, newdata=d_flpov, type="response")
roc_test <- roc(labels, pred, algorithm = 2) 
roc_test$auc

#----------------------------------------------------#
#-- cross-validated XGBoost in complete cases data --#


d_pov_highflcog <- dat_all_inr_complete %>% filter(pov_highcrcog==1) %>% 
  mutate(profile="pov_highcrcog",
         resilience=1)
d_pov_lowflcog <- dat_all_inr_complete %>% filter(pov_lowcrcog==1) %>% 
  mutate(profile="pov_lowcrcog",
         resilience=0)

d_flpov <- rbind(d_pov_highflcog,d_pov_lowflcog)

set.seed(123)
labels <- d_flpov$resilience

names(d_flpov)
new_tr <- as.matrix(d_flpov[,c(5,31:199)])

dtrain <- xgb.DMatrix(data = new_tr,label = labels) 

params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.3, gamma=0.1, max_depth=2, 
               min_child_weight=1, 
               subsample=1, colsample_bytree=1)

xgbcv <- xgb.cv(params = params, data = dtrain, nrounds = 500, 
                nfold = 10,print_every_n = 10, 
                prediction=TRUE,
                maximize = F , eval_metric = "error")
xgbpred <- xgbcv$pred

summary(xgbpred)
hist(xgbpred)
roc_test <- roc(labels, xgbpred, algorithm = 2) 
roc_test$auc


#-- Assess variable importance

xgb_util <- xgboost(data = new_tr, label = xgbpred, nrounds = 200, verbose=FALSE,params=params)
importance_matrix_util <- xgb.importance(model = xgb_util)
importance_matrix_util
## Create table comparing distributions of top 20 important variables by any_util
vars <- as.vector(unlist(importance_matrix_util[1:20,1]))
imp <- data.frame(importance_matrix_util)
names(imp) <- c("Variable","Gain","Cover","Frequency")

table_util_imp <- CreateTableOne(vars = vars,
                                 strata = c("resilience"), 
                                 includeNA = F, 
                                 addOverall = F,
                                 data = d_flpov)
table_util_imp <- data.frame(print(table_util_imp, explain=F, missing=T, catDigits = 2, contDigits = 2))
table_util_imp <- tibble::rownames_to_column(table_util_imp, "Characteristic")[,c(1:5)]
table_util_imp

names(table_util_imp) <- c("Variable", "Poverty_Low_Cog", "Poverty_High_Cog","p_value","prop_missing")

table_util_imp_label <- merge(x=table_util_imp, y=dict, by="Variable",all.x=T) %>%
  dplyr::select(Label,Variable,Poverty_Low_Cog,Poverty_High_Cog,p_value,prop_missing,Order,Order2)

table_util_imp_label_gain <- merge(x=table_util_imp_label, y=imp, all.x=T, by="Variable")
table_util_imp_label_gain

write.csv(table_util_imp_label_gain,"/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/INR_quart_cryst_cvXgb_complete_cases_table_util_imp_label_gain.csv")


#-- Run a logistic regression with the top 20 important predictors selected by xgboost
glm_cvxgboost = glm(data=d_flpov, formula(paste("resilience ~ ",paste(table_util_imp$Variable[2:21], collapse ="+"), collapse = " ")), family="binomial")

summary(glm_cvxgboost)

pred <- predict(glm_cvxgboost, newdata=d_flpov, type="response")
roc_test <- roc(labels, pred, algorithm = 2) 
roc_test$auc

#------------------------------------------------#
#-- cross-validated XGBoost in 1x imputed data --#

d <- complete(d_imp)

d_pov_highflcog <- d %>% filter(pov_highcrcog==1) %>% 
  mutate(profile="pov_highcrcog",
         resilience=1)
d_pov_lowflcog <- d %>% filter(pov_lowcrcog==1) %>% 
  mutate(profile="pov_lowcrcog",
         resilience=0)

d_flpov <- rbind(d_pov_highflcog,d_pov_lowflcog)

set.seed(123)
labels <- d_flpov$resilience

names(d_flpov)
new_tr <- as.matrix(d_flpov[,c(5,31:199)])

dtrain <- xgb.DMatrix(data = new_tr,label = labels) 

params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.15, gamma=0.1, max_depth=3, 
               min_child_weight=1, 
               subsample=1, colsample_bytree=1)

xgbcv <- xgb.cv(params = params, data = dtrain, nrounds = 500, 
                nfold = 10,print_every_n = 10, 
                prediction=TRUE,
                maximize = F , eval_metric = "error")
xgbpred <- xgbcv$pred

summary(xgbpred)
hist(xgbpred)
roc_test <- roc(labels, xgbpred, algorithm = 2) 
roc_test$auc

#-- Assess variable importance

xgb_util <- xgboost(data = new_tr, label = xgbpred, params = params, nrounds = 200, verbose=FALSE)
importance_matrix_util <- xgb.importance(model = xgb_util)
importance_matrix_util
## Create table comparing distributions of top 20 important variables by any_util
vars <- as.vector(unlist(importance_matrix_util[1:20,1]))
imp <- data.frame(importance_matrix_util)
names(imp) <- c("Variable","Gain","Cover","Frequency")

table_util_imp <- CreateTableOne(vars = vars,
                                 strata = c("resilience"), 
                                 includeNA = F, 
                                 addOverall = F,
                                 data = d_flpov)
table_util_imp <- data.frame(print(table_util_imp, explain=F, missing=T, catDigits = 2, contDigits = 2))
table_util_imp <- tibble::rownames_to_column(table_util_imp, "Characteristic")[,c(1:5)]
table_util_imp

names(table_util_imp) <- c("Variable", "Poverty_Low_Cog", "Poverty_High_Cog","p_value","prop_missing")

table_util_imp_label <- merge(x=table_util_imp, y=dict, by="Variable",all.x=T) %>%
  dplyr::select(Label,Variable,Poverty_Low_Cog,Poverty_High_Cog,p_value,prop_missing,Order,Order2)

table_util_imp_label_gain <- merge(x=table_util_imp_label, y=imp, all.x=T, by="Variable")
table_util_imp_label_gain

write.csv(table_util_imp_label_gain,"/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/INR_quart_cryst_cvXgb_imputed1x_table_util_imp_label_gain.csv")


#-- Run a logistic regression with the top 20 important predictors selected by xgboost
glm_cvxgboost = glm(data=d_flpov, formula(paste("resilience ~ ",paste(table_util_imp$Variable[2:21], collapse ="+"), collapse = " ")), family="binomial")

summary(glm_cvxgboost)

pred <- predict(glm_cvxgboost, newdata=d_flpov, type="response")
roc_test <- roc(labels, pred, algorithm = 2) 
roc_test$auc

#-----------------------------------------#
#-- SuperLearner in complete cases data --#


######################################################################################
# glmnet
######################################################################################
# Create glmnet wrappers for different alphas (0=ridge, 5=elastic, 1=lasso).
# The default value for alpha in SL.glmnet is 1
#
# Ridge will not be fed all variables, so we need to create a separate learner for it.
######################################################################################
tune.glmnet <- list(alpha = seq(0.1, 0.9, by = 0.2))
learners.glmnet <- create.Learner("SL.glmnet", tune = tune.glmnet, detailed_names = T, name_prefix = "SL.glmnet")
learners.glmnet0 <- create.Learner("SL.glmnet", tune = list(alpha = 0), detailed_names = T, name_prefix = "SL.glmnet")
learners.glmnet1 <- create.Learner("SL.glmnet", tune = list(alpha = 1), detailed_names = T, name_prefix = "SL.glmnet")


###################
# ranger
###################
# Set max.depth to 8, mtry to 9
###################
learners.ranger_3 <- create.Learner("SL.ranger", params = list(max.depth = 3, num.trees = 1000), detailed_names = T, name_prefix = "SL.ranger_3")
learners.ranger_4 <- create.Learner("SL.ranger", params = list(max.depth = 4, num.trees = 1000),  detailed_names = T, name_prefix = "SL.ranger_4")

#########
# xgboost
#########
tune.xgb.1 <- list(ntrees = 200,
                   max_depth = 4,
                   shrinkage = 0.05,
                   minobspernode = 5)
learners.xgb.1 <- create.Learner("SL.xgboost", tune = tune.xgb.1, detailed_names = T, name_prefix = "SL.xgboost")

tune.xgb.2 <- list(ntrees = 400,
                   max_depth = 4,
                   shrinkage = 0.025,
                   minobspernode = 5)
learners.xgb.2 <- create.Learner("SL.xgboost", tune = tune.xgb.2, detailed_names = T, name_prefix = "SL.xgboost")

tune.xgb.3 <- list(ntrees = 1000,
                   max_depth = 4,
                   shrinkage = 0.01,
                   minobspernode = 5)
learners.xgb.3 <- create.Learner("SL.xgboost", tune = tune.xgb.3, detailed_names = T, name_prefix = "SL.xgboost")


##################################################
# Define SL libraries and run sg
##################################################

SL.library = c(list(c('SL.glm','screen.corP'),
                    c("SL.glmnet_0",'screen.corP'), c("SL.glmnet_1"), c("SL.glmnet_0.1",'screen.corP'), c("SL.glmnet_0.3",'screen.corP'), 
                    c("SL.glmnet_0.5",'screen.corP'), c("SL.glmnet_0.7",'screen.corP'), c("SL.glmnet_0.9",'screen.corP'), 
                    c("SL.ranger_3_1"), c("SL.ranger_4_1"), c(learners.xgb.1$names), 
                    learners.xgb.2$names, 
                    learners.xgb.3$names,
                    c("SL.polymars",'screen.corP')))

SL.library

set.seed(123)
#-- Cross-validated SL
options(mc.cores = 4)
getOption("mc.cores")

labels <- d_flpov$resilience

X <- d_flpov[,c(5,30:198)]

set.seed(123)
sl_fit <- CV.SuperLearner(Y = labels, X = X, family = binomial(),
                          parallel = "multicore",
                          id = "rel_family_id",
                          SL.library = SL.library)
sl_fit$coef

pred = sl_fit$SL.predict
pred_rocr = ROCR::prediction(pred, labels)
auc = ROCR::performance(pred_rocr, measure = "auc", x.measure = "cutoff")@y.values[[1]]
auc


new_ts <- as.matrix(X)
dim(new_ts)
length(pred)

dtest <- xgb.DMatrix(data = new_ts, label = pred)

xgb_util <- xgboost(data = new_ts, label = pred, nrounds = 200, verbose=FALSE,params=params)
importance_matrix <- xgb.importance(model = xgb_util)
importance_matrix
## Create table comparing distributions of top 20 important variables by any_util
vars <- as.vector(unlist(importance_matrix_util[1:20,1]))
imp <- data.frame(importance_matrix_util)
names(imp) <- c("Variable","Gain","Cover","Frequency")

table_util_imp <- CreateTableOne(vars = vars,
                                 strata = c("resilience"), 
                                 includeNA = F, 
                                 addOverall = F,
                                 data = d_flpov)
table_util_imp <- data.frame(print(table_util_imp, explain=F, missing=T, catDigits = 2, contDigits = 2))
table_util_imp <- tibble::rownames_to_column(table_util_imp, "Characteristic")[,c(1:5)]
table_util_imp

names(table_util_imp) <- c("Variable", "Poverty_Low_Cog", "Poverty_High_Cog","p_value","prop_missing")

table_util_imp_label <- merge(x=table_util_imp, y=dict, by="Variable",all.x=T) %>%
  dplyr::select(Label,Variable,Poverty_Low_Cog,Poverty_High_Cog,p_value,prop_missing,Order,Order2)

table_util_imp_label_gain <- merge(x=table_util_imp_label, y=imp, all.x=T, by="Variable")
table_util_imp_label_gain

write.csv(table_util_imp_label_gain,"/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/INR_quart_cryst_cvSL_imputed1x_table_util_imp_label_gain.csv")


#-- Run a logistic regression with the top 20 important predictors selected by xgboost
glm_cvxgboost = glm(data=d_flpov, formula(paste("resilience ~ ",paste(table_util_imp$Variable[2:21], collapse ="+"), collapse = " ")), family="binomial")

summary(glm_cvxgboost)

pred <- predict(glm_cvxgboost, newdata=d_flpov, type="response")
roc_test <- roc(labels, pred, algorithm = 2) 
roc_test$auc

# 
# 
# #-- Split the data into training and testing sets
# set.seed(123)
# train <- d_flpov %>% dplyr::sample_frac(0.9)
# test  <- dplyr::anti_join(d_flpov, train, by = 'src_subject_id')
# 
# summary(as.factor(train$resilience))
# summary(as.factor(test$resilience))
# 
# labels <- train$resilience
# ts_label <- test$resilience
# 
# names(train)
# new_tr <- as.matrix(train[,c(5,30:198)])
# new_ts <- as.matrix(test[,c(5,30:198)])
# 
# dtrain <- xgb.DMatrix(data = new_tr,label = labels) 
# dtest <- xgb.DMatrix(data = new_ts,label = ts_label)
# 
# params <- list(booster = "gbtree", objective = "binary:logistic", 
#                eta=0.3, gamma=0.1, max_depth=2, 
#                min_child_weight=1, 
#                subsample=1, colsample_bytree=1)
# 
# xgb1 <- xgb.train(params = params, data = dtrain, nrounds = 500, 
#                   watchlist = list(val=dtest,train=dtrain), 
#                   print_every_n = 10, 
#                   early_stop_round = 10, 
#                   maximize = F , eval_metric = "error")
# 
# 
# xgbpred <- predict(xgb1,dtest)
# summary(xgbpred)
# hist(xgbpred)
# roc_test <- roc( ts_label, xgbpred, algorithm = 2) 
# roc_test$auc
# 
# 
# #-- Assess variable importance
# importance_matrix_util <- xgb.importance(model = xgb1)
# importance_matrix_util
# ## Create table comparing distributions of top 20 important variables by any_util
# vars <- as.vector(unlist(importance_matrix_util[1:20,1]))
# table_util_imp <- CreateTableOne(vars = vars,
#                                  strata = c("resilience"), 
#                                  includeNA = F, 
#                                  addOverall = F,
#                                  data = d_flpov)
# table_util_imp <- data.frame(print(table_util_imp, missing=F))
# table_util_imp <- tibble::rownames_to_column(table_util_imp, "Characteristic")[,c(1:4)]
# table_util_imp
