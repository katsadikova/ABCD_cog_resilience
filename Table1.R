library(dplyr)
library(ggplot2)
library(tidyr)
library(tableone)
library(kableExtra)


#-- Non-missing INR - retain 9572 out of 11868 obs
load("/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/temp_dat/dat_all_inr.Rda")

#-- Complete cases on all considered predictors - only 4809 out of 11868
load("/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/temp_dat/dat_all_inr_complete.Rda")

#-- 1x imputed data for 9572 out of 11868 with non-missing INR
load("/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/temp_dat/d_imp.Rdata")

#-- Load in data dictionary - to be able to label variables
dict <- read.csv("/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/temp_dat/data_dict.csv")

#------------------------------------------------#
d <- complete(d_imp)

d_pov_highcrcog <- d %>% filter(pov3_highcrcog==1) %>% 
  mutate(profile="pov3_highcrcog",
         resilience=1)
d_pov_lowcrcog <- d %>% filter(pov3_lowcrcog==1) %>% 
  mutate(profile="pov3_lowcrcog",
         resilience=0)
d_pov_highflcog <- d %>% filter(pov3_highflcog==1) %>% 
  mutate(profile="pov3_highflcog",
         resilience=1)
d_pov_lowflcog <- d %>% filter(pov3_lowflcog==1) %>% 
  mutate(profile="pov3_lowflcog",
         resilience=0)

d_flpov <- rbind(d_pov_highcrcog,d_pov_lowcrcog,d_pov_highflcog,d_pov_lowflcog)

#-- Pull in age 
abcd_p_demo <- read.csv("./abcd-general/abcd_p_demo.csv") %>%
  #-- Select baseline visit
  filter(eventname=="baseline_year_1_arm_1") %>%
  dplyr::select(src_subject_id, demo_brthdat_v2)
summary(abcd_p_demo$demo_brthdat_v2)


d <- merge(x=d_flpov, y=abcd_p_demo, by="src_subject_id", all.x=T) %>%
  mutate(
    female=ifelse(pubertal_sex_p==2,1,0)
  )


#-- Look at distributions of variables by fluid/crystallized resilience 

names(d)

vars <- names(d)[c(199,200,2:5)]

## Create Table 1 stratified by delay
table_all <- CreateTableOne(vars = vars,
                            strata = c("profile"),
                            includeNA = F, 
                            addOverall = F,
                            test=F,
                            data = d)
table_all_print <- data.frame(print(table_all, explain=F, missing=F, catDigits = 2, contDigits = 2))
table_all_print <- tibble::rownames_to_column(table_all_print, "Characteristic")

# Column labels
names(table_all_print) <- c("Variable", "pov3_highcrcog", "pov3_highflcog", "pov3_lowcrcog", "pov3_lowflcog")
names(dict)
table_all_print_label <- merge(x=table_all_print, y=dict, by="Variable",all.x=T) %>%
  dplyr::select(Label,Variable,pov3_highflcog, pov3_lowflcog, pov3_highcrcog, pov3_lowcrcog)
table_all_print_label

write.csv(table_all_print_label, file="/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/drafts/Table1.csv")

