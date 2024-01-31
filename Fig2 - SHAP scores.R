library(dplyr)
library(ggplot2)
library(tidyr)

#-- Load in variables selected by LASSO with SHAP values from XGBOOST
load("/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/imp_vars_33_inr_fluid_folds_updated.Rda")
load("/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/results/imp_vars_33_inr_cryst_folds_updated.Rda")

#-- Load in updated data dictionary - with concise variable labels and domains
dict <- read.csv("/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/temp_dat/Predictors_to_merge.csv")

dict$Label <- dict$Short_Label

imp <- merge(imp_vars_cryst, imp_vars_fluid, by=c("Label","Variable","Domain"), all=T) %>%
  mutate(
    tert_inr_cryst_imp_vars = round(ifelse(is.na(tert_inr_cryst_imp_vars),0,tert_inr_cryst_imp_vars),2),
    tert_inr_fluid_imp_vars = round(ifelse(is.na(tert_inr_fluid_imp_vars),0,tert_inr_fluid_imp_vars),2)
    ) %>%
  filter(tert_inr_cryst_imp_vars>0 | tert_inr_fluid_imp_vars>0) %>%
  #-- Remove labels - to be replaced with updated labels
  dplyr::select(Variable,tert_inr_cryst_imp_vars,tert_inr_fluid_imp_vars)

names(dict)

#-- Update labels to those in dict
imp <- merge(imp, dict, by="Variable", all.x=T)


## Calling geom_col twice
# Plot code from https://datascience.stackexchange.com/questions/73894/mirror-column-plot-with-different-y-axis-in-ggplot

imp <- imp %>% 
  group_by(Domain,Label) %>% 
  summarise_if(is.numeric, mean) %>% 
  mutate(Domain_num = case_when(
          Domain == "Child characteristics" ~ 1,
          Domain == "Family history" ~ 2,
          Domain == "Culture and environment" ~ 3),
    Label_domain = paste0(Domain_num, ".", Label)
  ) %>%
  arrange(Domain_num) %>%
  select(Label_domain, Domain_num, Domain, Label, 
         tert_inr_cryst_imp_vars, tert_inr_fluid_imp_vars)


MyColour <- c("#61b6c2", "#f4ad49", "#1e4b79") 
names(MyColour) <- c("Child characteristics", "Family history", "Culture and environment")

png("/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/drafts/Fig2.png", 
    width = 1600, height = 1000, units="px")
ggplot(imp) +
  geom_col(aes(x=factor(Label, levels = c('Participation in performing arts',
                                          'Participation in visual arts',
                                          'Behavioral activation - drive',
                                          'Positive urgency',
                                          'Negative urgency',
                                          'Lack of planning',
                                          'Sensation-seeking',
                                          'Prosocial behavior (youth report on self)',
                                          'CBCL Anxious / depressed',
                                          'CBCL Social problems',
                                          'CBCL Attention problems',
                                          'Number of close friends',
                                          'Age (months)',
                                          'Pubertal development score',
                                          'Physical activity',
                                          'Primary caregiver educational attainment',
                                          'Secondary caregiver educational attainment',
                                          'Birthweight',
                                          'Breastfeeding (months)',
                                          'Parent attention problems',
                                          'Mother seen a mental health professional',
                                          'Income-to-needs',
                                          'Family conflict (youth-reported)',
                                          'Lax parental rules on substances',
                                          'School environment',
                                          'Neighborhood advanced placement enrollment',
                                          'Neighborhood school poverty',
                                          'Neighborhood reading proficiency (3rd grade)',
                                          'Neighborhood housing vacancy rate',
                                          'Neighborhood industrial pollutants in the air',
                                          'Neighborhood employment rate',
                                          'Neighborhood poverty rate',
                                          'Neighborhood public assistance rate',
                                          'Index of Concentration at the Extremes (Income)',
                                          'Index of Concentration at the Extremes (Income + Race)')), y=tert_inr_cryst_imp_vars, fill=Domain)) +
  geom_col(aes(x=factor(Label, levels = c('Participation in performing arts',
                                          'Participation in visual arts',
                                          'Behavioral activation - drive',
                                          'Positive urgency',
                                          'Negative urgency',
                                          'Lack of planning',
                                          'Sensation-seeking',
                                          'Prosocial behavior (youth report on self)',
                                          'CBCL Anxious / depressed',
                                          'CBCL Social problems',
                                          'CBCL Attention problems',
                                          'Number of close friends',
                                          'Age (months)',
                                          'Pubertal development score',
                                          'Physical activity',
                                          'Primary caregiver educational attainment',
                                          'Secondary caregiver educational attainment',
                                          'Birthweight',
                                          'Breastfeeding (months)',
                                          'Parent attention problems',
                                          'Mother seen a mental health professional',
                                          'Income-to-needs',
                                          'Family conflict (youth-reported)',
                                          'Lax parental rules on substances',
                                          'School environment',
                                          'Neighborhood advanced placement enrollment',
                                          'Neighborhood school poverty',
                                          'Neighborhood reading proficiency (3rd grade)',
                                          'Neighborhood housing vacancy rate',
                                          'Neighborhood industrial pollutants in the air',
                                          'Neighborhood employment rate',
                                          'Neighborhood poverty rate',
                                          'Neighborhood public assistance rate',
                                          'Index of Concentration at the Extremes (Income)',
                                          'Index of Concentration at the Extremes (Income + Race)')), y=-tert_inr_fluid_imp_vars, fill=Domain)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=16, angle = 0, vjust = 0.6),
        axis.text.y = element_text(size=16),
        legend.text = element_text(size=16),
        legend.title = element_text(size=20)) +
  #-- Take absolute value of the x-axis labels (to correct the mirroring above)
  scale_y_continuous(labels = abs) +
  ylab("SHAP value") +
  scale_x_discrete(limits = rev) +
  xlab("Predictor") +
  coord_flip() +
  annotate("text", x=35, y=-0.15, size=10, label= "Fluid") + 
  annotate("text", x=35, y=0.15, size=10, label = "Crystallized") + 
  scale_fill_manual(values = MyColour, breaks=c("Child characteristics",
                                                "Family history",
                                                "Culture and environment"))
dev.off()

