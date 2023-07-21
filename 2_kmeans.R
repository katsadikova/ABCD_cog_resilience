library(fungible)
library(pheatmap)
library(tidyverse)
library(kableExtra)
library(gtsummary)
library(expss)
library(haven)
library(sjlabelled)
library(readxl)
library(readr)
library(ggplot2)
library(factoextra)
library(psych)

load("/Users/Kat/Library/CloudStorage/OneDrive-HarvardUniversity/abcd_study_with_kat/temp_dat/dat.Rda")

#-- Exclude rows with missing data (for now)
dat <- na.omit(dat)
names(dat)

# Drop variables with no variability (they can't be properly standardized)
d2 <- dat %>%
  dplyr::select(c(2:7))

d3<-scale(d2)

names(d2)

## Create a variable list
vars <- names(dat)[2:7]

# Run classic k-means on the unlabeled data
set.seed(12345)

tabs <- list()
for(i in seq(1:10)){
  print(i+1)
  kclu=kmeans(d3,i+1,25,nstart=25)
  kcluster=kclu$cluster
  d_kclu <- cbind(d2,kcluster)
  table_by_clu <- CreateTableOne(vars = vars,
                                 strata = c("kcluster"), 
                                 includeNA = F, 
                                 addOverall = T,
                                 data = d_kclu)
  tabs[[i]] <- table_by_clu$ContTable
}

kclu=kmeans(d3,4,25,nstart=25)
kcluster=kclu$cluster
d_kclu <- cbind(d2,kcluster)


#Plot the clusters
fviz_cluster(kclu, d_kclu, ellipse.type = "norm")

cohen.kappa(tab_kclu)  