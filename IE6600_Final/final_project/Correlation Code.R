#Correlation matrix and significance levels 
# install.packages("Hmisc")
library(Hmisc)

library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(ggrepel)
library(GGally)
library(plotly)



# write.csv(covid_19_countdeath, "spss_datafile.csv")

# Read data 
# setwd("C:/Users/sandy/Documents/Northeastern_Grad studies/IE 5374/Data")
covid_19_countdeath <- readRDS("www/data/count_death_cleaned_model.RDS")

#Select required columns   
covid_19_countdeath <- covid_19_countdeath[, c("cause_all", "cause_natural", "cause_other" ,"Septicemia (A40-A41)",
                                               "Malignant neoplasms (C00-C97)","Diabetes mellitus (E10-E14)", 
                                               "Alzheimer disease (G30)", 
                                               "Influenza and pneumonia (J09-J18)", 
                                               "Chronic lower respiratory diseases (J40-J47)" , 
                                               "Other diseases of respiratory system (J00-J06,J30-J39,J67,J70-J98)", 
                                               "Nephritis, nephrotic syndrome and nephrosis (N00-N07,N17-N19,N25-N27)", 
                                               "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified (R00-R99)", 
                                               "Diseases of heart (I00-I09,I11,I13,I20-I51)", 
                                               "Cerebrovascular diseases (I60-I69)", 
                                               "COVID-19 (U071, Underlying Cause of Death)")]


#Correlation analytics and significance level 
rcorr(covid_19_countdeath, type = c("pearson","spearman"))


res2 <- rcorr(as.matrix(covid_19_countdeath))
res2
