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

# C:/Users/JFON/Documents/GitHub/IE6600_project_shiny/IE6600_Final/final_project/Correlation Code.R

# write.csv(covid_19_countdeath, "spss_datafile.csv")

# Read data 
# setwd("C:/Users/sandy/Documents/Northeastern_Grad studies/IE 5374/Data")
covid_19_countdeath <- readRDS("www/data/count_death_cleaned_model.rds")



print(covid_19_countdeath)



colnames(covid_19_countdeath)




diseases <- colnames(covid_19_countdeath)[c(8,9,12:23)]
diseases
rename_diseases <- c("All Causes", "Natural Causes", "Septicemia", "Maligant neoplasms", "Diabetes mellitus", "Alzheimer disease",
                     "Influenza and pneumonia", "Chronic lower respiratory", "Respiratory system", "Nephritis", "Symtoms signs", "Diseases of heart",
                     "Cerebrovascular", "COVID-19")



covid_19_countdeath <- covid_19_countdeath[,diseases]
covid_19_countdeath
colnames(covid_19_countdeath) <- rename_diseases
covid_19_countdeath



cor.mat = cor(covid_19_countdeath, method = "spearman")
# install.packages("corrplot")



corrplot(cor.mat,  type='full',  method = 'color',
         col.lim = c(0.1,1),is.corr = FALSE,tl.col = 'black', diag = TRUE, 
         addCoef.col = 'black', col = COL2('RdBu', 10),  
         tl.cex = 0.8, cl.cex = 0.70, number.cex = 0.7)#, 
#tl.srt= 400

