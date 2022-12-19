library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)
library(shinyjs)
library(shinycssloaders)
library(rintrojs)

library(maps)
library(mapproj)
library(shiny)
library(lubridate)
library(usmap)
library(plotly)
library(corrplot)

library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(ggrepel)
library(ggExtra)
library(tidyr)
library(GGally)
library(scales)
library(viridis)

# References
# https://rstudio.github.io/shinydashboard/
# https://dreamrs.github.io/shinyWidgets/reference/pickerInput.html
# https://stackoverflow.com/questions/50914223/how-to-change-the-fonts-size-of-sidebar-in-shinydashboard
# https://stackoverflow.com/questions/49488228/how-to-show-spinning-wheel-or-busy-icon-while-waiting-in-shiny
# http://r-graph-gallery.com/283-the-hourly-heatmap.html
# https://github.com/ceefluz/radar


# Constant variable
# New York City
states <- c("alabama", "arizona", "arkansas", "california", "colorado", 
            "connecticut", "delaware", "district of columbia", 
            "florida", "georgia", "idaho", "illinois", "indiana", 
            "iowa", "kansas", "kentucky", "louisiana", "maine",
            "maryland", "massachusetts", "michigan", "minnesota", 
            "mississippi", "missouri", "montana", "nebraska",
            "nevada", "new hampshire", "new jersey", "new mexico", 
            "new york", "north carolina", "north dakota", "ohio",
            "oklahoma", "oregon", "pennsylvania", "rhode island", 
            "south carolina", "south dakota", "tennessee", "texas", "utah", "vermont", 
            "virginia", "washington", "west virginia", "wisconsin", "wyoming", "alaska", "hawaii")

rename_diseases <- c("All Causes", "Natural Causes", "Septicemia", 
                     "Maligant neoplasms", "Diabetes mellitus", 
                     "Alzheimer disease", "Influenza and pneumonia", 
                     "Chronic lower respiratory", "Respiratory system", "Nephritis", "Symtoms signs", "Diseases of heart",
                     "Cerebrovascular", "COVID-19")

# Load data ----
## Dataset 1
count_death_cleaned_model <- readRDS("www/data/count_death_cleaned_model.rds")
count_death_cleaned_visual_1 <- readRDS("www/data/count_death_cleaned_visual_1.rds")
count_death_cleaned_visual_2 <- readRDS("www/data/count_death_cleaned_visual_2.rds")
races <- as.character(unique(count_death_cleaned_model$race)) %>% rev()
db1_age_groups <- as.character(unique(count_death_cleaned_model$age_group))
db1_age_groups    

## Dataset 2
condition_covi_cleaned <- readRDS("www/data/condition_covi_cleaned.rds")
condition_groups <- as.character(unique(condition_covi_cleaned$condition_group))
age_groups <- as.character(unique(condition_covi_cleaned$age_group)[c(1:8)])

## Dataset 3
covi_data_cleaned_model <- readRDS("www/data/covi_data_cleaned_model.rds")
covi_data_cleaned_visual <- readRDS("www/data/covi_data_cleaned_visual.rds")

## Prediction 
arima_forecast = read_rds('www/data/covi_data_ARIMA_Forecast_T+100.rds')
arima_forecast$is_forecast = 'Y'
covi_data_cleaned_model$is_forecast = 'N'
covi_data_new = covi_data_cleaned_model[, c('date', 'state', 'new_case', 'is_forecast')]
covi_data_new = bind_rows(covi_data_new,arima_forecast)
covi_data_new = covi_data_new[order(covi_data_new$state),]
stateList = c(unique(covi_data_new['state']))
stateList = stateList[[1]]

# Source helper functions -----
source("www/functions/db1_kpi_fun.R")
source("www/functions/db1_cor_fun.R")
source("www/functions/db1_loli_fun.R")
source("www/functions/db1_bar_fun.R")
source("www/functions/db1_ts_fun.R")

## Dataset 2
source("www/functions/db2_kpi_fun.R")
source("www/functions/db2_map_fun.R")
source("www/functions/db2_loli_fun.R")
source("www/functions/db2_bar_fun.R")
source("www/functions/db2_ts_fun.R")


## Dataset 3
source("www/functions/db3_kpi_fun.R")
source("www/functions/db3_map_fun.R")
source("www/functions/db3_heat_fun.R")
source("www/functions/db3_ts_fun.R")

## Prediction
source("www/functions/pred_line_fun.R")
source("www/functions/pred_kpi_fun.R")

