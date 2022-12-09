# Load packages ----
library(maps)
library(mapproj)
library(shiny)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(usmap)
library(plotly)
library(DT)

# Fixed variable
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


# Load data ----

## Dataset 2
condition_covi_cleaned <- readRDS("www/data/condition_covi_cleaned.rdS")
condition_groups <- as.character(unique(condition_covi_cleaned$condition_group))
age_groups <- as.character(unique(condition_covi_cleaned$age_group)[c(1:8)])

# Source helper functions -----
## For Dataset 2
source("www/functions/db2_map_plot.R")
source("www/functions/db2_ts_fun.R")


# User interface ----
ui <- fluidPage(
  titlePanel("IE6600 Final Project"),
  selectInput(
    inputId = "dataset",
    label = "Default Dataset List",
    choices = c(choose = "List of datasets",
                "Dataset1", "Dataset2", "Dataset3")
  ),
    tabsetPanel(
      tabPanel("Map",
         sidebarPanel(
           
           uiOutput("time"),
           uiOutput("cond_group"),
           uiOutput("age"),
           
           
           #Debug
           verbatimTextOutput("aaa")
         ),
          mainPanel(plotOutput("map"))
           
      ),
      tabPanel("Time Series",
           sidebarPanel(
             
             uiOutput("db2_ts_date"),
             uiOutput("db2_ts_state"),
             uiOutput("db2_ts_age"),
             
             #Debug
             verbatimTextOutput("db2_ts_aaa")
           ),
           mainPanel(plotOutput("db2_ts"))
               
      )
    )
)

# Server logic ----
server <- function(input, output) {
  values <- reactiveValues(
    dataset = NULL,
    db2_date = NULL,
    db2_ages = NULL,
    db2_condition_groups = NULL,
    
    db2_ts_date = NULL,
    db2_ts_state = NULL,
    db2_ts_age = NULL,
  )
  
  
  
  observeEvent(input$dataset, {
    if(!NA %in% match(input$dataset, c("Dataset1", "Dataset2", "Dataset3"))){
      if(input$dataset == "Dataset1"){
        dataset = "Dataset1"
      }
      if(input$dataset == "Dataset2"){
        # TODO
        values$dataset = "Dataset2"
        
        output$time <- renderUI({
          sliderInput(
            "date",
            "Dates",
            min = as.Date("2020-01-01", "%Y-%m-%d"),
            max = as.Date("2022-10-31", "%Y-%m-%d"),
            step = 30,
            value = c(as.Date("2020-01-01", "%Y-%m-%d"), as.Date("2022-10-31", "%Y-%m-%d")),
            timeFormat = "%b %Y",
          )
          
        })
        output$cond_group <- renderUI({
          pickerInput(
            "condition_group",
            "Condition Groups",
            choices = condition_groups,
            multiple = TRUE,
            selected = condition_groups,
            options = pickerOptions(
              actionsBox = TRUE,
              title = "Please select conditon groups",
              # title = as.character(length(input$condition_group)),
              header = "Condition Groups"
            ),
          )
        })
        output$age <- renderUI({
          pickerInput(
            "age_group",
            "Age Groups",
            choices = age_groups,
            multiple = TRUE,
            selected = age_groups,
            options = pickerOptions(
              actionsBox = TRUE,
              title = "Please select age groups",
              # title = as.character(length(input$condition_group)),
              header = "Age Groups"
            ),
          )
        })
        
        output$db2_ts_date <- renderUI({
          sliderInput(
            "db2_time_series_date",
            "Dates",
            min = as.Date("2020-01-01", "%Y-%m-%d"),
            max = as.Date("2022-10-31", "%Y-%m-%d"),
            step = 30,
            value = c(as.Date("2020-01-01", "%Y-%m-%d"), as.Date("2022-10-31", "%Y-%m-%d")),
            timeFormat = "%b %Y",
          )
          
        })
        output$db2_ts_state <- renderUI({
          pickerInput(
            "db2_time_series_state",
            "States",
            choices = states,
            multiple = TRUE,
            selected = states,
            options = pickerOptions(
              actionsBox = TRUE,
              title = "Please select States",
              # title = as.character(length(input$condition_group)),
              header = "States"
            ),
          )
        })
        output$db2_ts_age <- renderUI({
          pickerInput(
            "db2_time_series_age",
            "Age Groups",
            choices = age_groups,
            multiple = TRUE,
            selected = age_groups,
            options = pickerOptions(
              actionsBox = TRUE,
              title = "Please select age groups",
              # title = as.character(length(input$condition_group)),
              header = "Age Groups"
            ),
          )
        })
        
      }
    }
  })
  
  observe({
    
    values$db2_date <- input$date
    values$db2_ages <- input$age_group
    values$db2_condition_groups <- input$condition_group
    
    values$db2_ts_date <- input$db2_time_series_date
    values$db2_ts_state <- input$db2_time_series_state
    values$db2_ts_age <- input$db2_time_series_age
    
    if(!is_null(values$dataset)){
      output$map <- renderPlot({
        args <- list(condition_covi_cleaned %>% filter(group == "By Month"),
                     values$db2_date,
                     values$db2_ages,
                     values$db2_condition_groups)

        do.call(db2_map_plot, args)

      })
      
      output$db2_ts <- renderPlot({
        args <- list(condition_covi_cleaned[,-11] %>% filter(group == "By Month"),
                     values$db2_ts_date,
                     values$db2_ts_state,
                     values$db2_ts_age)
        
        do.call(db2_ts_fun, args)
        
      })
    }
    
  
  })
  
  
  
  
  
  
  
  
  
  
  output$db2_ts_aaa <- renderPrint({
    condition_covi_cleaned[,-11] %>% filter(group == "By Month")
  })
}


# Run app ----
shinyApp(ui, server)

