# Load packages ----
library(maps)
library(mapproj)
library(shiny)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(usmap)

# Load data ----

## Dataset 2
condition_covi_cleaned <- readRDS("www/data/condition_covi_cleaned.rdS")
condition_groups <- as.character(unique(condition_covi_cleaned$condition_group))
age_groups <- as.character(unique(condition_covi_cleaned$age_group)[c(1:8)])

# Source helper functions -----
source("www/functions/db2_map_plot.R")


# User interface ----
ui <- fluidPage(
  titlePanel("IE6600 Final Project"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "dataset",
        label = "Default Dataset List",
        choices = c(choose = "List of datasets",
                    "Dataset1", "Dataset2", "Dataset3")
      ),
      
      uiOutput("time"),
      uiOutput("cond_group"),
      uiOutput("age"),
      
      
      #Debug
      verbatimTextOutput("aaa")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("map", plotOutput("map"))
      )
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
  )
  
  
  
  observeEvent(input$dataset, {
    if(!NA %in% match(input$dataset, c("Dataset1", "Dataset2", "Dataset3"))){
      if(input$dataset == "Dataset1"){
        dataset = "Dataset1"
      }
      if(input$dataset == "Dataset2"){
        values$dataset = "Dataset2"
        values$cgs <- condition_covi_cleaned$condition_groups %>% unique()
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
        
        
        
      }
    }
  })
  
  observe({
    
    values$db2_date <- input$date
    values$db2_ages <- input$age_group
    values$db2_condition_groups <- input$condition_group
    
    if(!is_null(values$dataset)){
      output$map <- renderPlot({
        args <- list(condition_covi_cleaned %>% filter(group == "By Month"),
                     values$db2_date,
                     values$db2_ages,
                     values$db2_condition_groups)

        do.call(db2_map_plot, args)

      })
    }
    
  
  })
  
  
  
  
  
  
  
  
  
  
  output$aaa <- renderPrint({
    values$db2_date[1] %>% class()
  })
}


# Run app ----
shinyApp(ui, server)

