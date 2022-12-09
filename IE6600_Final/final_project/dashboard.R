library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(maps)
library(mapproj)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(usmap)
library(plotly)
library(corrplot)

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

rename_diseases <- c("All Causes", "Natural Causes", "Septicemia", 
                     "Maligant neoplasms", "Diabetes mellitus", 
                     "Alzheimer disease", "Influenza and pneumonia", 
                     "Chronic lower respiratory", "Respiratory system", "Nephritis", "Symtoms signs", "Diseases of heart",
                     "Cerebrovascular", "COVID-19")


# Load data ----
## Dataset 1
count_death_cleaned_model <- readRDS("www/data/count_death_cleaned_model.RDS")
count_death_cleaned_visual_1 <- readRDS("www/data/count_death_cleaned_visual_1.RDS")
count_death_cleaned_visual_2 <- readRDS("www/data/count_death_cleaned_visual_2.RDS")
races <- as.character(unique(count_death_cleaned_model$race)) %>% rev()

## Dataset 2
condition_covi_cleaned <- readRDS("www/data/condition_covi_cleaned.rdS")
condition_groups <- as.character(unique(condition_covi_cleaned$condition_group))
age_groups <- as.character(unique(condition_covi_cleaned$age_group)[c(1:8)])

## Dataset 3
covi_data_cleaned_model <- readRDS("www/data/covi_data_cleaned_model.RDS")
covi_data_cleaned_visual <- readRDS("www/data/covi_data_cleaned_visual.RDS")

# Source helper functions -----
source("www/functions/db1_cor_fun.R")
source("www/functions/db1_bar_1_fun.R")
source("www/functions/db1_bar_2_fun.R")

## Dataset 2
source("www/functions/db2_map_plot.R")
source("www/functions/db2_bar_1_fun.R")
source("www/functions/db2_bar_2_fun.R")
source("www/functions/db2_ts_fun.R")

## Dataset 3
source("www/functions/db3_map_fun.R")
source("www/functions/db3_ts_fun.R")



# Put them together into a dashboardPage
ui <- dashboardPage(
  dashboardHeader(title = "Simple tabs"),
  dashboardSidebar(
    sidebarMenu(
      id = "dataset",
      menuItem("Dataset 1", tabName = "dataset1", icon = icon("table")),
      menuItem("Dataset 2", tabName = "dataset2", icon = icon("table")),
      menuItem("Dataset 3", tabName = "dataset3", icon = icon("table"))
    ),
    
    # UI for Dataset 1 Bar
    uiOutput("db1_bar_date"),
    uiOutput("db1_bar_sex"),
    uiOutput("db1_bar_race"),
    
    ## UI for Dataset 1 Correlation Map
    uiOutput("db1_cor_sex"),
    uiOutput("db1_cor_race"),
    
    ## UI for Dataset 2 Map
    uiOutput("db2_map_date"),
    uiOutput("db2_map_cond_group"),
    uiOutput("db2_map_age"),

    ## UI for Dataset 2 Bar
    uiOutput("db2_bar_state"),

    ## UI for Dataset 2 Time Series
    uiOutput("db2_ts_date"),
    uiOutput("db2_ts_state"),
    uiOutput("db2_ts_age"),


    ## UI for Dataset 3 Map
    uiOutput("db3_map_date"),

    ## UI for Dataset 3 Time Series
    uiOutput("db3_ts_date"),
    uiOutput("db3_ts_state"),
    uiOutput("db3_ts_choice"),
    
    
    
    
    ## Debug
    verbatimTextOutput("aaa")
  ),
  dashboardBody(
    tabItems(

      tabItem(tabName = "dataset1",
              tabBox(
                id = "db1_box",
                width = 12,
                tabPanel("Bar", fluidRow(
                  splitLayout(cellWidths = c("50%", "50%"), 
                              plotOutput("db1_bar_1"), 
                              plotOutput("db1_bar_2")))),
                tabPanel("CorMap", plotOutput("db1_cor"))
              )
      ),
      
      tabItem(tabName = "dataset2",
              tabBox(
                id = "db2_box",
                width = 12,
                tabPanel("Map", plotOutput("db2_map")),
                # tabPanel("Bar", fluidRow(plotOutput("db2_bar_1"), plotOutput("db2_bar_2"))),
                tabPanel("Bar", fluidRow(
                  splitLayout(cellWidths = c("50%", "50%"), 
                  plotOutput("db2_bar_1"), 
                  plotOutput("db2_bar_2")))),
                #tabPanel("Bar", plotOutput("db2_bar_2")),
                tabPanel("Time", plotOutput("db2_ts"))
              )
      ),
      tabItem(tabName = "dataset3",
              tabBox(
                id = "db3_box",
                width = 12,
                tabPanel("Map", plotOutput("db3_map")),
                tabPanel("Time", plotOutput("db3_ts"))
              )
      )
    )
  )
)

server <- function(input, output) {
  values <- reactiveValues(
    dataset = NULL,
    plot_type = NULL
  )
  
  observe({
    # Dataset 1 Bar Plot
    values$db1_bar_date <- input$db1_bar_date_val
    values$db1_bar_sex <- input$db1_bar_sex_val
    values$db1_bar_race <- input$db1_bar_race_val
    
    # Dataset 1 Correlation Map
    values$db1_cor_sex <- input$db1_cor_sex_val
    values$db1_cor_race <- input$db1_cor_race_val
    
    # Dataset 2 Map
    values$db2_map_date <- input$db2_map_date_val
    values$db2_map_ages <- input$db2_map_age_val
    values$db2_map_condition_groups <- input$db2_map_cond_group_val
    
    # Dataset 2 Bar
    values$db2_bar_state <- input$db2_bar_state_val
    
    # Dataset 2 Time Series
    values$db2_ts_date <- input$db2_ts_date_val
    values$db2_ts_state <- input$db2_ts_state_val
    values$db2_ts_age <- input$db2_ts_age_val
    
    # Dataset 3 Map
    values$db3_map_date <- input$db3_map_date_val
    
    # Dataset 3 Time Series
    values$db3_ts_date <- input$db3_ts_date_val
    values$db3_ts_state <- input$db3_ts_state_val
    values$db3_ts_choice <- input$db3_ts_choice_val
    
    
    #---------------------------------------------------------------
    
    # Dataset 1 Bar Chart
    output$db1_bar_1 <- renderPlot({
      args <- list(
        count_death_cleaned_visual_2,
        values$db1_bar_date,
        values$db1_bar_sex,
        values$db1_bar_race
      )
      do.call(db1_bar_1_fun, args)
    })
    
    output$db1_bar_2 <- renderPlot({
      args <- list(
        count_death_cleaned_visual_2,
        values$db1_bar_date,
        values$db1_bar_sex,
        values$db1_bar_race
      )
      do.call(db1_bar_2_fun, args)
    })
    
    # Dataset 1 Correlation Map Plot
    output$db1_cor <- renderPlot({
      args <- list(
        count_death_cleaned_model,
        values$db1_cor_sex,
        values$db1_cor_race
      )
      do.call(db1_cor_fun, args)
    })
    
    # Dataset 2 Map Plot
    output$db2_map <- renderPlot({
      args <- list(condition_covi_cleaned %>% filter(group == "By Month"),
                   values$db2_map_date,
                   values$db2_map_ages,
                   values$db2_map_condition_groups)
      do.call(db2_map_plot, args)
    })
    
    # Dataset 2 Bar Plot
    output$db2_bar_1 <- renderPlot({
      args <- list(condition_covi_cleaned %>% filter(group == "By Total" & 
                                                       state != "United States"),
                   values$db2_bar_state)
      do.call(db2_bar_2_fun, args)
    })
    output$db2_bar_2 <- renderPlot({
      args <- list(condition_covi_cleaned %>% filter(group == "By Total" & 
                                                       !age_group %in% c("Not stated", "All Ages")),
                   values$db2_bar_state)
      do.call(db2_bar_1_fun, args)
    })
    
    # Dataset 2 Time Series Plot
    output$db2_ts <- renderPlot({
      args <- list(condition_covi_cleaned[,-11] %>% filter(group == "By Month"),
                   values$db2_ts_date,
                   values$db2_ts_state,
                   values$db2_ts_age)
      do.call(db2_ts_fun, args)
    })
    
    
    # Dataset 3 Map Plot
    output$db3_map <- renderPlot({
      args <- list(
        covi_data_cleaned_visual %>% filter(number_type == "tot_cases"),
        values$db3_map_date
      )
      
      do.call(db3_map_fun, args)
    })
    
    # Dataset 3 Time Series Plot
    output$db3_ts <- renderPlot({
      args <- list(
        covi_data_cleaned_model %>% filter(!state %in% c("United States", "New York City", "Puerto Rico")),
        values$db3_ts_date,
        values$db3_ts_state,
        values$db3_ts_choice
      )
      
      do.call(db3_ts_fun, args)
    })
    
  })
  
  observeEvent(input$dataset,{
    output$db1_bar_date <- NULL
    output$db1_bar_sex <- NULL
    output$db1_bar_race <- NULL
      
    output$db1_cor_sex <- NULL
    output$db1_cor_race <- NULL
    
    output$db2_map_date <- NULL
    output$db2_map_cond_group <- NULL
    output$db2_map_age <- NULL
    
    output$db2_bar_state <- NULL
    
    output$db2_ts_date <- NULL
    output$db2_ts_state <- NULL
    output$db2_ts_age <- NULL
    
    output$db3_map_date <- NULL
    
    output$db3_ts_date <- NULL
    output$db3_ts_state <- NULL
    output$db3_ts_choice <- NULL
      
    if(input$dataset == "dataset1"){
      observeEvent(input$db1_box, {
        if(input$db1_box == "Bar"){
          output$db1_cor_sex <- NULL
          output$db1_cor_race <- NULL
          
          output$db2_map_date <- NULL
          output$db2_map_cond_group <- NULL
          output$db2_map_age <- NULL
          
          output$db2_bar_state <- NULL
          
          output$db2_ts_date <- NULL
          output$db2_ts_state <- NULL
          output$db2_ts_age <- NULL
          
          output$db3_map_date <- NULL
          
          output$db3_ts_date <- NULL
          output$db3_ts_state <- NULL
          output$db3_ts_choice <- NULL
          output$db1_bar_date <- renderUI({
            sliderInput(
              "db1_bar_date_val",
              "Dates(db1_bar_date)",
              min = as.Date("2019-07-01", "%Y-%m-%d"),
              max = as.Date("2021-09-01", "%Y-%m-%d"),
              step = 30,
              value = c(as.Date("2019-07-01", "%Y-%m-%d"), as.Date("2021-09-01", "%Y-%m-%d")),
              timeFormat = "%b %Y"
            )
            
          })
          output$db1_bar_sex <- renderUI({
            pickerInput(
              "db1_bar_sex_val",
              "Choose Sex",
              choices = c(
                "male",
                "female"
              ),
              selected = c("male", "female"),
              multiple = TRUE,
              options = pickerOptions(
                actionsBox = TRUE,
                title = "Select Sex",
                headr = "Sex"
              )
            )
          })
          output$db1_bar_race <- renderUI({
            pickerInput(
              "db1_bar_race_val",
              "Races",
              choices = races,
              multiple = TRUE,
              selected = races,
              options = pickerOptions(
                actionsBox = TRUE,
                title = "Please select Races",
                # title = as.character(length(input$condition_group)),
                header = "Races"
              ),
            )
          })
        }
        if(input$db1_box == "CorMap"){
          output$db1_bar_date <- NULL
          output$db1_bar_sex <- NULL
          output$db1_bar_race <- NULL
          
          output$db2_map_date <- NULL
          output$db2_map_cond_group <- NULL
          output$db2_map_age <- NULL
          
          output$db2_bar_state <- NULL
          
          output$db2_ts_date <- NULL
          output$db2_ts_state <- NULL
          output$db2_ts_age <- NULL
          
          output$db3_map_date <- NULL
          
          output$db3_ts_date <- NULL
          output$db3_ts_state <- NULL
          output$db3_ts_choice <- NULL
          output$db1_cor_sex <- renderUI({
            pickerInput(
              "db1_cor_sex_val",
              "Choose Sex",
              choices = c(
                "male",
                "female"
              ),
              selected = c("male", "female"),
              multiple = TRUE,
              options = pickerOptions(
                actionsBox = TRUE,
                title = "Select Sex",
                headr = "Sex"
              )
            )
          })
          output$db1_cor_race <- renderUI({
            pickerInput(
              "db1_cor_race_val",
              "Races",
              choices = races,
              multiple = TRUE,
              selected = races,
              options = pickerOptions(
                actionsBox = TRUE,
                title = "Please select Races",
                # title = as.character(length(input$condition_group)),
                header = "Races"
              ),
            )
          })
        }
      })
    }
  
    if(input$dataset == "dataset2"){
      
      observeEvent(input$db2_box, {
        if(input$db2_box == "Map"){
          output$db1_bar_date <- NULL
          output$db1_bar_sex <- NULL
          output$db1_bar_race <- NULL
          
          output$db1_cor_sex <- NULL
          output$db1_cor_race <- NULL

          output$db2_bar_state <- NULL
          
          output$db2_ts_date <- NULL
          output$db2_ts_state <- NULL
          output$db2_ts_age <- NULL
          
          output$db3_map_date <- NULL
          
          output$db3_ts_date <- NULL
          output$db3_ts_state <- NULL
          output$db3_ts_choice <- NULL
          output$db2_map_date <- renderUI({
            sliderInput(
              "db2_map_date_val",
              "Dates(db2_map_date)",
              min = as.Date("2020-01-01", "%Y-%m-%d"),
              max = as.Date("2022-10-01", "%Y-%m-%d"),
              step = 30,
              value = c(as.Date("2020-01-01", "%Y-%m-%d"), as.Date("2022-10-31", "%Y-%m-%d")),
              timeFormat = "%b %Y"
            )
            
          })
          output$db2_map_cond_group <- renderUI({
            pickerInput(
              "db2_map_cond_group_val",
              "Condition Groups",
              choices = condition_groups,
              multiple = TRUE,
              selected = condition_groups,
              options = pickerOptions(
                actionsBox = TRUE,
                title = "Please select conditon groups",
                # title = as.character(length(input$condition_group)),
                header = "Condition Groups"
              )
            )
          })
          output$db2_map_age <- renderUI({
            pickerInput(
              "db2_map_age_val",
              "Age Groups",
              choices = age_groups,
              multiple = TRUE,
              selected = age_groups,
              options = pickerOptions(
                actionsBox = TRUE,
                title = "Please select age groups",
                # title = as.character(length(input$condition_group)),
                header = "Age Groups"
              )
            )
          })
        }
        
        if(input$db2_box == "Bar"){
          output$db1_bar_date <- NULL
          output$db1_bar_sex <- NULL
          output$db1_bar_race <- NULL
          
          output$db1_cor_sex <- NULL
          output$db1_cor_race <- NULL
          
          output$db2_map_date <- NULL
          output$db2_map_cond_group <- NULL
          output$db2_map_age <- NULL
          
          output$db2_ts_date <- NULL
          output$db2_ts_state <- NULL
          output$db2_ts_age <- NULL
          
          output$db3_map_date <- NULL
          
          output$db3_ts_date <- NULL
          output$db3_ts_state <- NULL
          output$db3_ts_choice <- NULL
          output$db2_bar_state <- renderUI({
            pickerInput(
              "db2_bar_state_val",
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
        }
        
        if(input$db2_box == "Time"){
          output$db1_bar_date <- NULL
          output$db1_bar_sex <- NULL
          output$db1_bar_race <- NULL
          
          output$db1_cor_sex <- NULL
          output$db1_cor_race <- NULL
          
          output$db2_map_date <- NULL
          output$db2_map_cond_group <- NULL
          output$db2_map_age <- NULL
          
          output$db2_bar_state <- NULL
          
          output$db3_map_date <- NULL
          
          output$db3_ts_date <- NULL
          output$db3_ts_state <- NULL
          output$db3_ts_choice <- NULL
          output$db2_ts_date <- renderUI({
            sliderInput(
              "db2_ts_date_val",
              "Dates(db2_ts_date)",
              min = as.Date("2020-01-01", "%Y-%m-%d"),
              max = as.Date("2022-10-01", "%Y-%m-%d"),
              step = 30,
              value = c(as.Date("2020-01-01", "%Y-%m-%d"), as.Date("2022-10-31", "%Y-%m-%d")),
              timeFormat = "%b %Y",
            )
            
          })
          output$db2_ts_state <- renderUI({
            pickerInput(
              "db2_ts_state_val",
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
              "db2_ts_age_val",
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
      })
    }
    
    if(input$dataset == "dataset3"){
      observeEvent(input$db3_box, {
        if(input$db3_box == "Map"){
          output$db1_bar_date <- NULL
          output$db1_bar_sex <- NULL
          output$db1_bar_race <- NULL
          
          output$db1_cor_sex <- NULL
          output$db1_cor_race <- NULL
          
          output$db2_map_date <- NULL
          output$db2_map_cond_group <- NULL
          output$db2_map_age <- NULL
          
          output$db2_bar_state <- NULL
          
          output$db2_ts_date <- NULL
          output$db2_ts_state <- NULL
          output$db2_ts_age <- NULL
          
          output$db3_ts_date <- NULL
          output$db3_ts_state <- NULL
          output$db3_ts_choice <- NULL
          output$db3_map_date <- renderUI({
            sliderInput(
              "db3_map_date_val",
              "Dates(tb3_map_date)",
              min = as.Date("2020-01-22", "%Y-%m-%d"),
              max = as.Date("2022-10-18", "%Y-%m-%d"),
              step = 30,
              value = c(as.Date("2020-01-01", "%Y-%m-%d"), as.Date("2022-10-31", "%Y-%m-%d")),
              timeFormat = "%b %Y",
            )
            
          })
        }
        if(input$db3_box == "Time"){
          output$db1_bar_date <- NULL
          output$db1_bar_sex <- NULL
          output$db1_bar_race <- NULL
          
          output$db1_cor_sex <- NULL
          output$db1_cor_race <- NULL
          
          output$db2_map_date <- NULL
          output$db2_map_cond_group <- NULL
          output$db2_map_age <- NULL
          
          output$db2_bar_state <- NULL
          
          output$db2_ts_date <- NULL
          output$db2_ts_state <- NULL
          output$db2_ts_age <- NULL
          
          output$db3_map_date <- NULL
          
          output$db3_ts_date <- renderUI({
            sliderInput(
              "db3_ts_date_val",
              "Dates(db3_ts_date)",
              min = as.Date("2020-01-22", "%Y-%m-%d"),
              max = as.Date("2022-10-18", "%Y-%m-%d"),
              step = 30,
              value = c(as.Date("2020-01-01", "%Y-%m-%d"), as.Date("2022-10-31", "%Y-%m-%d")),
              timeFormat = "%b %Y",
            )
            
          })
          output$db3_ts_state <- renderUI({
            pickerInput(
              "db3_ts_state_val",
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
          output$db3_ts_choice <- renderUI({
            selectInput(
              "db3_ts_choice_val",
              "Choices: ",
              choices = c(
                "Cases",
                "Deaths"
              ),
              selected = "Cases"
            )
          })
        }
      })
    }
    
  })
  
  
  
  
  # output$aaa <- renderPrint({
  #   # covi_data_cleaned_visual %>% filter(number_type == "tot_cases")
  #   values$db1_bar_sex
  # })
  
}

shinyApp(ui, server)