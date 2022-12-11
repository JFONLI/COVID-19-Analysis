library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)
library(shinyjs)
library(shinycssloaders)

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
count_death_cleaned_model <- readRDS("www/data/count_death_cleaned_model.RDS")
count_death_cleaned_visual_1 <- readRDS("www/data/count_death_cleaned_visual_1.RDS")
count_death_cleaned_visual_2 <- readRDS("www/data/count_death_cleaned_visual_2.RDS")
races <- as.character(unique(count_death_cleaned_model$race)) %>% rev()
db1_age_groups <- as.character(unique(count_death_cleaned_model$age_group))
db1_age_groups    

## Dataset 2
condition_covi_cleaned <- readRDS("www/data/condition_covi_cleaned.rdS")
condition_groups <- as.character(unique(condition_covi_cleaned$condition_group))
age_groups <- as.character(unique(condition_covi_cleaned$age_group)[c(1:8)])

## Dataset 3
covi_data_cleaned_model <- readRDS("www/data/covi_data_cleaned_model.RDS")
covi_data_cleaned_visual <- readRDS("www/data/covi_data_cleaned_visual.RDS")

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


ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "COVID-19 SUMMARY", titleWidth = 250),
  dashboardSidebar(
    width = 250,
    div(
      id = "db1_ui",
      sidebarMenu(
        menuItem(
          "Date",
          icon = icon("calendar"),
          sliderInput(
            "db1_date",
            "",
            min = as.Date("2019-01-01", "%Y-%m-%d"),
            max = as.Date("2021-09-01", "%Y-%m-%d"),
            step = 30,
            value = c(as.Date("2019-01-01", "%Y-%m-%d"), as.Date("2021-09-01", "%Y-%m-%d")),
            timeFormat = "%b %Y"
          )
        ),
        menuItem(
          "Sex",
          icon = icon("venus-mars"),
          pickerInput(
            "db1_sex",
            "",
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
          ),
          br()
        ),
        menuItem(
          "Race",
          icon = icon("dna"),
          pickerInput(
            "db1_race",
            "",
            choices = races,
            multiple = TRUE,
            selected = races,
            options = pickerOptions(
              actionsBox = TRUE,
              title = "Please select Races",
              # title = as.character(length(input$condition_group)),
              header = "Races"
            )
          )
        ),
        menuItem(
          "Age Group",
          icon = icon("person-cane"),
          pickerInput(
            "db1_age",
            "",
            choices = db1_age_groups,
            multiple = TRUE,
            selected = db1_age_groups,
            options = pickerOptions(
              actionsBox = TRUE,
              title = "Please select age groups",
              # title = as.character(length(input$condition_group)),
              header = "Age Groups"
            )
          )
        )
        
      )
    ),
    div(
      id = "db2_ui",
      sidebarMenu(
        menuItem(
          "Date",
          icon = icon("calendar"),
          sliderInput(
            "db2_date",
            "",
            min = as.Date("2020-01-01", "%Y-%m-%d"),
            max = as.Date("2022-10-01", "%Y-%m-%d"),
            step = 30,
            value = c(as.Date("2020-01-01", "%Y-%m-%d"), as.Date("2022-10-01", "%Y-%m-%d")),
            timeFormat = "%b %Y"
          )
        ),
        menuItem(
          "Condition Group",
          icon = icon("disease"),
          pickerInput(
            "db2_condition",
            "",
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
        ),
        menuItem(
          "Age Group",
          icon = icon("person-cane"),
          pickerInput(
            "db2_age",
            "",
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
        ),
        menuItem(
          "States",
          icon = icon("map-pin"),
          pickerInput(
            "db2_state",
            "",
            choices = states,
            multiple = TRUE,
            selected = states,
            options = pickerOptions(
              actionsBox = TRUE,
              title = "Please select States",
              # title = as.character(length(input$condition_group)),
              header = "States"
            )
          )
        )
      )
    ),
    
    div(
      id = "db3_ui",
      sidebarMenu(
        menuItem(
          "Date",
          icon = icon("calendar"),
          sliderInput(
            "db3_date",
            "",
            min = as.Date("2020-01-22", "%Y-%m-%d"),
            max = as.Date("2022-10-18", "%Y-%m-%d"),
            step = 30,
            value = c(as.Date("2020-01-22", "%Y-%m-%d"), as.Date("2022-10-18", "%Y-%m-%d")),
            timeFormat = "%b %Y"
          )
        ),
        menuItem(
          "States",
          icon = icon("map-pin"),
          pickerInput(
            "db3_state",
            "",
            choices = states,
            multiple = TRUE,
            selected = states,
            options = pickerOptions(
              actionsBox = TRUE,
              title = "Please select States",
              # title = as.character(length(input$condition_group)),
              header = "States"
            )
          )
        ),
        menuItem(
          "Choices",
          icon = icon("square-check"),
          selectInput(
            "db3_choice",
            "",
            choices = c(
              "Cases",
              "Deaths"
            ),
            selected = "Cases"
          )
        )
      )
    ),
    
    div(
      id = "pred_ui",
      sidebarMenu(
        menuItem(
          "State",
          icon = icon("map-pin"),
          selectInput(
            "pred_state",
            "",
            choices = stateList,
            selected = stateList[1]
          )
        )
      )
    ),
    
    verbatimTextOutput("aaa")
  ),
  
  
  
  dashboardBody(
    useShinyjs(),
    fluidRow(
      column(3, 
        bsButton("dataset1", 
                 label = "US Mortality", 
                 icon = icon("database"), 
                 style = "primary",
                 size = "large",
                 width = "100%")
      ),
      column(3, 
             bsButton("dataset2", 
                      label = "COVID-19 Deaths & Conditions", 
                      icon = icon("database"), 
                      style = "primary",
                      size = "large",
                      width = "100%")
      ),
      column(3, 
             bsButton("dataset3", 
                      label = "COVID-19 Cases & Deaths", 
                      icon = icon("database"), 
                      style = "primary",
                      size = "large",
                      width = "100%")
      ),
      column(3, 
             bsButton("prediction", 
                      label = "Future COVID-19 Cases Prediction", 
                      icon = icon("table"), 
                      style = "primary",
                      size = "large",
                      width = "100%")
      )
      
      
      
    ),
      
    br(),
    
    div(
      id = "pred_panel",
      fluidRow(
        valueBoxOutput("pred_kpi_1"),
        valueBoxOutput("pred_kpi_2"),
        valueBoxOutput("pred_kpi_3")
      ),
      fluidRow(
        box(
          plotOutput("pred_line")
        )
      )
    ),
    
    div(
      id = "db1_panel",
      fluidRow(
        valueBoxOutput("db1_kpi_1") %>% withSpinner(color="#0dc5c1"),
        valueBoxOutput("db1_kpi_2"),
        valueBoxOutput("db1_kpi_3")
      ),
      fluidRow(
        box(
          title = "Correlation Map",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("db1_cor") %>% withSpinner(color="#0dc5c1")
        )
      ),
      fluidRow(
        box(
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          title = "Bar Chart",
          fluidRow(
            splitLayout(cellWidths = c("35%", "35%"), 
                        plotOutput("db1_loli") %>% withSpinner(color="#0dc5c1"),
                        plotOutput("db1_bar") %>% withSpinner(color="#0dc5c1"))
          )
        )
      )
    ),
    
    
    div(
      id = "db2_panel",
      fluidRow(
        valueBoxOutput("db2_kpi_1"),
        valueBoxOutput("db2_kpi_2"),
        valueBoxOutput("db2_kpi_3")
      ),
      fluidRow(
        box(
          width = 6,
          title = "Map",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("db2_map") %>% withSpinner(color="#0dc5c1")
        ),
        box(
          width = 6,
          title = "Time Series",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("db2_ts") %>% withSpinner(color="#0dc5c1")
        )
      ),
      fluidRow(
        box(
          width = 12,
          title = "Bar Chart",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          fluidRow(
            splitLayout(cellWidths = c("50%", "50%"), 
                        plotOutput("db2_loli") %>% withSpinner(color="#0dc5c1"),
                        plotOutput("db2_bar") %>% withSpinner(color="#0dc5c1"))
          )
        )
      )
    ),
    
    div(
      id = "db3_panel",
      fluidRow(
        # column(
        #   6,
        #   valueBoxOutput("db3_kpi_1")
        # ),
        # column(
        #   6,
        #   valueBoxOutput("db3_kpi_2")
        # )
        valueBoxOutput("db3_kpi_1", width = 6),
        valueBoxOutput("db3_kpi_2", width = 6)
      ),
      fluidRow(
        box(
          width = 6,
          title = "Map",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("db3_map") %>% withSpinner(color="#0dc5c1")
        ),
        box(
          width = 6,
          title = "Time Series",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("db3_ts") %>% withSpinner(color="#0dc5c1")
        )
      ),
      fluidRow(
        box(
          width = 12,
          height = 250,
          title = "Heat Map",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("db3_heat") %>% withSpinner(color="#0dc5c1")
        )
      )
    )
    
    # fluidRow(
    #   div(
    #     id = "db3_panel",
    #     column(
    #       width = 12,
    #       valueBoxOutput("db3_kpi_1"),
    #       valueBoxOutput("db3_kpi_2")
    #     ),
    #     box(
    #       title = "Map",
    #       plotOutput("db3_map")
    #     ),
    #     box(
    #       title = "Heat Map",
    #       plotOutput("db3_heat")
    #     ),
    #     box(
    #       title = "Time Series",
    #       plotOutput("db3_ts")
    #     )
    # 
    # 
    #     # fluidRow(
    #     #   column(
    #     #     width = 10,
    #     #     box(
    #     #       title = "Map",
    #     #       plotOutput("db3_map")
    #     #     )
    #     #   ),
    #     #   column(
    #     #     width = 10,
    #     #     box(
    #     #       title = "Heat Map",
    #     #       plotOutput("db3_heat")
    #     #     )
    #     #   )
    #     # ),
    #     # column(
    #     #   width = 12,
    #     #   box(
    #     #     title = "Time Series",
    #     #     plotOutput("db3_ts")
    #     #   )
    #     # )
    #   )
    # )
  )
)

server <- function(input, output){
  values <- reactiveValues(
    dataset = NULL,
    plot_type = NULL
  )
  
  observeEvent("", {
    shinyjs::show("db1_ui")
    shinyjs::show("db1_panel")
    shinyjs::hide("db2_ui")
    shinyjs::hide("db2_panel")
    shinyjs::hide("db3_ui")
    shinyjs::hide("db3_panel")
    shinyjs::hide("pred_ui")
    shinyjs::hide("pred_panel")
  })
  
  observeEvent(input$dataset1, {
    shinyjs::show("db1_ui")
    shinyjs::show("db1_panel")
    shinyjs::hide("db2_ui")
    shinyjs::hide("db2_panel")
    shinyjs::hide("db3_ui")
    shinyjs::hide("db3_panel")
    shinyjs::hide("pred_ui")
    shinyjs::hide("pred_panel")
  })
  
  observeEvent(input$dataset2, {
    shinyjs::hide("db1_ui")
    shinyjs::hide("db1_panel")
    shinyjs::show("db2_ui")
    shinyjs::show("db2_panel")
    shinyjs::hide("db3_ui")
    shinyjs::hide("db3_panel")
    shinyjs::hide("pred_ui")
    shinyjs::hide("pred_panel")
  })
  
  observeEvent(input$dataset3, {
    shinyjs::hide("db1_ui")
    shinyjs::hide("db1_panel")
    shinyjs::hide("db2_ui")
    shinyjs::hide("db2_panel")
    shinyjs::show("db3_ui")
    shinyjs::show("db3_panel")
    shinyjs::hide("pred_ui")
    shinyjs::hide("pred_panel")
  })
  
  observeEvent(input$prediction, {
    shinyjs::hide("db1_ui")
    shinyjs::hide("db1_panel")
    shinyjs::hide("db2_ui")
    shinyjs::hide("db2_panel")
    shinyjs::hide("db3_ui")
    shinyjs::hide("db3_panel")
    shinyjs::show("pred_ui")
    shinyjs::show("pred_panel")
  })
  
  
  observe({
    
    # Dataset 1 Varibales
    values$db1_date <- input$db1_date
    values$db1_sex <- input$db1_sex
    values$db1_race <- input$db1_race
    values$db1_age <- input$db1_age
    
    # Dataste 1 KPI
    values$db1_kpi <- db1_kpi_fun(
      count_death_cleaned_model,
      values$db1_date,
      values$db1_sex,
      values$db1_race,
      values$db1_age
    )
    
    output$db1_kpi_1 <- renderValueBox({
      valueBox(
        paste(values$db1_kpi[1], "%"), "US Overall Death Rates", icon = icon("skull-crossbones"),
        color = "purple"
      )
    })
    
    output$db1_kpi_2 <- renderValueBox({
      valueBox(
        paste(values$db1_kpi[2], "%"), "Accidents Death Rates", icon = icon("person-falling-burst"),
        color = "aqua"
      )
    })
    
    output$db1_kpi_3 <- renderValueBox({
      valueBox(
        paste(values$db1_kpi[3], "%"), "COVID-19 Death Rates", icon = icon("virus-covid"),
        color = "yellow"
      )
    })
    
    # Dataset 1 Correlation Map
    output$db1_cor <- renderPlot({
      args <- list(
        count_death_cleaned_model,
        values$db1_sex,
        values$db1_race
      )
      do.call(db1_cor_fun, args)
    })
    
    # Dataset 1 Loliplot & BarPlot
    output$db1_loli <- renderPlot({
      args <- list(
        count_death_cleaned_visual_2,
        values$db1_date,
        values$db1_sex,
        values$db1_race
      )
      do.call(db1_loli_fun, args)
    })
    
    output$db1_bar <- renderPlot({
      args <- list(
        count_death_cleaned_visual_2,
        values$db1_date,
        values$db1_sex,
        values$db1_race
      )
      do.call(db1_bar_fun, args)
    })
    
    # Dataset 2 Variables
    values$db2_date <- input$db2_date
    values$db2_condition <- input$db2_condition
    values$db2_age <- input$db2_age
    values$db2_state <- input$db2_state
    
    # Dataset 2 KPI
    values$db2_kpi <- db2_kpi_fun(condition_covi_cleaned, 
                                  values$db2_date,
                                  values$db2_state,
                                  values$db2_age)
    
    output$db2_kpi_1 <- renderValueBox({
      valueBox(
        paste(values$db2_kpi[[1]][1], "%"), paste("% Healthy People Die From", values$db2_kpi[[2]][1]), icon = icon("virus-covid"),
        color = "yellow"
      )
    })
    
    output$db2_kpi_2 <- renderValueBox({
      valueBox(
        paste(values$db2_kpi[[1]][2], "%"), paste("COVID-19 Death With ",values$db2_kpi[[2]][2]), icon = icon("ranking-star"),
        color = "yellow"
      )
    })
    
    output$db2_kpi_3 <- renderValueBox({
      valueBox(
        paste(values$db2_kpi[[1]][3], "%"), paste("COVID-19 Death With", values$db2_kpi[[2]][3]), icon = icon("ranking-star"),
        color = "yellow"
      )
    })
  
    # Dataset 2 Map
    output$db2_map <- renderPlot({
      args <- list(
        condition_covi_cleaned %>% filter(group == "By Month"),
        values$db2_date,
        values$db2_age,
        values$db2_condition
      )
      do.call(db2_map_fun, args)
    })
    
    # Dataset 2 Time Series
    output$db2_ts <- renderPlot({
      args <- list(
        condition_covi_cleaned[,-11] %>% filter(group == "By Month"),
        values$db2_date,
        values$db2_state,
        values$db2_age
      )
      do.call(db2_ts_fun, args)
    })
    
    # Dataset 2 Loliplot & Barplot
    output$db2_loli <- renderPlot({
      args <- list(
        condition_covi_cleaned,
        values$db2_date,
        values$db2_state,
        values$db2_age
      )
      do.call(db2_loli_fun, args)
    })
    
    output$db2_bar <- renderPlot({
      args <- list(
        condition_covi_cleaned,
        values$db2_date,
        values$db2_state,
        values$db2_age
      )
      do.call(db2_bar_fun, args)
    })
    
    # Dataset 3 Variables
    values$db3_date <- input$db3_date
    values$db3_state <- input$db3_state
    values$db3_choice <- input$db3_choice
    
    # Dataset 3 KPI
    values$db3_kpi <- db3_kpi_fun(covi_data_cleaned_visual, values$db3_date, values$db3_state)
    
    output$db3_kpi_1 <- renderValueBox({
      valueBox(
        paste(values$db3_kpi[1], "%"), "COVID-19 Confirmed Cases Rate", icon = icon("head-side-mask"),
        color = "yellow"
      )
    })
    
    output$db3_kpi_2 <- renderValueBox({
      valueBox(
        paste(values$db3_kpi[2], "%"), "COVID-19 Deaths Rate", icon = icon("face-dizzy"),
        color = "yellow"
      )
    })
    
    # Dataset 3 Map
    output$db3_map <- renderPlot({
      args <- list(
        covi_data_cleaned_visual,
        values$db3_date <- input$db3_date,
        values$db3_choice <- input$db3_choice
      )
      do.call(db3_map_fun, args)
    })
    
    # Dataset 3 Heat Map
    output$db3_heat <- renderPlot({
      args <- list(
        covi_data_cleaned_model,
        values$db3_state <- input$db3_state,
        values$db3_choice <- input$db3_choice
      )
      do.call(db3_heat_fun, args)
    })
    
    
    # Dataset 3 Time Series
    output$db3_ts <- renderPlot({
      args <- list(
        covi_data_cleaned_model,
        values$db3_date,
        values$db3_state,
        values$db3_choice
      )
      
      do.call(db3_ts_fun, args)
    })
    
    # Prediction Variables
    values$pred_state <- input$pred_state
    
    # Prediction KPI

    values$pred_kpi <- pred_kpi_fun(covi_data_new, values$pred_state)
    
    output$pred_kpi_3 <- renderValueBox({
      valueBox(
        paste(values$pred_kpi[1]), "Sum of COVID-19 Cases for Future 100 Days", icon = icon("viruses"),
        color = "yellow"
      )
    })
    
    output$pred_kpi_1 <- renderValueBox({
      valueBox(
        paste(values$pred_kpi[2]), "Updated New Case 2022/10/17", icon = icon("download"),
        color = "yellow"
      )
    })
    
    output$pred_kpi_2 <- renderValueBox({
      valueBox(
        paste(values$pred_kpi[3]), "COVID-19 Cases after 100 Days 2023/01/25", icon = icon("arrow-right"),
        color = "yellow"
      )
    })
    

    
    # Prediction Plot
    output$pred_line <- renderPlot({
      args <- list(
        covi_data_new,
        values$pred_state
      )
      
      do.call(pred_line_fun, args)
    })   
    
  })
  
  
  
  # output$aaa <- renderPrint({
  #   # covi_data_cleaned_visual %>% filter(number_type == "tot_cases")
  #   # values$db2_age
  #   values$db3_choice == "Cases"
  # })
  
}





shinyApp(ui, server)