library(shiny)
library(shinyWidgets)
library(shinydashboard)

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
## Dataset 2
source("www/functions/db2_map_plot.R")
source("www/functions/db2_ts_fun.R")



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

    ## UI for Dataset 2 Time Series
    uiOutput("db2_ts_date"),
    uiOutput("db2_ts_state"),
    uiOutput("db2_ts_age"),


    ## UI for Dataset 3 Map
    uiOutput("db3_map_date"),

    ## UI for Dataset 3 Time Series
    uiOutput("db3_ts_date"),
    uiOutput("db3_ts_state"),
    
    
    
    
    ## Debug
    verbatimTextOutput("aaa")
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dataset1",
              tabBox(
                id = "db1_box",
                tabPanel("Bar"),
                tabPanel("CorMap")
              )
      ),
      
      tabItem(tabName = "dataset2",
              tabBox(
                id = "db2_box",
                tabPanel("Map", plotOutput("db2_map")),
                tabPanel("Bar", plotOutput("db2_bar")),
                tabPanel("Time", plotOutput("db2_ts"))
              )
      ),
      tabItem(tabName = "dataset3",
              tabBox(
                id = "db3_box",
                tabPanel("Map"),
                tabPanel("Time")
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
    
    # Dataset 2 Map
    values$db2_map_date <- input$db2_map_date_val
    values$db2_map_ages <- input$db2_map_age_val
    values$db2_map_condition_groups <- input$db2_map_cond_group_val
    
    # Dataset 2 Time Series
    values$db2_ts_date <- input$db2_ts_date_val
    values$db2_ts_state <- input$db2_ts_state_val
    values$db2_ts_age <- input$db2_ts_age_val
    
    # Dataset 2 Map Plot
    output$db2_map <- renderPlot({
      args <- list(condition_covi_cleaned %>% filter(group == "By Month"),
                   values$db2_map_date,
                   values$db2_map_ages,
                   values$db2_map_condition_groups)
      do.call(db2_map_plot, args)
    })
    
    # Dataset 2 Time Series Plot
    output$db2_ts <- renderPlot({
      args <- list(condition_covi_cleaned[,-11] %>% filter(group == "By Month"),
                   values$db2_ts_date,
                   values$db2_ts_state,
                   values$db2_ts_age)
      do.call(db2_ts_fun, args)
    })
    
  })
  
  observeEvent(input$dataset,{
    output$db1_bar_date <- NULL
    output$db1_bar_sex <- NULL
    output$db1_bar_race <- NULL
  
    if(input$dataset == "dataset2"){
      
      observeEvent(input$db2_box, {
        if(input$db2_box == "Map"){
          output$db2_map_date <- renderUI({
            sliderInput(
              "db2_map_date_val",
              "Dates",
              min = as.Date("2020-01-01", "%Y-%m-%d"),
              max = as.Date("2022-10-31", "%Y-%m-%d"),
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
        
        if(input$db2_box == "Time"){
          output$db2_ts_date <- renderUI({
            sliderInput(
              "db2_ts_date_val",
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
    
  })
  
  
  
  
  output$aaa <- renderPrint({
    input$db2_box
  })
  
}

shinyApp(ui, server)