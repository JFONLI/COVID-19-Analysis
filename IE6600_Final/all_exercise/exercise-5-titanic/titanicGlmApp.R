# Load packages ----
library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(shinyWidgets)

# Source helper functions -----
source("www/functions/titanicGlm.R")

xis <- c("age", "fare")
color <- c(
  "plcass",
  "survived",
  "name" ,
  "sex",
  "age",
  "sibsp",
  "parch",
  "ticket",
  "fare",
  "cabin",
  "embarked",
  "boat",
  "body",
  "home.dest"
)
facet.1 <- c("pclass", "survived", "sex", "age")

# User interface ----
ui <- fluidPage(titlePanel("exercise-titanic"),
                sidebarLayout(
                  sidebarPanel(
                    width = 2,
                    helpText("Create GLM based on titanic dataset. "),
                    
                    # Input: Select a file ----
                    fileInput(
                      "titanic",
                      "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")
                    ),
                    
                    selectInput(
                      "xv",
                      label = "Choose a x variable",
                      choices = xis,
                      selected = "age"
                    ),
                    
                    selectInput(
                      "colr",
                      label = "Choose a color variable",
                      choices = color,
                      selected = "sex"
                    )
                    ,
                    
                    selectInput(
                      "fac",
                      label = "Choose a facet variable",
                      choices = facet.1,
                      selected = "sex"
                    )
                  ),
                  
                  mainPanel(fluidPage(fluidRow(
                    column(6,
                           DT::dataTableOutput("dataSet")),
                    column(6,
                           plotOutput(
                             "glm", width = "700px", height = "600px"
                           ))
                  )))
                ))

# Server logic ----
server <- function(input, output) {
  values <- reactiveValues(tbl=NULL)

  observeEvent(input$titanic, {
    # Store the uploaded file ----
    values$tbl <- read_csv(input$titanic$datapath)
    output$dataSet <- DT::renderDataTable({
      tryCatch({
        df <- values$tbl
      },
      error = function(e) {
        stop(safeError(e))
      })
    },
    extensions = c('Scroller', 'FixedColumns'),
    options = list(
      deferRender = TRUE,
      scrollX = TRUE,
      scrollY = 400,
      scroller = TRUE,
      dom = 'Bfrtip',
      fixedColumns = TRUE
    ))
  })

  output$glm <- renderPlot({

    titanicGlm(values$tbl, input$xv, input$colr, facet = input$fac)
  })
  
  
}




# Run app ----
shinyApp(ui, server)
