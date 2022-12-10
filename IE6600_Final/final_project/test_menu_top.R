library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)

source("www/functions/fluid_design.R")

ui <- dashboardPage(
  dashboardHeader(title = "COVID"),
  # dashboardSidebar(
  #   fluidRow(
  #     id = "db1_UI",
  #     column(
  #       width = 12,
  #       selectInput(
  #         "sex",
  #         "Sexs",
  #         choices = c("ALL", "MALE", "FEMALE"),
  #         selected = "ALL"
  #       ),
  #       selectInput(
  #         "state",
  #         "States",
  #         choices = c("Alabama", "California", "Texas"),
  #         selected = "Alabama"
  #       )
  #     )
  # 
  #   )
  #   
  # ),
  
  dashboardBody(
    useShinyjs(),
    fluidRow(
      column(
        width = 12,
          bsButton("patients", 
                   label = "PATIENTS", 
                   icon = icon("table"), 
                   style = "success"),
          bsButton("antimicrobials", 
                   label = "ANTIMICROBIALS", 
                   icon = icon("spinner", class = "spinner-box"), 
                   style = "success"),
          bsButton("diagnostics", 
                   label = "DIAGNOSTICS", 
                   icon = icon("flask", class = "flask-box"), 
                   style = "success"),
          bsButton("outcome", 
                   label = "OUTCOME", 
                   icon = icon("thumbs-o-up"), 
                   style = "success")
        )
    ),
    fluidRow(
      div(
        id = "db1_panel",
        column(
          width = 12,
          h2("Hello p1")
        ),
        column(
          width = 6,
          h2("Hello p2")
        ),
        column(
          width = 6,
          h2("Hello p3")
        )
      )
    ),
    fluid_design("db2_panel")
  )
)

server <- function(input, output){
  observeEvent("", {
    show("db1_panel")
    hide("db2_panel")
  })
  
  observeEvent(input$patients, {
    show("db1_panel")
    hide("db2_panel")
    show("db1_UI")
  })
  
  observeEvent(input$antimicrobials, {
    hide("db1_panel")
    show("db2_panel")
    hide("db1_UI")
  })
  
}





shinyApp(ui, server)