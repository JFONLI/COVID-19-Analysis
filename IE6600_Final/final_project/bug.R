library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)
library(shinyjs)



ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
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
                 icon = icon("table"), 
                 style = "success"),
        bsButton("diagnostics", 
                 label = "DIAGNOSTICS", 
                 icon = icon("flask", class = "flask-box"), 
                 style = "success"),
        bsButton("outcome", 
                 label = "OUTCOME", 
                 icon = icon("table"), 
                 style = "success")
      )
    ),
    fluidRow(
      div(
        id = "db1_panel",
        column(
          width = 12,
          h2("Hello p1 fun")
        ),
        column(
          width = 6,
          h2("Hello p2 fun")
        ),
        column(
          width = 6,
          h2("Hello p3 fun")
        )
      )
    ),
    fluidRow(
      div(
        id = "db2_panel",
        column(
          width = 12,
          h2("Hello p1 2")
        ),
        column(
          width = 6,
          h2("Hello p2 2")
        ),
        column(
          width = 6,
          h2("Hello p3 2")
        )
      )
    )
  )
)

server <- function(input, output){
  observeEvent("", {
    shinyjs::show("db1_panel")
    shinyjs::hide("db2_panel")
  })
  
  observeEvent(input$patients, {
    shinyjs::show("db1_panel")
    shinyjs::hide("db2_panel")
  })
  
  observeEvent(input$antimicrobials, {
    shinyjs::hide("db1_panel")
    shinyjs::show("db2_panel")
  })
}





shinyApp(ui, server)