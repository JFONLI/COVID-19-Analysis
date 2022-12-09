library(shiny)
library(shinydashboard)
library(shinyBS)

ui <- dashboardPage(
  dashboardHeader(title = "COVID-19 SUMMARY"),
  dashboardSidebar(
    sidebarMenu(
      id = "dataset",
      menuItem("Dataset 1", tabName = "dataset1", icon = icon("table")),
      menuItem("Dataset 2", tabName = "dataset2", icon = icon("table")),
      menuItem("Dataset 3", tabName = "dataset3", icon = icon("table"))
    )
  ),
  dashboardBody(
    fluidRow(
      column(
        width = 12,
        bsButton("patients", 
                 label = "PATIENTS", 
                 icon = icon("user"), 
                 style = "warning"),
        bsButton("antimicrobials", 
                 label = "ANTIMICROBIALS", 
                 icon = icon("spinner", class = "spinner-box"), 
                 style = "warning"),
        bsButton("diagnostics", 
                 label = "DIAGNOSTICS", 
                 icon = icon("flask", class = "flask-box"), 
                 style = "warning"),
        bsButton("outcome", 
                 label = "OUTCOME", 
                 icon = icon("thumbs-o-up"), 
                 style = "warning")
      )
    ),
  )
)



server <- function(input, output, session){
  update_all <- function(x) {
    updateSelectInput(session, "tab",
                      choices = c("", "Patients", "Antimicrobial consumption", "Diagnostics", "Outcome"),
                      label = "",
                      selected = x
    )
  }
  
  observeEvent(input$patients, {
    update_all("Patients")
  })
  observeEvent(input$antimicrobials, {
    update_all("Antimicrobial consumption")
  })
  observeEvent(input$diagnostics, {
    update_all("Diagnostics")
  })
  observeEvent(input$outcome, {
    update_all("Outcome")
  })
  
  output$box_pat2 <- renderUI({
    tabBox(
      id = "box_pat2"
    )
  })
  
  
  observeEvent(input$tab, {
    x <- input$tab
    updateButton(session, "patients", style = {
      if (x == "Patients") {
        paste("warning")
      } else {
        paste("success")
      }
    })
    updateButton(session, "antimicrobials", style = {
      if (x == "Antimicrobial consumption") {
        paste("warning")
      } else {
        paste("success")
      }
    })
    updateButton(session, "diagnostics", style = {
      if (x == "Diagnostics") {
        paste("warning")
      } else {
        paste("success")
      }
    })
    updateButton(session, "outcome", style = {
      if (x == "Outcome") {
        paste("warning")
      } else {
        paste("success")
      }
    })
  })
  
  
}

shinyApp(ui, server)