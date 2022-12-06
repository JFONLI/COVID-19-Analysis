library(shiny)
library(dplyr)
library(DT)
library(shinyWidgets)
library(plotly)

default.df <- as.data.frame(data(package = "datasets")["results"])
database.default <- as.character(default.df$results.Item)

ui <- fluidPage(#theme = shinytheme("paper"),
  titlePanel("1st Example - Zhenyuan Lu"),
  sidebarLayout(
    sidebarPanel(
      width = 2,
      #checkBoxInput
      awesomeCheckbox(
        width = "100%",
        inputId = "dbselect",
        label = "Default Database:",
        value = FALSE
      ),
      uiOutput("dbList"),
      uiOutput("obs1"),
      actionButton(
        inputId = "reset",
        label = "Reset Data",
        icon = icon("refresh"),
        width = "100%"
      ),
      
      verbatimTextOutput("aaa")
    ),
    mainPanel(fluidPage(fluidRow(
      column(6,
             DT::dataTableOutput("dataSet")),
      column(6,
             plotlyOutput(
               "histogram", width = "100%", height = "300px"
             ))
    )))
  ))

server <- function(input, output) {
  values <- reactiveValues(tbl = NULL,
                           obsList = NULL,
                           obs1 = NULL)
  
  output$dbList <- renderUI({
    if (input$dbselect == TRUE) {
      pickerInput(
        width = "100%",
        inputId = "dbList1",
        label = "Default Dataset List",
        choices = database.default ,
        options = list(title = "List of data.frame...")
      )
    }
  })
  
  observeEvent(input$dbList1, {
    if (!NA %in% match(input$dbList1, database.default)) {
      values$tbl <- as.data.frame(get(input$dbList1))
      values$obsList <- colnames(values$tbl)
      output$obs1 <- renderUI({
        pickerInput(
          inputId = "observationInput1",
          label = "1st observation",
          choices =  values$obsList,
          selected = "award_value_rec_tot"
        )
      })
    }
  })
  
  observeEvent(input$observationInput1, {
    values$obs1 <- input$observationInput1
    output$dataSet <- DT::renderDataTable({
      df <- values$tbl
    },
    extensions = c('Scroller', 'FixedColumns'),
    options = list(
      deferRender = TRUE,
      scrollX = TRUE,
      scrollY = 200,
      scroller = TRUE,
      dom = 'Bfrtip',
      fixedColumns = TRUE
    ))
    output$histogram <- renderPlotly({
      shiny::validate(need(values$tbl, ""))
      plot.df <- as.data.frame(values$tbl[, values$obs1])
      colnames(plot.df) <- c("x")
      plot_ly(plot.df, x = ~ x, type = "histogram")
    })
  })
  
  observeEvent(input$reset, {
    values$tbl <- NULL
    output$obs1 <- NULL
  })
  
  output$aaa <- renderPrint({
    values$obs1
  })
}

shinyApp(ui, server)