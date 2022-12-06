
server <- function(input, output) {
  output$dbList <- renderUI({
    if (input$dbselect == TRUE) {
      shinyWidgets::pickerInput(
        width = "100%",
        inputId = "dbList1",
        label = "Default Dataset List",
        choices = database.default ,
        options = list(title = "List of data.frame...")
      )
    }
  })
  values <- reactiveValues(tbl = NULL, obsList = NULL, obs1=NULL)
  
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
      shiny::validate(need(values$tbl,""))
      plot.df <- as.data.frame(values$tbl[,values$obs1])
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