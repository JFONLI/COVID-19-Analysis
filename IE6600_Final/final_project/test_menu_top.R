
  library(rintrojs)
  library(shiny)
  ui <- shinyUI(fluidPage(
    introjsUI(), # must include in UI
    mainPanel(
      introBox(
        tableOutput("mtcars"),
        data.step = 1,
        data.intro = "This is the table"
      ),
      introBox(
        actionButton("btn","Intro"),
        data.step = 2,
        data.intro = "This is the button"
      )
    )))
  server <- shinyServer(function(input, output, session) {
    output$mtcars <- renderTable({
      head(mtcars)
    })
    observeEvent(input$btn,
                 introjs(session))
  })
  # Run the application
  shinyApp(ui = ui, server = server)
