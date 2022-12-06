library(shiny)

ui <- fluidPage(titlePanel("exercise 3"),
                
                sidebarLayout(
                  sidebarPanel(
                    textInput("text", "Name", value = "The teacher Zhenyuan"),
                    
                    selectInput(
                      "variable",
                      label = "Characteristics",
                      choices = list(choose = "", "is handsome",
                                     "is smart",
                                     "is awesome")
                    ),
                    actionButton("evaluation", "True/False")
                  ),
                  
                  mainPanel(fluidPage(
                    fluidRow(verbatimTextOutput("textouput"),
                             br(),
                             uiOutput("truth"))
                  ))
                ))

server <- function(input , output) {
  values <- reactiveValues()
  
  output$textouput <- renderText({
    paste(input$text, input$variable, sep = " ")
  })
  
  
  
  
  observeEvent(input$text, {
    if (input$variable == "is handsome") {
      output$truth <- renderUI({
        h3(helpText(paste("Are you kidding me?"), style = "color:red"))
      })
    } else{
      if (input$variable == "is smart") {
        output$truth <-
          renderUI({
            h3(helpText(paste("Uh?!"), style = "color:red"))
          })
      } else{
        if (input$variable == "is awesome") {
          output$truth <- renderUI({
            h3(helpText(paste("He's lame!"),
                        style = "color:red"))
          })
        }
      }
    }
    
  })
  
  
  
}

shinyApp(ui, server)