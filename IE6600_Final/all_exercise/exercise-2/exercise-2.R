library(shiny)


ui <- fluidPage(
  titlePanel("exercise 2"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("text","Name",value="The teacher Zhenyuan"),
      
      selectInput("variable", 
                  label = "Characteristics",
                  choices = list(Select="",
                                 "is handsome", 
                                 "is smart",
                                 "is awesome")),
      
      sliderInput("range","Score:", min=0, max=10, value=0)
      ),
    
    mainPanel(fluidPage(
      fluidRow(
        verbatimTextOutput("textouput")
      )
    ))
  )
  )

server <- function(input ,output){
  output$textouput <- renderText({
    paste(input$text,input$variable,".",
          "(Score:",input$range,")",sep=" ")

  })

  
  
}

shinyApp(ui, server)