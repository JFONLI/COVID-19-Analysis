library(shiny)

ui <- fluidPage(
  titlePanel("exercise 1"),
              
              sidebarLayout(
              sidebarPanel(
              helpText("Create demographic maps with 
              information from the 2010 US Census."),
              
              selectInput("var", 
              label = "Choose a variable to display",
              choices = list("",
                             "Percent White", 
              "Percent Black",
              "Percent Hispanic", 
              "Percent Asian"),
              selected = "Percent White"),
              
              sliderInput("range", 
              label = "Range of interest:",
              min = 0, max = 100, value = c(10, 50))
              ),
              
              mainPanel(
              textOutput("selected_var"),
              textOutput("selected_num")
              )
              )
)
  

server <- function(input, output) {
  
  output$selected_var <- renderText({ 
    paste("You have selected", input$var)
  })
  output$selected_num <- renderText({
    paste("You have chosen a range that goes from ", input$range[1], "to", input$range[2])
  })
  
}

shinyApp(ui, server)
