## app.R ##
library(shiny)
library(shinydashboard)

ui <- fluidPage(
  titlePanel("exercise-4-rds"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "var",
        label = "Choose a variable to display",
        choices = c(
          "Percent White",
          "Percent Black",
          "Percent Hispanic",
          "Percent Asian"
        ),
        selected = "Percent White"
      ),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Map1", fluid = TRUE
        ),
        tabPanel(
          "Map2", fluid = TRUE
        )
      )
    )
  )
  
)


    


server <- function(input, output) { 
  output$map <- renderPlot(
    map("state", col = "green", fill = FALSE)
  )  
  
  output$map2 <- renderPlot(
    map("state", col = "blue", fill = FALSE)
  )
}

shinyApp(ui, server)
