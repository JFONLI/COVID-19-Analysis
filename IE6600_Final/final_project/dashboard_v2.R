library(shiny)
library(shinydashboard)

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
    tabItems(
      tabItem(
        tabName = "dataset1",
        fluidRow(
          valueBox(10 * 2, "New Orders"),
          valueBox(10 * 2, "New Orders"),
          valueBox(10 * 2, "New Orders")
        ),
        h2("1111")
      ),
      tabItem(
        tabName = "dataset2",
        fluidRow(
          valueBox(10 * 2, "New Orders"),
          valueBox(10 * 2, "New Orders"),
          valueBox(10 * 2, "New Orders")
        ),
        fluidRow(
          plotOutput("db2_map"),
          plotOutput("db2_ts")
        ),
        fluidRow(
          splitLayout(
            cellWidths = c("50%", "50%"),
            plotOutput("db2_loli"),
            plotOutput("db2_bar")
          ),
        ),
      ),
      tabItem(
        tabName = "dataset3",
        h3("333")
      )
    )
  )
)



server <- function(input, output){
  
}

shinyApp(ui, server)