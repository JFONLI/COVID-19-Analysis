ui <- fluidPage(#theme = shinytheme("paper"),
  titlePanel("1st Example"),
  sidebarLayout(
    sidebarPanel(
      width = 2,
      shinyWidgets::awesomeCheckbox(
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
             plotly::plotlyOutput(
               "histogram", width = "100%", height = "300px"
             ))
    )))
  ))