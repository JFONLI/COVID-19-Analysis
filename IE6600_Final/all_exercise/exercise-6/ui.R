ui <- fluidPage(#theme = shinytheme("paper"),
  titlePanel("2nd Example"),
  sidebarLayout(
    sidebarPanel(
      width=2,
      textInput(
        width = "100%",
        inputId = "title",
        label = "Title:",
        value = NA
      ),
      textInput(
        width="100%",
        inputId = "course",
        label="Course:",
        value = NA
      ),
      textInput(
        width = "100%",
        inputId = "country",
        label = "Country:",
        value = NA
      ),
      numericInput(
        width="100%",
        inputId="numbers",
        label="Numbers:",
        value = NA
      ),
      selectInput(
        width="100%",
        inputId = "gender",
        label="Gender:",
        choices = c(choose='',"Male","Female","Others")
      ),
      actionButton(
        inputId = "update",
        label = "Submit/Update",
        icon = icon("database"),
        width = "100%"
      ),
      actionButton(
        inputId="recall",
        label="recall",
        icon=icon("refresh"),
        width="100%"
      )
      #verbatimTextOutput("aaa")
    ),
    
    mainPanel(fluidPage(fluidRow(
      column(6,
             h3(textOutput("title",container = span)),
             tableOutput("dataSet"))
    )))
  ))