library(shiny)
library(plotly)

df.path <- file.path("www/data/students.csv")
#df.students <- data.frame(Course=as.character(),Country=as.character(), Students=as.numeric(), Gender=as.character())
#write.csv(df.students, df.path)



ui <- fluidPage(#theme = shinytheme("paper"),
  titlePanel("Exercise 6-Zhenyuan Lu"),
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
      
      selectInput(
        width="100%",
        inputId = "gender",
        label="Gender:",
        choices = c(choose='',"Male","Female","Others")
        ),
      numericInput(
        width="100%",
        inputId="numbers",
        label="Numbers:",
        value = NA
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
      #verbatimTextOutput("testTxt")
    ),
    
    mainPanel(fluidPage(fluidRow(
      column(6,
             h3(textOutput("title", container = span)),
             tableOutput("dataSet")),
      column(6,
             plotlyOutput(
               "barchart", width = "100%", height = "300px"
             ))
    )))
  ))

server <- function(input, output) {
  values <- reactiveValues()

  observeEvent(input$update,{
    values$dataInput <- data.frame(
      Course=input$course,
      Country=input$country,
      Numbers=input$numbers,
      Gender=input$gender
    )
    values$df <- as.data.frame(read.csv(df.path)[-1])
    if(!"TRUE"%in%is.na(values$dataInput)){
      values$newStudent <- na.omit(unique(rbind(values$df,values$dataInput)))
      write.csv(values$newStudent, df.path)}
  })
  
  output$dataSet <- renderTable(
    values$newStudent
  )
  
  output$testTxt <- renderPrint({
    values$newStudent
  })
  
  output$title <- renderText({
    input$title
  })
  
  observeEvent(input$recall, {
    values$newStudent <- values$newStudent[-dim(values$newStudent)[1],]
    write.csv(values$newStudent, df.path)
  })
  output$barchart <- renderPlotly({
    shiny::validate(need(values$newStudent,""))
    plot_ly(values$newStudent, x = ~Country, y = ~Numbers, type = 'bar', color=~Gender, colors = c("#1F618D", "#cc6900")) %>%
      layout(yaxis = list(title = 'Count'), barmode = 'group')
  })
}

shinyApp(ui, server)