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
  
  output$aaa <- renderPrint({
    values$newStudent
  })
  
  output$title <- renderText({
    input$title
  })
  
  observeEvent(input$recall, {
    values$newStudent <- values$newStudent[-dim(values$newStudent)[1],]
    write.csv(values$newStudent, df.path)
  })
}