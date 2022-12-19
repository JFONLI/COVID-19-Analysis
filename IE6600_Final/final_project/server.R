server <- function(input, output, session){
  values <- reactiveValues(
    dataset = NULL,
    plot_type = NULL
  )
  
  observeEvent("", {
    shinyjs::show("db1_ui")
    shinyjs::show("db1_panel")
    shinyjs::hide("db2_ui")
    shinyjs::hide("db2_panel")
    shinyjs::hide("db3_ui")
    shinyjs::hide("db3_panel")
    shinyjs::hide("pred_ui")
    shinyjs::hide("pred_panel")
  })
  
  observeEvent(input$dataset1, {
    shinyjs::show("db1_ui")
    shinyjs::show("db1_panel")
    shinyjs::hide("db2_ui")
    shinyjs::hide("db2_panel")
    shinyjs::hide("db3_ui")
    shinyjs::hide("db3_panel")
    shinyjs::hide("pred_ui")
    shinyjs::hide("pred_panel")
  })
  
  observeEvent(input$dataset2, {
    shinyjs::hide("db1_ui")
    shinyjs::hide("db1_panel")
    shinyjs::show("db2_ui")
    shinyjs::show("db2_panel")
    shinyjs::hide("db3_ui")
    shinyjs::hide("db3_panel")
    shinyjs::hide("pred_ui")
    shinyjs::hide("pred_panel")
  })
  
  observeEvent(input$dataset3, {
    shinyjs::hide("db1_ui")
    shinyjs::hide("db1_panel")
    shinyjs::hide("db2_ui")
    shinyjs::hide("db2_panel")
    shinyjs::show("db3_ui")
    shinyjs::show("db3_panel")
    shinyjs::hide("pred_ui")
    shinyjs::hide("pred_panel")
  })
  
  observeEvent(input$prediction, {
    shinyjs::hide("db1_ui")
    shinyjs::hide("db1_panel")
    shinyjs::hide("db2_ui")
    shinyjs::hide("db2_panel")
    shinyjs::hide("db3_ui")
    shinyjs::hide("db3_panel")
    shinyjs::show("pred_ui")
    shinyjs::show("pred_panel")
  })
  
  
  observe({
    
    
    
    # Dataset 1 Varibales
    values$db1_date <- input$db1_date
    values$db1_sex <- input$db1_sex
    values$db1_race <- input$db1_race
    values$db1_age <- input$db1_age
    
    # Dataste 1 KPI
    values$db1_kpi <- db1_kpi_fun(
      count_death_cleaned_model,
      values$db1_date,
      values$db1_sex,
      values$db1_race,
      values$db1_age
    )
    
    output$db1_kpi_1 <- renderValueBox({
      valueBox(
        paste(values$db1_kpi[1], "Deaths"), "Per 100,000 Population", icon = icon("skull-crossbones"),
        color = "purple"
      )
    })
    
    output$db1_kpi_2 <- renderValueBox({
      valueBox(
        paste(values$db1_kpi[2], "Accidents Deaths"), "Per 100,000 Population", icon = icon("person-falling-burst"),
        color = "aqua"
      )
    })
    
    output$db1_kpi_3 <- renderValueBox({
      valueBox(
        paste(values$db1_kpi[3], "COVID-19 Deaths"), "Per 100,000 Population", icon = icon("virus-covid"),
        color = "yellow"
      )
    })
    
    # Dataset 1 Correlation Map
    output$db1_cor <- renderPlot({
      validate(
        need(values$db1_sex != "", "Please select Sex")
      )
      validate(
        need(values$db1_race != "", "Please select Race")
      )
      validate(
        need(values$db1_age != "", "Please select Age Groups")
      )
      args <- list(
        count_death_cleaned_model,
        values$db1_sex,
        values$db1_race,
        values$db1_age
      )
      do.call(db1_cor_fun, args)
    })
    
    # Dataset 1 Time Series
    output$db1_ts <- renderPlotly({
      validate(
        need(values$db1_sex != "", "Please select Sex")
      )
      validate(
        need(values$db1_race != "", "Please select Race")
      )
      validate(
        need(values$db1_age != "", "Please select Age Groups")
      )
      args <- list(
        count_death_cleaned_visual_2,
        values$db1_date,
        values$db1_sex,
        values$db1_race,
        values$db1_age
      )
      do.call(db1_ts_fun, args)
    })
    
    # Dataset 1 Loliplot & BarPlot
    output$db1_loli <- renderPlot({
      validate(
        need(values$db1_sex != "", "Please select Sex")
      )
      validate(
        need(values$db1_race != "", "Please select Race")
      )
      validate(
        need(values$db1_age != "", "Please select Age Groups")
      )
      args <- list(
        count_death_cleaned_visual_2,
        values$db1_date,
        values$db1_sex,
        values$db1_race,
        values$db1_age
      )
      do.call(db1_loli_fun, args)
    })
    
    output$db1_bar <- renderPlot({
      validate(
        need(values$db1_sex != "", "Please select Sex")
      )
      validate(
        need(values$db1_race != "", "Please select Race")
      )
      validate(
        need(values$db1_age != "", "Please select Age Groups")
      )
      args <- list(
        count_death_cleaned_visual_2,
        values$db1_date,
        values$db1_sex,
        values$db1_race,
        values$db1_age
      )
      do.call(db1_bar_fun, args)
    })
    
    # Dataset 2 Variables
    values$db2_date <- input$db2_date
    values$db2_condition <- input$db2_condition
    values$db2_age <- input$db2_age
    values$db2_state <- input$db2_state
    
    # Dataset 2 KPI
    values$db2_kpi <- db2_kpi_fun(condition_covi_cleaned,
                                  values$db2_date,
                                  values$db2_state,
                                  values$db2_age)
    
    output$db2_kpi_1 <- renderValueBox({
      valueBox(
        paste(values$db2_kpi[[1]][1], "%"), paste("% Healthy People Die From", values$db2_kpi[[2]][1]), icon = icon("virus-covid"),
        color = "purple"
      )
    })
    
    output$db2_kpi_2 <- renderValueBox({
      valueBox(
        paste(values$db2_kpi[[1]][2], "%"), paste("COVID-19 Death With ",values$db2_kpi[[2]][2]), icon = icon("ranking-star"),
        color = "aqua"
      )
    })
    
    output$db2_kpi_3 <- renderValueBox({
      valueBox(
        paste(values$db2_kpi[[1]][3], "%"), paste("COVID-19 Death With", values$db2_kpi[[2]][3]), icon = icon("ranking-star"),
        color = "yellow"
      )
    })
    
    # Dataset 2 Map
    output$db2_map <- renderPlotly({
      args <- list(
        condition_covi_cleaned %>% filter(group == "By Month"),
        values$db2_date,
        values$db2_age,
        values$db2_condition
      )
      do.call(db2_map_fun, args)
    })
    
    # Dataset 2 Time Series
    output$db2_ts <- renderPlotly({
      validate(
        need(values$db2_age != "", "Please select Age Group")
      )
      validate(
        need(values$db2_state != "", "Please select States")
      )
      args <- list(
        condition_covi_cleaned[,-11] %>% filter(group == "By Month"),
        values$db2_date,
        values$db2_state,
        values$db2_age
      )
      do.call(db2_ts_fun, args)
    })
    
    # Dataset 2 Loliplot & Barplot
    output$db2_loli <- renderPlot({
      validate(
        need(values$db2_age != "", "Please select Age Group")
      )
      validate(
        need(values$db2_state != "", "Please select States")
      )
      args <- list(
        condition_covi_cleaned,
        values$db2_date,
        values$db2_state,
        values$db2_age
      )
      do.call(db2_loli_fun, args)
    })
    
    output$db2_bar <- renderPlot({
      validate(
        need(values$db2_age != "", "Please select Age Group")
      )
      validate(
        need(values$db2_state != "", "Please select States")
      )
      args <- list(
        condition_covi_cleaned,
        values$db2_date,
        values$db2_state,
        values$db2_age
      )
      do.call(db2_bar_fun, args)
    })
    
    # Dataset 3 Variables
    values$db3_date <- input$db3_date
    values$db3_state <- input$db3_state
    values$db3_choice <- input$db3_choice
    
    # Dataset 3 KPI
    values$db3_kpi <- db3_kpi_fun(covi_data_cleaned_visual, values$db3_date, values$db3_state)
    
    output$db3_kpi_1 <- renderValueBox({
      valueBox(
        paste(values$db3_kpi[1], "%"), "COVID-19 Confirmed Cases Rate", icon = icon("head-side-mask"),
        color = "purple"
      )
    })
    
    output$db3_kpi_2 <- renderValueBox({
      valueBox(
        paste(values$db3_kpi[2], "%"), "COVID-19 Deaths Rate", icon = icon("face-dizzy"),
        color = "yellow"
      )
    })
    
    # Dataset 3 Map
    output$db3_map <- renderPlotly({
      args <- list(
        covi_data_cleaned_visual,
        values$db3_date <- input$db3_date,
        values$db3_choice <- input$db3_choice
      )
      do.call(db3_map_fun, args)
    })
    
    # Dataset 3 Heat Map
    output$db3_heat <- renderPlot({
      validate(
        need(values$db3_state != "", "Please select States")
      )
      args <- list(
        covi_data_cleaned_model,
        values$db3_state <- input$db3_state,
        values$db3_choice <- input$db3_choice
      )
      do.call(db3_heat_fun, args)
    })
    
    
    # Dataset 3 Time Series
    output$db3_ts <- renderPlot({
      validate(
        need(values$db3_state != "", "Please select States")
      )
      args <- list(
        covi_data_cleaned_model,
        values$db3_date,
        values$db3_state,
        values$db3_choice
      )
      do.call(db3_ts_fun, args)
    })
    
    # Prediction Variables
    values$pred_state <- input$pred_state
    
    # Prediction KPI
    
    values$pred_kpi <- pred_kpi_fun(covi_data_new, values$pred_state)
    
    output$pred_kpi_3 <- renderValueBox({
      valueBox(
        paste(values$pred_kpi[1]), "Sum of COVID-19 Cases for Future 100 Days", icon = icon("viruses"),
        color = "yellow"
      )
    })
    
    output$pred_kpi_1 <- renderValueBox({
      valueBox(
        paste(values$pred_kpi[2]), "Updated New Case 2022/10/17", icon = icon("download"),
        color = "purple"
      )
    })
    
    output$pred_kpi_2 <- renderValueBox({
      valueBox(
        paste(values$pred_kpi[3]), "COVID-19 Cases after 100 Days 2023/01/25", icon = icon("arrow-right"),
        color = "aqua"
      )
    })
    
    
    
    # Prediction Plot
    output$pred_line <- renderPlot({
      args <- list(
        covi_data_new,
        values$pred_state
      )
      
      do.call(pred_line_fun, args)
    })
    
  })
  
  
  ## INTRO BOX
  ### https://github.com/ceefluz/radar
  observeEvent("", {
    showModal(modalDialog(
      includeHTML("www/data/intro_text.html"),
      easyClose = TRUE,
      footer = tagList(
        column(
          12,
          actionButton(inputId = "intro", label = "Sure! Give me a tour!", icon = icon("check")),
          align = "center"
        )
      )
    ))
  })
  
  observeEvent(input$intro,{
    removeModal()
  })
  
  # show intro tour
  observeEvent(input$intro,
               introjs(session, options = list(
                 "nextLabel" = "Next",
                 "prevLabel" = "Previous",
                 "doneLabel" = "Got it!"
               )
               )
  )
  
  
  observeEvent(input$help_button,{
    shinyjs::hide("db1_ui")
    shinyjs::hide("db1_panel")
    shinyjs::hide("db2_ui")
    shinyjs::hide("db2_panel")
    shinyjs::hide("db3_ui")
    shinyjs::hide("db3_panel")
    shinyjs::hide("pred_ui")
    shinyjs::hide("pred_panel")
  })
  
  
  
  
  # output$aaa <- renderPrint({
  #   # covi_data_cleaned_visual %>% filter(number_type == "tot_cases")
  #   # values$db2_age
  #   values$db3_choice == "Cases"
  # })
  
}