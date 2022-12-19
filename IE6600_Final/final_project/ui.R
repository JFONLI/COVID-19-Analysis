


ui <- dashboardPage(
  title = "COVID-19 Analysis",
  # tags$head(HTML("<title>Baller Lab</title>")),
  skin = "blue",
  # tags$head(HTML("<title>COVID-19 Analysis</title> <link rel='icon' type='image/gif/png' href='icon11.png'>")),
  dashboardHeader(
    
    title = span(img(src = "icon11.png", height = 35), "COVID-19 SUMMARY"),
    titleWidth = 300,
    tags$li(
      class = "dropdown",
      tags$button(
        id = "help_button",
        class="btn-primary",
        icon("question"),
        style = "margin-top: 12px;"
      )
    ),
    dropdownMenu(
      type = "notifications", 
      headerText = strong("CODE DICTIONARY"), 
      icon = icon("circle-info"), 
      badgeStatus = NULL,
      notificationItem(
        icon = icon("book"),
        text = "COVID-19 (U071, Underlying Cause of Death)"
      ),
      notificationItem(
        icon = icon("book"),
        text = "Septicemia (A40-A41)"
      ),
      notificationItem(
        icon = icon("book"),
        text = "Maligant neoplasms (C00-C97)"
      ),
      notificationItem(
        icon = icon("book"),
        text = "Diabetes mellitus (E10-E14)"
      ),
      notificationItem(
        icon = icon("book"),
        text = "Alzheimer disease (G30)"
      ),
      notificationItem(
        icon = icon("book"),
        text = "Diseases of heart (I00-I09,I11,I13,I20-I51)"
      ),
      notificationItem(
        icon = icon("book"),
        text = "Cerebrovascular diseases (I60-I69)"
      ),
      notificationItem(
        icon = icon("book"),
        text = "Other diseases of Respiratory system (J00-J06,J30-J39,J67,J70-J98)"
      ),
      notificationItem(
        icon = icon("book"),
        text = "Influenza and pneumonia (J09-J18)"
      ),
      notificationItem(
        icon = icon("book"),
        text = "Chronic lower respiratory (J40-J47)"
      ),
      notificationItem(
        icon = icon("book"),
        text = "Nephritis (N00-N07,N17-N19,N25-N27)"
      ),
      notificationItem(
        icon = icon("book"),
        text = "Symtoms signs (R00-R99)"
      )
    )
  ),
  dashboardSidebar(
    introjsUI(),
    width = 300,
    introBox(
      div(
        id = "db1_ui",
        sidebarMenu(
          HTML('<br><center><img src="filter.png" width ="100"></center>'),
          menuItem(
            "Date",
            icon = icon("calendar"),
            sliderInput(
              "db1_date",
              "",
              min = as.Date("2019-01-01", "%Y-%m-%d"),
              max = as.Date("2021-09-01", "%Y-%m-%d"),
              step = 30,
              value = c(as.Date("2019-01-01", "%Y-%m-%d"), as.Date("2021-09-01", "%Y-%m-%d")),
              timeFormat = "%b %Y"
            ),
            br()
          ),
          menuItem(
            "Sex",
            icon = icon("venus-mars"),
            pickerInput(
              "db1_sex",
              "",
              choices = c(
                "male",
                "female"
              ),
              selected = c("male", "female"),
              multiple = TRUE,
              options = pickerOptions(
                actionsBox = TRUE,
                title = "Select Sex",
                headr = "Sex"
              )
            ),
            br()
          ),
          menuItem(
            "Race",
            icon = icon("dna"),
            pickerInput(
              "db1_race",
              "",
              choices = races,
              multiple = TRUE,
              selected = races,
              options = pickerOptions(
                actionsBox = TRUE,
                title = "Please select Races",
                # title = as.character(length(input$condition_group)),
                header = "Races"
              )
            ),
            br()
          ),
          menuItem(
            "Age Group",
            icon = icon("person-cane"),
            pickerInput(
              "db1_age",
              "",
              choices = db1_age_groups,
              multiple = TRUE,
              selected = db1_age_groups,
              options = pickerOptions(
                actionsBox = TRUE,
                title = "Please select age groups",
                # title = as.character(length(input$condition_group)),
                header = "Age Groups"
              )
            ),
            br()
          )
          
        )
      ), 
      data.step = 1, data.intro = "Select the filter you want HERE"),
    
    div(
      id = "db2_ui",
      sidebarMenu(
        HTML('<br><center><img src="filter.png" width ="100"></center>'),
        menuItem(
          "Date",
          icon = icon("calendar"),
          sliderInput(
            "db2_date",
            "",
            min = as.Date("2020-01-01", "%Y-%m-%d"),
            max = as.Date("2022-10-01", "%Y-%m-%d"),
            step = 30,
            value = c(as.Date("2020-01-01", "%Y-%m-%d"), as.Date("2022-10-01", "%Y-%m-%d")),
            timeFormat = "%b %Y"
          ),
          br()
        ),
        menuItem(
          "Condition Group",
          icon = icon("disease"),
          pickerInput(
            "db2_condition",
            "",
            choices = condition_groups,
            multiple = TRUE,
            selected = condition_groups,
            options = pickerOptions(
              actionsBox = TRUE,
              title = "Please select conditon groups",
              # title = as.character(length(input$condition_group)),
              header = "Condition Groups"
            )
          ),
          br()
        ),
        menuItem(
          "Age Group",
          icon = icon("person-cane"),
          pickerInput(
            "db2_age",
            "",
            choices = age_groups,
            multiple = TRUE,
            selected = age_groups,
            options = pickerOptions(
              actionsBox = TRUE,
              title = "Please select age groups",
              # title = as.character(length(input$condition_group)),
              header = "Age Groups"
            )
          ),
          br()
        ),
        menuItem(
          "States",
          icon = icon("map-pin"),
          pickerInput(
            "db2_state",
            "",
            choices = states,
            multiple = TRUE,
            selected = states,
            options = pickerOptions(
              actionsBox = TRUE,
              title = "Please select States",
              # title = as.character(length(input$condition_group)),
              header = "States"
            )
          ),
          br()
        )
      )
    ),
    
    div(
      id = "db3_ui",
      sidebarMenu(
        HTML('<br><center><img src="filter.png" width ="100"></center>'),
        menuItem(
          "Date",
          icon = icon("calendar"),
          sliderInput(
            "db3_date",
            "",
            min = as.Date("2020-01-22", "%Y-%m-%d"),
            max = as.Date("2022-10-18", "%Y-%m-%d"),
            step = 30,
            value = c(as.Date("2020-01-22", "%Y-%m-%d"), as.Date("2022-10-18", "%Y-%m-%d")),
            timeFormat = "%b %Y"
          ),
          br()
        ),
        menuItem(
          "States",
          icon = icon("map-pin"),
          pickerInput(
            "db3_state",
            "",
            choices = states,
            multiple = TRUE,
            selected = states,
            options = pickerOptions(
              actionsBox = TRUE,
              title = "Please select States",
              # title = as.character(length(input$condition_group)),
              header = "States"
            )
          ),
          br()
        ),
        menuItem(
          "Choices",
          icon = icon("square-check"),
          selectInput(
            "db3_choice",
            "",
            choices = c(
              "Cases",
              "Deaths"
            ),
            selected = "Cases"
          ),
          br()
        )
        
      )
      
      
    ),
    
    div(
      id = "pred_ui",
      sidebarMenu(
        HTML('<br><center><img src="filter.png" width ="100"></center>'),
        menuItem(
          "State",
          icon = icon("map-pin"),
          selectInput(
            "pred_state",
            "",
            choices = stateList,
            selected = stateList[1]
          ),
          br()
        )
      )
    ),
    
    verbatimTextOutput("aaa")
  ),
  
  
  
  dashboardBody(
    # tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
    useShinyjs(),
    introjsUI(),
    tags$style(HTML(".sidebar-menu li a { font-size: 25px; }")),
    introBox(
      fluidRow(
        column(3,
               bsButton("dataset1",
                        label = "US Mortality",
                        icon = icon("database"),
                        style = "primary",
                        size = "large",
                        width = "100%")
        ),
        column(3,
               bsButton("dataset2",
                        label = "COVID-19 Deaths & Conditions",
                        icon = icon("database"),
                        style = "primary",
                        size = "large",
                        width = "100%")
        ),
        column(3,
               bsButton("dataset3",
                        label = "COVID-19 Cases & Deaths",
                        icon = icon("database"),
                        style = "primary",
                        size = "large",
                        width = "100%")
        ),
        column(3,
               bsButton("prediction",
                        label = "Future COVID-19 Cases Prediction",
                        icon = icon("table"),
                        style = "primary",
                        size = "large",
                        width = "100%")
        )
        
        
        
      ),
      data.step = 2, data.intro = "Select the dataset HERE"),
    
    
    br(),
    
    div(
      id = "pred_panel",
      fluidRow(
        valueBoxOutput("pred_kpi_1"),
        valueBoxOutput("pred_kpi_2"),
        valueBoxOutput("pred_kpi_3")
      ),
      fluidRow(
        box(
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          title = "Predictions for Daily New Cases in the US for Future 100 Days",
          plotOutput("pred_line")
        )
      )
    ),
    
    div(
      id = "db1_panel",
      introBox(
        fluidRow(
          valueBoxOutput("db1_kpi_1") %>% withSpinner(color="#0dc5c1"),
          valueBoxOutput("db1_kpi_2"),
          valueBoxOutput("db1_kpi_3")
        ),
        data.step = 3, data.intro = "KPI Cards"),
      introBox(
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            title = "Top Mortality by Underlying Health Conditions and Age Groups",
            fluidRow(
              column(8, plotOutput("db1_loli") %>% withSpinner(color="#0dc5c1")),
              column(4, plotOutput("db1_bar") %>% withSpinner(color="#0dc5c1"))
            )
          )
        ),
        data.step = 4, data.intro = "Data Visualization"),
      fluidRow(
        box(
          width = 4,
          title = "Correlation Relationship between All Underlying Health Conditions and COVID-19",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("db1_cor") %>% withSpinner(color="#0dc5c1")
        ),
        box(
          width = 8,
          title = "US Mortality Trends by Year & Underlying Health Conditions",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotlyOutput("db1_ts") %>% withSpinner(color="#0dc5c1")
        ),
        # HTML('<br><center><img src="icon10.png" width ="400"></center>')
      )
    ),
    
    
    div(
      id = "db2_panel",
      fluidRow(
        valueBoxOutput("db2_kpi_1"),
        valueBoxOutput("db2_kpi_2"),
        valueBoxOutput("db2_kpi_3")
      ),
      fluidRow(
        box(
          width = 6,
          title = "Top COVID-19 Deaths by US States",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotlyOutput("db2_map") %>% withSpinner(color="#0dc5c1")
        ),
        box(
          width = 6,
          title = "COVID-19 Death Trends by Year & Underlying Health Conditions",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotlyOutput("db2_ts") %>% withSpinner(color="#0dc5c1")
        )
      ),
      fluidRow(
        box(
          width = 12,
          title = "Top Underlying Health Conditions from COVID-19 Deaths between Healthy & Unhealthy People ",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          fluidRow(
            column(8, plotOutput("db2_loli") %>% withSpinner(color="#0dc5c1")),
            column(4, plotOutput("db2_bar") %>% withSpinner(color="#0dc5c1"))
          )
        )
      )
    ),
    
    div(
      id = "db3_panel",
      fluidRow(
        valueBoxOutput("db3_kpi_1", width = 6),
        valueBoxOutput("db3_kpi_2", width = 6)
      ),
      fluidRow(
        box(
          width = 6,
          title = "Top COVID-19 Confirmed Cases & Deaths by US States",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotlyOutput("db3_map") %>% withSpinner(color="#0dc5c1")
        ),
        box(
          width = 6,
          title = "COVID-19 Confirmed Cases vs Death Cases",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("db3_ts") %>% withSpinner(color="#0dc5c1")
        )
      ),
      fluidRow(
        box(
          width = 12,
          title = "COVID-19 New Confirmed Cases and New Deaths Variation by Seasonss",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("db3_heat", height = "1000px") %>% withSpinner(color="#0dc5c1")
        )
      )
    )
  )
)