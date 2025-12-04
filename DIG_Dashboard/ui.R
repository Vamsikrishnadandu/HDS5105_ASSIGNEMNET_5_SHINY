library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)

ui <- dashboardPage(
  dashboardHeader(title = "DIG Trial Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Demographics",
               tabName = "demo",
               icon = icon("users")),
      menuItem("Outcomes",
               tabName = "outcomes",
               icon = icon("heartbeat"))
    ),
    hr(),
    h4("Global Filters", style = "padding: 10px;"),
    sliderInput(
      "age_range",
      "Age Range:",
      min = 20,
      max = 100,
      value = c(40, 80)
    ),
    pickerInput(
      "treatment",
      "Treatment:",
      choices = c("All", "Placebo", "Digoxin"),
      selected = "All"
    ),
    pickerInput(
      "sex",
      "Sex:",
      choices = c("All", "Male", "Female"),
      selected = "All"
    )
  ),
  
  dashboardBody(
    tabItems(
      # DEMOGRAPHICS TAB
      tabItem(
        tabName = "demo",
        h2("Patient Demographics"),
        
        fluidRow(
          valueBoxOutput("n_patients", width = 3),
          valueBoxOutput("mean_age", width = 3),
          valueBoxOutput("mean_ef",  width = 3),
          valueBoxOutput("mean_bmi", width = 3)
        ),
        
        fluidRow(
          box(
            title = "Age Distribution by Treatment",
            status = "primary", solidHeader = TRUE,
            width = 6,
            plotOutput("age_plot")
          ),
          box(
            title = "Ejection Fraction by Treatment",
            status = "primary", solidHeader = TRUE,
            width = 6,
            plotOutput("ef_plot")
          )
        ),
        
        fluidRow(
          box(
            title = "Sex Distribution",
            status = "info", solidHeader = TRUE,
            width = 6,
            plotOutput("sex_bar")
          ),
          box(
            title = "Race Distribution",
            status = "info", solidHeader = TRUE,
            width = 6,
            plotOutput("race_bar")
          )
        ),
        
        fluidRow(
          box(
            title = "BMI Distribution",
            status = "warning", solidHeader = TRUE,
            width = 6,
            plotOutput("bmi_hist")
          ),
          box(
            title = "Blood Pressure by Treatment",
            status = "warning", solidHeader = TRUE,
            width = 6,
            plotOutput("bp_plot")
          )
        ),
        
        fluidRow(
          box(
            title = "Demographic Summary Table",
            status = "success", solidHeader = TRUE,
            width = 12,
            DTOutput("demo_table")
          )
        )
      ),
      
      # OUTCOMES TAB (Aadesh code for outcomes tab)
      tabItem(
        tabName = "outcomes",
        h2("Clinical Outcomes"),
        
        fluidRow(
          box(width = 8, status = "primary", solidHeader = TRUE,
              title = "Mortality by Treatment",
              plotOutput("death_plot")),
          
          box(width = 4, status = "danger", solidHeader = TRUE,
              title = "Mortality Rate Summary",
              plotOutput("mortality_summary_plot"))
        ),
        
        
        fluidRow(
          box(width = 6, status = "danger", title = "Hospitalisation by Treatment",
              plotOutput("hosp_plot")),
          
          box(width = 6, status = "info",
              title = "Death or Worsening HF (Primary Endpoint)",
              plotOutput("dwhf_plot"))
        ),
        
        fluidRow(
          box(width = 12, status = "success",
              title = "Time to Event Distributions",
              plotOutput("time_plot"))
        ),
        
        fluidRow(
          box(width = 12, status = "success", solidHeader = TRUE,
              title = "Event Rate Summary by Treatment",
              DTOutput("outcomes_summary"))
        )
      )
    )
  )
)
