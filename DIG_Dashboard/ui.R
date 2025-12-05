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
               icon = icon("heartbeat")),
      menuItem("Medications",
               tabName = "meds",
               icon = icon("pills")),
      menuItem("Data Explorer",
               tabName = "data_explorer",
               icon = icon("table"))
      
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
   tags$style(HTML("
  table.dataTable {
    table-layout: fixed !important;
    width: 100% !important;
  }
")),
   
   
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
        box(title = "Age Distribution by Treatment",
            status = "primary", solidHeader = TRUE,
            width = 6, plotOutput("age_plot")),
        
        box(title = "Ejection Fraction by Treatment",
            status = "primary", solidHeader = TRUE,
            width = 6, plotOutput("ef_plot"))
      ),
      
      fluidRow(
        box(title = "Sex Distribution",
            status = "info", solidHeader = TRUE,
            width = 6, plotOutput("sex_bar")),
        
        box(title = "Race Distribution",
            status = "info", solidHeader = TRUE,
            width = 6, plotOutput("race_bar"))
      ),
      
      fluidRow(
        box(title = "BMI Distribution",
            status = "warning", solidHeader = TRUE,
            width = 6, plotOutput("bmi_hist")),
        
        box(title = "Blood Pressure by Treatment",
            status = "warning", solidHeader = TRUE,
            width = 6, plotOutput("bp_plot"))
      ),
      
      fluidRow(
        box(title = "Demographic Summary Table",
            status = "success", solidHeader = TRUE,
            width = 12,
            DTOutput("demo_table"))
      )
    ),
    
    
    # OUTCOMES TAB 
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
        box(width = 6, status = "danger",
            title = "Hospitalisation by Treatment",
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
        box(width = 12, status = "success",
            solidHeader = TRUE,
            title = "Event Rate Summary by Treatment",
            DTOutput("outcomes_summary"))
      )
    ),
    
    # medications tab:
    tabItem(
      tabName = "meds",
      h2("Medication Use by Treatment"),
      
      fluidRow(
        box(
          title = "Medication Prevalence by Treatment",
          status = "primary", solidHeader = TRUE,
          width = 12,
          plotOutput("med_prevalence_plot")
        )
      ),
      
      fluidRow(
        box(
          title = "ACE Inhibitors vs Mortality",
          status = "info", solidHeader = TRUE,
          width = 6,
          plotOutput("ace_outcomes_plot")
        ),
        
        box(
          title = "Diuretics vs Mortality",
          status = "info", solidHeader = TRUE,
          width = 6,
          plotOutput("diuretics_outcomes_plot")
        )
      ),
      
      fluidRow(
        box(
          title = "Medication Summary Table",
          status = "success", solidHeader = TRUE,
          width = 12,
          DTOutput("med_table")
        )
      )
    ),
    
    # DATA EXPLORER TAB
    tabItem(
      tabName = "data_explorer",
      
      h2("Data Explorer - DIG Trial Dataset"),
      
      tags$small(
        icon("info-circle"),
        "Click column headers (▲▼) to sort data. Current sort is shown by arrow direction.",
        style = "color: #444;"
      ),
      
      p(
        "This table displays individual patient-level data after applying the global filters from the sidebar.",
        style = "font-size:14px;color:#555;"
      ),
      fluidRow(
        box(
          width = 12,
          status = "warning",
          solidHeader = TRUE,
          title = "Current Global Filter Selection",
          textOutput("filter_display")
        )
      ),
      
      
      fluidRow(
        box(
          title = "Filtered Dataset (Global Filters Apply)",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          style = "padding:0;",
          
          div(
            style = "padding:10px;",
            DTOutput("data_table")
          )
        )
      )
      
      
      
    )
  
    
  )
)

)
