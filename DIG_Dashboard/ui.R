library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "DIG Trial Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Outcomes",
               tabName = "outcomes",
               icon = icon("heartbeat"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # OUTCOMES TAB 
      tabItem(
        tabName = "outcomes",
        h2("Clinical Outcomes"),
        
        fluidRow(
          box(width = 4, status = "warning", title = "Filters",
              selectInput("out_trt", "Treatment:",
                          choices = c("All", "Placebo", "Digoxin"),
                          selected = "All"),
              selectInput("out_sex", "Sex:",
                          choices = c("All", "Male", "Female"),
                          selected = "All"),
              sliderInput("out_age", "Age Range:",
                          min = 20, max = 100,
                          value = c(40, 80))
          ),
          
          box(width = 8, status = "primary", title = "Mortality by Treatment",
              plotOutput("death_plot"))
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
        )
      )
      
    )
  )
)
