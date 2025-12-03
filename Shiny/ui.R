library(shiny)
library(shinydashboard)
library(shinyWidgets)

ui <- dashboardPage(
  dashboardHeader(title = "DIG Trial Explorer - Demographics"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Demographics", tabName = "demo", icon = icon("users")),
      menuItem("Outcomes", tabName = "outcomes", icon = icon("heart")),
      menuItem("Medications", tabName = "meds", icon = icon("pills")),
      menuItem("Data Explorer", tabName = "explorer", icon = icon("table"))
    ),
    hr(),
    h4("Filters", style = "padding: 10px;"),
    sliderInput(
      "age_range",
      "Age Range:",
      min = 20,
      max = 95,
      value = c(50, 80)
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
      # Demographics Tab
      tabItem(
        tabName = "demo",
        fluidRow(
          valueBoxOutput("n_patients", width = 3),
          valueBoxOutput("mean_age", width = 3),
          valueBoxOutput("mean_ef", width = 3),
          valueBoxOutput("mean_bmi", width = 3)
        ),
        fluidRow(
          box(
            title = "Age Distribution by Treatment",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotOutput("age_plot")
          ),
          box(
            title = "Ejection Fraction by Treatment",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotOutput("ef_plot")
          )
        ),
        fluidRow(
          box(
            title = "Sex Distribution",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotOutput("sex_bar")
          ),
          box(
            title = "Race Distribution",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotOutput("race_bar")
          )
        ),
        fluidRow(
          box(
            title = "BMI Distribution",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotOutput("bmi_hist")
          ),
          box(
            title = "Blood Pressure by Treatment",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotOutput("bp_plot")
          )
        ),
        fluidRow(
          box(
            title = "Demographic Summary Table",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            DTOutput("demo_table")
          )
        )
      ),
      
      # Placeholder tabs for other sections
      tabItem(tabName = "outcomes", h3("Outcomes Tab - Coming Soon")),
      tabItem(tabName = "meds", h3("Medications Tab - Coming Soon")),
      tabItem(tabName = "explorer", h3("Data Explorer Tab - Coming Soon"))
    )
  )
)
