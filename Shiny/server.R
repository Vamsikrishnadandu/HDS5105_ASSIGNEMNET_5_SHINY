library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)

# Load and prepare data
dig_data <- read.csv("DIG-1.csv")

dig_data <- dig_data %>%
  mutate(
    TRTMT_label = factor(TRTMT, levels = c(0, 1), labels = c("Placebo", "Digoxin")),
    SEX_label = factor(SEX, levels = c(1, 2), labels = c("Male", "Female")),
    RACE_label = factor(RACE, levels = c(1, 2), labels = c("White", "Non-White"))
  )

server <- function(input, output, session) {
  
  # Reactive filtered dataset
  filtered_data <- reactive({
    data <- dig_data %>%
      filter(AGE >= input$age_range[1] & AGE <= input$age_range[2])
    
    if (input$treatment != "All") {
      data <- data %>% filter(TRTMT_label == input$treatment)
    }
    
    if (input$sex != "All") {
      data <- data %>% filter(SEX_label == input$sex)
    }
    
    data
  })
  
  # ============ VALUE BOXES ============
  
  output$n_patients <- renderValueBox({
    valueBox(
      nrow(filtered_data()),
      "Total Patients",
      icon = icon("users"),
      color = "aqua"
    )
  })
  
  output$mean_age <- renderValueBox({
    valueBox(
      round(mean(filtered_data()$AGE, na.rm = TRUE), 1),
      "Mean Age (years)",
      icon = icon("birthday-cake"),
      color = "green"
    )
  })
  
  output$mean_ef <- renderValueBox({
    valueBox(
      round(mean(filtered_data()$EJF_PER, na.rm = TRUE), 1),
      "Mean EF (%)",
      icon = icon("heart"),
      color = "yellow"
    )
  })
  
  output$mean_bmi <- renderValueBox({
    valueBox(
      round(mean(filtered_data()$BMI, na.rm = TRUE), 1),
      "Mean BMI",
      icon = icon("weight"),
      color = "red"
    )
  })
  
  # ============ PLOTS ============
  
  output$age_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = AGE, fill = TRTMT_label)) +
      geom_histogram(bins = 25, alpha = 0.7, position = "identity") +
      labs(
        title = "Age Distribution",
        x = "Age (years)",
        y = "Count",
        fill = "Treatment"
      ) +
      theme_minimal() +
      theme(legend.position = "top")
  })
  
  output$ef_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = TRTMT_label, y = EJF_PER, fill = TRTMT_label)) +
      geom_boxplot(alpha = 0.7) +
      labs(
        title = "Ejection Fraction by Treatment",
        x = "Treatment",
        y = "Ejection Fraction (%)",
        fill = "Treatment"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$sex_bar <- renderPlot({
    sex_data <- filtered_data() %>%
      count(SEX_label) %>%
      mutate(pct = n / sum(n) * 100)
    
    ggplot(sex_data, aes(x = SEX_label, y = pct, fill = SEX_label)) +
      geom_col() +
      geom_text(aes(label = paste0(round(pct, 1), "%")), vjust = -0.5) +
      labs(
        title = "Sex Distribution",
        x = "Sex",
        y = "Percentage (%)"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$race_bar <- renderPlot({
    race_data <- filtered_data() %>%
      count(RACE_label) %>%
      mutate(pct = n / sum(n) * 100)
    
    ggplot(race_data, aes(x = RACE_label, y = pct, fill = RACE_label)) +
      geom_col() +
      geom_text(aes(label = paste0(round(pct, 1), "%")), vjust = -0.5) +
      labs(
        title = "Race Distribution",
        x = "Race",
        y = "Percentage (%)"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$bmi_hist <- renderPlot({
    ggplot(filtered_data(), aes(x = BMI, fill = TRTMT_label)) +
      geom_histogram(bins = 20, alpha = 0.7, position = "identity") +
      labs(
        title = "BMI Distribution",
        x = "BMI (kg/m²)",
        y = "Count",
        fill = "Treatment"
      ) +
      theme_minimal() +
      theme(legend.position = "top")
  })
  
  output$bp_plot <- renderPlot({
    # Create a dataframe with both SBP and DBP for plotting
    bp_data <- filtered_data() %>%
      select(TRTMT_label, SYSBP, DIABP) %>%
      rename(Systolic = SYSBP, Diastolic = DIABP) %>%
      pivot_longer(
        cols = c(Systolic, Diastolic),
        names_to = "BP_Type",
        values_to = "BP_Value"
      )
    
    ggplot(bp_data, aes(x = TRTMT_label, y = BP_Value, fill = BP_Type)) +
      geom_boxplot(alpha = 0.7, position = "dodge") +
      labs(
        title = "Blood Pressure by Treatment",
        x = "Treatment",
        y = "Blood Pressure (mmHg)",
        fill = "BP Type"
      ) +
      theme_minimal() +
      theme(legend.position = "top")
  })
  
  # ============ TABLES ============
  
  output$demo_table <- renderDT({
    filtered_data() %>%
      select(ID, TRTMT_label, AGE, SEX_label, RACE_label, BMI, EJF_PER, SYSBP, DIABP, HEARTRTE) %>%
      rename(
        "Patient ID" = ID,
        "Treatment" = TRTMT_label,
        "Age" = AGE,
        "Sex" = SEX_label,
        "Race" = RACE_label,
        "BMI" = BMI,
        "EF %" = EJF_PER,
        "SBP" = SYSBP,
        "DBP" = DIABP,
        "HR" = HEARTRTE
      ) %>%
      datatable(
        options = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE
      )
  })
}

