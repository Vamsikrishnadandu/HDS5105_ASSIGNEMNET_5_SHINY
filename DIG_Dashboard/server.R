library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(scales)

# Loading the dig data
dig <- read.csv("DIG-1.csv") %>%
  mutate(
    TRTMT = factor(TRTMT, levels = c(0, 1),
                   labels = c("Placebo", "Digoxin")),
    SEX   = factor(SEX,   levels = c(1, 2),
                   labels = c("Male", "Female")),
    DEATH = factor(DEATH, levels = c(0, 1),
                   labels = c("Alive", "Dead")),
    HOSP  = factor(HOSP,  levels = c(0, 1),
                   labels = c("No", "Yes")),
    DWHF  = factor(DWHF,  levels = c(0, 1),
                   labels = c("No", "Yes"))
  )

server <- function(input, output, session) {
  
  # demograohic reactive data:
  demo_data <- reactive({
    df <- dig
    
    if (!is.null(input$age_range)) {
      df <- df %>%
        filter(AGE >= input$age_range[1],
               AGE <= input$age_range[2])
    }
    
    if (!is.null(input$treatment) && input$treatment != "All") {
      df <- df %>% filter(TRTMT == input$treatment)
    }
    
    if (!is.null(input$sex) && input$sex != "All") {
      df <- df %>% filter(SEX == input$sex)
    }
    
    df
  })
  
  # demographic value boxes:
  output$n_patients <- renderValueBox({
    df <- demo_data()
    valueBox(
      nrow(df),
      "Total Patients",
      icon = icon("users"),
      color = "aqua"
    )
  })
  
  output$mean_age <- renderValueBox({
    df <- demo_data()
    valueBox(
      round(mean(df$AGE, na.rm = TRUE), 1),
      "Mean Age (years)",
      icon = icon("birthday-cake"),
      color = "green"
    )
  })
  
  output$mean_ef <- renderValueBox({
    df <- demo_data()
    valueBox(
      round(mean(df$EJF_PER, na.rm = TRUE), 1),
      "Mean EF (%)",
      icon = icon("heart"),
      color = "yellow"
    )
  })
  
  output$mean_bmi <- renderValueBox({
    df <- demo_data()
    valueBox(
      round(mean(df$BMI, na.rm = TRUE), 1),
      "Mean BMI",
      icon = icon("weight"),
      color = "red"
    )
  })
  
  # demographic plots:
  output$age_plot <- renderPlot({
    df <- demo_data()
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = AGE, fill = TRTMT)) +
      geom_histogram(bins = 25, alpha = 0.7, position = "identity") +
      labs(
        title = "Age Distribution by Treatment",
        x = "Age (years)",
        y = "Count",
        fill = "Treatment"
      ) +
      theme_minimal() +
      theme(legend.position = "top")
  })
  
  output$ef_plot <- renderPlot({
    df <- demo_data()
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = TRTMT, y = EJF_PER, fill = TRTMT)) +
      geom_boxplot(alpha = 0.7) +
      labs(
        title = "Ejection Fraction by Treatment",
        x = "Treatment",
        y = "Ejection Fraction (%)"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$sex_bar <- renderPlot({
    df <- demo_data()
    req(nrow(df) > 0)
    
    sex_data <- df %>%
      count(SEX) %>%
      mutate(pct = n / sum(n) * 100)
    
    ggplot(sex_data, aes(x = SEX, y = pct, fill = SEX)) +
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
    df <- demo_data()
    req(nrow(df) > 0)
    
    race_data <- df %>%
      count(RACE) %>%
      mutate(pct = n / sum(n) * 100)
    
    ggplot(race_data, aes(x = RACE, y = pct, fill = RACE)) +
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
    df <- demo_data()
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = BMI, fill = TRTMT)) +
      geom_histogram(bins = 20, alpha = 0.7, position = "identity") +
      labs(
        title = "BMI Distribution by Treatment",
        x = "BMI (kg/m²)",
        y = "Count",
        fill = "Treatment"
      ) +
      theme_minimal() +
      theme(legend.position = "top")
  })
  
  output$bp_plot <- renderPlot({
    df <- demo_data()
    req(nrow(df) > 0)
    
    bp_data <- df %>%
      select(TRTMT, SYSBP, DIABP) %>%
      rename(Systolic = SYSBP, Diastolic = DIABP) %>%
      pivot_longer(
        cols = c(Systolic, Diastolic),
        names_to = "BP_Type",
        values_to = "BP_Value"
      )
    
    ggplot(bp_data, aes(x = TRTMT, y = BP_Value, fill = BP_Type)) +
      geom_boxplot(alpha = 0.7, position = position_dodge(width = 0.7)) +
      labs(
        title = "Blood Pressure by Treatment",
        x = "Treatment",
        y = "Blood Pressure (mmHg)",
        fill = "BP Type"
      ) +
      theme_minimal() +
      theme(legend.position = "top")
  })
  
  #demographic tables:
  output$demo_table <- renderDT({
    df <- demo_data()
    req(nrow(df) > 0)
    
    df %>%
      select(ID, TRTMT, AGE, SEX, RACE, BMI, EJF_PER, SYSBP, DIABP, HEARTRTE) %>%
      rename(
        "Patient ID" = ID,
        "Treatment" = TRTMT,
        "Age" = AGE,
        "Sex" = SEX,
        "Race" = RACE,
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
  
  # outcome reactive data:
  outcomes_data <- reactive({
    df <- dig
    
    if (!is.null(input$age_range)) {
      df <- df %>%
        filter(AGE >= input$age_range[1],
               AGE <= input$age_range[2])
    }
    
    if (!is.null(input$treatment) && input$treatment != "All") {
      df <- df %>% filter(TRTMT == input$treatment)
    }
    
    if (!is.null(input$sex) && input$sex != "All") {
      df <- df %>% filter(SEX == input$sex)
    }
    
    df
  })
  
  # outcome plots
  output$death_plot <- renderPlot({
    df <- outcomes_data()
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = TRTMT, fill = DEATH)) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      labs(
        x = "Treatment",
        y = "Proportion",
        fill = "Vital Status",
        title = "Mortality by Treatment"
      ) +
      theme_minimal()
  })
  
  output$hosp_plot <- renderPlot({
    df <- outcomes_data()
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = TRTMT, fill = HOSP)) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      labs(
        x = "Treatment",
        y = "Proportion",
        fill = "Any Hospitalisation",
        title = "Hospitalisation by Treatment"
      ) +
      theme_minimal()
  })
  
  output$dwhf_plot <- renderPlot({
    df <- outcomes_data()
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = TRTMT, fill = DWHF)) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      labs(
        x = "Treatment",
        y = "Proportion",
        fill = "Death or WHF",
        title = "Primary Endpoint (Death / WHF)"
      ) +
      theme_minimal()
  })
  
  output$time_plot <- renderPlot({
    df <- outcomes_data()
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = DEATHDAY, fill = TRTMT)) +
      geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
      labs(
        x = "Days to death or last follow-up",
        y = "Count",
        fill = "Treatment",
        title = "Time to Death / Censoring"
      ) +
      theme_minimal()
  })
}
