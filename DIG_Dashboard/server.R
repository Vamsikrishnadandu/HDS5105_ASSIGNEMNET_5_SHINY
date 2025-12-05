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
      scale_fill_manual(values = c("Placebo" = "#3498db", "Digoxin" = "#e74c3c")) +
      labs(
        x = "Age (years)",
        y = "Count",
        fill = "Treatment"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "top"
      )
  })
  
  output$ef_plot <- renderPlot({
    df <- demo_data()
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = TRTMT, y = EJF_PER, fill = TRTMT)) +
      geom_boxplot(alpha = 0.7) +
      scale_fill_manual(values = c("Placebo" = "#3498db", "Digoxin" = "#e74c3c")) +
      labs(
        x = "Treatment",
        y = "Ejection Fraction (%)",
        fill = "Treatment"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "none"
      )
  })
  
  output$sex_bar <- renderPlot({
    df <- demo_data()
    req(nrow(df) > 0)
    
    sex_data <- df %>%
      filter(!is.na(SEX)) %>%
      count(SEX) %>%
      mutate(pct = n / sum(n) * 100)
    
    ggplot(sex_data, aes(x = SEX, y = pct, fill = SEX)) +
      geom_col(width = 0.6) +
      geom_text(aes(label = paste0(round(pct, 1), "%")), vjust = -0.3) +
      scale_fill_manual(values = c("Male" = "#3498db", "Female" = "#e74c3c")) +
      scale_y_continuous(limits = c(0, max(sex_data$pct) * 1.15)) +
      labs(
        x = "Sex",
        y = "Percentage (%)",
        fill = "Sex"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "none"
      )
  })
  
  output$race_bar <- renderPlot({
    df <- demo_data()
    req(nrow(df) > 0)
    
    race_data <- df %>%
      filter(!is.na(RACE)) %>%
      count(RACE) %>%
      mutate(pct = n / sum(n) * 100) %>%
      mutate(RACE_label = ifelse(RACE == 1, "White", "Non-White"))
    
    ggplot(race_data, aes(x = RACE_label, y = pct, fill = RACE_label)) +
      geom_col(width = 0.6) +
      geom_text(aes(label = paste0(round(pct, 1), "%")), vjust = -0.3) +
      scale_fill_manual(values = c("White" = "#3498db", "Non-White" = "#e74c3c")) +
      scale_y_continuous(limits = c(0, max(race_data$pct) * 1.15)) +
      labs(
        x = "Race",
        y = "Percentage (%)",
        fill = "Race"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "none"
      )
  })
  
  output$bmi_hist <- renderPlot({
    df <- demo_data()
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = BMI, fill = TRTMT)) +
      geom_histogram(bins = 20, alpha = 0.7, position = "identity") +
      scale_fill_manual(values = c("Placebo" = "#3498db", "Digoxin" = "#e74c3c")) +
      labs(
        x = "BMI (kg/m²)",
        y = "Count",
        fill = "Treatment"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "top"
      )
  })
  
  output$bp_plot <- renderPlot({
    df <- demo_data()
    req(nrow(df) > 0)
    
    bp_data <- df %>%
      filter(!is.na(SYSBP) & !is.na(DIABP)) %>%
      select(TRTMT, SYSBP, DIABP) %>%
      rename(Systolic = SYSBP, Diastolic = DIABP) %>%
      pivot_longer(
        cols = c(Systolic, Diastolic),
        names_to = "BP_Type",
        values_to = "BP_Value"
      )
    
    ggplot(bp_data, aes(x = TRTMT, y = BP_Value, fill = BP_Type)) +
      geom_boxplot(alpha = 0.7, position = position_dodge(width = 0.7)) +
      scale_fill_manual(values = c("Systolic" = "#3498db", "Diastolic" = "#e74c3c")) +
      labs(
        x = "Treatment",
        y = "Blood Pressure (mmHg)",
        fill = "BP Type"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "top"
      )
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
    
    death_data <- df %>%
      filter(!is.na(DEATH)) %>%
      group_by(TRTMT, DEATH) %>%
      summarise(Count = n(), .groups = "drop") %>%
      group_by(TRTMT) %>%
      mutate(Percentage = round(Count / sum(Count) * 100, 1)) %>%
      ungroup()
    
    ggplot(death_data, aes(x = TRTMT, y = Percentage, fill = DEATH)) +
      geom_col(position = "dodge", width = 0.6) +
      geom_text(aes(label = paste0(Percentage, "%")), vjust = -0.3, position = position_dodge(width = 0.6)) +
      scale_fill_manual(values = c("Alive" = "#3498db", "Dead" = "#e74c3c")) +
      scale_y_continuous(limits = c(0, 110)) +
      labs(
        x = "Treatment",
        y = "Percentage (%)",
        fill = "Vital Status"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "top"
      )
  })
  # MORTALITY RATE SUMMARY PLOT
  output$mortality_summary_plot <- renderPlot({
    df <- outcomes_data()
    req(nrow(df) > 0)
    
    mortality_data <- df %>%
      filter(!is.na(DEATH)) %>%
      group_by(TRTMT, DEATH) %>%
      summarise(Count = n(), .groups = "drop") %>%
      group_by(TRTMT) %>%
      mutate(
        Total = sum(Count),
        Percentage = round(Count / Total * 100, 1)
      ) %>%
      filter(DEATH == "Dead")
    
    ggplot(mortality_data, aes(x = TRTMT, y = Percentage, fill = TRTMT)) +
      geom_col(width = 0.5) +
      geom_text(aes(label = paste0(Percentage, "%\n(n=", Count, ")")), 
                vjust = -0.3, size = 4, fontface = "bold") +
      scale_fill_manual(values = c("Placebo" = "#3498db", "Digoxin" = "#e74c3c")) +
      scale_y_continuous(limits = c(0, max(mortality_data$Percentage) * 1.4)) +
      labs(
        x = "Treatment",
        y = "Mortality Rate (%)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.title.x = element_blank(),
        legend.position = "none"
      )
  })
  
  
  
  output$hosp_plot <- renderPlot({
    df <- outcomes_data()
    req(nrow(df) > 0)
    
    hosp_data <- df %>%
      filter(!is.na(HOSP)) %>%
      group_by(TRTMT, HOSP) %>%
      summarise(Count = n(), .groups = "drop") %>%
      group_by(TRTMT) %>%
      mutate(Percentage = round(Count / sum(Count) * 100, 1)) %>%
      ungroup()
    
    ggplot(hosp_data, aes(x = TRTMT, y = Percentage, fill = HOSP)) +
      geom_col(position = "dodge", width = 0.6) +
      geom_text(aes(label = paste0(Percentage, "%")), vjust = -0.3, position = position_dodge(width = 0.6)) +
      scale_fill_manual(values = c("No" = "#3498db", "Yes" = "#e74c3c")) +
      scale_y_continuous(limits = c(0, 110)) +
      labs(
        x = "Treatment",
        y = "Percentage (%)",
        fill = "Any Hospitalisation"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "top"
      )
  })
  
  output$dwhf_plot <- renderPlot({
    df <- outcomes_data()
    req(nrow(df) > 0)
    
    dwhf_data <- df %>%
      filter(!is.na(DWHF)) %>%
      group_by(TRTMT, DWHF) %>%
      summarise(Count = n(), .groups = "drop") %>%
      group_by(TRTMT) %>%
      mutate(Percentage = round(Count / sum(Count) * 100, 1)) %>%
      ungroup()
    
    ggplot(dwhf_data, aes(x = TRTMT, y = Percentage, fill = DWHF)) +
      geom_col(position = "dodge", width = 0.6) +
      geom_text(aes(label = paste0(Percentage, "%")), vjust = -0.3, position = position_dodge(width = 0.6)) +
      scale_fill_manual(values = c("No" = "#3498db", "Yes" = "#e74c3c")) +
      scale_y_continuous(limits = c(0, 110)) +
      labs(
        x = "Treatment",
        y = "Percentage (%)",
        fill = "Death or WHF"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "top"
      )
  })
  
  output$time_plot <- renderPlot({
    df <- outcomes_data()
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = DEATHDAY, fill = TRTMT)) +
      geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
      scale_fill_manual(values = c("Placebo" = "#3498db", "Digoxin" = "#e74c3c")) +
      labs(
        x = "Days to death or last follow-up",
        y = "Count",
        fill = "Treatment"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "top"
      )
  })
  
  
  output$outcomes_summary <- renderDT({
    df <- outcomes_data()
    req(nrow(df) > 0)
    
    summary_tbl <- df %>%
      group_by(TRTMT) %>%
      summarise(
        "Total Patients" = n(),
        "Deaths (n)" = sum(DEATH == "Dead", na.rm = TRUE),
        "Death Rate %" = round(sum(DEATH == "Dead", na.rm = TRUE) / n() * 100, 1),
        "Hospitalizations (n)" = sum(HOSP == "Yes", na.rm = TRUE),
        "Hosp Rate %" = round(sum(HOSP == "Yes", na.rm = TRUE) / n() * 100, 1),
        "DWHF (n)" = sum(DWHF == "Yes", na.rm = TRUE),
        "DWHF Rate %" = round(sum(DWHF == "Yes", na.rm = TRUE) / n() * 100, 1),
        .groups = "drop"
      )
    
    datatable(
      summary_tbl,
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  # DATA EXPLORER 
  output$filter_display <- renderText({
    paste0(
      "Age: ", input$age_range[1], "–", input$age_range[2],
      " | Treatment: ", input$treatment,
      " | Sex: ", input$sex
    )
  })
  
  output$data_table <- renderDT({
    df <- demo_data()
    req(nrow(df) > 0)
    
    df_clean <- df %>% 
      select(
        ID, TRTMT, AGE, SEX, RACE,
        BMI, EJF_PER, SYSBP, DIABP,
        HEARTRTE, DEATH, HOSP
      )
    
    datatable(
      df_clean,
      rownames = FALSE,
      escape = FALSE,
      class = "stripe hover compact nowrap",
      extensions = c("Buttons", "FixedHeader"),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = FALSE,
        fixedHeader = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf'),
        
        
        columnDefs = list(
          list(width = "60px",  targets = 0),   # ID
          list(width = "90px",  targets = 1),   # TRTMT
          list(width = "70px",  targets = 2),   # AGE
          list(width = "80px",  targets = 3),   # SEX
          list(width = "70px",  targets = 4),   # RACE
          list(width = "90px",  targets = 5),   # BMI
          list(width = "90px",  targets = 6),   # EF
          list(width = "90px",  targets = 7),   # SYSBP
          list(width = "90px",  targets = 8),   # DIABP
          list(width = "100px", targets = 9),   # HR
          list(width = "90px",  targets = 10),  # DEATH
          list(width = "80px",  targets = 11)   # HOSP
        )
      )
    ) %>%
      formatStyle(
        "DEATH",
        backgroundColor = styleEqual("Dead", "#f8d7da"),
        fontWeight = "bold"
      ) %>%
      formatStyle(
        "HOSP",
        backgroundColor = styleEqual("Yes", "#fff3cd"),
        fontWeight = "bold"
      )
  })

# medications reactive
meds_data <- reactive({
  df <- demo_data()   # reuse demographic filtered data
  req(nrow(df) > 0)
  df
})

# medication prevalence by treatment
output$med_prevalence_plot <- renderPlot({
  df <- meds_data()
  req(nrow(df) > 0)
  
  med_summary <- df %>%
    group_by(TRTMT) %>%
    summarise(
      `ACE Inhibitors`   = round(sum(ACEINHIB == 1, na.rm = TRUE) / n() * 100, 1),
      `Diuretics`        = round(sum(DIURET   == 1, na.rm = TRUE) / n() * 100, 1),
      `Nitrates`         = round(sum(NITRATES == 1, na.rm = TRUE) / n() * 100, 1),
      `Recent Digoxin`   = round(sum(DIGUSE   == 1, na.rm = TRUE) / n() * 100, 1),
      .groups = "drop"
    ) %>%
    pivot_longer(
      cols = -TRTMT,
      names_to = "Medication",
      values_to = "Percentage"
    )
  
  ggplot(med_summary, aes(x = Medication, y = Percentage, fill = TRTMT)) +
    geom_col(position = "dodge", width = 0.6) +
    geom_text(aes(label = paste0(Percentage, "%")),
              vjust = -0.3, position = position_dodge(width = 0.6), size = 3.5) +
    scale_fill_manual(values = c("Placebo" = "#3498db", "Digoxin" = "#e74c3c")) +
    scale_y_continuous(limits = c(0, 110)) +
    labs(
      x = "Medication",
      y = "Prevalence (%)",
      fill = "Treatment"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top"
    )
})

# ace inhibhitors vs mortality
output$ace_outcomes_plot <- renderPlot({
  df <- outcomes_data()
  req(nrow(df) > 0)
  
  ace_outcomes <- df %>%
    filter(!is.na(ACEINHIB), !is.na(DEATH)) %>%
    mutate(ACE = ifelse(ACEINHIB == 1, "On ACE-I", "No ACE-I")) %>%
    group_by(ACE, DEATH) %>%
    summarise(Count = n(), .groups = "drop") %>%
    group_by(ACE) %>%
    mutate(
      Total = sum(Count),
      Percentage = round(Count / Total * 100, 1)
    ) %>%
    filter(DEATH == "Dead")
  
  ggplot(ace_outcomes, aes(x = ACE, y = Percentage, fill = ACE)) +
    geom_col(width = 0.5) +
    geom_text(aes(label = paste0(Percentage, "%\n(n=", Count, ")")),
              vjust = -0.3, size = 4, fontface = "bold") +
    scale_fill_manual(values = c("On ACE-I" = "#3498db", "No ACE-I" = "#e74c3c")) +
    scale_y_continuous(limits = c(0, max(ace_outcomes$Percentage) * 1.4)) +
    labs(
      x = "ACE Inhibitor Use",
      y = "Death Rate (%)"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none"
    )
})

# diuretics vs mortality
output$diuretics_outcomes_plot <- renderPlot({
  df <- outcomes_data()
  req(nrow(df) > 0)
  
  diur_outcomes <- df %>%
    filter(!is.na(DIURET), !is.na(DEATH)) %>%
    mutate(Diuretic = ifelse(DIURET == 1, "On Diuretics", "No Diuretics")) %>%
    group_by(Diuretic, DEATH) %>%
    summarise(Count = n(), .groups = "drop") %>%
    group_by(Diuretic) %>%
    mutate(
      Total = sum(Count),
      Percentage = round(Count / Total * 100, 1)
    ) %>%
    filter(DEATH == "Dead")
  
  ggplot(diur_outcomes, aes(x = Diuretic, y = Percentage, fill = Diuretic)) +
    geom_col(width = 0.5) +
    geom_text(aes(label = paste0(Percentage, "%\n(n=", Count, ")")),
              vjust = -0.3, size = 4, fontface = "bold") +
    scale_fill_manual(values = c("On Diuretics" = "#3498db", "No Diuretics" = "#e74c3c")) +
    scale_y_continuous(limits = c(0, max(diur_outcomes$Percentage) * 1.4)) +
    labs(
      x = "Diuretic Use",
      y = "Death Rate (%)"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none"
    )
})

# medication summary table
output$med_table <- renderDT({
  df <- meds_data()
  req(nrow(df) > 0)
  
  med_tbl <- df %>%
    group_by(TRTMT) %>%
    summarise(
      `Total Patients`         = n(),
      `ACE Inhibitors (n)`     = sum(ACEINHIB == 1, na.rm = TRUE),
      `ACE Inhibitors (%)`     = round(`ACE Inhibitors (n)` / n() * 100, 1),
      `Diuretics (n)`          = sum(DIURET == 1, na.rm = TRUE),
      `Diuretics (%)`          = round(`Diuretics (n)` / n() * 100, 1),
      `Nitrates (n)`           = sum(NITRATES == 1, na.rm = TRUE),
      `Nitrates (%)`           = round(`Nitrates (n)` / n() * 100, 1),
      `Recent Digoxin (n)`     = sum(DIGUSE == 1, na.rm = TRUE),
      `Recent Digoxin (%)`     = round(`Recent Digoxin (n)` / n() * 100, 1),
      .groups = "drop"
    )
  
  datatable(
    med_tbl,
    options = list(pageLength = 10, scrollX = TRUE),
    rownames = FALSE
  )
})

}
