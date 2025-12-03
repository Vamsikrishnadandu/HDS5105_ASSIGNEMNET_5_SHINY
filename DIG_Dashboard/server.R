library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(scales)

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
  
  outcomes_data <- reactive({
    
    df <- dig
    
    if (input$out_trt != "All") {
      df <- df %>% filter(TRTMT == input$out_trt)
    }
    
    if (input$out_sex != "All") {
      df <- df %>% filter(SEX == input$out_sex)
    }
    
    df <- df %>% filter(AGE >= input$out_age[1],
                        AGE <= input$out_age[2])
    
    df
  })
  

  output$death_plot <- renderPlot({
    
    df <- outcomes_data()
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = TRTMT, fill = factor(DEATH))) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "Treatment",
           y = "Proportion",
           fill = "Vital Status",
           title = "Mortality by Treatment") +
      theme_minimal()
  })
  
  # HOSPITALISATION 
  output$hosp_plot <- renderPlot({
    
    df <- outcomes_data()
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = TRTMT, fill = factor(HOSP))) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "Treatment",
           y = "Proportion",
           fill = "Any Hospitalisation",
           title = "Hospitalisation by Treatment") +
      theme_minimal()
  })
  
  # PRIMARY ENDPOINT (DEATH / WHF)
  output$dwhf_plot <- renderPlot({
    
    df <- outcomes_data()
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = TRTMT, fill = factor(DWHF))) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "Treatment",
           y = "Proportion",
           fill = "Death or WHF",
           title = "Primary Endpoint (Death / WHF)") +
      theme_minimal()
  })
  
  # TIME TO EVENT 
  output$time_plot <- renderPlot({
    
    df <- outcomes_data()
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = DEATHDAY, fill = TRTMT)) +
      geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
      labs(x = "Days to death or last follow-up",
           y = "Count",
           fill = "Treatment",
           title = "Time to Death / Censoring") +
      theme_minimal()
  })
  
}
