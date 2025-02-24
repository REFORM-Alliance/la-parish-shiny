rm(list = ls())

####Read in Libraries####
library(tidyverse)
library(janitor)
library(data.table)
library(here)
library(tidycensus)
library(gitlink)
library(shiny)


####Read in Data####
##Sentence Level 
parish_data_wide_sentence_level <- 
  "data" %>% 
  here("parish_data_wide_sentence_level.csv") %>% 
  fread(sep = ",", header = TRUE, stringsAsFactors = FALSE)

##Fully Wide
parish_data_wide_full <- 
  "data" %>% 
  here("parish_data_wide_full.csv") %>% 
  fread(sep = ",", header = TRUE, stringsAsFactors = FALSE)

##Tidycesnsus LA Parish Data
census_data_la <- 
  "data" %>% 
  here("census_data_la.csv") %>% 
  fread(sep = ",", header = TRUE, stringsAsFactors = FALSE)


####Clean Up Data####
##Get Columns for Charge Description and Incarcerated Time
parish_data_clean <- 
  parish_data_wide_sentence_level %>% 
  mutate(parish = 
           parish %>% 
           str_to_title() %>% 
           paste0(" Parish"),
         charge_clean_full = 
           charge_clean_full %>% 
           str_replace_all("Attempt; ", "Attempted "), 
         parish = 
           parish %>% 
           str_replace_all("The", "the") %>% 
           str_replace_all("St ", "St. ") %>% 
           str_replace_all("Desoto", "De Soto") %>% 
           str_replace_all("Vermillion", "Vermilion")) %>% 
  left_join(census_data_la %>% 
              rename("pop_estimate" = "estimate") %>% 
              dplyr::select(geoid, parish, pop_estimate), 
            by = c("parish")) %>% 
  mutate(pop_estimate_cat = 
           case_when(pop_estimate <= 10000 ~ "< 10k", 
                     pop_estimate > 10000 & pop_estimate <= 20000 ~ "10k - 20k", 
                     pop_estimate > 20000 & pop_estimate <= 50000 ~ "20k - 50k", 
                     pop_estimate > 50000 & pop_estimate <= 100000 ~ "50k - 100k", 
                     pop_estimate > 100000 ~ "> 100k", 
                     TRUE ~ NA) %>% 
           factor(levels = c("< 10k", "10k - 20k", "20k - 50k",
                             "50k - 100k", "> 100k"))) %>% 
  mutate(time_inc_days = as.numeric(as.Date(today()) - as.Date(conviction_date)), 
         time_inc_years = round(time_inc_days/365.25)) 

##Define Full List of Charges
full_charges <- 
  parish_data_clean %>% 
  group_by(charge_clean_full) %>% 
  dplyr::summarize(count = n()) %>% 
  ungroup() %>% 
  arrange(charge_clean_full) %>% 
  pull(charge_clean_full)

full_charges_final <- c("Burglary (inclusive)", "Rape (inlcusive)", "Robbery (inclusive)", "Second Degree Murder (inclusive)", full_charges)
  

####Define App####
##User Interface
ui <- fluidPage(
  titlePanel("Louisiana Charge Data Visualizer"),
  
  sidebarLayout(
    sidebarPanel(
      
      # Age Slider Input (2-way)
      sliderInput("age_slider", "Select Age Range:", 
                  min = min(parish_data_clean$age, na.rm = TRUE), 
                  max = max(parish_data_clean$age, na.rm = TRUE), 
                  value = c(min(parish_data_clean$age, na.rm = TRUE), max(parish_data_clean$age, na.rm = TRUE))),
      
      # Time Incarcerated Slider Input (2-way)
      sliderInput("time_slider", "Select Time Incarcerated Range (in Years):", 
                  min = min(parish_data_clean$time_inc_years, na.rm = TRUE),
                  max = max(parish_data_clean$time_inc_years, na.rm = TRUE), 
                  value = c(min(parish_data_clean$time_inc_years, na.rm = TRUE), max(parish_data_clean$time_inc_years, na.rm = TRUE))), 
      
      # Checkbox Input for Charge 
      selectizeInput("charge_select", "Select Charge Type(s):", 
                     choices = c("All" = "All", full_charges_final),
                     selected = full_charges_final, 
                     multiple = TRUE, 
                     options = list(placeholder = "Select Charge Type(s)", maxItems = NULL, create = FALSE)),
      
      # Checkbox Input for Parishes
      selectizeInput("parish_select", "Select Parish(es):", 
                     choices = c("All" = "All", unique(parish_data_clean$parish)), 
                     selected = unique(parish_data_clean$parish), 
                     multiple = TRUE, 
                     options = list(placeholder = "Select Parish(es)", maxItems = NULL, create = FALSE))
      
    ), 
    
    mainPanel(
      plotOutput("bar_plot")
    )
  )
)

##Server
server <- function(input, output){
  
  # Reactive expression to filter crime data based on user inputs
  filtered_data <- reactive({
    parish_data_clean %>% 
      mutate(burglary_flag = ifelse(str_detect(charge_clean_full, "Burglary") == TRUE, 1, 0),
             rape_flag = ifelse(str_detect(charge_clean_full, "Rape") == TRUE, 1, 0),
             robbery_flag = ifelse(str_detect(charge_clean_full, "Robbery") == TRUE, 1, 0),
             second_degree_murder_flag = ifelse(str_detect(charge_clean_full, "Second Degree Murder") == TRUE, 1, 0),
             select_these_charges = case_when("All" %in% input$charge_select ~ 1, 
                                              "Burglary (inclusive)" %in% input$charge_select & burglary_flag == 1 ~ 1,
                                              "Rape (inclusive)" %in% input$charge_select & rape_flag == 1 ~ 1, 
                                              "Robbery (inclusive)" %in% input$charge_select & robbery_flag == 1 ~ 1, 
                                              "Second Degree Murder (inclusive)" %in% input$charge_select & second_degree_murder_flag == 1 ~ 1, 
                                              charge_clean_full %in% input$charge_select ~ 1, 
                                              TRUE ~ 0),
             select_these_parishes = case_when("All" %in% input$parish_select ~ 1, 
                                               parish %in% input$parish_select ~ 1, 
                                               TRUE ~ 0)) %>% 
      filter(age >= input$age_slider[1] & age <= input$age_slider[2] &
             time_incarcerated >= input$time_slider[1] & time_incarcerated <= input$time_slider[2] & 
             select_these_charges == 1 & 
             select_these_parishes == 1) %>% 
      dplyr::select(id) %>% 
      distinct()
  })
  
  # Reactive expression for the number of excluded data points
  excluded_data <- reactive({
    parish_data_clean %>% 
      mutate(burglary_flag = ifelse(str_detect(charge_clean_full, "Burglary") == TRUE, 1, 0),
             rape_flag = ifelse(str_detect(charge_clean_full, "Rape") == TRUE, 1, 0),
             robbery_flag = ifelse(str_detect(charge_clean_full, "Robbery") == TRUE, 1, 0),
             second_degree_murder_flag = ifelse(str_detect(charge_clean_full, "Second Degree Murder") == TRUE, 1, 0),
             select_these_charges = case_when("All" %in% input$charge_select ~ 1, 
                                              "Burglary (inclusive)" %in% input$charge_select & burglary_flag == 1 ~ 1,
                                              "Rape (inclusive)" %in% input$charge_select & rape_flag == 1 ~ 1, 
                                              "Robbery (inclusive)" %in% input$charge_select & robbery_flag == 1 ~ 1, 
                                              "Second Degree Murder (inclusive)" %in% input$charge_select & second_degree_murder_flag == 1 ~ 1, 
                                              charge_clean_full %in% input$charge_select ~ 1, 
                                              TRUE ~ 0),
             select_these_parishes = case_when("All" %in% input$parish_select ~ 1, 
                                               parish %in% input$parish_select ~ 1, 
                                               TRUE ~ 0)) %>% 
      filter(!(age >= input$age_slider[1] & age <= input$age_slider[2] &
               time_incarcerated >= input$time_slider[1] & time_incarcerated <= input$time_slider[2] & 
               select_these_charges == 1 & 
               select_these_parishes == 1)) %>% 
      dplyr::select(id) %>% 
      distinct()
  })
  
  # Render bar plot to show included and excluded
  output$bar_plot <- renderPlot({
    included_count <- 
      filtered_data() %>% 
      nrow()
    
    excluded_count <- 
      excluded_data() %>% 
      nrow()
    
    # Create Plot
    plot_data <- 
      data.frame(status = c("Included", "Excluded"),
                 count = c(included_count, excluded_count))
    
    plot_data %>% 
      ggplot(aes(y = status, x = count, fill = status)) + 
      geom_bar(stat = "identity", show.legend = FALSE) + 
      labs(caption = "Number of Individuals Included in New Filter", 
           x = "Count", 
           y = "Inclusion/Exclusion Status") + 
      theme_minimal()
    
  })
}

shinyApp(ui = ui, server = server)
