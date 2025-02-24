rm(list = ls())

####Read in Libraries####
library(tidyverse)
library(janitor)
library(data.table)
library(here)
library(tidycensus)


####Read in and Clean Data####
##Raw Data
parish_data_raw <- 
  "data-raw" %>% 
  here("PJI_Data_Parish.Age.Charge.Victim.csv") %>% 
  fread(sep = ",", header = TRUE, stringsAsFactors = FALSE) %>% 
  clean_names() 

##Clean Data
parish_data_clean <- 
  parish_data_raw %>% 
  mutate(across(where(is_character), 
                ~.x %>% 
                  na_if("")), 
         age = 
           age %>% 
           na_if("Invalid date") %>% 
           str_replace_all("years", "") %>% 
           str_trim(side = "both") %>% 
           as.numeric(),
         victim_crime = case_when(victim_crime == "No" ~ 0, 
                                  victim_crime == "Yes" ~ 1, 
                                  TRUE ~ NA), 
         charge = 
           charge %>% 
           str_trim(side = "both") %>% 
           str_replace_all("RS 14:27 RS 14:61.1 Attempt First Degree Robbery", "RS 14:27 Attempt, RS 14:61.1 First Degree Robbery") %>% 
           str_replace_all("RS 14:81 [(]27[)]", "RS 14:81(27)"), 
         id = 1:n()) %>% 
  separate_rows(charge, sep = ",") %>% 
  group_by(id) %>% 
  mutate(sentence_id = 1:n()) %>% 
  ungroup() %>% 
  separate_rows(charge, sep = "/") %>% 
  mutate(charge = 
           charge %>% 
           str_trim(side = "both") %>% 
           na_if("") %>% 
           na_if('""') %>% 
           str_replace_all('""', "") %>% 
           ifelse(str_detect(., "^[0-9]") == TRUE, paste0("RS ", .), .)) %>% 
  filter(is.na(charge) == FALSE) %>% 
  mutate(charge = 
           charge %>% 
           str_replace_all("^Rs", "RS") %>% 
           str_replace_all("^rs", "RS") %>% 
           str_replace_all("^rS", "RS") %>% 
           str_replace_all("^RS:", "RS"),
         rs_flag = 
           ifelse(str_detect(charge, "^RS") == TRUE, 1, 0) %>% 
           cumsum()) %>% 
  group_by(id, rs_flag, jdc, parish, age, victim_crime) %>% 
  dplyr::summarize(charge = paste(charge, collapse = ", "), 
                   sentence_id = min(sentence_id, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(state_crime_id = 
           charge %>% 
           str_extract("RS\\s*[0-9:.A-Za-z]+(?:\\([A-Za-z0-9]+\\))*[0-9A-Za-z]*"), 
         charge_clean = 
           charge %>% 
           str_remove("RS\\s*[0-9:.A-Za-z]+(?:\\([A-Za-z0-9]+\\))*[0-9A-Za-z]*") %>% 
           str_remove_all("-") %>% 
           str_remove_all("—") %>% 
           str_trim(side = "both") %>% 
           na_if("") %>% 
           str_to_title() %>% 
           str_replace_all("Ii", "II")) %>% 
  arrange(state_crime_id) %>% 
  group_by(state_crime_id) %>% 
  tidyr::fill(charge_clean, .direction = "updown") %>% 
  mutate(charge_clean_full = paste(charge_clean, sep = ";")) %>% 
  ungroup() %>% 
  dplyr::select(-charge_clean) %>% 
  mutate(charge = paste(state_crime_id, charge_clean_full, sep = " ")) %>% 
  relocate(sentence_id, .after = "id") %>% 
  arrange(id, sentence_id, rs_flag)
  
##Get Wide Format - Sentence Level
parish_data_wide_sentence_level <- 
  parish_data_clean %>% 
  group_by(id, sentence_id, jdc, parish, age, victim_crime) %>% 
  dplyr::summarize(across(c("charge", "state_crime_id", "charge_clean_full"), 
                          ~paste(., collapse = "; "))) %>% 
  ungroup() 

##Get Fully Wide Format
parish_data_wide_full <- 
  parish_data_wide_sentence_level %>% 
  arrange(id, charge_clean_full) %>% 
  group_by(id, jdc, parish, age, victim_crime) %>% 
  dplyr::summarize(across(c("charge", "state_crime_id", "charge_clean_full"), 
                          ~paste(., collapse = " / "))) %>% 
  ungroup() 
  

####Tidycensus Parish Data####
##Set Census API Key
api_key <- "1385862bd7c86f7bd96631cdf808f09a1514dcbd"
census_api_key(api_key, install = TRUE, overwrite = TRUE)

##Get Data
census_data_la <- 
  get_acs(geography = "county", 
          variables = "B01003_001", 
          year = 2023, 
          survey = "acs5", 
          state = "LA") %>% 
  clean_names() %>% 
  separate_wider_delim(cols = "name", delim = ", ", names = c("parish", "state"), too_few = "align_start") %>% 
  dplyr::select(geoid, parish, estimate)
  
  
