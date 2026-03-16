library(dplyr)
library(readr)
library(tibble)
library(purrr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(stringr)

# summarize netting data by site and year 

file1 <- ("~/Documents/Ecology /Project R /Pheno Proj/Netting file/Netting Data 2024 - Sheet1.csv")
file2 <- ("~/Documents/Ecology /Project R /Pheno Proj/Netting file/2023 Pollinator Collection - Sheet1.csv")
file3 <- ("~/Documents/Ecology /Project R /Pheno Proj/Netting file/Collection Data 2022.xlsx - Pollinator collection data.csv")


# Read and standardize data types, then combine
combined_data <- bind_rows(
  read.csv(file1) %>% mutate(across(everything(), as.character)),
  read.csv(file2) %>% mutate(across(everything(), as.character)),
  read.csv(file3) %>% mutate(across(everything(), as.character))
)

# Standardize site names
combined_data <- combined_data %>%
  mutate(
    Site = case_when(
      Site %in% c("Earthdance", "Earth Dance") ~ "EarthDance", 
      Site == "Virginia Ave" ~ "Virginia",     
      Site %in% c("Our Lady", "Our Lady ", "Our Lady Of the Holy Cross", "Our Lady of the Holy Cross") ~ "Holy Cross", 
      Site == "Old Ferguson" ~ "Ferguson",
      Site == "McKinley " ~ "McKinley",
      Site == "HLS" ~ "HOLS",
      Site %in% c("GROW", "Grow", "grow", "GROW Spanish Lake") ~ "Rustic Roots", 
      Site == "Forissant" ~ "Florissant",
      
      TRUE ~ Site                               
    )
  )

# Summarize data by Site and Year
summary_data <- combined_data %>%
  mutate(
    Date = parse_date_time(Date, orders = c("d-b-Y", "Y-m-d", "d/m/Y", "m/d/Y")) %>% as.Date(),
    Year = lubridate::year(Date),
    `No..of.Samples` = as.numeric(`No..of.Samples`)
  ) %>%
  filter(!is.na(Date)) %>%
  # Group by Site and Year
  group_by(Site, Year) %>%
  summarise(
    summarized_date_netting = n_distinct(Date),
    .groups = "drop"
  )

# Display summary
print(summary_data, n = 125)


write.csv(summary_data, "Netting Data - All years.csv", row.names = FALSE)



