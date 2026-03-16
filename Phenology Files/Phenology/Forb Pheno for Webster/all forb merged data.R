library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(stringr)
library(data.table)

file1 <- read.csv("~/Documents/Ecology /Project R /Pheno Proj/Phenology/Forb Pheno for Webster/Forb Pheno 2022.csv")
file2 <- read.csv("~/Documents/Ecology /Project R /Pheno Proj/Phenology/Forb Pheno for Webster/Forb Pheno 2023.csv")
file3 <- read.csv("~/Documents/Ecology /Project R /Pheno Proj/Phenology/Forb Pheno for Webster/Forb Pheno 2024.csv")
file4 <- read.csv("~/Documents/Ecology /Project R /Pheno Proj/Phenology/Forb Pheno for Webster/Forb Pheno - OURS WEBSTER.csv")

# Create a robust date parsing function
parse_dates <- function(date_col) {
  # Try multiple date formats
  parsed_dates <- as.Date(date_col, tryFormats = c("%d-%b-%y", "%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y", "%b %d, %Y"))
  
  # If parsing fails, try additional conversions
  if(any(is.na(parsed_dates))) {
    # Convert character to date with additional parsing attempts
    parsed_dates <- as.Date(as.character(date_col), tryFormats = c("%d-%b-%y", "%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y", "%b %d, %Y"))
  }
  
  return(parsed_dates)
}

# Modify the initial summary creation
initial_summary <- bind_rows(
  mutate(file1, Date = parse_dates(Date)),
  mutate(file2, Date = parse_dates(Date)),
  mutate(file3, Date = parse_dates(Date))
) %>%
  mutate(Year = lubridate::year(Date)) %>%
  group_by(Site, Year) %>%
  summarise(
    initial_distinct_dates = n_distinct(Date), 
    .groups = "drop"
  )

# Process file4 separately
file4_processed <- file4 %>%
  mutate(Date = parse_dates(Date)) %>%
  mutate(Year = lubridate::year(Date))

# Function to calculate final summary
calculate_final_summary <- function(initial_summary, file4_processed) {
  final_summary <- bind_rows(
    mutate(file1, Date = parse_dates(Date)),
    mutate(file2, Date = parse_dates(Date)),
    mutate(file3, Date = parse_dates(Date)),
    file4_processed
  ) %>%
    mutate(Year = lubridate::year(Date)) %>%
    group_by(Site, Year) %>%
    summarise(
      final_distinct_dates = n_distinct(Date), 
      .groups = "drop"
    )
  
  # Merge the initial and final summaries
  result <- initial_summary %>%
    full_join(final_summary, by = c("Site", "Year")) %>%
    mutate(
      summarized_date_forb = ifelse(
        is.na(final_distinct_dates), 
        initial_distinct_dates, 
        final_distinct_dates
      )
    ) %>%
    select(Site, Year, summarized_date_forb) %>%
    arrange(Site, Year)
  
  return(result)
}

# Generate the final summary
summary_data <- calculate_final_summary(initial_summary, file4_processed) 

# Print the results
print(summary_data, n = 300)

write.csv(summary_data, "Forb Pheno, all years.csv", row.names = FALSE)





