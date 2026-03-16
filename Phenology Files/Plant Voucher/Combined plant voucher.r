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

# Parse dates for each dataset
file1 <- file1 %>%
  mutate(Date = parse_dates(Date))

file2 <- file2 %>%
  mutate(Date = parse_dates(Date))

file3 <- file3 %>%
  mutate(Date = parse_dates(Date))

file4 <- file4 %>%
  mutate(Date = parse_dates(Date))

# Combine all datasets
combined_data <- bind_rows(
  file1,
  file2,
  file3,
  file4
)

# Extract year from Date
combined_data <- combined_data %>%
  mutate(Year = year(Date))

# Create summary: number of flowering plants per Site × Year × Date
summary_data <- combined_data %>%
  group_by(Site, Year, Date) %>%
  summarise(
    No_Flowering_Plants = n(),
    .groups = "drop"
  ) %>%
  arrange(Site, Year, Date)

# View the summary
print(summary_data, n = 300)

# Save the output file
# write.csv(summary_data, "nonfocal_flowering_plants_per_site_year_date.csv", row.names = FALSE)





