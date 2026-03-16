library(dplyr)
library(tidyr)
library(lubridate)

# Define CSV file paths
file1 <- "~/Documents/Ecology /Project R /Pheno Proj/Tree Flowering Phenology - TreePhenophase2024.csv" 
file2 <- "~/Documents/Ecology /Project R /Pheno Proj/Tree Flowering Phenology - FocalTreeFlwrCounts 2023-07-06.csv"
file3 <- "~/Documents/Ecology /Project R /Pheno Proj/Tree Flowering Phenology 2023-07-06(TreePhenophase).csv"
file4 <- "~/Documents/Ecology /Project R /Pheno Proj/Insect pollinator data - Pollen Count_Pollinator Data.csv"

# Function to process phenophase data with TreePhenophase and proper dates
process_phenophase <- function(file) {
  df <- read.csv(file)
  df$Date <- parse_date_time(df$Date, orders = c("d-%b-%y", "Y-m-d", "d/%m/%Y", "m/d/Y"))
  
  # Convert to Date format (removing time component)
  df$Date <- as.Date(df$Date)
  
  # Site, year, number of sampling dates for flowers, and TreePhenophase
  summary_df <- df %>%
    mutate(Year = year(Date)) %>%  # Extract Year as a separate column
    group_by(Site, Year, TreePhenophase) %>%
    summarise(
      num_sampling_dates_flowers = n_distinct(Date),
      Start_Date = min(Date),
      End_Date = max(Date),
      Duration = as.numeric(difftime(max(Date), min(Date), units = "days"))
    ) %>%
    ungroup() %>%
    filter(TreePhenophase > 1 & TreePhenophase < 4) %>%
  
  return(summary_df)
}

# Function to combine phenophase data
combine_phenophase <- function(...) {
  phenophase_list <- list(...)
  
  combined_phenophase_df <- do.call(rbind, lapply(phenophase_list, function(x) x))  # Combine the data
  combined_phenophase_df <- combined_phenophase_df %>%
    group_by(Site, Year, TreePhenophase) %>%
    summarise(
      num_sampling_dates_flowers = sum(num_sampling_dates_flowers, na.rm = TRUE),
      Start_Date = min(Start_Date),
      End_Date = max(End_Date),
      Duration = as.numeric(difftime(max(End_Date), min(Start_Date), units = "days"))
    ) %>%
    ungroup()
  
  return(combined_phenophase_df)
}

# Process phenophase files
phenophase1 <- process_phenophase(file1)
phenophase2 <- process_phenophase(file2)
phenophase3 <- process_phenophase(file3)

# Combine all phenophase files
combined_phenophase <- combine_phenophase(phenophase1, phenophase2, phenophase3)

# Print the dataframe with sorted Date
print(combined_phenophase, n=500)


