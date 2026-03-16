library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(stringr)
library(data.table)

file1 <- ("~/Documents/Ecology /Project R /Pheno Proj/Phenology/Forb Pheno for Webster/ForbFlowerPhenology - ForbPhenology(enter data here).csv")

# Function to process insect summarization for one CSV file
process_csv <- function(file) {
  df <- read.csv(file)
  
  # Rename Location as Site because most of the dataset has "Site"
  df <- df %>%
    rename(Site = Location)
  
  # Rename Site Names
  df$Site <- recode(df$Site,
                    "Earthdance" = "EarthDance",
                    "HLS" = "HOLS",
                    "House of Living Stone" = "HOLS",
                    "Old Ferguson" = "Ferguson",
                    "Spanish Lake" = "Rustic Roots",
                    "Virginia " = "Virginia",
                    "Virgina" = "Virginia")
  
  # Parse the Date column with multiple formats and extract the Year
  df$Date <- lubridate::parse_date_time(df$Date, orders = c("%d-%b-%y", "%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y", "dmy", "mdy", "ymd"))
  df$Date <- as.Date(df$Date)
  df$Year <- lubridate::year(df$Date)
  
  # Count distinct dates per site and year
  data_combined <- df %>%
    group_by(Site, Year, Date) %>%
    summarize(
      summarized_date_ours = n_distinct(Date, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Return the summarized result
  return(data_combined)
}

# Process the csv using the function for just one file
processed_data <- process_csv(file1)

# Print the data
print(processed_data, n = 100)


# Save the summary data to a new CSV file
write.csv(processed_data, "Forb Pheno - OURS WEBSTER.csv", row.names = FALSE)