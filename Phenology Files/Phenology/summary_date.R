library(dplyr)
library(readr)
library(tibble)
library(purrr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(stringr)


file1 <- ("Tree Pheno No Trees, No Types, By Date.csv")
file2 <- ("Insect Dataframe - All Years.csv")
file3 <- ("Forb Pheno, all years.csv")
file4 <- ("Netting Data - All years.csv")

# Function to process insect summarization 
process_csv <- function(file) {
  df <- read.csv(file)
  
  # Rename Site Names
  df$Site <- recode(df$Site,
                    "Livingstone" = "HOLS",
                    "Living Stone" = "HOLS",
                    "Orcahrd in Virginia" = "Virginia")
  
  # Return the summarized result
  return(df)
}

# Process the csv using the function
process1 <- process_csv(file1)
process2 <- process_csv(file2)
process3 <- process_csv(file3)
process4 <- process_csv(file4)

# Read and merge all files
data_combined <- bind_rows(process1, process2, process3, process4) %>%
  arrange(Site, Year)

data_combinedd <- data_combined %>%
  group_by(Site, Year,) %>%
  summarize(
    summarized_date_tree = sum(summarized_date_tree, na.rm = TRUE),
    summarized_date_insect = sum(summarized_date_insect, na.rm = TRUE),
    summarized_date_forb = sum(summarized_date_forb, na.rm = TRUE),
    summarized_date_cameras = sum(summarized_date_cameras, na.rm = TRUE),
    summarized_date_netting = sum(summarized_date_netting, na.rm = TRUE),
    .groups = "drop" 
  )

# Print the data
print(data_combinedd, n = 100)

write.csv(data_combinedd, "All Merged Survey Count - Pheno, Netting, Forb, Cams, GoPros, Bee Hotels.csv", row.names = FALSE)
