library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(stringr)
library(data.table)

# Define CSV file paths
file1 <- c(
  "~/Documents/Ecology /Project R /Pheno Proj/Plant Voucher/Plant Voucher 24/Plant Vouche Sum '24.xlsx - Carondelet.csv",
  "~/Documents/Ecology /Project R /Pheno Proj/Plant Voucher/Plant Voucher 24/Plant Vouche Sum '24.xlsx - HOLS.csv",
  "~/Documents/Ecology /Project R /Pheno Proj/Plant Voucher/Plant Voucher 24/Plant Vouche Sum '24.xlsx - Kellogg.csv",
  "~/Documents/Ecology /Project R /Pheno Proj/Plant Voucher/Plant Voucher 24/Plant Vouche Sum '24.xlsx - SLU.csv",
  "~/Documents/Ecology /Project R /Pheno Proj/Plant Voucher/Plant Voucher 24/Plant Vouche Sum '24.xlsx - Virginia.csv"
)

# Read CSV files and add the Site column based on the filename
dfs <- lapply(file1, function(file) {
  df <- read.csv(file)
  # Extract Site from filename (e.g., "Carondelet", "Kellogg")
  df$Site <- gsub(".*- (.*?)\\.csv", "\\1", basename(file))
  return(df)
})

# Combine all dataframes into one (wide format)
merged_df <- bind_rows(dfs)

# Rename columns for better understanding
colnames(merged_df) <- c("Plant_Family", "Plant_Species", "Pollen_Sample", 
                         "Visual_Voucher", "X25.Mar", "X1.Apr", "X8.Apr", 
                         "X15.Apr", "X22.Apr", "X29.Apr", "X6.May", 
                         "X13.May", "X20.May", "Notes", "Visibility", "Site")

merged_df$Plant_Family <- recode(merged_df$Plant_Family,
                                 "Rosa " = "Rosa")

# Convert wide format to long format
long_df <- merged_df %>%
  pivot_longer(cols = starts_with("X"), 
               names_to = "Date", 
               values_to = "Presence") %>%
  select(-Notes) # Remove the Notes column

# Convert Date column to proper date format
long_df <- long_df %>%
  mutate(Date = str_replace(Date, "^X", "")) %>%     # Remove "X" prefix
  mutate(Date = case_when(
    str_detect(Date, "Mar") ~ paste0("2024-03-", str_pad(str_extract(Date, "\\d+"), 2, pad = "0")),
    str_detect(Date, "Apr") ~ paste0("2024-04-", str_pad(str_extract(Date, "\\d+"), 2, pad = "0")),
    str_detect(Date, "May") ~ paste0("2024-05-", str_pad(str_extract(Date, "\\d+"), 2, pad = "0")),
    TRUE ~ NA_character_   # Handle unexpected formats
  )) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))          # Convert to Date object

# Summarize the data by Plant_Family, Plant_Species, and Date
summary_df <- long_df %>%
  group_by(Site, Date, Plant_Family, Plant_Species) %>%
  summarize(Presence = sum(as.numeric(Presence), na.rm = TRUE), .groups = "drop")


# Print the first few rows of the long format dataframe
print(summary_df, n = 900)


write.csv(summary_df, "Forb Pheno 2024.csv", row.names = FALSE)

