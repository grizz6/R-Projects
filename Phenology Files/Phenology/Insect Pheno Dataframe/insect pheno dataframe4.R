library(dplyr)
library(tidyr)
library(lubridate)

# Define CSV file paths
file1 <- "~/Documents/Ecology /Project R /Pheno Proj/Tree Flowering Phenology - TreePhenophase2024.csv" 
file2 <- "~/Documents/Ecology /Project R /Pheno Proj/Tree Flowering Phenology - FocalTreeFlwrCounts 2023-07-06.csv"
file3 <-  "~/Documents/Ecology /Project R /Pheno Proj/Tree Flowering Phenology 2023-07-06(TreePhenophase).csv"
file4 <- "~/Documents/Ecology /Project R /Pheno Proj/Insect pollinator data - Pollen Count_Pollinator Data (1).csv"

# Function to process phenophase data with TreePhenophase and proper dates
process_phenophase <- function(file) {
  df <- read.csv(file)
  df$Date <- parse_date_time(df$Date, orders = c("d-%b-%y", "Y-m-d", "d/%m/%Y", "m/d/Y"))
  
  # 1. First two phenophase 2 dates for each site
  phenophase_df <- df %>%
    filter(TreePhenophase == 2) %>%
    group_by(Site, TreePhenophase) %>%
    arrange(Date) %>%
    slice(1:2) %>%
    ungroup() %>%
    select(Site, Date, TreePhenophase)
  
  # 2. Site, year, number of sampling dates for flowers, and TreePhenophase
  summary_df <- df %>%
    group_by(Site, Year = parse_date_time(df$Date, orders = c("d-%b-%y", "Y-m-d", "d/%m/%Y", "m/d/Y")), TreePhenophase) %>%
    summarise(num_sampling_dates_flowers = n_distinct(Date)) %>%
    ungroup()
  
  return(list(phenophase_df = phenophase_df, summary_df = summary_df))
}

# Function to process insect data with Insect.Type and proper dates
process_insects <- function(file) {
  df <- read.csv(file)
  df$Date <- parse_date_time(df$Date, orders = c("d-%b-%y", "Y-m-d", "d/%m/%Y", "m/d/Y"))
  
  # 1. First insect (Insect.Type) date for each site
  insect_df <- df %>%
    group_by(Orchard, Insect) %>%
    arrange(Date) %>%
    slice(1) %>%
    ungroup() %>%
    select(Orchard, Insect, Date)
  
  # 2. Orchard, year, number of sampling dates for insects, and Insect.Type
  summary_df <- df %>%
    group_by(Orchard, Year = parse_date_time(df$Date, orders = c("d-%b-%y", "Y-m-d", "d/%m/%Y", "m/d/Y")), Insect) %>%
    summarise(num_sampling_dates_insects = n_distinct(Date)) %>%
    ungroup()
  
  return(list(insect_df = insect_df, summary_df = summary_df))
}

# Function to combine phenophase data
combine_phenophase <- function(...) {
  phenophase_list <- list(...)
  
  combined_phenophase_df <- do.call(rbind, lapply(phenophase_list, function(x) x$summary_df))
  combined_phenophase_df <- combined_phenophase_df %>%
    group_by(Site, Year, TreePhenophase) %>%
    summarise(num_sampling_dates_flowers = sum(num_sampling_dates_flowers, na.rm = TRUE)) %>%
    ungroup()
  
  return(combined_phenophase_df)
}

# Function to merge phenophase and insect data, including insect names and proper labels
merge_data <- function(phenophase_data, insect_data) {
  # Merge phenophase and insect summary data
  merged_df <- merge(phenophase_data, insect_data$summary_df, 
                     by.x = c("Site", "Year"), by.y = c("Orchard", "Year"), all = TRUE)
  
  # Merge on Site and Insect.Type to include insect collection dates
  merged_df <- merge(merged_df, insect_data$insect_df, 
                     by.x = c("Site", "Insect"), by.y = c("Orchard", "Insect"), all.x = TRUE)
  
  # Replace NA values in num_sampling_dates columns
  merged_df <- merged_df %>% 
    mutate(
      num_sampling_dates_flowers = ifelse(is.na(num_sampling_dates_flowers), 0, num_sampling_dates_flowers),
      num_sampling_dates_insects = ifelse(is.na(num_sampling_dates_insects), 0, num_sampling_dates_insects),
      total_sampling_dates = num_sampling_dates_flowers + num_sampling_dates_insects,
      Insect= ifelse(is.na(Insect), "", Insect), 
      num_sampling_dates_insects = ifelse(num_sampling_dates_insects == 0, "", num_sampling_dates_insects)
    ) %>%
    filter(TreePhenophase == 2) %>%
    select(Site, Year, TreePhenophase, num_sampling_dates_flowers, Insect, 
           num_sampling_dates_insects, total_sampling_dates) 
  
  return(merged_df)
}

# Process phenophase files
phenophase1 <- process_phenophase(file1)
phenophase2 <- process_phenophase(file2)
phenophase3 <- process_phenophase(file3)

# Combine all phenophase files
combined_phenophase <- combine_phenophase(phenophase1, phenophase2, phenophase3)

# Process insect file
insect <- process_insects(file4)

# Merge phenophase and insect data
merged_data <- merge_data(combined_phenophase, insect)

# Increase max.print to a higher number (e.g., 100)
options(max.print = 5000)

# Print the dataframe again
print(merged_data)