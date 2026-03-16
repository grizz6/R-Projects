library(dplyr)
library(lubridate)

# Define CSV file paths
file1 <- "~/Documents/Ecology /Project R /Pheno Proj/Tree Flowering Phenology - TreePhenophase2024.csv" 
file2 <- "~/Documents/Ecology /Project R /Pheno Proj/Tree Flowering Phenology - FocalTreeFlwrCounts 2023-07-06.csv"
file3 <-  "~/Documents/Ecology /Project R /Pheno Proj/Tree Flowering Phenology 2023-07-06(TreePhenophase).csv"
file4 <- "~/Documents/Ecology /Project R /Pheno Proj/Insect pollinator data - Pollen Count_Pollinator Data.csv"

# Function to process phenophase data
process_phenophase <- function(file) {
  df <- read.csv(file)
  df$Date <- parse_date_time(df$Date, orders = c("d-%b-%y", "Y-m-d", "d/%m/%Y", "m/d/Y"))
  
  # 1. First two phenophase 2 dates for each site
  phenophase_df <- df %>%
    filter(TreePhenophase == 2) %>%
    group_by(Site) %>%
    arrange(Date) %>%
    slice(1:2) %>%
    ungroup() %>%
    select(Site, Date)
  
  # 3. Site, year, and number of sampling dates for flowers
  summary_df <- df %>%
    group_by(Site, Year = as.numeric(format(Date, "%Y"))) %>%
    summarise(num_sampling_dates_flowers = n_distinct(Date)) %>%
    ungroup()
  
  print(paste("First two phenophase 2 dates for each site in file:", basename(file)))
  print(phenophase_df, n = 500)
  
  print(paste("Sampling summary for phenophase file:", basename(file)))
  print(summary_df, n = 500)
  
  return(list(phenophase_df = phenophase_df, summary_df = summary_df))
}

# Function to process insect data
process_insects <- function(file) {
  df <- read.csv(file)
  df$Date <- parse_date_time(df$Date, orders = c("d-%b-%y", "Y-m-d", "d/%m/%Y", "m/d/Y"))
  
  # 2. First insect (Insect.Type) date for each site
  insect_df <- df %>%
    group_by(Orchard, Insect.Type) %>%
    arrange(Date) %>%
    slice(1) %>%
    ungroup() %>%
    select(Orchard, Insect.Type, Date)
  
  # 3. Orchard, year, and number of sampling dates for insects
  summary_df <- df %>%
    group_by(Orchard, Year = as.numeric(format(Date, "%Y"))) %>%
    summarise(num_sampling_dates_insects = n_distinct(Date)) %>%
    ungroup()
  
  print(paste("First insect date for each insect type in file:", basename(file)))
  print(insect_df, n = 500)
  
  print(paste("Sampling summary for insect file:", basename(file)))
  print(summary_df, n = 500)
  
  return(list(insect_df = insect_df, summary_df = summary_df))
}

# Function to create a merged dataframe
merge_data <- function(phenophase_data, insect_data) {
  merged_df <- merge(phenophase_data$summary_df, insect_data$summary_df, 
                     by.x = c("Site", "Year"), by.y = c("Orchard", "Year"), all = TRUE)
  
  merged_df <- merged_df %>%
    mutate(total_sampling_dates = num_sampling_dates_flowers + num_sampling_dates_insects) %>%
    select(Site, Year, num_sampling_dates_flowers, num_sampling_dates_insects, total_sampling_dates)
  
  print("Merged data summary:")
  print(merged_df, n = 500)
  
  return(merged_df)
}

# Process each phenophase file
phenophase1 <- process_phenophase(file1)
phenophase2 <- process_phenophase(file2)
phenophase3 <- process_phenophase(file3)

# Process insect file
insect <- process_insects(file4)




