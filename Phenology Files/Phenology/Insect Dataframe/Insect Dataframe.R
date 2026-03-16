library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

# Define CSV file paths
file1 <- "~/Documents/Ecology /Project R /Pheno Proj/Insect pollinator data - Pollen Count_Pollinator Data (1).csv"
file2 <- "~/Documents/Ecology /Project R /Pheno Proj/Orchard Video MetaData(Metadata 2024).csv"
file3 <- "~/Documents/Ecology /Project R /Pheno Proj/Orchard Video MetaData(Metadata 2022).csv"
file4 <- "~/Documents/Ecology /Project R /Pheno Proj/Orchard Video MetaData(Metadata 2023).csv"


# Function to process insect summarization 
process_insects <- function(file) {
  df <- read.csv(file)
  
  # Rename Orchard as Site because most of the dataset has "Site"
  df <- df %>%
    rename(Site = Orchard)
  
  # Rename Site Names
  df$Site <- recode(df$Site,
                    "Our Lady Of the Holy Cross" = "Holy Cross",
                    "Our Lady of the Holy Cross" = "Holy Cross",
                    "Lady of the Holy Cross (Baden)" = "Holy Cross",
                    "Lady of the Holy Cross (Baden) " = "Holy Cross",
                    "Our Lady of Holy Cross" = "Holy Cross",
                    "Our Lady" = "Holy Cross",
                    "Our Lady " = "Holy Cross",
                    "HolyCross" = "Holy Cross",
                    "Holy Cross " = "Holy Cross",
                    "HLS" = "HOLS",
                    "House of Living Stone" = "HOLS",
                    "Kelogg" = "Kellogg",
                    "Kellogg Park" = "Kellogg",
                    "Kellog" = "Kellogg",
                    "Kellog " = "Kellogg",
                    "Kellogg " = "Kellogg",
                    "Florissant " = "Florissant",
                    "Florrisant "= "Florissant",
                    "Florrisant" = "Florissant",
                    "Emmanuel " = "Emmanuel",
                    "Virginia Ave" = "Virginia",
                    "Blessings and Glory" = "Blessing & Glory",
                    "Mckinley" = "McKinley",
                    "Carondalet" = "Carondelet",
                    "Corondelet" = "Carondelet",
                    "Carondolet" = "Carondelet",
                    "Corondolet" = "Carondelet",
                    "grow" = "Rustic Roots",
                    "Grow" = "Rustic Roots",
                    "GROW Spanish Lake" = "Rustic Roots",
                    "Spanish Lake" = "Rustic Roots",
                    "Rustic of Roots" = "Rustic Roots",
                    "GROW" = "Rustic Roots",
                    "COLA Victory Garden" = "COLA",
                    "Cola" = "COLA",
                    "Old Ferguson" = "Ferguson",
                    "Old Ferguson West" = "Ferguson",
                    "Ferguson " = "Ferguson",
                    "Earth Dance" = "EarthDance",
                    "Earthdance" = "EarthDance")
  
  # Ensure only the date component is stored
  df$Date <- parse_date_time(df$Date, orders = c("d-%b-%y", "Y-m-d", "d/%m/%Y", "m/d/Y"))
  df$Date <- as.Date(df$Date)
  
  # Extract the year from the Date column
  df$Year <- lubridate::year(df$Date)
  
  # Group by Orchard, Date, and Insect, then count occurrences
  insect_counts <- df %>%
    group_by(Site, Year, Date) %>%
    summarise(TotalInsectCount = n(),
              summarized_date_insect = n_distinct(Date),
              .groups = 'drop')
  
  # Return the summarized result
  return(list(data = df, summary = insect_counts))
}

# Function to process camcorders/gopros/bee hotels
process_cams <- function(file) {
  df <- read.csv(file)
  
  # Rename Site Names
  df$Site <- recode(df$Site,
                    "Our Lady Of the Holy Cross" = "Holy Cross",
                    "Our Lady of the Holy Cross" = "Holy Cross",
                    "Lady of the Holy Cross (Baden)" = "Holy Cross",
                    "Lady of the Holy Cross (Baden) " = "Holy Cross",
                    "Our Lady of Holy Cross" = "Holy Cross",
                    "Our Lady" = "Holy Cross",
                    "Our Lady " = "Holy Cross",
                    "HolyCross" = "Holy Cross",
                    "Holy Cross " = "Holy Cross",
                    "HLS" = "HOLS",
                    "Living Stone" = "HOLS",
                    "House of Living Stone" = "HOLS",
                    "Kelogg" = "Kellogg",
                    "Kellogg Park" = "Kellogg",
                    "Kellog" = "Kellogg",
                    "Kellog " = "Kellogg",
                    "Kellogg " = "Kellogg",
                    "Florissant " = "Florissant",
                    "Florrisant "= "Florissant",
                    "Florrisant" = "Florissant",
                    "Emmanuel " = "Emmanuel",
                    "Virginia Ave" = "Virginia",
                    "Blessings and Glory" = "Blessing & Glory",
                    "Mckinley" = "McKinley",
                    "Carondalet" = "Carondelet",
                    "Carondeleet" = "Carondelet",
                    "Corondelet" = "Carondelet",
                    "Carondolet" = "Carondelet",
                    "Corondolet" = "Carondelet",
                    "grow" = "Rustic Roots",
                    "Grow" = "Rustic Roots",
                    "GROW Spanish Lake" = "Rustic Roots",
                    "Spanish Lake" = "Rustic Roots",
                    "Rustic of Roots" = "Rustic Roots",
                    "GROW" = "Rustic Roots",
                    "Rustic Root" = "Rustic Roots",
                    "COLA Victory Garden" = "COLA",
                    "Cola" = "COLA",
                    "Old Ferguson" = "Ferguson",
                    "Old Ferguson West" = "Ferguson",
                    "Ferguson " = "Ferguson",
                    "Earth Dance" = "EarthDance",
                    "Earthdance" = "EarthDance")
  
  # Adjust Camera names to match the patterns
  df$Camera <- str_replace(df$Camera, "^Camera ?([1-9]|1[0-2])$", "C\\1")
  df$Camera <- str_replace(df$Camera, "^G([1-9]|10)$", "GoPro \\1")
  df$Camera <- str_replace(df$Camera, "^GoPro ?([1-9]|1[0-2])$", "GoPro \\1")
  
  # Ensure only the date component is stored
  df$Date <- parse_date_time(df$Date, orders = c("d-%b-%y", "Y-m-d", "d/%m/%Y", "m/d/Y"))
  df$Date <- as.Date(df$Date)
  
  # Ensure TreeNumber is a string
  df$Tree <- as.character(df$Tree)
  
  # Categorize Camera Type
  df <- df %>%
    mutate(CameraType = case_when(
      grepl("^C[1-9]|^C1[0-2]$", Camera) ~ "Camcorders",
      (grepl("^GoPro [1-9]|^GoPro 1[0-2]$", Camera) & Tree == "Bee hotel") ~ "Bee Hotels",
      grepl("^GoPro [1-9]|^GoPro 1[0-2]$", Camera) ~ "GoPros",
      grepl("^H[1-2]$", Camera) ~ "Bee Hotels",
      TRUE ~ "Unknown"
    ))

  # Extract the year from the Date column
  df$Year <- lubridate::year(df$Date)
  
  # Group by Site, Date, and CameraType, then calculate cumulative count
  cam_counts <- df %>%
    group_by(Site, Year, Date) %>%
    summarise(
      Camcorders = sum(CameraType == "Camcorders"),
      GoPros = sum(CameraType == "GoPros"),
      BeeHotels = sum(CameraType == "Bee Hotels"),
      summarized_date_cameras = n_distinct(Date),
      .groups = 'drop'
    )
  
  return(list(data = df, summary = cam_counts))
}

# Process insect file
insect_result <- process_insects(file1)
insect_data <- insect_result$data
insect_count <- insect_result$summary

# Process camcorders file
camcorders_count1 <- process_cams(file2)
camcorders_count2 <- process_cams(file3)
camcorders_count3 <- process_cams(file4)

# Combine all processed data
cam_data <- bind_rows(camcorders_count1$data, camcorders_count2$data, camcorders_count3$data)
cam_counts <- bind_rows(camcorders_count1$summary, camcorders_count2$summary, camcorders_count3$summary)


# Merge the insect data with the combined camcorder data by Site and Year
merged_counts <- bind_rows(insect_count, cam_counts) %>%
  arrange(Site, Year) %>%
  mutate(
    across(where(is.numeric), ~replace_na(.,0))
  ) %>%
  filter(Year != 0 & Site != "")


# Summarize duplicated rows based on Site and Year by summing the numeric columns
summarized_counts <- merged_counts %>%
  group_by(Site, Year) %>%
  summarise(
    TotalInsectCount = sum(TotalInsectCount, na.rm = TRUE),
    Camcorders = sum(Camcorders, na.rm = TRUE),
    GoPros = sum(GoPros, na.rm = TRUE),
    BeeHotels = sum(BeeHotels, na.rm = TRUE),
    summarized_date_insect = sum(summarized_date_insect, na.rm = TRUE),
    summarized_date_cameras = sum(summarized_date_cameras, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(Site != "17210")

# Print the summarized data
print(summarized_counts, n = 900)

write.csv(summarized_counts, "Insect Dataframe - All Years.csv", row.names = FALSE)



