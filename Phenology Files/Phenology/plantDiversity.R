library(dplyr)
library(tidyr)
library(lubridate)

# align it with all trees

# Define CSV file paths
file1 <- "~/Documents/Ecology /Project R /Pheno Proj/Tree Flowering Phenology - TreePhenophase (DATA).csv"
file2 <- "~/Documents/Ecology /Project R /Pheno Proj/ALL tree database - Tree_Master_Updated Earthdance_COLA_Carondelet.csv"

# Process files 1 for TreePhenophase filtering
process_csv_phenophase <- function(file) {
  # Read the CSV file
  df <- read.csv(file)
  
  # Parse dates into a standard format
  df$Date <- parse_date_time(df$Date, orders = c("d-b-y", "Y-m-d", "d/%m/%Y", "m/d/Y"))
  df$Date <- as.Date(df$Date)

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
                    "Kellog & HOLS" = "Kellogg",
                    "Kelogg" = "Kellogg",
                    "Kellogg Park" = "Kellogg",
                    "Kellog" = "Kellogg",
                    "Kellog " = "Kellogg",
                    "Kellogg " = "Kellogg",
                    "Florissant " = "Florissant",
                    "Florrisant" = "Florissant",
                    "Emmanuel " = "Emmanuel",
                    "Virginia Ave" = "Virginia",
                    "Blessings and Glory" = "Blessings & Glory",
                    "Blessings and Glory " = "Blessings & Glory",
                    "Mckinley" = "McKinley",
                    "Carondalet" = "Carondelet",
                    "Carondeleet" = "Carondelet",
                    "Corondelet" = "Carondelet",
                    "Carondolet" = "Carondelet",
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
                    "EArthDance" = "EarthDance",
                    "Earthdance" = "EarthDance")
  
  
  # Rename TreeNumber based on Site
  df_filtered <- df %>%
    filter(TreePhenophase %in% c(2, 3, 3.5)) %>%
    mutate(TreeNumber = as.character(TreeNumber)) %>%
    mutate(TreeNumber = case_when(
      Site == "SLU" & TreeNumber == "1" ~ "51",
      Site == "SLU" & TreeNumber == "1-51" ~ "51",
      Site == "Carondelet" & TreeNumber == "105-131" ~ "131",
      Site == "Carondelet" & TreeNumber == "123-132" ~ "132",
      Site == "Carondelet" & TreeNumber == "126-133" ~ "133",
      Site == "Carondelet" & TreeNumber == "106-129-136" ~ "136",
      Site == "Carondelet" & TreeNumber == "106-129-134" ~ "134",
      Site == "Carondelet" & TreeNumber == "129" ~ "136",
      Site == "Carondelet" & TreeNumber == "131-106" ~ "106",
      Site == "Carondelet" & TreeNumber == "101-135" ~ "135",
      Site == "Carondelet" & TreeNumber == "108-136" ~ "136",
      Site == "HOLS" & TreeNumber == "143-152" ~ "152",
      Site == "Kellogg" & TreeNumber == "168-169" ~ "169",
      Site == "Kellogg" & TreeNumber == "169-168" ~ "168",
      Site == "Florisant" & TreeNumber == "196-198" ~ "198",
      Site == "Florisant" & TreeNumber == "197-199" ~ "199",
      Site == "Rustic Roots" & TreeNumber == "252-255" ~ "255",
      Site == "Rustic Roots" & TreeNumber == "242-256" ~ "256",
      Site == "Botanical Heights" & TreeNumber == "268-275" ~ "275",
      Site == "Botanical Heights" & TreeNumber == "269-276" ~ "276",
      Site == "Botanical Heights" & TreeNumber == "270-274" ~ "274",
      Site == "Botanical Heights" & TreeNumber == "272-281" ~ "281",
      Site == "Botanical Heights" & TreeNumber == "271-282" ~ "282",
      Site == "Botanical Heights" & TreeNumber == "273-283" ~ "283",
      Site == "Botanical Heights" & TreeNumber == "277-284" ~ "284",
      Site == "Botanical Heights" & TreeNumber == "268" ~ "275",
      Site == "Botanical Heights" & TreeNumber == "269" ~ "276",
      Site == "Botanical Heights" & TreeNumber == "270" ~ "274",
      Site == "Botanical Heights" & TreeNumber == "272" ~ "281",
      Site == "Botanical Heights" & TreeNumber == "271" ~ "282",
      Site == "Botanical Heights" & TreeNumber == "273" ~ "283",
      Site == "Botanical Heights" & TreeNumber == "277" ~ "284",
      Site == "Blessings & Glory" & TreeNumber == "301-309" ~ "309",
      Site == "COLA" & TreeNumber == "227-343" ~ "343",
      Site == "COLA" & TreeNumber == "227-342" ~ "342",
      Site == "COLA" & TreeNumber == "227" ~ "343",
      Site == "McKinley" & TreeNumber == "352-360" ~ "360",
      Site == "McKinley" & TreeNumber == "355-361" ~ "361",
      Site == "McKinley" & TreeNumber == "356-362" ~ "362",
      Site == "13th Street" & TreeNumber == "325-371" ~ "371",
      Site == "13th Street" & TreeNumber == "325" ~ "371",
      Site == "13th Street" & TreeNumber == "327-372" ~ "372",
      TRUE ~ TreeNumber 
    )) 
  
  return(df_filtered)
}

# Process file 2 for TreeType data
process_csv_treetype <- function(file) {
  # Read the CSV file
  df <- read.csv(file)
  
  # Rename 'Orchard' to 'Site', 'Species' to 'TreeType', and 'Tree' to 'TreeNumber'
  df <- df %>%
    rename(Site = Orchard, TreeType = Species, TreeNumber = Tree) %>%
    mutate(Site = recode(Site,
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
                    "Kellog & HOLS" = "Kellogg",
                    "Kelogg" = "Kellogg",
                    "Kellogg Park" = "Kellogg",
                    "Kellog" = "Kellogg",
                    "Kellog " = "Kellogg",
                    "Kellogg " = "Kellogg",
                    "Florissant " = "Florissant",
                    "Florrisant" = "Florissant",
                    "Emmanuel " = "Emmanuel",
                    "Virginia Ave" = "Virginia",
                    "Blessings and Glory" = "Blessings & Glory",
                    "Blessings and Glory " = "Blessings & Glory",
                    "Mckinley" = "McKinley",
                    "Carondalet" = "Carondelet",
                    "Carondeleet" = "Carondelet",
                    "Corondelet" = "Carondelet",
                    "Carondolet" = "Carondelet",
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
                    "EArthDance" = "EarthDance",
                    "Earthdance" = "EarthDance")
    )
  
  df_changed <- df %>%
    mutate(TreeNumber = as.character(TreeNumber)) %>%
    mutate(TreeNumber = case_when(
      Site == "SLU" & TreeNumber == "1" ~ "51",
      Site == "SLU" & TreeNumber == "1-51" ~ "51",
      Site == "Carondelet" & TreeNumber == "105-131" ~ "131",
      Site == "Carondelet" & TreeNumber == "123-132" ~ "132",
      Site == "Carondelet" & TreeNumber == "126-133" ~ "133",
      Site == "Carondelet" & TreeNumber == "106-129-136" ~ "136",
      Site == "Carondelet" & TreeNumber == "106-129-134" ~ "134",
      Site == "Carondelet" & TreeNumber == "129" ~ "136",
      Site == "Carondelet" & TreeNumber == "131-106" ~ "106",
      Site == "Carondelet" & TreeNumber == "101-135" ~ "135",
      Site == "Carondelet" & TreeNumber == "108-136" ~ "136",
      Site == "HOLS" & TreeNumber == "143-152" ~ "152",
      Site == "Kellogg" & TreeNumber == "168-169" ~ "169",
      Site == "Kellogg" & TreeNumber == "169-168" ~ "168",
      Site == "Florisant" & TreeNumber == "196-198" ~ "198",
      Site == "Florisant" & TreeNumber == "197-199" ~ "199",
      Site == "Rustic Roots" & TreeNumber == "252-255" ~ "255",
      Site == "Rustic Roots" & TreeNumber == "242-256" ~ "256",
      Site == "Botanical Heights" & TreeNumber == "268-275" ~ "275",
      Site == "Botanical Heights" & TreeNumber == "269-276" ~ "276",
      Site == "Botanical Heights" & TreeNumber == "270-274" ~ "274",
      Site == "Botanical Heights" & TreeNumber == "272-281" ~ "281",
      Site == "Botanical Heights" & TreeNumber == "271-282" ~ "282",
      Site == "Botanical Heights" & TreeNumber == "273-283" ~ "283",
      Site == "Botanical Heights" & TreeNumber == "277-284" ~ "284",
      Site == "Botanical Heights" & TreeNumber == "268" ~ "275",
      Site == "Botanical Heights" & TreeNumber == "269" ~ "276",
      Site == "Botanical Heights" & TreeNumber == "270" ~ "274",
      Site == "Botanical Heights" & TreeNumber == "272" ~ "281",
      Site == "Botanical Heights" & TreeNumber == "271" ~ "282",
      Site == "Botanical Heights" & TreeNumber == "273" ~ "283",
      Site == "Botanical Heights" & TreeNumber == "277" ~ "284",
      Site == "Blessings & Glory" & TreeNumber == "301-309" ~ "309",
      Site == "COLA" & TreeNumber == "227-343" ~ "343",
      Site == "COLA" & TreeNumber == "227-342" ~ "342",
      Site == "COLA" & TreeNumber == "227" ~ "343",
      Site == "McKinley" & TreeNumber == "352-360" ~ "360",
      Site == "McKinley" & TreeNumber == "355-361" ~ "361",
      Site == "McKinley" & TreeNumber == "356-362" ~ "362",
      Site == "13th Street" & TreeNumber == "325-371" ~ "371",
      Site == "13th Street" & TreeNumber == "325" ~ "371",
      Site == "13th Street" & TreeNumber == "327-372" ~ "372",
      TRUE ~ TreeNumber  
    )) 
  
  return(df_changed)
}

# Process files 1
phenophase1 <- process_csv_phenophase(file1)

# Process file 4 (TreeType data)
treetype_data <- process_csv_treetype(file2)

# Merge updated phenophase with updated data
#final_data <- bind_rows(
#  phenophase1,
#  treetype_data) %>%
#  arrange(Site)

final_data <- left_join(
  phenophase1,
  treetype_data)
  by = c("Site")

# Final Summary by Site and Date
final_summary <- final_data %>%
  mutate(Year = lubridate::year(Date)) %>% 
  group_by(Site, Date, Year) %>%
  summarise(
    Number_of_Trees_in_Bloom = n(),  # Count of trees in bloom
    Number_of_Tree_Types_in_Bloom = n_distinct(TreeType),  # Count of unique tree types in bloom
    summarized_date = n_distinct(Date),
    .groups = 'drop'
  )

total_summary <- final_summary %>%
  mutate(Year = lubridate::year(Date)) %>% 
  group_by(Site, Year) %>%
  summarise(
    summarized_date_tree = n_distinct(Date),
    .groups = 'drop'
  )

# Print the final dataframe showing Site, TreeNumber, and Total_Trees_Bloomed
print(final_summary, n = 100)
print(total_summary, n = 100)

write.csv(total_summary, "Tree Pheno No Trees, No Types, By Date.csv")



