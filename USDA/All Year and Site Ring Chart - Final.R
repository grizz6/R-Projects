# Load the packages, if you wanna know which function uses what package; just google i'll be honest
library(dplyr)
library(readr)
library(ggplot2)
library(ggridges)
library(lubridate)

# Load the dataset (Change the directory)
df1 <- read_csv("~/Documents/Ecology /USDA Grant/Pheno Files/ALL tree database - Tree_Master_Updated Earthdance_COLA_Carondelet.csv")
df2 <- read_csv("~/Documents/Ecology /USDA Grant/Pheno Files/Insect pollinator data - Pollen Count_Pollinator Data.csv")

# Rename columns for consistency
df1 <- df1 %>% rename(TreeNumber = Tree)

# Merge df1 and df2 on common columns: Orchard and TreeNumbers
merged_df <- merge(df1, df2, by = c("Orchard", "TreeNumber"))

# Rename long column name for species to a simpler one
merged_df <- merged_df %>%
  rename(Species = `Species (2024 corrected by Michael Hayes)`)

# Rename site names to its common name
merged_df <- merged_df %>%
  mutate(Orchard = case_when(
    Orchard %in% c("Blessings and Glory") ~ "Blessings and Glory",
    Orchard %in% c("Carondalet") ~ "Carondelet",
    Orchard %in% c("COLA Victory Garden") ~ "COLA",
    Orchard %in% c("GROW Spanish Lake", "Spanish Lake") ~ "Rustic Roots",
    Orchard %in% c("Kellogg Park") ~ "Kellogg",
    Orchard %in% c("Lady of the Holy Cross (Baden)") ~ "Holy Cross",
    Orchard %in% c("Mckinley") ~ "McKinley",
    Orchard %in% c("Old Ferguson West", "Old Ferguson") ~ "Ferguson",
    Orchard %in% c("Earthdance") ~ "EarthDance",
    Orchard %in% c("HLS", "House of Living Stone", "House of the Living Stone") ~ "HOLS",
    TRUE ~ Orchard
  ))

# Reassign OTU for Apis mellifera explicitly
merged_df <- merged_df %>%
  mutate(OTU = if_else(Insect == "Apis mellifera", "Apis mellifera", OTU))

# Remove rows with missing OTU values
merged_df <- merged_df %>% filter(!is.na(OTU))

# Categorize insects into groups (Flies, Beetles)
merged_df <- merged_df %>%
  mutate(BeeType = case_when(
    grepl("diptera", OTU, ignore.case = TRUE) ~ "Flies",
    grepl("coleoptera", OTU, ignore.case = TRUE) ~ "Beetles",
    grepl("Hemiptera", OTU, ignore.case = TRUE) ~ "True Bugs",
    TRUE ~ OTU
  )) %>%
  mutate(BeeType = case_when(
    BeeType == "Apis mellifera" ~ "Honey Bees",
    BeeType == "Bee" ~ "Bees",
    BeeType == "Beetles" ~ "Beetles",
    BeeType == "Flies" ~ "Flies",
    BeeType == "Butterfly" ~ "Butterflies",
    BeeType == "Wasp" ~ "Wasps",
    BeeType == "Ant" ~ "Ants",
    BeeType == "Hemiptera" ~ "True Bugs",
    BeeType == "Sawfly" ~ "Sawflies",
    TRUE ~ BeeType
  ))

insect_data_all <- merged_df %>%
  group_by(BeeType) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(
    percent = count / sum(count) * 100,
    legend_label = paste0(BeeType, " (", round(percent, 1), "%)")
  ) %>%
  arrange(desc(percent))

# Plot ring charts for all site
ring_all <- ggplot(insect_data_all, aes(
  x = 2, y = percent,
  fill = factor(legend_label, levels = insect_data_all$legend_label)
)) +
  geom_col(width = 1, color = "black") +
  coord_polar("y", start = 0) +
  xlim(0.5, 2.5) +
  theme_void() +
  labs(
    title = "Insect Type Distribution Across All Orchards",
    fill = "Insect Type"
  ) +
  theme(
    legend.title = element_text(size = 17),
    legend.text = element_text(size = 15),
    legend.key.size = unit(1.5, "cm"),
    legend.key.width = unit(1.5, "cm"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20)
  )

print(ring_all)

# Loop over each orchard to create ring charts (site specific)
for (orchard in unique(merged_df$Orchard)) {
  insect_data <- merged_df %>%
    filter(Orchard == orchard) %>%
    group_by(BeeType) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(
      percent = count / sum(count) * 100,
      legend_label = paste0(BeeType, " (", round(percent, 1), "%)") 
    )

  # Create ring chart using ggplot
  ring <- ggplot(insect_data, aes(x = 2, y = percent, fill = legend_label)) +
    geom_col(width = 1, color = "black") +
    coord_polar("y", start = 0) +
    xlim(0.5, 2.5) +   
    theme_void() +
    labs(title = paste("Insect Type Distribution for", orchard),
         fill = "Insect Type") +
    theme(
      legend.title = element_text(size = 17),
      legend,text = element_text(size = 15),
      legend.key.size = unit(1.5, "cm"),  
      legend.key.width = unit(1.5, "cm"),
      plot.title = element_text(hjust = 0.5, face = "bold",size = 20)
    )
  
  print(ring)
}


