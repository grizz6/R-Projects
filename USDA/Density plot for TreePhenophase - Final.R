# Load the packages, if you wanna know which function uses what package; just google i'll be honest
library(ggplot2)
library(dplyr)
library(lubridate)
library(ggridges)
library(purrr)

# Load the dataset (Change the directory)
df1 <- read.csv("Tree Flowering Phenology - TreePhenophase.csv", stringsAsFactors = FALSE)
df2 <- read.csv("ALL tree database - Tree_Master_Updated Earthdance_COLA_Carondelet.csv", stringsAsFactors = FALSE)

# Rename columns for consistency
df2 <- df2 %>% rename(c(Site = Orchard, TreeNumber = Tree))

# Merge df1 and df2 on common columns: Site and TreeNumber
merged_df <- merge(df1, df2, by = c("Site", "TreeNumber"))

# Rename the colomn name cause its long
merged_df <- merged_df %>%
  rename(Species = Species..2024.corrected.by.Michael.Hayes.) # You might have to correct it with smth like Species =  (`Species 2024 corrected by Michael Hayes`)    

# Format dates and filter valid records
merged_df <- merged_df %>%
  mutate(
    Date = dmy(Date),                                 
    DateFormatted = format(Date, "%d-%b-%Y"),          
    YearlessDate = as.Date(paste0("2000-", month(Date), "-", day(Date)))
  ) %>%
  filter(
    !is.na(TreePhenophase),                            
    TreePhenophase %in% c(2, 3, 3.5),                 
    !is.na(Date),                                      
    !is.na(Site)                                       
  )

# Standardize site names across multiple variants
merged_df <- merged_df %>%
  mutate(Site = case_when(
    Site %in% c("Blessings and Glory") ~ "Blessings and Glory",
    Site %in% c("Carondalet", "Carondalet ") ~ "Carondelet",
    Site %in% c("COLA Victory Garden", "Cola") ~ "COLA",
    Site %in% c("GROW Spanish Lake", "Spanish Lake", "GROW", "Grow") ~ "Rustic Roots",
    Site %in% c("Kellogg Park", "Kellog") ~ "Kellogg",
    Site %in% c("Emmanuel ") ~ "Emmanuel",
    Site %in% c("Florisant", "Florrisant", "Florissant ") ~ "Florissant",
    Site %in% c("Lady of the Holy Cross (Baden)") ~ "Holy Cross",
    Site %in% c("Mckinley") ~ "McKinley",
    Site %in% c("Old Ferguson West", "Old Ferguson") ~ "Ferguson",
    Site %in% c("Earthdance") ~ "EarthDance",
    Site %in% c("HLS", "House of Living Stone", "House of the Living Stone") ~ "HOLS",
    TRUE ~ Site
  ))

# Loop through each site and create density ridge plots of phenophase by species
unique(merged_df$Site) %>% walk(function(site) {
  # Filter data for current site
  df_site <- merged_df %>% filter(Site == site)
  
  # Keep phenophases that have at least 1 observation
  kept_pheno <- df_site %>%
    count(TreePhenophase) %>%
    filter(n >= 1) %>%
    pull(TreePhenophase)
  df_site <- df_site %>% filter(TreePhenophase %in% kept_pheno)
  if (nrow(df_site) == 0) return(NULL)
  
  # Keep only species with more than 2 observations
  species_to_keep <- df_site %>%
    count(Species) %>%
    filter(n > 2) %>%
    pull(Species)
  df_site <- df_site %>% filter(Species %in% species_to_keep)
  if (nrow(df_site) == 0) return(NULL)
  
  # Remove ambiguous species category
  df_site <- df_site %>% filter(Species != "Pear OR Apple")

  # Order species by their earliest bloom date
  species_order <- df_site %>%
    group_by(Species) %>%
    dplyr::summarize(first_bloom = min(YearlessDate, na.rm = TRUE)) %>%
    arrange(first_bloom) %>%
    pull(Species)
  df_site$Species <- factor(df_site$Species, levels = species_order)
  
  # Set x-axis limits with buffer of 14 days on each side
  min_date <- min(df_site$YearlessDate) - 14
  max_date <- max(df_site$YearlessDate) + 14
  
  # Create density ridge plot for phenophase timing across species and plot
  p <- ggplot(df_site, aes(x = YearlessDate, y = Species, fill = Species)) +
    stat_density_ridges(
      geom = "density_ridges",
      scale = 1.25,
      rel_min_height = 0.0001,
      alpha = 0.9,
      color = "black",
      bandwidth = 5,
      show.legend = FALSE
    ) +
    scale_x_date(
      limits = c(min_date, max_date),
      date_breaks = "1 week",
      date_labels = "%d-%b"
    ) +
    theme_ridges(font_size = 14, grid = FALSE) +
    theme(
      axis.title.x = element_text(size = 10, face = "bold", hjust = 0.5),
      axis.title.y = element_text(size = 10, face = "bold", hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
      axis.text.y = element_text(size = 12, face = "italic"),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      panel.grid = element_blank(),
      panel.background = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    labs(
      title = paste("Phenophase Density Plot -", site), 
      x = "Day of the Year",                            
      y = "Species"                                    
    )
  
  print(p) 
})


# ALL SITES COMBINED
df_all <- merged_df %>%
  filter(Species != "Pear OR Apple") %>%
  group_by(Species) %>%
  filter(n() > 2) %>%
  ungroup()

species_order_all <- df_all %>%
  group_by(Species) %>%
  summarise(first_bloom = min(YearlessDate, na.rm = TRUE)) %>%
  arrange(first_bloom) %>%
  pull(Species)

df_all$Species <- factor(df_all$Species, levels = species_order_all)

p_all <- ggplot(df_all, aes(x = YearlessDate, y = Species, fill = Species)) +
  stat_density_ridges(
    geom = "density_ridges",
    scale = 1.25,
    rel_min_height = 0.0001,
    alpha = 0.9,
    color = "black",
    bandwidth = 5,
    show.legend = FALSE
  ) +
  scale_x_date(
    date_breaks = "1 week",
    date_labels = "%d-%b"
  ) +
  theme_ridges(font_size = 14, grid = FALSE) +
  theme(
    axis.title.x = element_text(size = 10, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 10, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.text.y = element_text(size = 12, face = "italic"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  labs(
    title = "Phenophase Density Plot - All Sites",
    x = "Day of the Year",
    y = "Species"
  )

print(p_all)

