# Load the packages, if you wanna know which function uses what package; just google i'll be honest
library(dplyr)
library(ggplot2)
library(ggridges)
library(purrr)
library(lubridate)

# Load the dataset (Change the directory)
df1 <- read.csv("ALL tree database - Tree_Master_Updated Earthdance_COLA_Carondelet.csv")
df2 <- read.csv("Insect pollinator data - Pollen Count_Pollinator Data.csv")

# Rename columns for consistency
df1 <- df1 %>% rename(TreeNumber = Tree)

# Merge df1 and df2 on common columns: Orchard and TreeNumbers
merged_df <- merge(df1, df2, by = c("Orchard", "TreeNumber"))

# Rename the column name cause its long
merged_df <- merged_df %>%
  rename(Species = Species..2024.corrected.by.Michael.Hayes.) # You might have to correct it with smth like Species =  (`Species 2024 corrected by Michael Hayes`)    

# Rename Orchard names to its common name
merged_df <- merged_df %>%
  mutate(Orchard = case_when(
    Orchard %in% c("Blessings and Glory") ~ "Blessings and Glory",
    Orchard %in% c("Carondalet") ~ "Carondelet",
    Orchard %in% c("COLA Victory Garden") ~ "COLA",
    Orchard %in% c("GROW Spanish Lake", "Spanish Lake") ~ "Rustic Roots",
    Orchard %in% c("Kellogg Park") ~ "Kellogg",
    Orchard %in% c("Lady of the Holy Cross (Baden)") ~ "Holy Cross",
    Orchard %in% c("Our Lady of Holy Cross") ~ "Holy Cross",
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


# Density Ridge Plot for Insect Types (OTU)

# Ensure Date column is in Date format
merged_df2 <- df2 %>%
  mutate(
    Date = dmy(Date),                                  # Convert Date column to proper date format
    DateFormatted = format(Date, "%d-%b-%Y"),         
    YearlessDate = as.Date(paste0("2000-", month(Date), "-", day(Date))) 
  )  %>%
  filter(!is.na(OTU)) %>%
  group_by(OTU) %>%
  filter(n() > 0) %>%            
  ungroup()

merged_df2 <- merged_df2 %>%
  mutate(OTU = if_else(Insect == "Apis mellifera", "Apis mellifera", OTU))

# Order OTUs by first observation date
otu_order <- merged_df2 %>%
  group_by(OTU) %>%
  summarise(first_obs = min(YearlessDate, na.rm = TRUE)) %>%
  arrange(first_obs) %>%
  pull(OTU)

# Renaming the insects
merged_df2 <- merged_df2 %>%
  mutate(
    OTU = case_when(
      grepl("Apis mellifera", OTU, ignore.case = TRUE) ~ "Honey Bees",
      grepl("Bee", OTU, ignore.case = TRUE) ~ "Bees",
      grepl("Wasp", OTU, ignore.case = TRUE) ~ "Wasps",
      grepl("Ant", OTU, ignore.case = TRUE) ~ "Ants",
      grepl("diptera", OTU, ignore.case = TRUE) ~ "Flies",
      grepl("coleoptera", OTU, ignore.case = TRUE) ~ "Beetles",
      grepl("butterfly", OTU, ignore.case = TRUE) ~ "Butterflies",
      grepl("hemiptera", OTU, ignore.case = TRUE) ~ "True Bugs",
      grepl("sawfly", OTU, ignore.case = TRUE) ~ "Sawflies",
      TRUE ~ "Other"
    )
  ) %>%
  filter(OTU != "Ants") 

# Order insect types (OTU) by earliest observation date
otu_order <- merged_df2 %>%
  group_by(OTU) %>%
  summarise(first_obs = min(YearlessDate, na.rm = TRUE)) %>%
  arrange(first_obs) %>%
  pull(OTU)

merged_df2$OTU <- factor(merged_df2$OTU, levels = otu_order)


# Function to create site-specific density ridge plot
plot_insect_density_by_site <- function(data) {
  
  # Loop through each orchard (Site)
  unique(data$Orchard) %>% walk(function(site_name) {
    
    # Filter data for current orchard
    df_site <- data %>% filter(Orchard == site_name)
    
    # Filter out OTUs with fewer than 2 observations
    otu_to_keep <- df_site %>%
      count(OTU) %>%
      filter(n > 2) %>%
      pull(OTU)
    df_site <- df_site %>% filter(OTU %in% otu_to_keep)
    
    # Skip if not enough records
    if (nrow(df_site) < 2) {
      message("Skipping ", site_name, " (too few records)")
      return(NULL)
    }
    
    # Order OTUs by first observation date within this orchard
    otu_order <- df_site %>%
      group_by(OTU) %>%
      summarise(first_obs = min(YearlessDate, na.rm = TRUE)) %>%
      arrange(first_obs) %>%
      pull(OTU)
    
    df_site$OTU <- factor(df_site$OTU, levels = otu_order)
    
    # Define x-axis limits (add ±14 days buffer)
    min_date <- min(df_site$YearlessDate, na.rm = TRUE) - 14
    max_date <- max(df_site$YearlessDate, na.rm = TRUE) + 14
    
    # Create density ridge plot for this orchard
    p <- ggplot(df_site, aes(x = YearlessDate, y = OTU, fill = OTU)) +
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
        title = paste("Insect Observation Density -", site_name),
        x = "Observation Date",
        y = "Insect Type (OTU)"
      )
    
    print(p)
  })
}

# Run the function on your processed dataset
plot_insect_density_by_site(merged_df2)


# Set x-axis limits with buffer of 14 days on each side
min_date <- as.Date("2000-02-07")
max_date <- as.Date("2000-06-05")

# Insect density plot for all Orchards
p_insects <- ggplot(merged_df2, aes(x = YearlessDate, y = OTU, fill = OTU)) +
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
    title = "Insect Activity Density Plot - All Orchards",
    x = "Day of the Year",
    y = "Insect Type (OTU)"
  )

print(p_insects)




