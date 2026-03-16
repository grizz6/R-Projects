library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)

# Read the data
data <- read.csv("~/Documents/Ecology /Project R /Pheno Proj/Tree Flowering Phenology - FocalTreeFlwrCounts 2023-07-06.csv", stringsAsFactors = FALSE)

# Convert Date column to Date type
data$Date <- as.Date(data$Date, format = "%d-%b-%y")

# Create new columns for Month
data$Month <- month(data$Date, label = TRUE)

# Aggregate data by Month and TreePhenoStage, including NA values
agg_data <- data %>%
  group_by(Month, TreePhenoStage) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  complete(Month, TreePhenoStage, fill = list(Count = 0))  # Fill in missing combinations with 0

# Check the structure of aggregated data
print(head(agg_data))

# Create the plot for each Month
plot <- ggplot(agg_data, aes(x = TreePhenoStage, y = Count, fill = TreePhenoStage)) +
  geom_col(color = "black") +
  facet_wrap(~ Month, scales = "free_y") +
  labs(title = "Number of Records by Tree Phenophase for Each Month", x = "Tree Phenophase Stage", y = "Number of Records") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the plot
print(plot)

#------------------------------------------------------------------------------------------

data <- data %>%
  filter(!is.na(Site) & !is.na(Date) & !is.na(TreePhenoStage))

# Aggregate data by Site, Month, and TreePhenoStage
agg_data <- data %>%
  group_by(Site, Month, TreePhenoStage) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  complete(Site, Month, TreePhenoStage, fill = list(Count = 0))  # Fill in missing combinations with 0

# Filter to include only months where Count > 0
filtered_data <- agg_data %>%
  filter(Count > 0)

# Print the unique months where records exist for each site
filtered_data %>%
  group_by(Site) %>%
  summarise(ExistingMonths = paste(unique(Month), collapse = ", ")) %>%
  print()

# Create a plot for each Site
plot_list <- filtered_data %>%
  split(.$Site) %>%  # Split data by Site
  lapply(function(df) {
    ggplot(df, aes(x = TreePhenoStage, y = Count, fill = TreePhenoStage)) +
      geom_col(color = "black") +
      facet_wrap(~ Month, scales = "free_y") +
      labs(title = paste("Number of Records by Tree Phenophase for Site:", unique(df$Site)),
           x = "Tree Phenophase Stage",
           y = "Number of Records") +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +  # Set y-axis to integer labels
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 0, hjust = 1))
  })

# Print each plot
lapply(plot_list, print)
