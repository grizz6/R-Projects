# Load necessary libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(forecast)
library(plotly)

# Read the CSV file
data <- read.csv("~/Documents/Ecology /Project R /Pheno Proj/Tree Flowering Phenology - FocalTreeFlwrCounts 2023-07-06.csv", stringsAsFactors = FALSE)

# Convert Date column to Date type
data$Date <- as.Date(data$Date, format = "%d-%b-%y")

# Remove rows with NA values in critical columns
data <- data %>%
  filter(!is.na(Site) & !is.na(Date) & !is.na(TreePhenophase))


#------------------------------------------------------------ Histogram -----------------------------------------------


# Ensure TreePhenoStage is a factor
data$TreePhenophase <- as.factor(data$TreePhenophase)

# Get unique Sites
sites <- unique(data$Site)

# Define the plotting function
plot_histogram <- function(data, site_name) {
  ggplot(data, aes(x = Week, y = Frequency, fill = TreePhenophase)) +
    geom_col(position = "stack", color = "black") +
    labs(title = paste("Frequency of TreePhenophase by Week for Site:", site_name),
         x = "Date", y = "Frequency") +
    theme_minimal() +
    scale_x_date(date_labels = "%d-%b-%Y", date_breaks = "1 week") +  # Show day-month-year on x-axis
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_discrete(name = "Tree Pheno Stage")  # Include fill legend for Tree Pheno Stage
}

# Loop through each Site and plot the histogram
for (site in sites) {
  site_data <- data %>% 
    filter(Site == site) %>%
    group_by(Week = floor_date(Date, "week"), TreePhenophase) %>%
    summarise(Frequency = n(), .groups = 'drop')
  
  # Print plot only if there are records
  if (nrow(site_data) > 0) {
    print(plot_histogram(site_data, site))
  }
}


#------------------------------------------------------------ Lineplot -----------------------------------------------


plot_line <- function(data, site_name) {
  ggplot(data, aes(x = Date, y = Frequency, color = TreePhenophase)) +
    geom_line(size = 1.2) +
    labs(title = paste("Trend of TreePhenophase by Date for Site:", site_name),
         x = "Date", y = "Frequency") +
    theme_minimal() +
    scale_x_date(date_labels = "%d-%m-%Y", date_breaks = "1 week") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_color_discrete(name = "Tree Phenophase")
}

for (site in sites) {
  site_data <- freq_data %>%
    filter(Site == site)
  
  if (nrow(site_data) > 0) {
    print(plot_line(site_data, site))
  }
}


#------------------------------------------------------------ Barplot -----------------------------------------------


ggplot(freq_data, aes(x = Site, y = Frequency, fill = TreePhenophase)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of TreePhenophase Frequencies Across Sites",
       x = "Site", y = "Frequency") +
  theme_minimal() +
  scale_fill_discrete(name = "Tree Phenophase")


#------------------------------------------------------------ Heatmap -----------------------------------------------


ggplot(freq_data, aes(x = Date, y = Site, fill = Frequency)) +
  geom_tile() +
  labs(title = "Heatmap of TreePhenophase Frequency by Date and Site",
       x = "Date", y = "Site") +
  theme_minimal() +
  scale_fill_gradient(low = "white", high = "blue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#----------------------------------------------------- Interactive Visualization -------------------------------------


p <- ggplot(freq_data, aes(x = Date, y = Frequency, fill = TreePhenophase)) +
  geom_col() +
  labs(title = "Interactive TreePhenophase Frequency",
       x = "Date", y = "Frequency") +
  theme_minimal()

ggplotly(p)


#----------------------------------------------------- Faceted Plots -------------------------------------------------


ggplot(freq_data, aes(x = Date, y = Frequency, fill = TreePhenophase)) +
  geom_col() +
  facet_wrap(~ Site) +
  labs(title = "TreePhenophase Frequency Across Sites",
       x = "Date", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#----------------------------------------------------- Scatterplot -------------------------------------------------



