# Load necessary libraries
library(ggplot2)
library(dplyr)
library(lubridate)

# Read the CSV file (replace "data.csv" with your file path)
data <- read.csv("~/Documents/Ecology /Project R /Pheno Proj/Tree Flowering Phenology - TreePhenophase2024.csv", stringsAsFactors = FALSE)


# Ensure Date column is correctly formatted
data$Date <- dmy(data$Date)  # Convert date format using lubridate's dmy function

# Remove rows with NA or 'N/A' values in TreePhenophase and critical columns
data <- data %>%
  filter(!is.na(Site) & !is.na(Date) & !is.na(TreePhenophase) & TreePhenophase != "" & TreePhenophase != "N/A")

# Calculate the frequency of each TreePhenophase per date and site
freq_data <- data %>%
  group_by(Date, TreePhenophase) %>%
  summarise(Frequency = n(), .groups = 'drop')

# Ensure TreePhenophase is a factor for consistent plotting
freq_data$TreePhenophase <- as.factor(freq_data$TreePhenophase)

# Define the plotting function for overall data
plot_overall_scatterplot <- function(data) {
  max_freq <- max(data$Frequency, na.rm = TRUE)  # Find the maximum frequency value
  
  ggplot(data, aes(x = Date, y = Frequency, color = TreePhenophase)) +
    geom_point(size = 3) +
    labs(title = "Overall Frequency of TreePhenophase by Date",
         x = "Date", y = "Frequency") +
    theme_minimal() +
    scale_x_date(date_labels = "%d-%m-%Y", date_breaks = "1 week") +  # Show day-month-year on x-axis
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(breaks = seq(0, max_freq, by = 5), limits = c(0, max_freq)) +  # Set y-axis breaks to 5, 10, 15, etc. and cover entire range
    scale_color_discrete(name = "Tree Phenophase")  # Include color legend for Tree Phenophase
}

# Plot the overall scatterplot
plot_overall_scatterplot(freq_data)
