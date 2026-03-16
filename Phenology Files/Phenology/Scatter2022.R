# Load necessary libraries
library(ggplot2)
library(dplyr)
library(lubridate)

# Read the CSV file (replace "data.csv" with your file path)
data <- read.csv("~/Documents/Ecology /Project R /Pheno Proj/Tree Flowering Phenology - FocalTreeFlwrCounts 2023-07-06.csv", stringsAsFactors = FALSE)


# Ensure Date column is correctly formatted
data$Date <- dmy(data$Date)  # Convert date format using lubridate's dmy function

# Remove rows with NA or 'N/A' values in TreePhenoStage and critical columns
data <- data %>%
  filter(!is.na(Site) & !is.na(Date) & !is.na(TreePhenoStage) & TreePhenoStage != "" & TreePhenoStage != "N/A")

# Calculate the frequency of each TreePhenoStage per date and site
freq_data <- data %>%
  group_by(Date, TreePhenoStage) %>%
  summarise(Frequency = n(), .groups = 'drop')

# Ensure TreePhenoStage is a factor for consistent plotting
freq_data$TreePhenoStage <- as.factor(freq_data$TreePhenoStage)

# Define the plotting function for overall data
plot_overall_scatterplot <- function(data) {
  max_freq <- max(data$Frequency, na.rm = TRUE)  # Find the maximum frequency value
  
  ggplot(data, aes(x = Date, y = Frequency, color = TreePhenoStage)) +
    geom_point(size = 3) +
    labs(title = "Overall Frequency of TreePhenoStage by Date",
         x = "Date", y = "Frequency") +
    theme_minimal() +
    scale_x_date(date_labels = "%d-%m-%Y", date_breaks = "1 week") +  # Show day-month-year on x-axis
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(breaks = seq(0, max_freq, by = 5), limits = c(0, max_freq)) +  # Set y-axis breaks to 5, 10, 15, etc. and cover entire range
    scale_color_discrete(name = "TreePhenoStage")  # Include color legend for Tree TreePhenoStage
}

# Plot the overall scatterplot
plot_overall_scatterplot(freq_data)
