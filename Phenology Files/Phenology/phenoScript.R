library(datasets)
library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)
library(lubridate)
library(forecast)
library(class)
library(caret)
library(corrplot)

pheno2022 <- read.csv("~/Documents/Ecology /Project R /Pheno Proj/Tree Flowering Phenology - FocalTreeFlwrCounts 2023-07-06.csv")
pheno2023 <- read.csv("~/Documents/Ecology /Project R /Pheno Proj/Tree Flowering Phenology 2023-07-06(TreePhenophase).csv")
pheno2024 <- read.csv("~/Documents/Ecology /Project R /Pheno Proj/Tree Flowering Phenology - TreePhenophase2024.csv")

pheno2022$Date <- as.Date(pheno2022$Date, format = "%d-%b-%y")
pheno2023$Date <- as.Date(pheno2023$Date, format = "%d-%b-%y")
pheno2024$Date <- as.Date(pheno2024$Date, format = "%d-%b-%y")

file_pathsss <- c("~/Documents/Ecology /Project R /Pheno Proj/Tree Flowering Phenology - FocalTreeFlwrCounts 2023-07-06.csv", 
                "~/Documents/Ecology /Project R /Pheno Proj/Tree Flowering Phenology 2023-07-06(TreePhenophase).csv",
                "~/Documents/Ecology /Project R /Pheno Proj/Tree Flowering Phenology - TreePhenophase2024.csv")

#-----------------------------------------------Histogram---------------------------------------------------

generate_histograms <- function(file_paths) {
  histograms <- list()  # List to store histogram plots
  
  for (file_path in file_paths) {
    df <- read.csv(file_path)
    
    # Ensure 'Date' column is in the correct format and remove non-finite values
    df$Date <- as.Date(df$Date, format = "%d-%b-%y")
    df <- df[!is.na(df$Date), ]
    
    # Generate histogram
    hist_plot <- ggplot(df, aes(x = Date)) + 
      geom_histogram(binwidth = 1, fill = "green", color = "orange") +
      labs(title = paste("Histogram of Dates -", basename(file_path)), x = "Date", y = "Frequency") +
      theme_minimal()
    
    histograms[[length(histograms) + 1]] <- hist_plot  # Add histogram to the list
  }
  
  # Arrange histograms in a grid layout and display them on a single page
  do.call("grid.arrange", c(histograms, ncol = 1))
}

# Example usage:
file_paths <- c("~/Documents/Ecology /Project R /Pheno Proj/Tree Flowering Phenology - FocalTreeFlwrCounts 2023-07-06.csv", 
                "~/Documents/Ecology /Project R /Pheno Proj/Tree Flowering Phenology 2023-07-06(TreePhenophase).csv",
                "~/Documents/Ecology /Project R /Pheno Proj/Tree Flowering Phenology - TreePhenophase2024.csv")
generate_histograms(file_paths)

#-----------------------------------------------Boxplot---------------------------------------------------

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Function to generate box plots
generate_boxplots <- function(file_paths) {
  
  process_file <- function(file_path) {
    # Read the CSV file
    df <- read.csv(file_path)
    
    # Check if the required columns exist
    required_columns <- c("Date", "TreePhenoStage", "Site")
    if (!all(required_columns %in% colnames(df))) {
      message(paste("Required columns are missing in", file_path))
      return(NULL)
    }
    
    # Convert the Date column to Date type
    df$Date <- as.Date(df$Date, format="%d-%b-%y")
    
    # Calculate the frequency of TreePhenoStage occurrences
    df_freq <- df %>%
      group_by(Date, Site) %>%
      summarise(Frequency = n(), .groups = 'drop')
    
    # Boxplot of Frequency vs. Date
    box_plot1 <- ggplot(df_freq, aes(x = as.factor(Date), y = Frequency)) + 
      geom_boxplot(fill = "blue", color = "black") +
      labs(title = paste("Box Plot of Frequency vs. Date -", file_path),
           x = "Date", y = "Frequency") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    # Boxplot of Frequency vs. Site
    box_plot2 <- ggplot(df_freq, aes(x = as.factor(Site), y = Frequency)) + 
      geom_boxplot(fill = "red", color = "black") +
      labs(title = paste("Box Plot of Frequency vs. Site -", file_path),
           x = "Site", y = "Frequency") +
      theme_minimal()
    
    list(box_plot1, box_plot2)
  }
  
  # Process each file and collect box plots
  all_plots <- lapply(file_paths, process_file)
  
  # Filter out NULL entries
  all_plots <- all_plots[!sapply(all_plots, is.null)]
  
  # Display all box plots
  par(mfrow = c(length(file_paths), 2))
  for (plots in all_plots) {
    for (plot in plots) {
      print(plot)
    }
  }
  par(mfrow = c(1, 1))  # Reset the plotting area
}

# Define file paths for the 3 CSV files
file_paths <- c("~/Documents/Ecology /Project R /Pheno Proj/Tree Flowering Phenology - FocalTreeFlwrCounts 2023-07-06.csv", 
                "~/Documents/Ecology /Project R /Pheno Proj/Tree Flowering Phenology 2023-07-06(TreePhenophase).csv",
                "~/Documents/Ecology /Project R /Pheno Proj/Tree Flowering Phenology - TreePhenophase2024.csv")

# Call the function with the file paths
generate_boxplots(file_paths)


#-----------------------------------------------Scatterplot---------------------------------------------------


# Function to generate scatter plots
generate_scatterplots <- function(file_paths) {
  
  process_file <- function(file_path) {
    # Read the CSV file
    df <- read.csv(file_path)
    
    # Check if the required columns exist
    required_columns <- c("Date", "TreePhenoStage", "Site")
    if (!all(required_columns %in% colnames(df))) {
      message(paste("Required columns are missing in", file_path))
      return(NULL)
    }
    
    # Convert the Date column to Date type
    df$Date <- as.Date(df$Date, format="%d-%b-%y")
    
    # Calculate the frequency of TreePhenoStage occurrences
    df_freq <- df %>%
      group_by(Date, Site) %>%
      summarise(Frequency = n(), .groups = 'drop')
    
    # Scatter plot of Frequency vs. Date
    scatter_plot1 <- ggplot(df_freq, aes(x = Date, y = Frequency)) + 
      geom_point(color = "blue") +
      labs(title = paste("Scatter Plot of Frequency vs. Date -", file_path),
           x = "Date", y = "Frequency") +
      theme_minimal()
    
    # Scatter plot of Frequency vs. Site
    scatter_plot2 <- ggplot(df_freq, aes(x = Site, y = Frequency)) + 
      geom_point(color = "red") +
      labs(title = paste("Scatter Plot of Frequency vs. Site -", file_path),
           x = "Site", y = "Frequency") +
      theme_minimal()
    
    list(scatter_plot1, scatter_plot2)
  }
  
  # Process each file and collect scatter plots
  all_plots <- lapply(file_paths, process_file)
  
  # Filter out NULL entries
  all_plots <- all_plots[!sapply(all_plots, is.null)]
  
  # Display all scatter plots
  par(mfrow = c(length(file_paths), 2))
  for (plots in all_plots) {
    for (plot in plots) {
      print(plot)
    }
  }
  par(mfrow = c(1, 1))  # Reset the plotting area
}

# Define file paths for the 3 CSV files
file_paths <- c("~/Documents/Ecology /Project R /Pheno Proj/Tree Flowering Phenology - FocalTreeFlwrCounts 2023-07-06.csv", 
                "~/Documents/Ecology /Project R /Pheno Proj/Tree Flowering Phenology 2023-07-06(TreePhenophase).csv",
                "~/Documents/Ecology /Project R /Pheno Proj/Tree Flowering Phenology - TreePhenophase2024.csv")

# Call the function with the file paths
generate_scatterplots(file_paths)


#----------------------------------------------- Correlation Plot ---------------------------------------------------

# Function to generate correlation matrix plots
generate_correlation_plots <- function(file_paths) {
  
  process_file <- function(file_path) {
    # Read the CSV file
    df <- read.csv(file_path)
    
    # Print the structure of the data frame for debugging
    print(paste("Columns in", file_path, ":", paste(names(df), collapse = ", ")))
    
    # Convert columns to numeric where possible
    df <- df %>% 
      mutate(across(everything(), ~ as.numeric(as.character(.)), .names = "numeric_{col}"))
    
    # Remove columns with all NAs after conversion
    df <- df %>% select(where(~ !all(is.na(.))))
    
    # Check if the data frame has at least one numeric column
    numeric_cols <- df %>%
      select(where(is.numeric))
    
    # Print the structure of numeric columns for debugging
    print(paste("Numeric columns in", file_path, ":", paste(names(numeric_cols), collapse = ", ")))
    
    if (ncol(numeric_cols) < 2) {
      message(paste("Not enough numeric columns in", file_path))
      return(NULL)
    }
    
    # Calculate the correlation matrix
    corr_matrix <- cor(numeric_cols, use = "complete.obs")
    
    # Plot the correlation matrix
    corr_plot <- corrplot(corr_matrix, method = "circle", 
                          tl.col = "black", 
                          title = paste("Correlation Matrix -", file_path))
    
    return(corr_plot)
  }
  
  # Process each file and collect correlation plots
  all_plots <- lapply(file_paths, process_file)
  
  # Filter out NULL entries
  all_plots <- all_plots[!sapply(all_plots, is.null)]
  
  # Display all correlation plots
  if (length(all_plots) > 0) {
    # Set up the plotting area for multiple plots
    par(mfrow = c(1, length(all_plots)))
    for (plot in all_plots) {
      print(plot)
    }
    par(mfrow = c(1, 1))  # Reset the plotting area
  } else {
    message("No correlation plots to display.")
  }
}

# Define file paths for the CSV files
file_paths <- c("~/Documents/Ecology /Project R /Pheno Proj/Tree Flowering Phenology - FocalTreeFlwrCounts 2023-07-06.csv", 
                "~/Documents/Ecology /Project R /Pheno Proj/Tree Flowering Phenology 2023-07-06(TreePhenophase).csv",
                "~/Documents/Ecology /Project R /Pheno Proj/Tree Flowering Phenology - TreePhenophase2024.csv")

# Call the function with the file paths
generate_correlation_plots(file_paths)

