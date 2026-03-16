# Load required packages
library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(broom)
library(ggplot2)

# Load all csv's
weather_2022 <- read.csv("~/Documents/Ecology /USDA Grant/weather data/Weather Regression/Abiotic Data all years .xlsx - 2022.csv")%>%
  select(-1) # Not considering the first row/column since its all NA
weather_2023 <- read.csv("~/Documents/Ecology /USDA Grant/weather data/Weather Regression/Abiotic Data all years .xlsx - 2023.csv")%>%
  select(-1)
weather_2024 <- read.csv("~/Documents/Ecology /USDA Grant/weather data/Weather Regression/Abiotic Data all years .xlsx - 2024.csv")%>%
  select(-1)
weather_file <- read.csv("~/Documents/Ecology /USDA Grant/weather data/Weather Regression/Weather Data - Sheet1.csv")
weatherdb_2022 <- read.csv("~/Documents/Ecology /USDA Grant/weather data/Weather Regression/Weather Database - 2022.csv")
weatherdb_2023 <- read.csv("~/Documents/Ecology /USDA Grant/weather data/Weather Regression/Weather Database - 2023.csv")
weatherdb_2024 <- read.csv("~/Documents/Ecology /USDA Grant/weather data/Weather Regression/Weather Database - 2024.csv")
weatherdb_2025 <- read.csv("~/Documents/Ecology /USDA Grant/weather data/Weather Regression/Weather Database - 2025.csv")
  
# Convert Date column to uniform DD-MM-YYYY format for weather_file and selecting required column
weather_file <- weather_file %>%
  mutate(
    Date = parse_date_time(Date, orders = c("d/m/Y", "d-b-Y", "m/d/Y")),
    Year = year(Date),
    Date = format(Date, "%d-%m-%Y"),
    
    # Temperature: Celsius -> Fahrenheit
    Temperature..degree.fahrenheit.. = ifelse(
      !is.na(Temperature..degree.celsius..),
      (Temperature..degree.celsius.. * 9/5) + 32,
      NA
    ),
    
    # Wind speed conversions into mph
    Avg..Wind.Speed..mph. = case_when(
      !is.na(Average.wind.speed..m.s.) ~ Average.wind.speed..m.s. * 2.23694,  # m/s → mph
      !is.na(Avg..Wind.Speed..km.h.) ~ Avg..Wind.Speed..km.h. / 1.60934,   # km/h → mph
      TRUE ~ NA_real_
    ),
    
    # Convert text time to POSIXct (meaning changing 9:47 AM into TIME format HH:MM:SS which is 09:47:00, its a function name dw abt it)
    Time_24 = format(parse_date_time(Time, orders = c("h:M p", "H:M")), "%H%M"),
    # Convert to numeric 
    Time_24 = as.numeric(Time_24),
    # Extract last two digits (minutes)
    minutes = Time_24 %% 100,
    # Custom rounding rule: >30 round up, else round down 
    TimeRounded = ifelse(minutes > 30,
                         ceiling(Time_24 / 100) * 100,   # round up
                         floor(Time_24 / 100) * 100)     # round down             
  ) %>%
  select(
    Location, Year, Date, TimeRounded, everything(),
    -any_of(c(
      "Notes", "Collector.Name", "Team", "Weather.condition", # you can remove Time_24 and minutes to see whats really happening with POSIXct
      "X..Trees.Blooming", "Temperature..degree.celsius..",   # you can also remove some of them from this quotations as needed, just removed to maintain clean data
      "Average.wind.speed..m.s.", "Avg..Wind.Speed..km.h.",
      "minutes", "Time_24", "Cloud.cover"
    ))
  ) %>%
  rename(
    `Temperature (F)` = Temperature..degree.fahrenheit..,
    `Humidity (%)` = Relative.humidity.., 
    Pressure = Barometric.Pressure..mmHg,
    `Hour (CST)` = TimeRounded,
   `WindSpeed (mph)` = Avg..Wind.Speed..mph.
  )

# Doing the same as above but for the weatherdb
all_weatherdb <- bind_rows(weatherdb_2022, weatherdb_2023, weatherdb_2024, weatherdb_2025)

all_weatherdb <- all_weatherdb %>%
  mutate(
    Date = format(make_date(YEAR, MONTH, DAY), "%d-%m-%Y")
  ) %>% 
  select(Date, YEAR, everything(), -MONTH, -DAY) %>%
  rename(
    Year = YEAR, 
    `Hour (CST)` = HOUR.CST,
    `Temperature (F)` = AVG.TEMP.F,
    `WindSpeed (mph)` = WIND.SPEED.MPH,
    `Humidity (%)` = AVG.REL.HUMIDITY..,
    Pressure = PRESSURE.INCHES
  )

# ignoring the blank row 14 (the row above Note) and changing the date for 2024 datasheet 
weather_2023 <- weather_2023[-14, ]
weather_2024[1, ] <- gsub("27-Apr-2024", "27-Apr-24", weather_2024[1, ])

# Function to pivot wide into long for weather_2022, weather_2023, and weather_2024
process_weather <- function(weather_data) {
  # Removing note row from the file 2023 
  weather_data <- weather_data[!grepl("^\\s*Note", weather_data[[1]], ignore.case = TRUE), ]
  
  # Extract headers
  dates_row <- weather_data[1, ] %>% unlist() %>% as.character()
  weeks_row <- weather_data[2, ] %>% unlist() %>% as.character()
  types_row <- weather_data[3, ] %>% unlist() %>% as.character()
  
  # Extract data starting from row 4
  data <- weather_data[4:nrow(weather_data), ]
  
  # Build column names
  col_names <- "Location"
  week_counter <- 1
  week_dates <- c()
  
  for (i in 2:ncol(data)) {
    if (!is.na(dates_row[i]) && dates_row[i] != "N/A") {
      current_date <- dates_row[i]
    }
    
    if (!is.na(types_row[i]) && grepl("Temperature", types_row[i], ignore.case = TRUE)) {
      col_names <- c(col_names, paste0("Week_", week_counter, "_Temperature(F)"))
    } else if (!is.na(types_row[i]) && grepl("Weather", types_row[i], ignore.case = TRUE)) {
      col_names <- c(col_names, paste0("Week_", week_counter, "_Weather"))
      # Store the date for this week
      week_dates[paste0("Week_", week_counter)] <- dates_row[i - 1]
      week_counter <- week_counter + 1
    } else {
      col_names <- c(col_names, paste0("Column_", i))
    }
  }
  
  colnames(data) <- col_names
  
  # Remove all-NA columns
  data <- data %>% select(where(~!all(is.na(.))))
  
  # Pivot longer
  long_data <- data %>%
    pivot_longer(
      cols = -Location,
      names_to = c("Week", ".value"),
      names_pattern = "(Week_\\d+)_(.*)"
    ) %>%
    filter(!is.na(Location) & Location != "N/A")
  
  # Add corresponding date for each week
  long_data <- long_data %>%
    mutate(Date = week_dates[Week])
  
  # Convert Date to proper format and extract Year
  long_data <- long_data %>%
    mutate(
      Date = as.Date(Date, format = "%d-%b-%y"),
      Year = year(Date),
      Date = format(Date, "%d-%m-%Y")
    ) %>%
    select(Location, Year, Date, Week, "Temperature(F)", Weather)
  
  return(long_data)
}

# Apply function to all three datasets
weather_2022_long <- process_weather(weather_2022)
weather_2023_long <- process_weather(weather_2023)
weather_2024_long <- process_weather(weather_2024)%>%
  filter(Week != "Week_9")

# Combine into a single dataframe
all_weather_long <- bind_rows(weather_2022_long, weather_2023_long, weather_2024_long) %>% 
  rename(`Temperature (F)` = `Temperature(F)`) %>%
  mutate(
    `Temperature (F)` = as.numeric(`Temperature (F)`)
  )

# Define a named vector for mapping old names to new names
location_mapping <- c(
  "Living Stone" = "HOLS",
  "13th Street " = "13th Street",
  "Carondalet" = "Carondelet",
  "Carondelet " = "Carondelet",
  "HLS" = "HOLS",
  "Holy Cross " = "Holy Cross",
  "Orchard on Virginia" = "Virginia",
  "SLU Orchard" = "SLU",
  "Earthdance" = "EarthDance",
  "Earth Dance" = "EarthDance",
  "Florisant" = "Florissant",
  "Forissant" = "Florissant",
  "Kellog" = "Kellogg",
  "Kelloggg" = "Kellogg",
  "Kellogg " = "Kellogg",
  "Rustic Roots " = "Rustic Roots",
  "Emmanuel " = "Emmanuel",
  "Mckinley" = "McKinley"
)

# Rename locations in weather_file
weather_file <- weather_file %>%
  mutate(Location = recode(Location, !!!location_mapping))

# Rename locations in all_weather_long
all_weather_long <- all_weather_long %>%
  mutate(Location = recode(Location, !!!location_mapping))

# Removing NAs/blank spaces from Time
weather_file <- weather_file %>%
  filter(!is.na(Time) & Time != "")

# Merge dataset by Location and Date
#merged_weather <- bind_rows(all_weather_long, weather_file)
merged_weather <- full_join(weather_file, all_weatherdb,
  by = c("Date", "Hour (CST)", "Year"),
  suffix = c("", "_db")
)

# Removing NAs/blank spaces from Time
merged_weather <- merged_weather %>%
  filter(!is.na(Time) & Time != "") %>%
  select(Location, Year, Date, `Hour (CST)`, Time, everything()) 

merged_weather <- merged_weather %>% select(-Pressure)

# Ensure Location is a factor
merged_weather$Location <- as.factor(merged_weather$Location)

# Run linear regression for each Location
locations <- unique(merged_weather$Location)

for (loc in locations) {
  cat("\n====================\n")
  cat("Location:", loc, "\n")
  cat("====================\n")
  
  # Filter dataset for this location
  site_data <- merged_weather %>%
    filter(Location == loc)
  
  # Temperature regression and plotting
  cat("\n--- Temperature Regression ---\n")
  temp_data <- site_data %>% select(`Temperature (F)`, `Temperature (F)_db`) %>% na.omit()
  model_temp <- lm(`Temperature (F)` ~ `Temperature (F)_db`, data = temp_data)
  print(summary(model_temp))
  
  p_temp <- ggplot(temp_data, aes(x = `Temperature (F)_db`, y = `Temperature (F)`)) +
    geom_point(alpha = 0.6, color = "steelblue") +
    geom_smooth(method = "lm", se = TRUE, color = "red") +
    labs(
      title = paste("Temperature Comparison at", loc),
      x = "Weather Station Temperature (F)",
      y = "Our Weather Data Temperature (F)"
    ) +
    theme_minimal(base_size = 13)
  print(p_temp)
  
  # Humidity regression and plotting
  cat("\n--- Humidity Regression ---\n")
  hum_data <- site_data %>% select(`Humidity (%)`, `Humidity (%)_db`) %>% na.omit()
  model_hum <- lm(`Humidity (%)` ~ `Humidity (%)_db`, data = hum_data)
  print(summary(model_hum))
  
  p_hum <- ggplot(hum_data, aes(x = `Humidity (%)_db`, y = `Humidity (%)`)) +
    geom_point(alpha = 0.6, color = "darkgreen") +
    geom_smooth(method = "lm", se = TRUE, color = "orange") +
    labs(
      title = paste("Humidity Comparison at", loc),
      x = "Weather Station Humidity (%)",
      y = "Our Weather Data Humidity (%)"
    ) +
    theme_minimal(base_size = 13)
  print(p_hum)
  
  # Wind Speed regression and plotting
  cat("\n--- Wind Speed Regression ---\n")
  wind_data <- site_data %>% select(`WindSpeed (mph)`, `WindSpeed (mph)_db`) %>% na.omit()
  model_wind <- lm(`WindSpeed (mph)` ~ `WindSpeed (mph)_db`, data = wind_data)
  print(summary(model_wind))
  
  p_wind <- ggplot(wind_data, aes(x = `WindSpeed (mph)_db`, y = `WindSpeed (mph)`)) +
    geom_point(alpha = 0.6, color = "purple") +
    geom_smooth(method = "lm", se = TRUE, color = "red") +
    labs(
      title = paste("Wind Speed Comparison at", loc),
      x = "Weather Station Wind Speed (mph)",
      y = "Our Weather Data Wind Speed (mph)"
    ) +
    theme_minimal(base_size = 13)
  print(p_wind)
}

# create a table to look into the intercept 
# Location, variables (temp, humidity, wind speed), slope, intercept, F stats, p value, multiple r2

# Create an empty results data frame
results <- data.frame(
  Location = character(),
  Variable = character(),
  Slope = numeric(),
  Intercept = numeric(),
  F_Statistic = numeric(),
  P_Value = numeric(),
  Multiple_R2 = numeric(),
  stringsAsFactors = FALSE
)

# Loop over each Location
locations <- unique(merged_weather$Location)

for (loc in locations) {
  cat("\n====================\n")
  cat("Location:", loc, "\n")
  cat("====================\n")
  
  site_data <- merged_weather %>%
    filter(Location == loc)
  
  # Temperature
  temp_data <- site_data %>% select(`Temperature (F)`, `Temperature (F)_db`) %>% na.omit()
  if (nrow(temp_data) > 1) {
    model_temp <- lm(`Temperature (F)` ~ `Temperature (F)_db`, data = temp_data)
    sum_temp <- summary(model_temp)
    results <- rbind(results, data.frame(
      Location = loc,
      Variable = "Temperature",
      Slope = coef(model_temp)[2],
      Intercept = coef(model_temp)[1],
      F_Statistic = sum_temp$fstatistic[1],
      P_Value = pf(sum_temp$fstatistic[1], sum_temp$fstatistic[2], sum_temp$fstatistic[3], lower.tail = FALSE),
      Multiple_R2 = sum_temp$r.squared
    ))
  }
  
  # Humidity
  hum_data <- site_data %>% select(`Humidity (%)`, `Humidity (%)_db`) %>% na.omit()
  if (nrow(hum_data) > 1) {
    model_hum <- lm(`Humidity (%)` ~ `Humidity (%)_db`, data = hum_data)
    sum_hum <- summary(model_hum)
    results <- rbind(results, data.frame(
      Location = loc,
      Variable = "Humidity",
      Slope = coef(model_hum)[2],
      Intercept = coef(model_hum)[1],
      F_Statistic = sum_hum$fstatistic[1],
      P_Value = pf(sum_hum$fstatistic[1], sum_hum$fstatistic[2], sum_hum$fstatistic[3], lower.tail = FALSE),
      Multiple_R2 = sum_hum$r.squared
    ))
  }
  
  # Wind Speed
  wind_data <- site_data %>% select(`WindSpeed (mph)`, `WindSpeed (mph)_db`) %>% na.omit()
  if (nrow(wind_data) > 1) {
    model_wind <- lm(`WindSpeed (mph)` ~ `WindSpeed (mph)_db`, data = wind_data)
    sum_wind <- summary(model_wind)
    results <- rbind(results, data.frame(
      Location = loc,
      Variable = "Wind Speed",
      Slope = coef(model_wind)[2],
      Intercept = coef(model_wind)[1],
      F_Statistic = sum_wind$fstatistic[1],
      P_Value = pf(sum_wind$fstatistic[1], sum_wind$fstatistic[2], sum_wind$fstatistic[3], lower.tail = FALSE),
      Multiple_R2 = sum_wind$r.squared
    ))
  }
}

# View the final regression summary table
results <- results %>% arrange(Location, Variable)
rownames(results) <- NULL
print(results)
View(results)

# write.csv(results, "weather_regression_results.csv", row.names = FALSE)


