# Load libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(pollen) # For GDD calculation
library(readr)
library(lmerTest)
library(ggplot2)

# Load the csv files
ws2022 <- read_csv("~/Documents/Ecology /USDA Grant/weather data/Weather Regression/Weather Database - 2022.csv")
ws2023 <- read_csv("~/Documents/Ecology /USDA Grant/weather data/Weather Regression/Weather Database - 2023.csv")
ws2024 <- read_csv("~/Documents/Ecology /USDA Grant/weather data/Weather Regression/Weather Database - 2024.csv")
weather_reg <- read_csv("~/Documents/Ecology /USDA Grant/weather data/Weather Regression/weather_regression_results.csv")
gis_data <- read_csv("~/Documents/Ecology /USDA Grant/weather data/Weather Regression/Orchards_GISdata_October_2023 (USE THIS with PC1) - Site information.csv")

# Merge the weatherstation data from 2022, 2023, 2024
ws_data <- bind_rows(ws2022, ws2023, ws2024)

# Filter to only temperature for merging and regressions
temp_reg <- weather_reg %>%
  filter(Variable == "Temperature") %>%
  dplyr::select(Location, Slope, Intercept)

# Merge regression coefficients to WS data
ws_pred <- ws_data %>%
  crossing(temp_reg)%>%
  mutate(TEMP_pred = Slope * `AVG TEMP F` + Intercept) %>%
  dplyr::select("Location", "Slope", "Intercept", everything() )

# Calculate GDH10 (hourly above 50F)
threshold <- 50

gdh_df <- ws_pred %>%
  group_by(Location, YEAR) %>%
  summarise(GDH50 = sum(TEMP_pred > threshold), .groups="drop")

# Calculate GDD10 (daily sum above 50F)
gdd_df <- ws_pred %>%
  # Build proper date from your columns
  mutate(
    date = as.Date(paste(YEAR, MONTH, DAY, sep = "-"))
  ) %>%
  
  # Convert hourly → daily Tmin & Tmax from TEMP_pred
  group_by(Location, YEAR, date) %>%
  summarise(
    Tmin = min(TEMP_pred, na.rm = TRUE),
    Tmax = max(TEMP_pred, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  # Apply pollen GDD formula (base = 50°F, NO upper cap)
  mutate(
    GDD50_daily = gdd(Tmin, Tmax, tbase = threshold, tbase_max = Inf)
  ) %>%
  
  # Cumulative GDD50 per orchard & year
  group_by(Location, YEAR) %>%
  summarise(
    GDD50 = sum(GDD50_daily, na.rm = TRUE),
    .groups = "drop"
  )

# Merge GDH and GDD
climate_summary <- gdh_df %>%
  left_join(gdd_df, by=c("Location","YEAR"))

# Rename GIS orchard names to easy site names
gis_data <- gis_data %>%
  rename(Location = `Project Name`)

gis_data <- gis_data %>%
  mutate(Location = case_when(
    Location == "Our Lady of the Holy Cross Catholic Church Orchard" ~ "Holy Cross",
    Location == "COLA Victory Garden" ~ "COLA",
    Location == "Rustic Roots (formerly GROW)" ~ "Rustic Roots",
    Location == "Old Ferguson West" ~ "Ferguson",
    Location == "Emmanuel Episcopal Church Orchard" ~ "Emmanuel",
    Location == "Kellogg Park Community Garden and Orchard" ~ "Kellogg",
    Location == "Thies Farm and Greenhouses, Inc." ~ "Thies",
    Location == "House of Living Stone" ~ "HOLS",
    Location == "Florissant Community Garden" ~ "Florissant",
    Location == "St. Louis University Teaching Garden and Orchard" ~ "SLU",
    Location == "McKinley Meadows" ~ "McKinley",
    Location == "13th Street Community Garden" ~ "13th Street",
    Location == "The Orchard on Virginia" ~ "Virginia",
    Location == "Carondalet Food Forest" ~ "Carondelet",
    TRUE ~ Location
  ))


# Merge with GIS data
climate_gis <- climate_summary %>%
  left_join(gis_data, by=c("Location"))

# Figure 1 - Growing Degree Hours vs Urbanization

library(ggplot2)

gdh_plot_data <- climate_gis %>%
  filter(
    !is.na(GDH50),
    !is.na(`PC1 500m`)
  )

gdh_plot <- ggplot(
  gdh_plot_data,
  aes(
    x = `PC1 500m`,
    y = GDH50
  )
) +
  geom_point(
    size = 3,
    alpha = 0.8,
    color = "steelblue"
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "firebrick",
    linewidth = 1
  ) +
  labs(
    title = "Growing Degree Hours by Urbanization",
    x = "Urbanization (PC1, 500 m)",
    y = "Growing Degree Hours (GDH50)"
  ) +
  theme_classic(base_size = 14)

print(gdh_plot)

ggsave(
  "GDH_vs_PC1.png",
  gdh_plot,
  width = 8,
  height = 6,
  dpi = 300
)


# Figure 2 - Growing Degree Days vs Urbanization

gdd_plot_data <- climate_gis %>%
  filter(
    !is.na(GDD50),
    !is.na(`PC1 500m`)
  )

gdd_plot <- ggplot(
  gdd_plot_data,
  aes(
    x = `PC1 500m`,
    y = GDD50
  )
) +
  geom_point(
    size = 3,
    alpha = 0.8,
    color = "steelblue"
  ) +
  labs(
    title = "Growing Degree Days by Urbanization",
    x = "Urbanization (PC1, 500 m)",
    y = "Growing Degree Days (GDD50)"
  ) +
  theme_classic(base_size = 14)

print(gdd_plot)

ggsave(
  "GDD_vs_PC1.png",
  gdd_plot,
  width = 8,
  height = 6,
  dpi = 300
)

# Run mixed-effects regression: GDH50 ~ PC1_500m + (1|YEAR)
model_gdh <- lmer(GDH50 ~ `PC1 500m` + (1|YEAR), data = climate_gis)
summary(model_gdh)

# Same for GDD50
model_gdd <- lmer(GDD50 ~ `PC1 500m` + (1|YEAR), data = climate_gis)
summary(model_gdd)


# write the methods in my own words
# find a paper that does seems to do alike