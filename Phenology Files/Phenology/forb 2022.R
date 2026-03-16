library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(stringr)
library(data.table)


# Define CSV file paths
file1 <- c(
  "~/Documents/Ecology /Project R /Pheno Proj/Plant Voucher/Plan Voucher 22/Plant Voucher Sum '22.xlsx - Carondalet.csv",
  "~/Documents/Ecology /Project R /Pheno Proj/Plant Voucher/Plan Voucher 22/Plant Voucher Sum '22.xlsx - COLA.csv",
  "~/Documents/Ecology /Project R /Pheno Proj/Plant Voucher/Plan Voucher 22/Plant Voucher Sum '22.xlsx - EarthDance.csv",
  "~/Documents/Ecology /Project R /Pheno Proj/Plant Voucher/Plan Voucher 22/Plant Voucher Sum '22.xlsx - Ferguson.csv",
  "~/Documents/Ecology /Project R /Pheno Proj/Plant Voucher/Plan Voucher 22/Plant Voucher Sum '22.xlsx - Florissant.csv",
  "~/Documents/Ecology /Project R /Pheno Proj/Plant Voucher/Plan Voucher 22/Plant Voucher Sum '22.xlsx - Kellogg.csv",
  "~/Documents/Ecology /Project R /Pheno Proj/Plant Voucher/Plan Voucher 22/Plant Voucher Sum '22.xlsx - Living Stone.csv",
  "~/Documents/Ecology /Project R /Pheno Proj/Plant Voucher/Plan Voucher 22/Plant Voucher Sum '22.xlsx - Orchard on Virginia.csv",
  "~/Documents/Ecology /Project R /Pheno Proj/Plant Voucher/Plan Voucher 22/Plant Voucher Sum '22.xlsx - Rustic Roots.csv",
  "~/Documents/Ecology /Project R /Pheno Proj/Plant Voucher/Plan Voucher 22/Plant Voucher Sum '22.xlsx - SLU.csv"
)

# Read CSV files and add the Location column based on the filename
dfs <- lapply(file1, function(file) {
  df <- read.csv(file)
  # Extract location from filename
  df$Location <- gsub(".*- (.*?)\\.csv", "\\1", basename(file))
  return(df)
})

# Combine all dataframes into one (wide format)
merged_df <- bind_rows(dfs)

# Renaming the Plant Family and Plant Species
merged_df <- merged_df %>%
  rename(
    Plant_Family = X,
    Plant_Species = X.1
  )

# Recode in the column 'X' 
merged_df$Plant_Family <- recode(merged_df$Plant_Family,
                      "Rosaceae " = "Rosaceae",
                      "Malvaceae " = "Malvaceae",
                      "Lamiaceae " = "Lamiaceae",
                      "Brassicaceae " = "Brassicaceae",
                      "Asteraceae " = "Asteraceae",
                      "Plantaginaceae " = "Plantaginaceae")

# Remove empty strings ("") from the 'X' column in merged_df
merged_df$Plant_Family <- na_if(merged_df$Plant_Family, "")

# Recode in the column 'Y' 
merged_df$Plant_Species <- recode(merged_df$Plant_Species,
                      "Sambucus canadensis " = "Sambucus canadensis",
                      "Malephora crocea " = "Malephora crocea",
                      "Allium aflatunense " = "Allium aflatunense",
                      "Allium schoenoprasum " = "Allium schoenoprasum",
                      "Ilex decidua " = "Ilex decidua",
                      "Kniphofia uvaria " = "Kniphofia uvaria",
                      "Achilea millefolium " = "Achilea millefolium",
                      "Calendula officinalis " = "Calendula officinalis",
                      "Carduus nutans " = "Carduus nutans",
                      "Penstemon digitalis " = "Penstemon digitalis",
                      "Paeonia lactiflora " = "Paeonia lactiflora",
                      "Oenothera fruticosa " = "Oenothera fruticosa",
                      "Callirhoe digitata " = "Callirhoe digitata",
                      "Stachys byzantina " = "Stachys byzantina",
                      "Tulipa agenesis " = "Tulipa agenesis",
                      "Lamium amplexicaule " = "Lamium amplexicaule",
                      "Glechoma hederacea " = "Glechoma hederacea",
                      "Iris psuedacorus " = "Iris psuedacorus",
                      "Rubus pensilvanicus " = "Rubus pensilvanicus",
                      "Rosa alba " = "Rosa alba",
                      "Physocarpus opulifolius " = "Physocarpus opulifolius",
                      "Fragaria ananassa " = "Fragaria ananassa",
                      "Duchesnnea indica " = "Duchesnnea indica",
                      "Aquilegia canadensis " = "Aquilegia canadensis",
                      "Rubus parviflorus " = "Rubus parviflorus",
                      "Ranunculus fascicularis " = "Ranunculus fascicularis",
                      "Aquilegia vulgaris " = "Aquilegia vulgaris",
                      "Anemonne canadensis " = "Anemonne canadensis",
                      "Plantago lanceolata " = "Plantago lanceolata",
                      "Ajuga reptans " = "Ajuga reptans",
                      "Hydrangea quercifolia " = "Hydrangea quercifolia",
                      "Hydrangea arborescens " = "Hydrangea arborescens",
                      "Geranium carolinianum " = "Geranium carolinianum",
                      "Vicia eriocarpa " = "Vicia eriocarpa",
                      "Trifolium repens " = "Trifolium repens",
                      "Melilotus officinalis " = "Melilotus officinalis",
                      "Cornus amomum " = "Cornus amomum",
                      "Tradescantia ohiensis " = "Tradescantia ohiensis",
                      "Tradescantia occidentalis " = "Tradescantia occidentalis",
                      "Lobularia maritima " = "Lobularia maritima",
                      "Brassica montana " = "Brassica montana",
                      "Symphytum officinale " = "Symphytum officinale",
                      "Symphytum asperum " = "Symphytum asperum",
                      "Taraxacum officinale " = "Taraxacum officinale",
                      "Tagetes erecta " = "Tagetes erecta",
                      "Senecio ampullaceus " = "Senecio ampullaceus",
                      "Leucanthemum rotundifolium " = "Leucanthemum rotundifolium",
                      "Lycium barbarum " = "Lycium barbarum",
                      "Amelanchier spicata " = "Amelanchier spicata",
                      "Ranunculus lanuginosus " = "Ranunculus lanuginosus",
                      "Veronica arvensis " = "Veronica arvensis",
                      "Salvia officinalis " = "Salvia officinalis",
                      "Lavandula angustifolia " = "Lavandula angustifolia",
                      "Glechoma hederacea " = "Glechoma hederacea",
                      "Ajuga reptans " = "Ajuga reptans",
                      "Vicia tenuifolia " = "Vicia tenuifolia",
                      "Euphorbia cyparissias " = "Euphorbia cyparissias",
                      "Convolvulus farinosus " = "Convolvulus farinosus",
                      "Convolvulus arvensis " = "Convolvulus arvensis",
                      "Calystegia macrostegia " = "Calystegia macrostegia",
                      "Vicia tenuifolia " = "Vicia tenuifolia",
                      "Euphorbia cyparissias " = "Euphorbia cyparissias",
                      "Stellaria media " = "Stellaria media",
                      "Thlaspi arvense " = "Thlaspi arvense",
                      "Symphytum offcinale " = "Symphytum offcinale",
                      "Borago officinalis " = "Borago officinalis",
                      "Taraxacum offinale " = "Taraxacum offinale",
                      "Sonchus oleraceus " = "Sonchus oleraceus",
                      "Packera glabella " = "Packera glabella",
                      "Leucanthemum vulgare " = "Leucanthemum vulgare",
                      "Errigeron annuus " = "Errigeron annuus",
                      "Echinacea purpurea " = "Echinacea purpurea",
                      "Cyanus segetum blue " = "Cyanus segetum blue",
                      "Cosmos bipinnatus " = "Cosmos bipinnatus",
                      "Coreopsis pubescens " = "Coreopsis pubescens",
                      "Cichorium endivia " = "Cichorium endivia",
                      "Achillea millefolium " = "Achillea millefolium",
                      "Solanum tuberosum " = "Solanum tuberosum",
                      "Cerastium fontanum " = "Cerastium fontanum",
                      "Glechoma hederacea " = "Glechoma hederacea",
                      "Lamium purpureum " = "Lamium purpureum",
                      "Lavandula latifolia " = "Lavandula latifolia",
                      "Tulipa agenensis " = "Tulipa agenensis",
                      "Rosa pendulina " = "Rosa pendulina"
                      )

# Remove empty strings ("") from the 'X' column in merged_df
merged_df$Plant_Species <- na_if(merged_df$Plant_Species, "")

# Check and update column names for week labels
week_labels <- c("2022-03-25", "2022-04-01", "2022-04-08", "2022-04-15", "2022-04-22", "2022-04-29", 
                 "2022-05-06", "2022-05-13", "2022-05-20", "2022-05-28", "2022-06-03", "2022-06-13")

# Verify and rename the columns
old_columns <- c("Prescence.Absence", "X.4", "X.5", "X.6", "X.7", "X.8", 
                 "X.9", "X.10", "X.11", "X.12", "X.13", "X.14")

# Rename columns if they exist
if (all(old_columns %in% names(merged_df))) {
  names(merged_df)[match(old_columns, names(merged_df))] <- week_labels
} else {
  stop("One or more old column names are missing in the dataset.")
}


# Ensure Date is in the correct format
cleaned_df <- merged_df %>%
  select(Location, Plant_Family, Plant_Species, all_of(week_labels)) %>%
  filter(!grepl("No voucher|No vouchers|Plant family|Plant Species", Plant_Family, ignore.case = TRUE)) %>%  
  filter(!grepl("No voucher|No vouchers|Plant family|Plant Species", Plant_Species, ignore.case = TRUE)) %>% 
  filter(!is.na(Plant_Family) & !is.na(Plant_Species)) %>%
  pivot_longer(cols = all_of(week_labels), names_to = "Date", values_to = "Presence") %>%  
  mutate(
    Presence = as.numeric(Presence),
    Site = recode(Location,
                  "Carondalet" = "Carondelet",
                  "Living Stone" = "HOLS",
                  "Orchard on Virginia" = "Virginia")
  ) %>%
  select(Site, Date, Plant_Family, Plant_Species, Presence) %>%
  group_by(Site, Plant_Family, Plant_Species) %>%
  arrange(Site)

# Print the cleaned dataframe
print(cleaned_df, n = 900)

write.csv(cleaned_df, "Forb Pheno 2022.csv", row.names = FALSE)



