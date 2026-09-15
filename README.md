# R Projects: Urban Orchard Phenology, Pollinators & Microclimate

## About

R scripts from a USDA-funded study of urban community orchards in St. Louis. The goal is to understand how **flowering timing (phenology)**, **pollinator activity**, and **local temperature** vary across sites with different levels of urbanization. The scripts turn three field seasons (2022–2024) of raw survey spreadsheets into:

- cleaned, merged datasets and per-site/year survey summaries
- plots of when trees and forbs flower and when insects are active
- a site-level microclimate model that estimates growing degree days from weather-station data

Field data is not included. Most scripts read CSVs from hard-coded paths under `~/Documents/Ecology /...`, so point the `read.csv()` lines at your own copies first.

---

## Repository structure

```
R-Projects/
├── Phenology Files/
│   ├── Phenology/                         # tree & forb flowering analysis, survey summaries
│   │   ├── Pheno2022.R / 2023 / 2024      # weekly phenophase histograms + line plots per site
│   │   ├── Scatter2022.R / 2023 / 2024    # phenophase frequency by date
│   │   ├── phenoScript.R                  # histograms, boxplots, correlation plot across years
│   │   ├── trial.R                        # monthly phenophase counts, overall and per site
│   │   ├── plantDiversity.R               # aligns phenophase records with the tree-type database
│   │   ├── forb 2022.R / 2023 / 2024      # reshapes weekly forb presence sheets (wide → long)
│   │   ├── summary_date.R                 # merges survey counts across every method
│   │   ├── Forb Pheno for Webster/        # forb flowering summaries per site/year
│   │   ├── Insect Dataframe/              # netting + camcorder/GoPro/bee-hotel insect counts
│   │   └── Insect Pheno Dataframe/        # sampling-effort tables (flower vs. insect survey dates)
│   ├── Netting file/                      # netting survey summary, all years
│   └── Plant Voucher/                     # flowering plants per site × year × date
├── Density plot for pheno and insect/     # ridgeline density plots (local file paths)
├── Pheno and Insect Ring plot/            # insect-type donut charts (local file paths)
└── USDA/                                  # weather calibration, degree-day backcast,
                                           # and copies of the density/ring scripts
                                           # that read files from the working directory
```

---

## What the analyses do

### 1. Flowering phenology
`Pheno202x.R`, `Scatter202x.R`, `phenoScript.R`, `trial.R`

Reads focal-tree flowering records, drops rows missing a date or phenophase, and counts how many trees were in each flowering stage **per week, month, or survey date, per site**. Output is stacked histograms, line plots, scatterplots, and boxplots showing each orchard's bloom progression. `phenoScript.R` also runs a correlation plot across the numeric phenology fields.

### 2. Survey summaries and data assembly
`forb 202x.R`, `Forb Pheno for Webster/`, `Insect Dataframe/`, `Insect Pheno Dataframe/`, `Netting file/`, `Plant Voucher/`, `summary_date.R`, `plantDiversity.R`

These scripts standardize and merge the different field methods into site × year (or site × year × date) tables:

- **Forbs:** pivots weekly presence sheets from wide to long and fixes plant family and species names.
- **Insects:** combines netting records with camcorder, GoPro, and bee-hotel counts.
- **Sampling effort:** counts distinct survey dates for flowers and for insects at each site and year, so sites can be compared fairly.
- **All methods:** `summary_date.R` writes one merged table, `All Merged Survey Count - Pheno, Netting, Forb, Cams, GoPros, Bee Hotels.csv`.

### 3. When are insects active vs. when trees bloom?
`Density Plot for all Insects - Final.R`, `Density plot for TreePhenophase - Final.R`

- Joins the tree database to pollinator observations by orchard and tree number.
- Standardizes orchard names and groups taxa into insect types (honey bees, other bees, flies, beetles, true bugs, wasps, butterflies, ants, sawflies).
- Strips the year from each date so all three seasons line up on one calendar axis.
- Draws **ridgeline density plots** (`ggridges::stat_density_ridges`), one ridge per insect type or tree phenophase, for each orchard and for all orchards together.

Comparing the insect and tree plots shows how well pollinator activity overlaps with bloom.

### 4. Who visits the flowers?
`All Year and Site Ring Chart - Final.R`

Donut charts (`geom_col` + `coord_polar`) showing the **share of each insect type** among pollinator observations, first across all orchards and then for each orchard.

### 5. Site microclimate and growing degree days
`USDA/weather regression.R` → `USDA/backcast weather.R`

Some orchards have their own weather loggers, but they don't cover every hour of every season. These two scripts use nearby weather-station data to fill that gap.

1. **Harmonize:**
   - Load the on-site abiotic data (2022–2024) and the station database (2022–2025).
   - Pivot the wide sheets to long format and unify date formats.
   - Convert units: °C → °F, and m/s or km/h → mph.
   - Standardize location names, then join on-site and station readings by location and date.
2. **Calibrate:** for each orchard, fit a linear regression of the on-site reading on the station reading, separately for temperature, humidity, and wind speed. Slopes, intercepts, F statistics, p-values, and R² are collected into a results table.
3. **Backcast:** apply each orchard's temperature slope and intercept to the full hourly station record to estimate that orchard's hourly temperature.
4. **Growing degree metrics** (base 50 °F):
   - **GDH50:** hours above 50 °F per site and year
   - **GDD50:** daily growing degree days from daily min/max temperature (`pollen::gdd`), summed per site and year
5. **Relate to urbanization:**
   - Plot GDH50 and GDD50 against each site's urbanization score (`PC1 500m`, from GIS data) and save the figures.
   - Fit mixed-effects models `GDH50 ~ PC1_500m + (1 | YEAR)` and `GDD50 ~ PC1_500m + (1 | YEAR)` with `lmerTest`.

---

## The methods

- **Linear regression calibration** (`lm`): models on-site conditions as `slope × station reading + intercept`, one model per site and variable. R² shows how well the station tracks each orchard.
- **Growing degree days / hours**: accumulated heat above a 50 °F base temperature, a standard way to track plant and insect development. Warmer, more urbanized sites should accumulate them faster.
- **Linear mixed-effects models** (`lmerTest::lmer`): estimate the effect of urbanization on degree days with a random intercept for year, which absorbs season-to-season weather differences.
- **Kernel density estimation** (`ggridges`): a smoothed distribution of when in the calendar year each insect type or phenophase is observed.
- **Wide-to-long reshaping and name standardization** (`tidyr::pivot_longer`, `dplyr::case_when`): turns inconsistent field sheets into tidy tables before any analysis.

## Built with

R: `dplyr`, `tidyr`, `tidyverse`, `readr`, `data.table`, `lubridate`, `stringr`, `purrr`, `ggplot2`, `ggridges`, `gridExtra`, `plotly`, `corrplot`, `pollen`, `lmerTest`.

## License

MIT. See [`LICENSE`](LICENSE).
