# R Projects

A collection of ecological data-analysis scripts written in R, built for a USDA-funded urban orchard pollination study. The work spans plant phenology (flowering/leaf timing), pollinator activity, and local weather, and turns years of raw field-observation CSVs into cleaned datasets, statistical models, and publication-style plots.

---

## Repository structure

```
R-Projects/
├── Phenology Files/                   # Core phenology + insect dataframe building & modeling
├── Density plot for pheno and insect/ # Kernel density visualizations
├── Pheno and Insect Ring plot/        # Circular/ring seasonal plots
└── USDA/                              # Weather regression + USDA-linked ring/density plots
```

## How it's done

Every script follows the same basic shape:

1. **Ingest** one or more raw CSVs with `read.csv()` / `read_csv()` (tidyverse), one per field season/year.
2. **Clean & standardize** — parse inconsistent date/time strings with `lubridate::parse_date_time()`, rename messy site/species names to canonical labels with `dplyr::case_when()`, unit-convert (°C→°F, m/s→mph) and drop irrelevant columns.
3. **Reshape** — `merge()`/`dplyr::bind_rows()` across years and data sources (e.g. joining a tree phenology table to an insect-pollinator table by orchard + tree number), and pivot wide weather sheets into long format for plotting.
4. **Model / summarize** — group-by aggregation (`dplyr::group_by` + `summarise`), correlation checks, and for the weather data, linear regression (`lm`) relating weather variables to phenology timing.
5. **Visualize** — `ggplot2` (plus `ggridges`, `gridExtra`, `grid`) to produce histograms, boxplots, kernel density plots, and polar/ring plots of seasonal activity.

## Code & libraries used

`ggplot2`, `dplyr`, `tidyr`, `readr`, `lubridate`, `ggridges`, `gridExtra`, `grid`, `forecast`, `class`, `caret`, `corrplot`, `purrr`, `broom`.

## The algorithms

- **Kernel density estimation** (via `ggplot2::geom_density` / density plot scripts) — estimates a smooth probability density of *when in the year* an event (flowering, insect sighting) happens, so two distributions (e.g. plant bloom timing vs. insect activity) can be visually compared for overlap.
- **Ordinary least squares regression** (`lm()` in `USDA/weather regression.R`) — fits weather variables (temperature, wind speed, humidity, pressure) as predictors of phenological/weather outcomes, after unit-harmonizing every input source to a common scale.
- **Categorical recoding / rule-based classification** (`case_when()` chains) — collapses noisy raw taxonomic labels (e.g. many spellings of an orchard name, or an insect order like "Diptera") into a small set of canonical categories (`BeeType`, standardized `Orchard`) before any modeling happens.
- **Ring/polar plotting** — dates are mapped onto a 365-day circular axis so seasonal peaks in phenology and insect activity can be compared at a glance, independent of calendar year.
