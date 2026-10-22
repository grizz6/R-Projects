# R Projects: Urban Orchard Phenology, Pollinators, and Microclimate

## The goal

R scripts from a USDA-funded study of community orchards in St. Louis. I wanted to
understand how flowering timing, pollinator activity, and local temperature vary across
sites with different levels of urbanization, using three field seasons (2022 to 2024) of raw
survey spreadsheets.

## What I achieved

- **One consistent dataset from many field methods.** The scripts reshape and merge weekly
  flowering sheets, insect netting, camcorder, GoPro, and bee-hotel counts into site by year
  tables, standardize site and species names, and count survey dates per site so sites can
  be compared fairly.
- **A picture of who pollinates the orchards.** The ring charts show each insect type's share
  of pollinator observations, overall and per orchard. In the project's field report, honey
  bees made up 33.5% of insects collected, other bees 42.7%, and flies 16%.
- **Bloom and pollinator timing on one calendar.** Ridgeline density plots line up when each
  insect type is active against when trees flower, for every orchard and all orchards
  together.
- **Site temperatures where loggers had gaps.** I calibrated each orchard's on-site readings
  against a nearby weather station with a linear regression per site. Station temperature
  tracked the orchards closely (R² of 0.84 or higher at 12 of 15 sites), while wind speed did
  not, so only temperature is backcast. Applying each site's calibration to the full hourly
  station record gives an estimated hourly temperature for every orchard.
- **Growing degree metrics per site and year** (hours above 50 °F, and daily degree days),
  plotted against each site's urbanization score and modeled with a mixed-effects model that
  treats year as a random effect.

Field data is not included. Most scripts read CSVs from paths under `~/Documents/Ecology /...`,
so point the `read.csv()` lines at your own copies first.

## Repository

| Folder | What it covers |
|---|---|
| `Phenology Files/` | Tree and forb flowering by week and site, survey summaries, merged survey counts |
| `Density plot for pheno and insect/` | When insects are active vs. when trees bloom |
| `Pheno and Insect Ring plot/` | Share of each insect type, overall and per orchard |
| `USDA/` | Weather calibration, temperature backcast, and degree-day analysis |

Built with R: `tidyverse`, `data.table`, `lubridate`, `ggridges`, `pollen`, `lmerTest`.

## License

MIT. See [`LICENSE`](LICENSE).
