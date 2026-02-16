# fire_dashboard

This repository contains a Shiny dashboard showing biomass burning attribution results. The dashboard directly fetches data from CREA MongoDB.

Results have been computed and stored through the `creadeweather`.

Dashboard can be deployed to Shinyapps.io through the `deploy_on_shinyapps` function in `shiny.R`.

## Setup

Requires a `.Renviron` file with:
- `CREA_MONGODB_URL`: MongoDB connection string for the `creafire` database.
- Shinyapps.io credentials for dashboard deployment:
    - `SHINYAPP_ACCOUNT`
    - `SHINYAPP_TOKEN`
    - `SHINYAPP_SECRET`

