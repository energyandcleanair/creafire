# creafire

Shiny dashboard and pipeline scripts for fire/biomass burning impact on air quality.

## What it does

- **Dashboard** (`inst/shiny/`): interactive Shiny app showing fire-related air quality data, deployed to shinyapps.io.
- **Defire pipeline** (`R/defire.R`): orchestrates weather collection, fire-deweathering, and result caching. Designed to run via cron jobs.
- **Flaring** (`R/flaring.R`): downloads and processes NOAA nighttime fire (flaring) data.

## Key dependencies

- [`creadeweather`](https://github.com/energyandcleanair/creadeweather): weather collection, deweathering models, and MongoDB caching of fire-weather/measurement data (db functions).
- [`creatrajs`](https://github.com/energyandcleanair/creatrajs): trajectory computation and fire attachment.
- [`rcrea`](https://github.com/energyandcleanair/rcrea): measurement data and location metadata.

## Setup

Requires a `.Renviron` file with:
- `CREA_MONGODB_URL`: MongoDB connection string for the `creafire` database.
- Shinyapps.io credentials for dashboard deployment.

## To Do
Some fire files are wrong e.g.

/mnt/crea_nfs/fire/firms//suomi-npp-viirs-c2/Global/SUOMI_VIIRS_C2_Global_VNP14IMGTDL_NRT_2024206.txt

is
<!DOCTYPE html>
<!--[if lt IE 7]>      <html class="no-js lt-ie9 lt-ie8 lt-ie7"> <![endif]-->
<!--[if IE 7]>         <html class="no-js lt-ie9 lt-ie8"> <![endif]-->
<!--[if IE 8]>         <html class="no-js lt-ie9"> <![endif]-->
<!--[if gt IE 8]><!--> <html class="no-js"> <!--<![endif]-->
<head>
<meta charset="utf-8">
<meta http-equiv="X-UA-Compatible" content="IE=edge,chrome=1">
<title>Earthdata is currently unavailable</title>
<meta name="description" content="Default HTML5 goodness for NASA EOSDIS sites">
<meta name="viewport" content="width=device-width">
<style type="text/css" media="screen">
html {
-webkit-font-smoothing: antialiased;
}

we should build a script to identify those, delete them, and list gaps where no fire data is available so that we can download those from viirs archive service
