require(rcrea)
require(DT)
require(shinyWidgets)
library(shinycssloaders)
library(countrycode)
library(dplyr)

Sys.setenv("GCS_AUTH_FILE"=Sys.getenv("GCS_AUTH_FILE", "keys/gcs.shiny.auth.json"))
Sys.setenv("GCS_DEFAULT_BUCKET"=Sys.getenv("GCS_DEFAULT_BUCKET", "crea-public"))
library(googleCloudStorageR)


# trajectories ------------------------------------------------------------
trajs.bucket <- "crea-public"
trajs.bucket_base_url <- "https://storage.googleapis.com/crea-public/"
trajs.folder <- "data/trajectories"


trajs_gibs_layers <- list(
  "Aerosol Optical Depth"=c("MODIS_Combined_Value_Added_AOD"),
  "True Color"=c(
    # "MODIS_Terra_CorrectedReflectance_TrueColor",
    # "MODIS_Aqua_CorrectedReflectance_TrueColor",
    "VIIRS_SNPP_CorrectedReflectance_TrueColor")
)

sentinel_url = "https://creodias.sentinel-hub.com/ogc/wms/a6993002-3fe5-45f9-8318-e23b1a5f21b1"
sentinel_layers = list("NO2"="NO2VISUALISED")


# Default values
met_type = "gdas1"
height = 10