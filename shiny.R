#' @export
run_shiny <- function() {
  appDir <- "inst/shiny"
  shiny::runApp(appDir, display.mode = "normal")
}

#' @export
deploy_on_shinyapps <- function() {
  if(!require(rsconnect)) install.packages('rsconnect')
  if(!require(dotenv)) install.packages('dotenv')
  if(!require(devtools)) install.packages('devtools')

  try(dotenv::load_dot_env())
  try(readRenviron(".Renviron"))
  
  urls <- c(
    "tidyverse/lubridate",
    "hubert-thieriot/splitr",
    "energyandcleanair/creatrajs",
    "energyandcleanair/leaflet.extras2",
    "energyandcleanair/rcrea")

  devtools::install_github(urls, force=F, upgrade="never", auth_token = Sys.getenv("GITHUB_PAT"))

  library(lubridate)
  library(leaflet.extras2)

  rsconnect::setAccountInfo(name=Sys.getenv("SHINYAPP_ACCOUNT"),
                            token=Sys.getenv("SHINYAPP_TOKEN"),
                            secret=Sys.getenv("SHINYAPP_SECRET"))

  # We could deploy like this:
  # rsconnect::deployApp(get_app_dir())
  # but it would miss the auth file that is not pushed to Github

  rsconnect::deployApp("inst/shiny",
                       appName="fire",
                       account = Sys.getenv("SHINYAPP_ACCOUNT"),
                       forceUpdate = TRUE)

}
