library(shiny)
library(shinydashboard)

library(shinyBS)
library(leaflet)
library(plotly)

ui <- navbarPage(
    title=div(img(src="crea_logo.svg",
                  height=44)),

    windowTitle="CREA - Air Quality Dashboard",
    theme = "theme.css",
    id = "nav-page",

    source(file.path("ui", "tab_cities.R"),  local = TRUE)$value,
    # source(file.path("ui", "tab_provinces.R"),  local = TRUE)$value,
    source(file.path("ui", "tab_about.R"),  local = TRUE)$value
)