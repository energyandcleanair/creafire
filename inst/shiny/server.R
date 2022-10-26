library(shiny)
library(shinydashboard)
require(rcrea)
library(lubridate)
library(scales)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras2)

# library(creatrajs)
library(plotly)

server <- function(input, output, session) {

    # URL Management
    observe({
        query <- parseQueryString(session$clientData$url_search)
        if(!is.null(query$tab)) {
            updateNavbarPage(session,
                             "nav-page",
                             selected = query$tab)
        }
    })

    source(file.path("server", "reactive.R"),  local = TRUE)$value
  
    # Tab 1: Trajectories              -----------------------------------------------------
    source(file.path("server", "tab_cities.R"),  local = TRUE)$value
    source(file.path("server", "tab_cities_plots.R"),  local = TRUE)$value
    source(file.path("server", "tab_cities_side_plots.R"),  local = TRUE)$value
    
    # source(file.path("server", "tab_provinces.R"),  local = TRUE)$value
    source(file.path("server", "tab_about.R"),  local = TRUE)$value
}
