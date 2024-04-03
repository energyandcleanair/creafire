

# Some log related functions
# trajs_logs_raw <- ""
trajs_logs <- reactiveValues(msg="")

trajs_add_log <- function(message){
  trajs_logs$msg <- paste(isolate(trajs_logs$msg), "\n", message)
}

# read_gcs_url <- function(url, force_uncache=T){
#   tryCatch({
#     trajs_add_log(url)
#     if(force_uncache){
#       url <- paste0(url,"?a=1") # GCS caches files otherwise
#     }
#     r <- readRDS(url(gsub(" ","%20",url)))
#     trajs_add_log("SUCCESS")
#     r
#   },error=function(e){
#     trajs_add_log("FAILED")
#     return(NULL)
#   })
# }

# Not refreshing for every intermediate step
# when sliding date


# Output Elements --------------------------------------

output$selectInputConfig <- renderUI({
  
  req(available_at_location())
  req(location_id())
  
  row_to_str <- function(row){
    lst=as.list(row); paste0(sprintf("%s=%s", names(lst), unlist(lst)), collapse="|\t")
  }
  
  d <- available_at_location() %>%
    mutate(idx=row_number()) %>% # used to retrieve associated metadata
    filter(location_id==location_id()) %>%
    select(-c(location_id, location_name, country))
  
  choices <- apply(d %>% select(-idx), 1, row_to_str)
  pickerInput("config","Configuration",
              choices=`names<-`(d$idx, choices),
              options = list(`actions-box` = F), multiple = F)
})


output$tableConfigs <- renderDT({
  
  req(available_at_location())
  
  data <- available_at_location() %>%
    distinct(location_id, .keep_all = T) %>%
    select(-c(location_id, location_name, country))
  
  datatable(data,
            rownames = FALSE,
            options = list(
              dom = 't',
              autoWidth = TRUE,
              lengthChange = FALSE,
              deferRender = TRUE,
              scrollY = 200,
              scroller = TRUE,
              ordering=F
            ),
            selection='single',
            extensions = c('Scroller', 'Responsive'),
            )
})

output$radioMode <- renderUI({
  radioButtons("cities_mode", NULL, choices=c("Map"="map", "Charts"="charts"),
               selected="map", inline=T)
})


output$selectInputRunning <- renderUI({
  req(locations()) # Not required, but added so that all ui elements appear together
  
  windows <- list(
    "None"=0,
    "One week (7)"=7,
    "Two weeks (14)"=14,
    "One month (31)"=31,
    "One year (365)"=365)
  
  pickerInput("running_width", "Running average", choices=windows, selected=c(7),
              options = list(`actions-box` = F), multiple = F)
  
  # numericInputIcon("running_width", "Rolling average (day)", min=1, max=30, value=7, step=1)
  # numericInputIcon(
  #   inputId,
  #   label,
  #   value,
  #   min = NULL,
  #   max = NULL,
  #   step = NULL,
  #   icon = NULL,
  #   size = NULL,
  #   help_text = NULL,
  #   width = NULL
  # )
})


output$selectInputCountry <- renderUI({
  req(available())
  countries <- unique(available()$country)
  names(countries) = unlist(countrycode(countries, origin='iso2c', destination='country.name',
                                        custom_match = list(XK='Kosovo')))

  pickerInput("country","Country", choices=countries, options = list(`actions-box` = TRUE), multiple = F)
})



output$selectInputCity <- renderUI({
  req(available())
  req(input$country)
  
  cities <- available() %>%
    filter(country==input$country) %>%
    distinct(location_name, location_id) %>%
    tibble::deframe()
  
  pickerInput("city","City", choices=cities, options = list(`actions-box` = TRUE), multiple = F)
})


output$selectInputDuration <- renderUI({
  req(trajs_durations())
  pickerInput("trajs_duration","Duration", choices=trajs_durations(), options = list(`actions-box` = TRUE), multiple = F)
})


output$selectInputBuffer <- renderUI({
  req(trajs_buffers())
  pickerInput("trajs_buffer","Buffer", choices=trajs_buffers(), options = list(`actions-box` = TRUE), multiple = F)
})


output$selectInputFireSource <- renderUI({
  req(firesources())
  pickerInput("firesource","Fire source", choices=firesources(), options = list(`actions-box` = TRUE), multiple = F)
})


output$selectInputPoll <- renderUI({
  req(polls())
  selected <- input$poll
  if(is.null(selected) || !selected %in% polls()){
    selected <- polls()[1]
  }
  pickerInput("poll", "Pollutant", choices=polls(),
              selected=selected,
              options = list(`actions-box` = TRUE), multiple = F)
})



createInputDate <- function(value=NULL){
  dates <- trajs_dates()
  if(is.null(value)){
    value <- max(dates)
  }

  # pickerInput("trajs_date","Date", choices=dates, options = list(`actions-box` = TRUE), multiple = F)
  # dateInput("trajs_date","Date", min=min(dates), max=max(dates))
  sliderInput("trajs_date",
              NULL,
              min = as.Date(min(dates),"%Y-%m-%d"),
              max = as.Date(max(dates),"%Y-%m-%d"),
              value=as.Date(value),
              timeFormat="%Y-%m-%d",
              ticks=T,
              width="100%")
}


output$selectInputDates <- renderUI({
  req(trajs_dates())
  createInputDate()
})


output$trajsInfos <- renderUI({
  req(location_id())
  req(trajs_date())
  req(trajs_meas_date())

  l <- locations() %>%
    dplyr::filter(id==location_id()) %>%
    distinct(name)
  
  d <- trajs_meas_date() %>%
    tidyr::spread(variable, value)

  HTML(paste0("<b>",l$name," - ",d[["poll"]],"</b><br/>",
              trajs_date(),"<br/>",
              # "Observed: ", round(d[["observed"]]), " ",d[["unit"]], "<br/>",
              # "Predicted: ", round(d[["predicted"]]), " ",d[["unit"]], "<br/>",
              "Predicted (nofire): ", round(d[["predicted_nofire"]]), " ",d[["unit"]], "<br/>"
  ))
})

output$trajsLogs <- renderText({
  trajs_logs$msg
})



add_sentinel_layers <- function(map, date){
  for(l in names(sentinel_layers)){
    map <- map %>% addWMSTiles(
      sentinel_url,
      layers = sentinel_layers[[l]],
      layerId = l,
      group = l,
      options = WMSTileOptions(
        tileSize= 512,
        # attribution= '&copy; <a href="http://www.sentinel-hub.com/" target="_blank">Sentinel Hub</a>',
        # urlProcessingApi="https://services.sentinel-hub.com/ogc/wms/aeafc74a-c894-440b-a85b-964c7b26e471",
        # maxcc=20,
        # minZoom:6,
        # maxZoom:16,
        preset=sentinel_layers[[l]],
        # layers:"NO2",
        time=date,
        format = "image/png",
        transparent = F)
    )
  }
  return(map)
}

output$maptrajs <- renderLeaflet({
  map <- leaflet(options = leafletOptions(
    crs = leafletCRS(crsClass = "L.CRS.EPSG3857"),
    preferCanvas = TRUE,
    zoomControl = FALSE)) %>%
    setView(80,30,6) %>%
    # addProviderTiles(providers$Stamen.TonerLite,
    #                  options = providerTileOptions(noWrap = TRUE)
    # )
    addProviderTiles('Stadia.StamenTerrain', group="Terrain",
                     options=providerTileOptions(zindex=0)) %>%
    addProviderTiles('Esri.WorldImagery', group="Satellite",
                     options=providerTileOptions(zindex=0)) %>%
    addProviderTiles('OpenStreetMap', group = "OpenStreetMap",
                     options=providerTileOptions(zindex=0)) %>%
    # addProviderTiles("CartoDB.PositronOnlyLabels", group="Satellite") %>%
    # addProviderTiles('Esri.Topographic', group="Topographic") %>%
    # addProviderTiles('Esri.Terrain', group="Terrain") %>%
    addProviderTiles(providers$CartoDB.Positron, group="Light") %>%
    addLayersControl(
      baseGroups = c("Terrain", "Satellite", "OpenStreetMap", "Light"),
      overlayGroups = c("Trajectories", "Active fires",
                        names(trajs_gibs_layers),
                        names(sentinel_layers)),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    hideGroup(c(names(trajs_gibs_layers),
                 names(sentinel_layers)))


  for(l in names(trajs_gibs_layers)){
    map <- map %>%leaflet.extras2::addGIBS(
      layers=trajs_gibs_layers[[l]],
      group=l,
      dates=lubridate::today(),
      transparent = T,
      opacity = 0.7
    )
  }


  # Add S5P - NO2
  map <- add_sentinel_layers(map, date=lubridate::today())


  # sentinelHub = L.tileLayer.wms(baseUrl, {
  #   tileSize: 512,
  #   attribution: '&copy; <a href="http://www.sentinel-hub.com/" target="_blank">Sentinel Hub</a>',
  #   urlProcessingApi:"https://services.sentinel-hub.com/ogc/wms/aeafc74a-c894-440b-a85b-964c7b26e471",
  #   maxcc:20,
  #   minZoom:6,
  #   maxZoom:16,
  #   preset:"NO2",
  #   layers:"NO2",
  #   time:"2020-09-01/2021-03-17",
  #
  # });

  return(map)
})


observe({

  req(trajs_date())

  wms_layer <- "fires_viirs_snpp"
  
  # See https://firms.modaps.eosdis.nasa.gov/mapserver/wms-info/#firms-wms
  wms_url <- sprintf("https://firms.modaps.eosdis.nasa.gov/mapserver/wms/fires/%s/",
                     Sys.getenv("FIRMS_KEY"))
  
  leaflet_layer_id <- "firms_wms"
  date_str <- strftime(as.Date(trajs_date()),"%Y-%m-%d")


  leafletProxy("maptrajs") %>%
    removeTiles(leaflet_layer_id) %>%
    leaflet::addWMSTiles(
      wms_url,
      layers = wms_layer,
      layerId = leaflet_layer_id,
      group="Active fires",
      options = WMSTileOptions(
        format = "image/png",
        transparent = TRUE,
        colors = "255+12+25",
        TIME = date_str,
        size=5,
        zIndex=1000,
        version='1.1.1'
      )) %>%
      leaflet.extras2::setDate(layers=stack(trajs_gibs_layers)$values,
                               dates=as.Date(trajs_date())) %>%

    # removeTiles(stack(trajs_gibs_layers)$values) %>%
    removeTiles(names(sentinel_layers)) %>%
    add_sentinel_layers(date=date_str)
})

# Incremental changes to the map. Each independent set of things that can change
# should be managed in its own observer.
observe({

  req(trajs_points())
  
  map <- leafletProxy("maptrajs") %>%
    clearShapes() %>%
    clearMarkers

  trajs <- trajs_points() %>%
    dplyr::arrange(run, date)

  for(run in unique(trajs$run)){
    map <- addPolylines(map,
                        lng= ~ lon,
                        lat= ~ lat,
                        data = trajs[trajs$run==run,],
                        group = "Trajectories",
                        weight = 3)
  }

  map
})

observe({

  req(location_geometry())
  tryCatch({

    leafletProxy("maptrajs") %>%
      setView(
        lng=sf::st_coordinates(location_geometry())[1],
        lat=sf::st_coordinates(location_geometry())[2],
        zoom = 6
      )
  }, error=function(e){NULL})

})


# Click date
clickedDate <- reactiveVal()

observe({
  req(input$poll)
  d <- unlist(event_data(event = "plotly_click",
                         priority = "event"))

  if(is.null(d)){return(NULL)}
  name <- intersect(c("x","x1"), names(d))
  clickedDate(d[[name]])
  output$selectInputDates = renderUI(createInputDate(value=d[[name]]))

})

