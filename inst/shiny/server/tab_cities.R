

# Some log related functions
# trajs_logs_raw <- ""
trajs_logs <- reactiveValues(msg="")

trajs_add_log <- function(message){
  trajs_logs$msg <- paste(isolate(trajs_logs$msg), "\n", message)
}

read_gcs_url <- function(url, force_uncache=T){
  tryCatch({
    trajs_add_log(url)
    if(force_uncache){
      url <- paste0(url,"?a=1") # GCS caches files otherwise
    }
    r <- readRDS(url(gsub(" ","%20",url)))
    trajs_add_log("SUCCESS")
    r
  },error=function(e){
    trajs_add_log("FAILED")
    return(NULL)
  })
}

# Not refreshing for every intermediate step
# when sliding date
trajs_date <- reactive({
  input$trajs_date
}) %>% debounce(1000)


trajs_files <- reactive({
  trajs_add_log("Listing available files")

  # Get list of trajectories available
  gcs_get_bucket(trajs.bucket)
  files <- gcs_list_objects(prefix=paste0(trajs.folder,"/"),
                            detail = "summary",
                            delimiter = "/")
  trajs_add_log(sprintf("%d files found", nrow(files)))

  # Trajectories (no fire source in it)
  files.trajs <- files %>%
    dplyr::filter(stringr::str_detect(name, ".trajs..*.RDS$")) %>%
    mutate(location_id=gsub(".trajs.*.RDS","",basename(name))) %>%
    mutate(details=stringr::str_match(basename(name),
                                      sprintf("%s.trajs.(.*?).RDS",location_id))[,2]) %>%
    tidyr::separate(details, c("buffer","duration","pbl")) %>%
    rename(gcs_name_trajs=name) %>%
    filter(pbl %in% c("10m","50m")) %>%
    select(location_id, buffer, duration, pbl, gcs_name_trajs)

  # Weather & measurements
  files.meas_weather <- files %>%
    dplyr::filter(stringr::str_detect(name, ".(weatherlite|meas)..*.RDS$")) %>%
    mutate(location_id=gsub(".(weatherlite|meas).*.RDS","",basename(name))) %>%
    mutate(details=stringr::str_match(basename(name),
                                      sprintf("%s.(weatherlite|meas).(.*?).RDS",location_id))[,3]) %>%
    tidyr::separate(details, c("buffer","duration","pbl","firesource")) %>%
    rename(gcs_name=name) %>%
    mutate(meas_or_weather=ifelse(stringr::str_detect(gcs_name, ".(weatherlite)..*.RDS$"), "weather","meas")) %>%
    select(location_id, buffer, duration, pbl, gcs_name, firesource, meas_or_weather) %>%
    tidyr::pivot_wider(names_from=meas_or_weather, values_from=gcs_name, names_prefix="gcs_name_") %>%
    filter(pbl %in% c("10m","50m"))

  full_join(files.trajs, files.meas_weather)
})


locations <- reactive({
  req(trajs_files())
  rcrea::locations(id=unique(trajs_files()$location_id),
                   level=c("station","city"),
                   with_metadata=F,
                   with_geometry=T) %>%
    dplyr::left_join(trajs_files(),
                     by=c("id"="location_id")) %>%
    dplyr::distinct(id, name, country, geometry)
})


location_id <- reactive({
  req(input$city)
  req(input$country)

  locations() %>%
    dplyr::filter(country==input$country,
                  name==input$city) %>%
    dplyr::pull(id) %>%
    unique()
})


location_geometry <- reactive({
  req(input$city)
  req(input$country)

  locations() %>%
    dplyr::filter(country==input$country,
                  name==input$city) %>%
    dplyr::distinct(geometry) %>%
    dplyr::pull(geometry)
})


trajs_file <- reactive({
  req(location_id())
  req(input$trajs_buffer)
  req(input$trajs_duration)
  req(input$firesource)

  trajs_files() %>%
    filter(location_id==location_id(),
           buffer==input$trajs_buffer,
           duration==input$trajs_duration,
           (input$firesource=="NA" & is.na(firesource)) | (firesource==input$firesource))
})


trajs <- reactive({
  req(trajs_file())
  gcs_url <- paste0(trajs.bucket_base_url, trajs_file()$gcs_name_trajs)
  read_gcs_url(gcs_url)
})


trajs_dates <- reactive({
  req(trajs())
  trajs() %>%
    dplyr::pull(date) %>%
    unique() %>%
    sort(decreasing=T)
})


trajs_durations <- reactive({
  req(location_id())
  req(trajs_files())

  trajs_files() %>%
    filter(location_id==location_id()) %>%
    pull(duration) %>%
    unique()
})


trajs_buffers <- reactive({
  req(location_id())
  req(trajs_files())

  trajs_files() %>%
    filter(location_id==location_id()) %>%
    pull(buffer) %>%
    unique()
})


firesources <- reactive({
  req(location_id())
  req(trajs_files())

  trajs_files() %>%
    filter(location_id==location_id()) %>%
    pull(firesource) %>%
    unique() %>%
    tidyr::replace_na("NA")
})


weather <- reactive({
  req(trajs_file())
  gcs_url <- paste0(trajs.bucket_base_url, trajs_file()$gcs_name_weather)
  read_gcs_url(gcs_url)
})


trajs_meas_all <- reactive({
  req(trajs_file())
  gcs_url <- paste0(trajs.bucket_base_url, trajs_file()$gcs_name_meas)
  read_gcs_url(gcs_url)
})


polls <- reactive({
  req(trajs_meas_all())

  polls <- trajs_meas_all() %>%
    pull(poll) %>%
    unique()

  names(polls) <- rcrea::poll_str(polls)
  return(polls)
})


trajs_meas_date <- reactive({

  req(trajs_meas_all())
  req(trajs_date())

  trajs_meas_all() %>%
    dplyr::filter(date==trajs_date())

})


trajs_points <- reactive({
  req(trajs())
  req(trajs_date())

  date_ <- tolower(trajs_date())

  tryCatch({
    trajs() %>%
      dplyr::filter(date==date_) %>%
      tidyr::unnest(trajs, names_sep=".") %>%
      dplyr::select(date=trajs.traj_dt, lon=trajs.lon, lat=trajs.lat, run=trajs.run)
  }, error=function(e){
    trajs_add_log(sprintf("Failed to read trajectories (%s)",e))
    return(NULL)
  })
})


# Output Elements --------------------------------------
output$radioMode <- renderUI({
  radioButtons("cities_mode", NULL, choices=c("Map"="map", "Charts"="charts"),
               selected="map", inline=T)
})


output$selectInputRunning <- renderUI({
  req(locations()) # Not required, but added so that all ui elements appear together
  sliderInput("running_width", "Rolling average (day)", min=1, max=30, value=7, step=1, sep="")
})


output$selectInputCountry <- renderUI({

  countries <- locations()$country %>% unique()
  names(countries) = unlist(countrycode(countries, origin='iso2c', destination='country.name',
                                        custom_match = list(XK='Kosovo')))

  pickerInput("country","Country", choices=countries, options = list(`actions-box` = TRUE), multiple = F)
})


output$selectInputCity <- renderUI({
  req(input$country)
  cities <- locations() %>%
    dplyr::filter(country==input$country) %>%
    dplyr::pull(name) %>%
    unique()
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
  d <- trajs_meas_date()

  HTML(paste0("<b>",l$name," - ",d[["poll"]],"</b><br/>",
              trajs_date(),"<br/>",
              "Observed: ", round(d[["observed"]]), " ",d[["unit"]], "<br/>",
              "Predicted: ", round(d[["predicted"]]), " ",d[["unit"]], "<br/>",
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
  map <- leaflet(options = leafletOptions(preferCanvas = TRUE,
                                   zoomControl = FALSE)) %>%
    setView(80,30,6) %>%
    # addProviderTiles(providers$Stamen.TonerLite,
    #                  options = providerTileOptions(noWrap = TRUE)
    # )
    addProviderTiles('Stamen.Terrain', group="Terrain",
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

  wms_url <- sprintf("https://firms.modaps.eosdis.nasa.gov/wms/key/%s/",Sys.getenv("FIRMS_KEY"))
  wms_layer <- "fires_viirs_snpp"
  leaflet_layer_id <- "firms_wms"
  date_str <- strftime(as.Date(trajs_date()),"%Y-%m-%d")

  #https://firms.modaps.eosdis.nasa.gov/wms/key/YourMapKey/?REQUEST=GetMap&layers=fires_viirs,fires_modis&TIME=2020-01-01/2020-01-10&WIDTH=1024&HEIGHT=512&colors=240+40+40,250+200+50&size=2,2&BBOX=-180,-90,180,90


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
        zIndex=1000
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

