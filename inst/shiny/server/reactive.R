trajs_date <- reactive({
  input$trajs_date
}) %>% debounce(1000)


available <- reactive({
  w <- db.available_weather()
  
  locations <- rcrea::locations(id=w$location_id, with_source = F) %>%
    distinct(id, .keep_all = T)
  
  w %>%
    left_join(locations %>%
                select(location_id=id, location_name=name, country))
})


locations <- reactive({
  req(available())
  rcrea::locations(id=unique(available()$location_id),
                   level=c("station","city"),
                   with_source=F,
                   with_metadata=F,
                   with_geometry=T) %>%
    dplyr::distinct(id, name, country, geometry)
})


location_id <- reactive({
  req(input$city)
  input$city
  # req(selected_metadata())
  # m <- selected_metadata()
  # 
  # if(is.null(m) || length(m)==0){
  #   return(NULL)
  # }
  # 
  # m$location_id
})


selected_metadata <- reactive({
  req(input$config)
  req(available_at_location())
  
  as.list(available_at_location()[as.numeric(input$config),])
})

available_at_location <- reactive({
  req(available())
  req(location_id())
  
  available() %>%
    filter(location_id==location_id())
})


location_geometry <- reactive({
  req(locations())
  req(location_id())
  locations() %>%
    dplyr::filter(id==location_id()) %>%
    dplyr::distinct(geometry) %>%
    dplyr::pull(geometry)
})


weather <- reactive({
  req(selected_metadata())
  m <- selected_metadata()
  
  if(is.null(m) || length(m)==0){
    return(NULL)
  }
  
  w <- db.download_weather(
    location_id=m$location_id,
    met_type=m$met_type,
    # height=m$height,
    buffer_km=m$buffer_km,
    duration_hour=m$duration_hour,
    hours=m$hours,
    fire_source=m$firesource,
    fire_split_regions = m$fire_split_regions
  ) %>%
    filter((height==m$height) | (is.na(m$height) & is.na(height)))
  
  if(is.null(w)){
    return(NULL)
  }
  w %>%
    select(weather) %>% 
    tidyr::unnest(weather)
})


meas <- reactive({
  req(selected_metadata())
  m <- selected_metadata()
  
  if(is.null(m) || length(m)==0){
    return(NULL)
  }
  
  meas <- db.download_meas(
    location_id=m$location_id,
    met_type=m$met_type,
    height=NULL,
    buffer_km=m$buffer_km,
    duration_hour=m$duration_hour,
    hours=m$hours,
    fire_source=m$firesource,
    fire_split_regions = m$fire_split_regions
  )
  
  if(!is.null(meas)){
    return(
      meas %>%
        filter((height==m$height) | (is.na(m$height) & is.na(height))) %>%
        select(meas) %>%
        tidyr::unnest(meas)
    )
  }else{
    return(NULL)
  }
  
})


trajs_dates <- reactive({
  req(selected_metadata())
  m <- selected_metadata()
  
  if(is.null(m) || length(m)==0){
    return(NULL)
  }
  
  creatrajs::db.available_dates(
    location_id=m$location_id,
    duration_hour=as.numeric(m$duration_hour),
    met_type=m$met_type,
    height=NULL, #m$height
  ) %>%
    sort(decreasing=T)
})


trajs_durations <- reactive({
  req(available_location())
  
  available_location() %>%
    pull(duration_hour) %>%
    unique()
})


trajs_buffers <- reactive({
  req(available_location())
  
  available_location() %>%
    pull(buffer_km) %>%
    unique()
})


firesources <- reactive({
  req(available_location())
  
  available_location() %>%
    pull(fire_source) %>%
    unique() %>%
    tidyr::replace_na("NA")
})


polls <- reactive({
  req(meas())
  
  polls <- unique(meas()$poll)
  names(polls) <- rcrea::poll_str(polls)
  return(polls)
})


trajs_meas_date <- reactive({
  
  req(meas())
  req(trajs_date())
  
  meas() %>%
    dplyr::filter(date==trajs_date())
  
})


trajs_points <- reactive({
  
  req(selected_metadata())
  req(trajs_date())
  m <- selected_metadata()
  
  if(is.null(m) || length(m)==0){
    return(NULL)
  }
  
  trajs <- creatrajs::db.download_trajs(
    location_id=m$location_id,
    met_type=m$met_type,
    height=m$height,
    date=as.Date(trajs_date()),
    duration_hour=m$duration_hour,
    hours=m$hours
  )
  
  if(is.null(trajs)){
    return(NULL)
  }
  
  trajs %>%
    tidyr::unnest(trajs, names_sep=".") %>%
    dplyr::select(date=trajs.traj_dt, lon=trajs.lon, lat=trajs.lat, run=trajs.run)
})
