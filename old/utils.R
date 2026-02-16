utils.ggmap_register <- function(){
  try(readRenviron(".Renviron"))
  ggmap::register_google(Sys.getenv("GOOGLE_MAP_API_KEY"))
}

utils.attach.basemaps <- function(m, radius_km=100, zoom_level=6){

  utils.ggmap_register()

  mc <- m %>%
    distinct(location_id, geometry) %>%
    filter(!sf::st_is_empty(geometry))

  geometry_to_basemap <- function(g, radius_km, zoom_level){

    tryCatch({
      bbox_100km <- g %>%
        st_set_crs(4326) %>%
        st_transform(crs=3857) %>%
        st_buffer(radius_km*1000) %>%
        st_transform(crs=4326) %>%
        st_bbox()
      ggmap::get_map(location=unname(bbox_100km),zoom=zoom_level,
                     source="stamen", terrain="terrain")
    }, error=function(err){
      print("Failed to build basemap")
      print(g)
      return(NA)
    })
  }

  basemaps <- mc %>%
    rowwise() %>%
    mutate(basemap = list(geometry_to_basemap(geometry, radius_km, zoom_level))) %>%
    filter(!all(is.na(basemap)))

  return(m %>% left_join(basemaps))

}
