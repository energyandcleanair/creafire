#' This function fills weather data with missing dates found in meas.
#'
#' @param meas
#' @param weather
#'
#' @return
#' @export
#'
#' @examples
update_weather <- function(weather,
                           meas,
                           duration_hour,
                           buffer_km,
                           height,
                           file_trajs){

  meas_missing <- meas %>%
    left_join(weather %>% distinct(location_id, date) %>% mutate(exists=T)) %>%
    filter(is.na(exists))

  years <- unique(lubridate::year(meas_missing$date))

  # Nest and sf
  meas_missing <- meas_missing %>%
    dplyr::group_by(location_id, poll, unit, source, process_id, country, geometry) %>%
    tidyr::nest() %>%
    dplyr::rename(meas=data) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(geometry=suppressWarnings(sf::st_centroid(geometry))) %>%
    sf::st_as_sf(sf_column_name="geometry", crs = 4326)


  old_trajs <- readRDS(file_trajs)

  print("Getting new weather data")
  new_weather <- creadeweather::collect_weather(
    meas_missing,
    years=years,
    years_force_refresh=years,
    n_per_station=4,
    add_pbl=any(grepl("pbl", names(weather))),
    add_sunshine=F,
    add_fire=any(grepl("fire", names(weather))),
    fire_source="viirs",
    fire_mode="trajectory",
    fire_duration_hour=duration_hour,
    fire_buffer_km=buffer_km,
    trajs_height=height,
    save_trajs_filename=file_trajs
  )

  print("Updating trajectories file")
  bind_rows(old_trajs,
            readRDS(file_trajs) %>% select_at(names(old_trajs))) %>%
    saveRDS(file_trajs)

  print("Combining weather")
  updated_weather <- bind_rows(weather,
                               new_weather)
  return(updated_weather)
}