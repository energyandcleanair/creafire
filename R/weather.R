#' This function fills weather data with missing dates found in meas.
#'
#' @param meas 
#' @param weather 
#'
#' @return
#' @export
#'
#' @examples
update_weather <- function(weather, meas, duration_hour, buffer_km, height){
  
  meas_missing <- meas %>%
    left_join(weather %>% distinct(location_id, date) %>% mutate(exists=T)) %>%
    filter(is.na(exists))
  
  years <- unique(lubridate::year(meas_missing$date))
  
  weather <- creadeweather::collect_weather(meas,
                             years=seq(lubridate::year(lubridate::date(min(time_vars_output$training_start))),
                                       lubridate::year(lubridate::today())),
                             years_force_refresh=years_force_refresh,
                             n_per_station=4,
                             add_pbl=add_pbl,
                             add_sunshine=F,
                             add_fire=T,
                             fire_source="viirs",
                             fire_mode="trajectory",
                             fire_duration_hour=duration_hour,
                             fire_buffer_km=buffer_km,
                             trajs_height=height,
                             save_trajs_filename=save_trajs_filename
  )
  
  
}