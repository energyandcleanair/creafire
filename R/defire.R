#' Deweather with fire data for a set of locations
#'
#' @param location_ids
#' @param level
#' @param city
#' @param source
#' @param source_city
#' @param country
#' @param process_id
#' @param weather_sources
#' @param date_from
#' @param date_to
#' @param training_end_anomaly
#' @param poll
#' @param duration_hour
#' @param buffer_km
#' @param height
#' @param met_type
#' @param use_cache
#' @param save_to_cache
#' @param fire_source
#' @param parallel
#'
#' @return
#' @export
#'
#' @examples
defire <- function(location_ids=NULL,
                   level="city",
                   city=NULL,
                   source=NULL,
                   source_city=NULL,
                   country=NULL,
		               process_id="city_day_mad",
		               weather_sources=c("era5"),
                   date_from="2016-01-01",
		               date_to=lubridate::today(),
		               training_end_anomaly=lubridate::today(),
		               poll=c("pm25"),
		               duration_hour=120,
		               buffer_km=50,
		               height=10, #if null or NA, will be PBL average
		               met_type="gdas1",
		               use_cache=T,
		               save_to_cache=use_cache,
		               fire_source="viirs",
		               parallel=T
){

  if(is.null(location_ids)){
    location_ids <- rcrea::locations(country=country,
                                     city=city,
                                     source=source,
                                     source_city=source_city,
                                     level=level
    ) %>%
      dplyr::pull(id) %>% unique()
  }

  message(length(location_ids)," locations found")

  lapply(location_ids,
         function(location_id){
           tryCatch({
             message("Getting measurements: ", location_id)
             m <- rcrea::measurements(location_id=location_id,
                                    process_id=process_id,
                                    poll=poll,
                                    source_city=source_city,
                                    date_from=date_from,
                                    date_to=date_to,
                                    source=source,
                                    with_geometry = T,
                                    with_metadata = T)

             # Filling with NAs to get fire data
             # even when there is no measurement
             dates <- seq.Date(as.Date(date_from), as.Date(date_to), by="d")
             m <- m %>%
               distinct_at(setdiff(names(m), c("date","value"))) %>%
               crossing(tibble(date=dates)) %>%
               left_join(m)

             print("Done")

             weather_filepath <- file.path(tempdir(), "weather.RDS")

             if(use_cache){
               weather <- creadeweather::db.download_weather(location_id=location_id,
                                              duration_hour=duration_hour,
                                              buffer_km=buffer_km,
                                              height=height,
                                              fire_source=fire_source,
                                              weather_sources=weather_sources)


               if(!is.null(weather) && nrow(weather)>1){stop("More than one weather cache file found")}

               if(is.null(weather) || nrow(weather)==0){
                 print("No weather cache found")
                 w <- creadeweather::collect_weather(
                   meas=m[m$location_id==location_id,] %>%
                     dplyr::group_by(location_id, country, geometry) %>%
                     tidyr::nest() %>%
                     dplyr::rename(meas=data),
                   weather_sources=weather_sources,
                   years=unique(lubridate::year(m$date)),
                   years_force_refresh=NULL,
                   n_per_station=4,
                   add_pbl=T,
                   add_sunshine=F,
                   add_fire=T,
                   fire_source=fire_source,
                   fire_mode="trajectory",
                   fire_duration_hour=duration_hour,
                   fire_buffer_km=buffer_km,
                   trajs_height=height
                 )
               }

               if(!is.null(weather) && nrow(weather)==1){
                 print("Found weather in cache")
                 w <- weather$weather[[1]]
                 w <- update_weather(weather=w,
                                     meas=m,
                                     duration_hour=duration_hour,
                                     buffer_km=buffer_km,
                                     height=height,
                                     fire_source=fire_source,
                                     weather_sources=weather_sources)
               }

               if(save_to_cache){
                 creadeweather::db.upload_weather(w,
                                   location_id=location_id,
                                   duration_hour=duration_hour,
                                   buffer_km=buffer_km,
                                   height=height,
                                   met_type=met_type,
                                   fire_source=fire_source,
                                   weather_sources=weather_sources)
               }
               saveRDS(w, weather_filepath)
              }

           print("Deweathering")
           m.dew <- creadeweather::deweather(meas=m[m$location_id==location_id,],
                                             poll=poll,
                                             output="anomaly",
                                             add_fire = T,
                                             weather_sources=weather_sources,
                                             fire_mode = "trajectory",
                                             training_start_anomaly=date_from,
                                             training_end_anomaly=training_end_anomaly,
                                             lag=3,
                                             training.fraction=0.9,
                                             cv_folds=6,
                                             fire_source=fire_source,
                                             fire_duration_hour = duration_hour,
                                             fire_buffer_km = buffer_km,
                                             trajs_height=height,
                                             trajs_parallel=parallel,
                                             keep_model = T,
                                             link=c("linear"),
                                             upload_results = F,
                                             save_weather_filename=weather_filepath,
                                             read_weather_filename=weather_filepath
           )
           print("Done")

           # Export measurements
           m <- m.dew %>%
             filter(output %in% c("counterfactual")) %>%
             tidyr::unnest(normalised) %>%
             dplyr::select(
               location_id, date, poll, unit, source, observed, predicted, predicted_nofire, model
             )
           creadeweather::db.upload_meas(m,
                          location_id=location_id,
                          duration_hour=duration_hour,
                          buffer_km=buffer_km,
                          height=height,
                          met_type=met_type,
                          fire_source=fire_source)

         }, error=function(e){
           msg <- paste0("Failed for location: ", location_id, "\n", e)
           print(msg)
           warning(msg)
         })

      })
}
