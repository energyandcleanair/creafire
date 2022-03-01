#' Deweather and export trajs + fires to GCS for dashboard
#'
#' @param city
#' @param source
#' @param date_from
#' @param poll
#' @param date_to
#' @param met_type
#' @param duration_hour
#' @param height
#' @param radius_km
#' @param buffer_km
#' @param fires
#' @param add_fires
#' @param powerplants
#' @param folder
#' @param upload_results
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


  # dir.create(upload_folder, showWarnings = F)
  # fs <- list.files(upload_folder, include.dirs = F, full.names = T, recursive = T)

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


           # Trajs are the same whether or not we use viirs or gfas
           # suffix_trajs <- sprintf("%skm.%sh.%s.RDS",
           #                         buffer_km,
           #                         duration_hour,
           #                         ifelse(is.null(height)|is.na(height),"pbl",paste0(height,"m")))
           # 
           # suffix <- gsub("\\.RDS",
           #                ifelse(fire_source=="viirs",".viirs.RDS",".gfas.RDS"),
           #                suffix_trajs)
           # 
           # 
           # prefix <- location_id
           # 
           # file.trajs <- file.path(upload_folder, sprintf("%s.trajs.%s", prefix, suffix_trajs))
           # file.fires <- file.path(upload_folder, sprintf("%s.fires.%s", prefix, suffix))
           # file.meas <- file.path(upload_folder, sprintf("%s.meas.%s", prefix, suffix))
           # file.weather <- file.path(upload_folder, sprintf("%s.weather.%s", prefix, suffix))
           # file.weatherlite <- file.path(upload_folder, sprintf("%s.weatherlite.%s", prefix, suffix)) # A liter version to be used by dashboard

           # Download existing weather data if required
           # if(!force_recompute_weather & download_from_gcs){
           #     fs <- basename(c(file.meas, file.trajs, file.weather))
           #     gcs.download(fs, upload_folder)
           # }

           # Update weather data if required
             weather_filepath <- NULL
             if(use_cache){
               weather <- db.download_weather(location_id=location_id,
                                              duration_hour=duration_hour,
                                              buffer_km=buffer_km,
                                              height=height,
                                              fire_source=fire_source)
               
               if(is.null(weather) || nrow(weather)==0){print("No cache found")}
               if(!is.null(weather) && nrow(weather)>1){print("More than one cache file found")}
               if(!is.null(weather) && nrow(weather)==1){
                 w <- weather$weather[[1]]
                 w <- update_weather(weather=w,
                                     meas=m,
                                     duration_hour=duration_hour,
                                     buffer_km=buffer_km,
                                     height=height,
                                     fire_source=fire_source)
                 
                 if(save_to_cache){
                   db.upload_weather(w, location_id=location_id,
                                     duration_hour=duration_hour,
                                     buffer_km=buffer_km,
                                     height=height,
                                     met_type=met_type,
                                     fire_source=fire_source)  
                 }
                 
                 weather_filepath <- file.path(tempdir(), "weather.RDS")
                 saveRDS(w, weather_filepath)
               }
              }
           # if(!force_recompute_weather & file.exists(file.weather)){
           #   read_weather_filename <- tryCatch({
           #     readRDS(file.weather) %>%
           #       update_weather(meas=m,
           #                      duration_hour=duration_hour,
           #                      buffer_km=buffer_km,
           #                      height=height,
           #                      file_trajs = file.trajs)
           #     
           #     saveRDS(weather, file.weather)
           #     # Use it
           #     file.weather
           #   }, error=function(e){
           #     print(sprintf("Failed to read or update: %s", file.weather))
           #     return(NULL)
           #   })
           # }else{
           #   read_weather_filename <- NULL
           # }


           print("Deweathering")
           m.dew <- creadeweather::deweather(meas=m[m$location_id==location_id,],
                                             poll=poll,
                                             output="anomaly",
                                             add_fire = T,
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
                                             
                                             # Save (soon won't be used)
                                             # save_trajs_filename=file.trajs,
                                             # save_weather_filename=weather_filepath,
                                             read_weather_filename=weather_filepath
           )
           print("Done")

           # Export measurements -----------------------------------------------------
           m <- m.dew %>%
             filter(output %in% c("counterfactual")) %>%
             tidyr::unnest(normalised) %>%
             dplyr::select(
               location_id, date, poll, unit, source, observed, predicted, predicted_nofire, model
             )
           db.upload_meas(m,
                          location_id=location_id,
                          duration_hour=duration_hour,
                          buffer_km=buffer_km,
                          height=height,
                          met_type=met_type,
                          fire_source=fire_source) 
           # saveRDS(m, file.meas)
        
           # Export trajs (lite) -----------------------------------------------------
           # Will be read online each time. Better have something as light as possible
           # trajs <- readRDS(file.trajs)
           # trajs.lite <- trajs %>%
           #   select(location_id, date, trajs) %>%
           #   rowwise() %>%
           #   mutate(trajs=list(trajs %>% select(
           #     run, traj_dt, lat, lon)
           #   ))
           # saveRDS(trajs.lite, file.trajs)


           # Export weather (lite) -----------------------------------------------------
           # Will be read online each time. Better have something as light as possible
          #  weather <- readRDS(file.weather)
          #  weather.lite <- weather %>%
          #    dplyr::select(location_id, date, fire_frp, fire_count, precip)
          # 
          #  saveRDS(weather.lite, file.weatherlite)
          # 
          #  # Upload ------------------------------------------------------------------
          # if(upload_results){
          #   fs <- c(file.meas, file.trajs, file.weatherlite)
          #   gcs.upload(fs)
          # }


           # Memory usage keeps growing, not sure why
           # Lines below probably don't do much
           # rm(m)
           # rm(m.dew)
           # rm(trajs)
           # rm(trajs.lite)
           # gc()

         }, error=function(e){
           msg <- paste0("Failed for location: ", location_id, "\n", e)
           print(msg)
           warning(msg)
         })

      })
}


