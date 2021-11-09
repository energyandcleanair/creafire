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
fill_dashboard <- function(
  location_ids=NULL,
  level="city",
  city=NULL,
  source,
  source_city=NULL,
  country=NULL,
  process_id="city_day_mad",
  date_from="2015-01-01",
  fire_source="viirs",
  poll=c("pm25"),
  date_to=lubridate::today(),
  upload_results=T,
  duration_hour=120,
  buffer_km=50,
  force_recompute=F,
  height=NULL #if null or NA, will be PBL average
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


  dir.create("cache", showWarnings = F)
  dir.create("upload", showWarnings = F)
  fs <- list.files(file.path("upload"), include.dirs = F, full.names = T, recursive = T)
  # file.remove(fs)

  lapply(location_ids,
         function(location_id){
           tryCatch({
             message("Getting measurements: ", location_id)
             m <- rcrea::measurements(location_id=location_id,
                                    process_id=process_id,
                                    poll=poll,
                                    source_city=source_city,
                                    date_from=date_from,
                                    source=source,
                                    with_geometry = T,
                                    with_metadata = T)
           print("Done")

           # Trajs are the same whether or not we use viirs or gfas
           suffix_trajs <- sprintf("%skm.%sh.%s.RDS",
                             buffer_km,
                             duration_hour,
                             ifelse(is.null(height)|is.na(height),"pbl",paste0(height,"m")))

           suffix <- gsub("\\.RDS",
                       ifelse(fire_source=="viirs",".viirs.RDS",".gfas.RDS"),
                       suffix_trajs)


           prefix <- location_id
           file.trajs <- file.path("upload", sprintf("%s.trajs.%s", prefix, suffix_trajs))
           file.fires <- file.path("upload", sprintf("%s.fires.%s", prefix, suffix))
           file.meas <- file.path("upload", sprintf("%s.meas.%s", prefix, suffix))

           file.weather.cache <- file.path("cache", sprintf("%s.weather.%s", prefix, suffix)) # The version cached
           file.weather <- file.path("upload", sprintf("%s.weather.%s", prefix, suffix)) # A simplified version to upload

           message("Deweathering: ", location_id)
           read_weather_filename <- if(force_recompute) NULL else file.weather.cache
           message("Weather file: ", file.weather.cache)

           m.dew <- creadeweather::deweather(meas=m[m$location_id==location_id,],
                                             poll="pm25",
                                             output="anomaly", #c("anomaly","anomaly_yday"),
                                             add_fire = T,
                                             fire_mode = "trajectory",
                                             training_start_anomaly=date_from,
                                             training_end_anomaly="2021-12-31",
                                             lag=3, #c(0,1,2,3),
                                             training.fraction=0.9, #c(0.8, 0.9, 1),
                                             save_trajs_filename=file.trajs,
                                             save_weather_filename=file.weather.cache,
                                             fire_source=fire_source,
                                             fire_duration_hour = duration_hour,
                                             fire_buffer_km = buffer_km,
                                             trajs_height=height,
                                             read_weather_filename=read_weather_filename,
                                             keep_model = T,
                                             link=c("linear"),
                                             upload_results = F
           )


           print("Done")

           # Export measurements -----------------------------------------------------
           m.dew %>%
             filter(output %in% c("counterfactual")) %>%
             tidyr::unnest(normalised) %>%
             dplyr::select(
               location_id, date, poll, unit, source, observed, predicted, predicted_nofire, model
             ) %>%
             saveRDS(file.meas)


           # Export trajs (lite) -----------------------------------------------------
           # Will be read online each time. Better have something as light as possible
           trajs <- readRDS(file.trajs)
           trajs.lite <- trajs %>%
             select(location_id, date, trajs) %>%
             rowwise() %>%
             mutate(trajs=list(trajs %>% select(
               run, traj_dt, lat, lon)
             ))
           saveRDS(trajs.lite, file.trajs)


           # Export weather (lite) -----------------------------------------------------
           # Will be read online each time. Better have something as light as possible
           weather <- readRDS(file.weather.cache)
           weather.lite <- weather %>%
             dplyr::select(location_id, date, fire_frp, fire_count, precip)

           saveRDS(weather.lite, file.weather)

           # Upload ------------------------------------------------------------------
           trajs.folder <- "data/trajectories"
           trajs.bucket <- Sys.getenv("GCS_DEFAULT_BUCKET", "crea-public")
           if(Sys.getenv('GCS_AUTH_FILE')!=""){
             googleCloudStorageR::gcs_auth(Sys.getenv('GCS_AUTH_FILE'))
           }


           fs <- c(file.meas, file.trajs, file.weather)
           lapply(fs,
                  function(f){
                    googleCloudStorageR::gcs_upload(f,
                                                    bucket=trajs.bucket,
                                                    name=paste0(trajs.folder,"/",basename(f)),
                                                    predefinedAcl="default")
                  })


           # Memory usage keeps growing, not sure why
           # Lines below probably don't do much
           rm(m)
           rm(m.dew)
           rm(trajs)
           rm(trajs.lite)
           gc()

         }, error=function(e){
           warning("Failed for location: ", location_id, "\n", e)
         })

      })
}

#
# wd <- "../../development/crea/defire/"
# fs <- list.files(file.path(wd, "cache"),"*weather*")
#
# lapply(fs,
#        function(f){
#          weather <- readRDS(file.path(wd,"cache",f))
#          weather.lite <- weather %>%
#            dplyr::select(location_id, meas_weather) %>%
#            tidyr::unnest(meas_weather)
#            # dplyr::select(location_id, date, fire_frp, fire_count, precip)
#
#          saveRDS(weather.lite, file.path(wd,"upload",f))
#          googleCloudStorageR::gcs_upload(file.path(wd,"upload",f),
#                                          bucket=trajs.bucket,
#                                          name=paste0(trajs.folder,"/",basename(f)),
#                                          predefinedAcl="default")
#        })
