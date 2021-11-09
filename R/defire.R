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
defire <- function(city,
                   source,
                   source_city=NULL,
                   country=NULL,
		               process_id="city_day_mad",
                   date_from="2017-01-01",
                   poll=c("pm25"),
                   date_to=lubridate::today(),
                   upload_results=T,
		               upload_folder=upload_folder,
		               cache_folder="cache",
		               duration_hour=120,
		               buffer_km=50,
		               height=NULL #if null or NA, will be PBL average
){

  print("Getting measurements")
  m <- rcrea::measurements(country=country,
                           process_id=process_id,
                           poll=poll,
                           city=city,
                           source_city=source_city,
                           date_from=date_from,
                           source=source,
                           with_geometry = T,
                           with_metadata = T)
  print("Done")
  location_ids <- unique(m$location_id)

  dir.create(upload_folder, showWarnings = F)
  fs <- list.files(upload_folder, include.dirs = F, full.names = T, recursive = T)

  lapply(location_ids,
         function(location_id){

           suffix <- sprintf("%skm.%sh.%s.RDS", buffer_km, duration_hour,ifelse(is.null(height)|is.na(height),"pbl",paste0(height,"m")))
           prefix <- location_id
           file.trajs <- file.path(upload_folder, sprintf("%s.trajs.%s", prefix, suffix))
           file.fires <- file.path(upload_folder, sprintf("%s.fires.%s", prefix, suffix))
           file.meas <- file.path(upload_folder, sprintf("%s.meas.%s", prefix, suffix))
           file.weather.cache <- file.path(cache_folder, sprintf("%s.weather.%s", prefix, suffix)) # The version cached
           file.weather <- file.path(upload_folder, sprintf("%s.weather.%s", prefix, suffix)) # A simplified version to upload

           print("Deweathering")
           m.dew <- creadeweather::deweather(meas=m[m$location_id==location_id,],
                                             poll=poll,
                                             output="anomaly",
                                             add_fire = T,
                                             fire_mode = "trajectory",
                                             training_start_anomaly=date_from,
                                             training_end_anomaly="2021-12-31",
                                             lag=3, 
                                             training.fraction=0.9,
                                             save_trajs_filename=file.trajs,
                                             save_weather_filename=file.weather.cache,
                                             fire_duration_hour = duration_hour,
                                             fire_buffer_km = buffer_km,
                                             trajs_height=height,
                                             read_weather_filename=file.weather.cache,
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


           # Export fires ------------------------------------------------------------
           # print("Downloading fires")
           # min_fire_date <- "2021-01-01" #Can be quite heavy otherwise
           #
           # creatrajs::fire.download(date_from=as.Date(min_fire_date)-lubridate::hours(72),
           #                          date_to=max(trajs$date))
           # print("Done")
           #
           # print("Buffering")
           # trajs <- trajs %>%
           #   dplyr::filter(date>=min_fire_date) %>%
           #   rowwise() %>%
           #   mutate(extent=creatrajs::trajs.buffer(trajs=trajs, buffer_km=10))
           # print("Done")
           #
           # extent <- sf::as_Spatial(trajs$extent[!sf::st_is_empty(trajs$extent)])
           #
           # print("Reading fires")
           # fires <- creatrajs::fire.read(
           #   date_from="2020-01-01",
           #   date_to=max(trajs$date),
           #   extent.sp=extent)
           #
           # saveRDS(fires, file.fires)
           # print("Done")

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

         })
}