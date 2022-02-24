# Trying many parameters for deweathering
library(tidyverse)
library(creadeweather)
library(rcrea)

lag <- seq(1,3)
training_fraction <- 0.9
location_id <- rcrea::cities(name=c("Lahore", "Bangkok"))$id #Delhi")$id # name=c("Chandigarh", "Varanasi", "Kolkata", "Gurugram", "Lucknow"))$id
training_start <- c("2015-01-01")
cv_folds <- c(3, 9)
duration_hour <- c(24, 72, 120)
buffer_km <- c(10, 50)
split_days <- c(T,F)
fire_source <- c("viirs") #, "gfas")
source <- c("cpcb","airnow","air4thai","doemy")

configs <- crossing(lag, training_fraction, location_id, training_start, source=list(source),
                    duration_hour, buffer_km, split_days, fire_source,
                    cv_folds)
c=configs[1,]
deweather(1, 0.9, "lahore_pak.7_1_pk",
          "2015-01-01", c("cpcb","airnow","air4thai","doemy"), 24,10,F,'viirs',3)

deweather <- function(lag, training_fraction, location_id, training_start, source,
                      duration_hour, buffer_km, split_days, fire_source, cv_folds,
                      force=F){
  
  suffix <- sprintf("%s_%s_%dh_%dkm_%s_%s",
                    location_id,
                    gsub("-","",training_start),
                    duration_hour,
                    buffer_km,
                    ifelse(split_days, "split", "nosplit"),
                    fire_source)
  
  weather_file <- sprintf("sensitivity/weather_%s.RDS", suffix)
  mdew_file <- sprintf("sensitivity/mdew_%s_lag%d_cv%d.RDS", suffix, lag, cv_folds)
  
  if(!force & file.exists(mdew_file)){
    return(readRDS(mdew_file))
  }
  
  if(file.exists(weather_file)){
    read_weather_filename <- weather_file
    save_weather_filename <- NULL
  }else{
    read_weather_filename <- NULL
    save_weather_filename <- weather_file
  }
  
  mdew <- creadeweather::deweather(location_id=location_id,
                                   poll=c("pm25","pm10"),
                                   source=source,
                                   output=c("anomaly"),
                                   read_weather_filename=read_weather_filename,
                                   save_weather_filename=save_weather_filename,
                                   lag = lag,
                                   add_fire = T,
                                   fire_mode="trajectory",
                                   fire_duration_hour=duration_hour,
                                   fire_buffer_km=buffer_km,
                                   fire_split_days = split_days,
                                   fire_source = fire_source,
                                   cv_folds=cv_folds,
                                   training.fraction = training_fraction,
                                   training_start_anomaly = training_start,
                                   training_end_anomaly = "2020-12-31",
                                   upload_results = F) %>%
    mutate(training_fraction=training_fraction,
           lag=lag,
           training_start=training_start,
           duration_hour=duration_hour,
           buffer_km=buffer_km,
           split_days=split_days,
           fire_source=fire_source,
           cv_folds=cv_folds)
  
  saveRDS(mdew, mdew_file)
  return(mdew)
}


results <- mapply(deweather, configs$lag, configs$training_fraction,
                  configs$location_id, configs$training_start, configs$source,
                  configs$duration_hour, configs$buffer_km,
                  configs$split_days, configs$fire_source,
                  configs$cv_folds,
                  force=F,
                  SIMPLIFY = F) %>%
  do.call(bind_rows, .)

saveRDS(results, "sensitivity/results.RDS")
results <- readRDS("sensitivity/results.RDS")
fs_delhi <-list.files("sensitivity", pattern="mdew_delhi", full.names = T)
results <- lapply(fs_delhi, readRDS) %>% do.call(bind_rows, .)

# Diagnostic --------------------------------------------------------------
results %>%
  rowwise() %>%
  filter(poll=="pm25") %>%
  mutate(
    rmse_training=model$rmse_training,
    rmse_testing=model$rmse_testing,
    rmse_prediction=model$rmse_prediction,
    rsquared_training=model$rsquared_training,
    rsquared_testing=model$rsquared_testing,
    rsquared_prediction=model$rsquared_prediction) %>%
  filter(output=="anomaly") %>%
  select(training_fraction,
         lag,
         training_start,
         poll,
         fire_source,
         split_days,
         cv_folds,
         duration_hour,
         buffer_km,
         # rmse_training,
         rmse_testing,
         # rmse_prediction,
         # rsquared_training,
         rsquared_testing
         # rsquared_prediction
  ) %>%
  ungroup() %>%
  tidyr::pivot_longer(-c(training_fraction, lag, training_start, poll, fire_source, split_days, cv_folds, duration_hour, buffer_km),
                      names_to="indicator") %>%
  ggplot() +
  geom_point(aes(x=training_start, y=value, col=factor(cv_folds), alpha=duration_hour, size=buffer_km)) +
  facet_grid(indicator ~ lag + split_days + fire_source,
             scales="free_y")





# Timeseries --------------------------------------------------------------
results %>%
  filter(output=="anomaly",
         training_start=="2015-01-01",
         poll=="pm25") %>%
  tidyr::unnest(normalised) %>%
  select(training_fraction,
         lag,
         training_start,
         cv_folds,
         poll,
         fire_source,
         split_days,
         duration_hour,
         buffer_km,
         date,
         poll,
         value
  ) %>%
  rcrea::utils.running_average(7) %>%
  ggplot() +
  geom_line(aes(x=date, y=value, col=factor(cv_folds))) +
  facet_grid(lag + fire_source ~ split_days +  duration_hour + buffer_km)



# # Update file names
# fs <- list.files("sensitivity", pattern=".*lag[0-9]", full.names = T)
# fs_new <- paste0(gsub(".RDS","", fs), "_cv3.RDS")
# file.rename(fs, fs_new)

# # Rename
# fs <- list.files("sensitivity", pattern="_cv6_cv3.RDS", full.names = T)
# lapply(fs, function(f){
#   f_new <- gsub("cv6_cv3","cv6", f)
#   file.rename(f, f_new)
# })
#
# fs <- list.files("sensitivity", pattern="cv6.RDS", full.names = T)
# lapply(fs, function(f){
#   readRDS(f) %>% mutate(cv_folds=6) %>% saveRDS(f)
# })
#
# fs_delhi <-list.files("sensitivity", pattern="mdew_delhi", full.names = T)
# results <- lapply(fs_delhi, readRDS) %>% do.call(bind_rows, .)
