library(tidyverse)
library(rcrea)
library(creadeweather)

setwd('../defire')

m.la <- rcrea::measurements(city="Los Angeles",
                            source="airvisual",
                            process_id="city_day_mad",
                            poll="pm25",
                            date_from="2017-01-01",
                         with_geometry = T)

m.lahore <- rcrea::measurements(city="Lahore",
                                source="openaq_government",
                                process_id="city_day_mad",
                                poll="pm25",
                                date_from="2017-01-01",
                                with_geometry = T)

m.tianjin <- rcrea::measurements(city="Tianjin",
                                source="mee",
                                process_id="city_day_mad",
                                poll="pm25",
                                date_from="2017-01-01",
                                with_geometry = T)

m <- bind_rows(
  m.la,
  m.lahore,
  m.tianjin)

do_deweather <- function(m, buffer_km, duration_hour, lags, outputs=c("trend", "anomaly", "anomaly_yday")){

  location_ids <- unique(m$location_id)

  do_deweather_unique <- function(m){
    print(unique(m$location_id))
    weather.filename <- file.path("sensitivity",
                                  paste0(c("weather",
                                           unique(m$location_id),
                                           paste0(duration_hour,"h"),
                                           paste0(buffer_km,"km"),
                                           "RDS"), collapse="."))

    m.dew.filename <- file.path("sensitivity",
                                paste0(c("m.dew.defired.",
                                         unique(m$location_id),
                                         paste0(duration_hour,"h"),
                                         paste0(buffer_km,"km"),
                                         "RDS"), collapse="."))

    if(file.exists(m.dew.filename)){
      return(readRDS(m.dew.filename) %>%
        mutate(
          buffer_km=buffer_km,
          duration_hour=duration_hour
        ))
    }else{
      m.dew <- creadeweather::deweather(
        meas=m,
        lag=lags,
        poll="pm25",
        output=outputs,
        add_pbl=T,
        training.fraction = 0.9,
        training_end_anomaly = "2020-12-31",
        upload_results=F,
        engine=c("gbm","svr"),
        add_fire=T,
        fire_mode="trajectory",
        fire_duration_hour=duration_hour,
        fire_buffer_km=buffer_km,
        save_weather_filename=weather.filename,
        keep_model = T,
        read_weather_filename=weather.filename
      ) %>% mutate(
        buffer_km=buffer_km,
        duration_hour=duration_hour
        )

      saveRDS(m.dew, m.dew.filename)
      return(m.dew)
  }

  }

  lapply(split(m, m$location_id),
         do_deweather_unique
         ) %>%
    do.call(bind_rows, .)
}


configs <- tidyr::crossing(
  duration_hour=c(72,120),
  buffer_km=c(10,50,120)
)

m.dew <- mapply(do_deweather,
                m=list(m),
                lags=list(c(1,2,3)),
                duration_hour=configs$duration_hour,
                buffer_km=configs$buffer_km,
                SIMPLIFY = F
                )

saveRDS(m.dew, file.path("sensitivity","m.dew.defired.RDS"))
m.dew <- readRDS(file.path("../defire/sensitivity","m.dew.defired.RDS"))


m.dew <- m.dew %>% do.call("bind_rows",.)

readWeather <- function(p){
  p.parts <- strsplit(basename(p),"\\.")[[1]]
  readRDS(p) %>%
    mutate(path=p,
           buffer_km=p.parts[5],
           duration_hour=p.parts[4])
}

lapply(list.files("sensitivity", "weather*", full.names = T),
       readWeather) %>% do.call(bind_rows, .) -> weather

# Looking at peaks -------------------------------------------------------

peaks <- tibble(
  location_id=c("los angeles_usa.5_1_us","los angeles_usa.5_1_us"),
  date_from=c("2020-07-01","2017-10-08"),
  date_to=c("2021-07-07","2017-11-01")
)

for(i in seq(1, nrow(peaks))){

  peak <- peaks[i,]
  weather %>%
    filter(station_id==peak$location_id) %>%
    select(-c(timezone)) %>%
    tidyr::unnest(meas_weather) %>%
    filter(date>=peak$date_from,
           date<=peak$date_to) %>%
    select(location_id=station_id, date, value=fire_count, buffer_km, duration_hour) %>%
    mutate(type=paste(buffer_km, duration_hour, sep="_"), subplot="fire") -> w

  m.dew %>%
    filter(location_id==peak$location_id) %>%
    tidyr::unnest(normalised) %>%
    filter(date>=peak$date_from,
           date<=peak$date_to) %>%
    filter(!is.na(observed)) %>%
    distinct(location_id, date, observed) %>%
    tidyr::gather("type","value",observed) %>%
    mutate(subplot="measurement") -> m

  ggplot(bind_rows(m,w)) +
    geom_line(aes(date, value, col=type)) +
    facet_wrap(~subplot, scales="free_y", nrow=2)

}



# Analyse results---------------------------------------------------------
m.dew <- lapply(list.files("sensitivity","m.dew.l.*.RDS", full.names = T),
                function(f){readRDS(f) %>% mutate(filename=f)}) %>%
  do.call(bind_rows, .) %>%
  dplyr::filter(output %in% c("anomaly_yday","anomaly")) %>%
  rowwise() %>%
  mutate(engine=jsonlite::fromJSON(process_deweather[1])$engine,
         lag=jsonlite::fromJSON(process_deweather[1])$lag,
         buffer_km=gsub(".*\\.(.*)\\.RDS", "\\1",filename),
         duration_hour=gsub(".*\\.(.*)\\..*\\.RDS", "\\1",filename)) %>%
  tidyr::unnest(normalised)


# Reorder factors
buffer_kms = unique(m.dew$buffer_km)
m.dew$buffer_km <- factor(m.dew$buffer_km,
                          levels=buffer_kms[order(as.numeric(stringr::str_remove(buffer_kms,"km")))])
duration_hours <- unique(m.dew$duration_hour)
m.dew$duration_hour <- factor(m.dew$duration_hour,
                              levels=duration_hours[order(as.numeric(stringr::str_remove(duration_hours,"h")))])


require(Metrics)

m.dew.summary <- m.dew %>%
  group_by(engine, lag, buffer_km, location_id, duration_hour, output) %>%
  summarise(
    mae=Metrics::mae(observed, predicted),
    rsquared=summary(lm(observed ~ predicted, data=tibble(observed, predicted)))$r.squared
  )

m.dew.summary %>% group_by(location_id) %>% filter(mae==min(mae) | rsquared==max(rsquared))


# Time series -------------------------------------------------------------

(plt_ts <- ggplot(m.dew %>%
         select(location_id, date, output, buffer_km, duration_hour, lag, engine, observed, predicted, predicted_nofire) %>%
         rcrea::utils.running_average(30, vars_to_avg = c("predicted","predicted_nofire", "observed")),
       aes(color=engine)) +
  geom_line(aes(date, observed), linetype="solid", color="black") +
  geom_line(aes(date, predicted), linetype="solid") +
  geom_line(aes(date, predicted_nofire), linetype="dashed") +
  # geom_text(data=m.dew.summary,
  #           aes(x=min(m.dew$date),
  #               y=max(m.dew$observed),
  #               label=paste0("MAE:",round(mae,0),"\nR2:",round(rsquared,2))))+
  facet_grid(location_id + output ~ buffer_km + lag + duration_hour, scales="free_y") +
  ylim(0,NA))

ggsave('sensitivity/sensitivity_ts.png',
       plot=plt_ts,
       width=25,height=15)


# Fire contribution -------------------------------------------------------

(plt_fc <- m.dew %>%
  filter(lubridate::year(date)==2020) %>%
  group_by(engine, lag, buffer_km, location_id, duration_hour, output) %>%
  summarise(predicted=mean(predicted),
            predicted_nofire=mean(predicted_nofire)) %>%
  mutate(fire_contribution=(predicted-predicted_nofire)/predicted) %>%
  ggplot(aes(x=engine,y=fire_contribution,fill=buffer_km)) +
  geom_bar(stat="identity",
           position="dodge") +
  geom_text(aes(label=paste0(round(fire_contribution*100),"%")),
            position = position_dodge(width=1)) +
  scale_y_continuous(labels=scales::percent,
                     limits=c(0,NA)) +
  facet_grid(location_id +output ~  lag + duration_hour))

ggsave('sensitivity/sensitivity_bar.png',
       plot=plt_fc,
       width=25,height=15)


# Tianjin special case ----------------------------------------------------
# As we have some SA study isolating biomass burning

