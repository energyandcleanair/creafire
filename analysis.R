require(rcrea)
require(dplyr)
require(creadeweather)
require(ggmap)
require(sf)

setwd("../defire")
m <- rcrea::measurements(country=c("PK","IN"),
                         process_id="city_day_mad",
                         poll="pm25",
                         source_city = list("openaq_government"="lahore"),
                                            # "cpcb"="delhi"),
                         with_geometry = T,
                         with_metadata = T)

# # For debugging purposes
# m.dew.fast <- creadeweather::deweather(meas=m,
#                    poll="pm25",
#                    output="anomaly",
#                    add_fire = F,
#                    add_pbl=F,
#                    training_start_anomaly="2017-01-01",
#                    training_end_anomaly="2021-12-31",
#                    lag=2,
#                    training.fraction=0.9
# )

m.dew <- creadeweather::deweather(meas=m,
                   poll="pm25",
                   output=c("anomaly","anomaly_yday"),
                   add_fire = T,
                   fire_mode = "trajectory",
                   training_start_anomaly="2017-01-01",
                   training_end_anomaly="2021-12-31",
                   lag=c(0,1,2,3),
                   training.fraction=c(0.8, 0.9, 1),
                   save_weather_filename="weather.RDS",
                   read_weather_filename="weather.RDS",
                   keep_model = T,
                   link=c("linear","log"),
                   upload_results = F
                   )

saveRDS(m.dew, "m.dew.RDS")


m.plot <- m.dew %>%
  filter(output%in%c("counterfactual_nofire","counterfactual_nofire_yday")) %>%
  tidyr::unnest(normalised) %>%
  mutate(date=lubridate::date(date)) %>%
  mutate(yday=stringr::str_detect(output,"yday")) %>%
  left_join(m %>% mutate(date=lubridate::date(date)) %>%
              select(location_id, date, observed=value),
            by=c("location_id","date")) %>%
  tidyr::gather("type","value", observed, predicted, predicted_nofire) %>%
  select(location_id, date, value, type, yday, process_deweather, model) %>%
  rowwise() %>%
  mutate(lag=jsonlite::fromJSON(process_deweather)$lag,
         training.fraction=jsonlite::fromJSON(process_deweather)$training.fraction,
         link=jsonlite::fromJSON(process_deweather)$link) %>%
  mutate(rmse_cv=model$rmse_cv,
         rmse_valid=model$rmse_valid,
         rsquared_training=model$rsquared_training) %>%
  select(-c(model))


m.plot.meta <- m.plot %>%
  distinct(location_id, lag, yday, link, training.fraction, rmse_cv, rmse_valid, rsquared_training) %>%
  rowwise() %>%
  mutate(label=paste(
    paste0(link),
    paste0("rmse_cv: ", round(rmse_cv, digits = 2)),
    paste0("rmse_valid: ", round(rmse_valid,digits = 2)),
    paste0("R2 training: ", round(rsquared_training,digits = 2)),
    sep="\n"))

plt <- ggplot(m.plot %>% rcrea::utils.running_average(7)) +
  geom_line(aes(date, value, col=type, linetype=link)) +
  geom_text(data=m.plot.meta,
            inherit.aes = F,
            aes(x=lubridate::date("2019-04-01"),
                y=ifelse(link=="linear", 300, 100),
                label=label),
            hjust=0,
            size=3) +
  facet_grid(lag+yday~training.fraction+location_id)

ggsave('ts.png', plot=plt, width=25, height=20)


m.plot.month <- m.dew  %>%
  filter(output%in%c("counterfactual_nofire","counterfactual_nofire_yday")) %>%
  tidyr::unnest(normalised) %>%
  mutate(date=lubridate::date(date)) %>%
  left_join(m %>% mutate(date=lubridate::date(date)) %>%
              select(date, observed=value)) %>%
  group_by(process_deweather, model, month=lubridate::floor_date(date, "month")) %>%
  summarise_at(c("observed","predicted","predicted_nofire"), mean, na.rm=T) %>%
  rowwise() %>%
  mutate(lag=jsonlite::fromJSON(process_deweather)$lag) %>%
  mutate(link=jsonlite::fromJSON(process_deweather)$link) %>%
  mutate(yday=jsonlite::fromJSON(process_deweather)$time_vars) %>%
  mutate(training.fraction=jsonlite::fromJSON(process_deweather)$training.fraction) %>%
  mutate(rmse_cv=model$rmse_cv,
         rmse_valid=model$rmse_valid,
         rsquared_training=model$rsquared_training) %>%
  tidyr::gather("indicator","value",observed, predicted, predicted_nofire)



ggplot(m.plot, aes(y=value, x=month, fill=indicator)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(data=m.plot.meta,
            inherit.aes = F,
            aes(x=lubridate::date("2019-04-01"),
                y=200,
                label=label),
            hjust=0,
            size=3) +
  scale_x_date(date_minor_breaks = "1 month") +
  facet_grid(lag+yday+link~training.fraction)

ggsave('lahore.bar.png', width=25,height=20)

readRDS("lahore.weather.RDS") %>%
  filter(process_id=="city_day_mad") %>%
  select(meas_weather) %>%
  tidyr::unnest(meas_weather) %>%
  rename(pm25=value) %>%
  tidyr::gather("indicator","value",-c(date, timezone)) %>%
  ggplot(aes(date,as.numeric(value))) +
  geom_line() +
  facet_wrap(~indicator, scales="free")




# Fire count --------------------------------------------------------------
m.fire <- m %>%
  distinct(station_id=location_id, geometry) %>%
  mutate(extent=frp.circular_extent(geometry,buffer_km=100)) %>%
  left_join(
    tibble(station_id=unique(m$location_id)) %>%
      tidyr::crossing(date_fire=seq.Date(lubridate::date("2016-10-01"),
                                         lubridate::date("2020-12-31"),
                                         by="day"))
  ) %>%
  mutate(date_meas=date_fire)


frp.active.download(date_from=min(m.fire$date_fire),
                    date_to=max(m.fire$date_fire))

m.fire <- frp.active.attach(m.fire,
                            one_extent_per_date=T,
                            duration_hour=24)

require(lubridate)
ggplot(m.fire %>%
         left_join(m %>% distinct(station_id=location_id, location_name)) %>%
         rcrea::utils.running_average(7, vars_to_avg = c("fire_frp","fire_count")) %>%
         mutate(year=lubridate::year(date),
                date0='year<-'(date,1))) +
  geom_line(aes(date0, fire_count, col=factor(year))) +
  facet_wrap(~location_name, scales="free_y") +
  rcrea::theme_crea() +
  scale_color_brewer(palette="Reds", name=NULL) +
  theme(panel.grid.minor.x = element_line("grey95"),
        panel.grid.major.x = element_line("grey95")
        ) +
  labs(subtitle="Number of daily active fires within 100km. 7-day running average",
       caption="Source: CREA analysis based on VIIRS (Suomi NPP)",
       y="Number of active daily fires",
       x=NULL) +
  scale_x_date(date_labels = "%b",
               date_breaks = "1 month")
ggsave("ts.fire.png", width=10, height=5)




# Trajectories ------------------------------------------------------------
trajs <- m %>%
  distinct(station_id=location_id, geometry) %>%
  rowwise() %>%
  mutate(trajs=list(creadeweather::frp.trajs_at_dates("2020-05-10",
                                       unique(station_id),
                                       unique(geometry)[[1]],
                                       met_type="gdas1",
                                       heights=500,
                                       duration_hour=72))) %>%
  tidyr::unnest(trajs) %>%
  mutate(date_meas=lubridate::date(traj_dt_i)) %>%
  mutate(date_fire=lubridate::date(traj_dt)) %>%
  tidyr::nest(trajs=-c(station_id, date_meas, date_fire)) %>%
  rowwise() %>%
  mutate(extent=frp.traj_extent(trajs=trajs, buffer_km=10))

m.trajs <- utils.attach.basemaps(m)

