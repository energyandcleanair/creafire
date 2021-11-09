require(creadeweather)
require(tidyverse)
source('fill_dashboard.R')


configs <- list(
    c(city="Jakarta", country="ID", source="openaq_government"),
    c(city="Lahore", country="PK", source="openaq_government"),
    # c(city="Delhi", country="IN", source="cpcb"),
    c(city="Beijing", country="CN", source="mee", date_from="2016-01-01"),
    c(city="Tianjin", country="CN", source="mee"),
    c(city="Bandung", country="ID", source="airvisual", date_from=NA),
    c(city="Surabaya", country="ID", source="airvisual"),
    c(city="Pekanbaru", country="ID", source="airvisual"),
    # c(city="Xiamen", country="CN", source="mee"),
    # c(city="Shanghai", country="CN", source="mee"),
    # c(city="xi'an", country="CN", source="mee"),
    c(city="bangkok", country="TH", source="airvisual", date_from="2017-01-01"),
    c(city="chiangmai", country="TH", source="airvisual"),
    c(city="Los Angeles", country="US", source="openaq_government")
  ) %>%
  do.call(bind_rows, .) %>%
  as.data.frame() %>%
  tidyr::crossing(
    duration_hour=c(72,120),
    buffer_km=c(20,50),
    height=c(NA, 50)
  ) %>%
  mutate(date_from=tidyr::replace_na(date_from, "2017-01-01"))

print(configs)
lapply(seq(nrow(configs)),
       function(i){
         c <- configs[i,]
         fill_dashboard(city=c$city,
                        source=c$source,
                        country=c$country,
                        duration_hour=c$duration_hour,
                        buffer_km=c$buffer_km,
                        date_from=c$date_from,
                        height=c$height)
       })

# Also perform without fire to compare MAE and see if fire brings something
configs.nofire <- configs %>% distinct(city, country, source, date_from)

lapply(seq(nrow(configs.nofire)),
       function(i){
         c <- configs.nofire[i,]

         m <- rcrea::measurements(country=c$country,
                                  process_id="city_day_mad",
                                  poll="pm25",
                                  city=c$city,
                                  date_from=c$date_from,
                                  source=c$source,
                                  with_geometry = T,
                                  with_metadata = T)

         location_id <- unique(m$location_id)

         file.meas <- file.path("upload", sprintf("%s.meas.RDS", location_id))

         print("Deweathering")
         m.dew <- creadeweather::deweather(meas=m,
                                           poll="pm25",
                                           output="anomaly",
                                           add_fire = F,
                                           training_start_anomaly=c$date_from,
                                           training_end_anomaly="2021-12-31",
                                           lag=3,
                                           training.fraction=0.9,
                                           keep_model = T,
                                           link=c("linear"),
                                           upload_results = F
         )

         # Export measurements -----------------------------------------------------
         m.dew %>%
           filter(output %in% c("counterfactual")) %>%
           tidyr::unnest(normalised) %>%
           dplyr::select(
             location_id, date, poll, unit, source, observed, predicted, model
           ) %>%
           saveRDS(file.meas)
       })
