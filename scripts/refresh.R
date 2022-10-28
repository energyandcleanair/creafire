install.packages('tidyverse',repos='http://cran.us.r-project.org')
install.packages('curl',repos='http://cran.us.r-project.org')
install.packages('googleAuthR',repos='http://cran.us.r-project.org')

library(tidyverse)
library(remotes)

remotes::install_github('energyandcleanair/creadeweather', upgrade=F)
remotes::install_github('hubert-thieriot/splitr', upgrade=F)
remotes::install_github('energyandcleanair/creatrajs', upgrade=F)
remotes::install_github('energyandcleanair/creafire', upgrade=F)
remotes::install_github('energyandcleanair/rcrea', upgrade=F)


# Faced HTTP/2 stream 0 was not closed cleanly: PROTOCOL_ERROR
# Fixing attempt: force using HTTP 1.1 (the number 2 is an enum value)
handle <- curl::new_handle(verbose = TRUE)
curl::handle_setopt(handle, http_version = 2)
httr::set_config(httr::config(http_version = 2))

library(creadeweather)
library(tidyverse)
library(creafire)


config_india <- tibble(
  city=c("Chandigarh","Delhi", "Varanasi", "Kolkata", "Gurugram", "Lucknow"),
  source='cpcb',
  poll=list(c("pm25","pm10")),
  level='city',
  process_id='city_day_mad',
  # duration_hour=120,
  # buffer_km=50,
  # height=10,
  # date_from="2016-01-01",
  # fire_source="viirs"
  )

config_pakistan <- tibble(
  city=c("Lahore","Islamabad"),
  source='openaq_government',
  poll=list(c("pm25")),
  level='city',
  process_id='city_day_mad',
  # duration_hour=120,
  # buffer_km=50,
  # height=10,
  # date_from="2016-01-01",
  # fire_source="viirs"
  )


config_thailand <- tibble(
  city=c("Bangkok","Chiang Mai","Chiang Rai"),
  source='air4thai',
  poll=list(c("pm25")),
  level='city',
  process_id='city_day_mad',
  # duration_hour=120,
  # buffer_km=50,
  # height=10,
  # date_from="2016-01-01",
  # fire_source="viirs"
  )

config_malaysia <- tibble(
  city=c("Kuala Lumpur"),
  source='doemy',
  poll=list(c("pm25")),
  level='city',
  process_id='city_day_mad',
  # duration_hour=120,
  # buffer_km=50,
  # height=10,
  # date_from="2016-01-01",
  # fire_source="viirs"
  )


configs <- bind_rows(
  config_thailand,
  config_pakistan,
  config_thailand,
  config_malaysia,
  config_india
)

print(configs)
lapply(seq(nrow(configs)),
       function(i){
         c <- configs[i,]
         creadeweather::deweather(city=c$city,
                # level=c$level,
                poll=c$poll[[1]],
                source=c$source,
                process_id=c$process_id,
                # duration_hour=c$duration_hour,
                # fire_source=c$fire_source,
                # buffer_km=c$buffer_km,
                # date_from=c$date_from,
                # height=c$height,
                # upload_folder = "upload",
                # force_recompute_weather=F,
                upload_results = T,
                upload_fire = T,
                add_fire = T,
                output=c('trend'),
                weather_vars=c('air_temp_min','air_temp_max', 'atmos_pres', 'wd', 'ws_max', 'precip', 'RH_max'),
                )
       })


# Also deweather data for darhboard
# deweather(city=unique(configs$city),
#           source=unique(configs$source),
#           poll=unique(unlist(configs$poll)))


# And calculate regional fires!
# g1 <- creahelpers::get_adm(level=1, iso2s=c("IN","PK","TH","MY"))
# g2 <- creahelpers::get_adm(level=2, iso2s=c("IN","PK","TH","MY"))
#
# g <- bind_rows(sf::st_as_sf(g1) %>% mutate(level=1),
#                sf::st_as_sf(g2) %>% mutate(level=2))
#
# readRenviron(".Renviron")
#
# f <- creatrajs::fire.aggregate("2021-12-01", "2021-12-10", geometries = g)
#
# f %>%
#   select_at(grep("date|GID_.*|NAME_.*",names(.)))

