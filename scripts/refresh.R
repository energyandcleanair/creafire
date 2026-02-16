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
  process_id='city_day_mad'
  )

config_pakistan <- tibble(
  city=c("Lahore","Islamabad"),
  source='openaq_government',
  poll=list(c("pm25")),
  level='city',
  process_id='city_day_mad'
  )


config_thailand <- tibble(
  city=c("Bangkok","Chiang Mai","Chiang Rai"),
  source='air4thai',
  poll=list(c("pm25")),
  level='city',
  process_id='city_day_mad'
  )

config_malaysia <- tibble(
  city=c("Kuala Lumpur"),
  source='doemy',
  poll=list(c("pm25")),
  level='city',
  process_id='city_day_mad'
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
                upload_results = T,
                upload_fire = T,
                add_fire = T,
                output=c('trend'),
                weather_vars=c('air_temp_min','air_temp_max', 'atmos_pres', 'wd', 'ws_max', 'precip', 'RH_max'),
                )
       })