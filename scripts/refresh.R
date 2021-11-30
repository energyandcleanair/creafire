install.packages('tidyverse',repos='http://cran.us.r-project.org')
library(tidyverse)
library(remotes)

remotes::install_github('energyandcleanair/creadeweather', upgrade=F)
remotes::install_github('energyandcleanair/creafire', upgrade=F)
remotes::install_github('energyandcleanair/rcrea', upgrade=F)

library(creadeweather)
library(tidyverse)
library(creafire)


configs <- tibble(
  city=c("Chandigarh","Delhi", "Varanasi", "Kolkata", "Gurugram", "Lucknow"),
  source='cpcb',
  poll=list(c("pm25","pm10")),
  level='city',
  process_id='city_day_mad',
  duration_hour=120,
  buffer_km=50,
  height=10,
  date_from="2016-01-01",
  fire_source="viirs")

print(configs)
lapply(seq(nrow(configs)),
       function(i){
         c <- configs[i,]
         defire(city=c$city,
                level=c$level,
                poll=c$poll[[1]],
                source=c$source,
                process_id=c$process_id,
                duration_hour=c$duration_hour,
                fire_source=c$fire_source,
                buffer_km=c$buffer_km,
                date_from=c$date_from,
                height=c$height,
                upload_folder = "upload",
                force_recompute_weather=F)
       })