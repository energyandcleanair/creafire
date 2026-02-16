# Validating results against Source Apportionment studies
library(creadeweather)
library(tidyverse)

setwd('../defire')


# Tianjin -----------------------------------------------------------------
location_id <- "tianjin_chn.27_1_cn"

fs <- list.files("upload",paste0(location_id, ".meas*"), full.names = T)
m.dew <- lapply(fs,function(f){
  details <- stringr::str_match(basename(f),"meas\\.(\\w*)\\.(\\w*)")
  readRDS(f) %>% mutate(buffer=details[2],
                        duration=details[3])
}) %>% do.call(bind_rows,.)


saveRDS(m.dew, file.path("validation","m.dew.tianjin.RDS"))

# “The source apportionment of PM 1 and PM 2.5 by positive matrix factorization (PMF)
# model indicated the main sources of secondary aerosols (25% and 34%),
# biomass burning (17% and 20%), traffic emission (20% and 14%),
# and coal combustion (17% and 14%).” …
# “The daily sampling (23 hr, from 9:40 am to 8:40 am the
# next day) was conducted during early summer (warm season) from
# May 06, 2018 to July 04, 2018”

date_from <- "2018-05-06"
date_to <- "2018-07-04"
contrib.crea <- m.dew %>%
  filter(date>=date_from,
         date<=date_to) %>%
  group_by(location_id, poll, buffer, duration) %>%
  summarise(contrib=1-mean(predicted_nofire, na.rm=T)/mean(predicted, na.rm=T)) %>%
  mutate(config=paste(buffer, duration, sep=" - "),
         source="CREA")

contrib.off <- tibble(location_id,
                      poll="pm25",
                      config="PMF",
                      contrib=0.2,
                      source="Khan et al. 2021")

ggplot(bind_rows(contrib.crea, contrib.off),
       aes(config, contrib, fill=source)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(round(contrib*100,1),"%")),
            vjust=-0.3) +
  rcrea::theme_crea() +
  scale_y_continuous(expand=expansion(mult=c(0,1.1), add=0)) +
  labs(title="Biomass burning contribution to PM2.5 levels in Tianjin",
       subtitle=sprintf("%s to %s", date_from, date_to),
       y=NULL, x=NULL
       )




# Beijing -----------------------------------------------------------------
# Need to run it before 2017

location_id <- "beijing_chn.2_1_cn"

fs <- list.files("upload",paste0(location_id, ".meas*"), full.names = T)
m.dew <- lapply(fs,function(f){
  details <- stringr::str_match(basename(f),"meas\\.(\\w*)\\.(\\w*)")
  readRDS(f) %>% mutate(buffer=details[2],
                        duration=details[3])
}) %>% do.call(bind_rows,.)


saveRDS(m.dew, file.path("validation","m.dew.beijing.RDS"))
# the source contribution in Beijing ranked as secondary sources (44 %) > 
# traffic sources (18 %) > coal combustion (16 %) > 
#  biomass burning (9 %) > industrial sources (8 %) > dust (5 %)
# Online sampling of PM2.5 was conducted from November to December 2016 in winter


date_from <- "2016-11-01"
date_to <- "2016-12-31"
contrib.crea <- m.dew %>%
  filter(date>=date_from,
         date<=date_to) %>%
  group_by(location_id, poll, buffer, duration) %>%
  summarise(contrib=1-mean(predicted_nofire, na.rm=T)/mean(predicted, na.rm=T)) %>%
  mutate(config=paste(buffer, duration, sep=" - "),
         source="CREA")

contrib.off <- tibble(location_id,
                      poll="pm25",
                      config="PMF",
                      contrib=0.09,
                      source="Liu et al. 2018")

ggplot(bind_rows(contrib.crea, contrib.off),
       aes(config, contrib, fill=source)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(round(contrib*100,1),"%")),
            vjust=-0.3) +
  rcrea::theme_crea() +
  scale_y_continuous(expand=expansion(mult=c(0,1.1), add=0)) +
  labs(title="Biomass burning contribution to PM2.5 levels in Beijing",
       subtitle=sprintf("%s to %s", date_from, date_to),
       y=NULL, x=NULL
  )



# Shanghai -----------------------------------------------------------------

location_id <- "shanghai_chn.24_1_cn"

fs <- list.files("upload",paste0(location_id, ".meas*"), full.names = T)
m.dew <- lapply(fs,function(f){
  details <- stringr::str_match(basename(f),"meas\\.(\\w*)\\.(\\w*)")
  readRDS(f) %>% mutate(buffer=details[2],
                        duration=details[3])
}) %>% do.call(bind_rows,.)

saveRDS(m.dew, file.path("validation","m.dew.shanghai.RDS"))
# MM-PMF 24 identified 11 types of pollution sources, with biomass burning,
# cooking and secondary organic aerosol (SOA) as the 25 additional sources identified.
# The three sources accounted for 4.9%, 2.6% and 14.7% of the total PM2.5 mass,
# respectively
# the average contribution 27 of secondary pollution sources is as high as
# 63.8% of the total PM2.5 mass.
# [...] was conducted in Shanghai from November 9 to December 3, 2018

date_from <- "2018-11-09"
date_to <- "2018-12-03"

contrib.crea <- m.dew %>%
  filter(date>=date_from,
         date<=date_to) %>%
  group_by(location_id, poll, buffer, duration) %>%
  summarise(contrib=1-mean(predicted_nofire, na.rm=T)/mean(predicted, na.rm=T)) %>%
  mutate(config=paste(buffer, duration, sep=" - "),
         source="CREA")

contrib.off <- tibble(location_id,
                      poll="pm25",
                      config="PMF",
                      contrib=0.049,
                      source="Li et al. 2019")

ggplot(bind_rows(contrib.crea, contrib.off),
       aes(config, contrib, fill=source)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(round(contrib*100,1),"%")),
            vjust=-0.3) +
  rcrea::theme_crea() +
  scale_y_continuous(expand=expansion(mult=c(0,1.1), add=0)) +
  labs(title="Biomass burning contribution to PM2.5 levels in Shanghai",
       subtitle=sprintf("%s to %s", date_from, date_to),
       y=NULL, x=NULL
  )

# Jakarta -----------------------------------------------------------------
location_id <- "jakarta_idn.7_1_id"

fs <- list.files("upload",paste0(location_id, ".meas*"), full.names = T)
m.dew <- lapply(fs,function(f){
  details <- stringr::str_match(basename(f),"meas\\.(\\w*)\\.(\\w*)")
  readRDS(f) %>% mutate(buffer=details[2],
                        duration=details[3])
}) %>% do.call(bind_rows,.)

saveRDS(m.dew, file.path("validation","m.dew.jakarta.RDS"))

# Open burning of biomass or other fuels: 11% in the outskirts of the city (KJ and LB) in wet season.
# Open burning: 9% in the east (LB) in Dry season
# One wet season (October 2018–March 2019) and one dry
# season (July–September 2019)

date_from_wet <- "2018-10-01"
date_to_wet <- "2019-03-31"
date_from_dry <- "2019-07-01"
date_to_dry <- "2019-09-30"

contrib.crea <- m.dew %>%
  filter(date>=date_from_wet,
         date<=date_to_wet) %>%
  mutate(season="wet") %>%
  bind_rows(.,
            m.dew %>%
              filter(date>=date_from_dry,
                     date<=date_to_dry) %>%
              mutate(season="dry")) %>%
  group_by(location_id, season, poll, buffer, duration) %>%
  summarise(contrib=1-mean(predicted_nofire, na.rm=T)/mean(predicted, na.rm=T)) %>%
  mutate(config=paste(buffer, duration, sep=" - "),
         source="CREA")

contrib.off <- tibble(location_id,
                      poll="pm25",
                      config="Receptor Model",
                      season=c("wet","dry"),
                      contrib=c(0.11,0.09),
                      source="ITB")

ggplot(bind_rows(contrib.crea, contrib.off),
       aes(config, contrib, fill=source)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(round(contrib*100,1),"%")),
            vjust=-0.3) +
  facet_wrap(~season) +
  rcrea::theme_crea() +
  scale_y_continuous(limits=c(0,NA), expand=expansion(mult=c(0,1.1), add=0)) +
  labs(title="Biomass burning contribution to PM2.5 levels in Jakarta",
       subtitle=sprintf("Dry: %s to %s | Wet: %s to %s",
                        date_from_dry, date_to_dry,
                        date_from_wet, date_to_wet
                        ),
       y=NULL, x=NULL
  )



# Bangkok -----------------------------------------------------------------

location_id <- "bangkok_tha.3_1_th"
fs <- list.files("upload", paste0(location_id, ".meas*"), full.names = T)

m.dew <- lapply(fs,function(f){
  details <- stringr::str_match(basename(f),"meas\\.(\\w*)\\.(\\w*)\\.(\\w*)")
  readRDS(f) %>% mutate(buffer=details[2],
                        duration=details[3],
                        height=details[4])
}) %>% do.call(bind_rows,.)


saveRDS(m.dew, file.path("validation","m.dew.bangkok.RDS"))

# Principal Component Analysis (PCA) successfully classified five principal sources of PM2.5,
# including vehicular exhaust, biomass burning, sea salt aerosol, power plant,
# and industrial emissions, which accounted for 43.7%, 24.0%, 10.5%, 6.48%, and 4.46%, respectively.
# The monitoring campaign was conducted from August 2017 to March 2018

date_from <- "2017-08-01"
date_to <- "2018-03-31"

contrib.crea <- m.dew %>%
  filter(date>=date_from,
         date<=date_to) %>%
  group_by(location_id, poll, buffer, duration, height) %>%
  summarise(contrib=1-mean(predicted_nofire, na.rm=T)/mean(predicted, na.rm=T)) %>%
  mutate(config=paste(buffer, duration, height, sep=" - "),
         source="CREA")

contrib.off <- tibble(location_id,
                      poll="pm25",
                      config="Receptor Model",
                      contrib=c(0.24),
                      source="ChooChuay et al. 2020")

ggplot(bind_rows(contrib.crea, contrib.off),
       aes(config, contrib, fill=source)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(round(contrib*100,1),"%")),
            vjust=-0.3) +
  rcrea::theme_crea() +
  scale_y_continuous(limits=c(0,NA), expand=expansion(mult=c(0,1.1), add=0)) +
  labs(title="Biomass burning contribution to PM2.5 levels in Bangkok",
       subtitle=sprintf("%s to %s",
                        date_from,
                        date_to
       ),
       y=NULL, x=NULL
  )




