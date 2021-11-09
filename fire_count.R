library(creatrajs)
library(sp)
library(sf)
library(GADMTools)
library(tidyverse)
library(rcrea)

g <- lapply(c("IDN","VNM","THA","PHL","MMR","LAO","KHM"), function(iso3){
  GADMTools::gadm_sf.loadCountries(iso3, 2)$sf
}) %>% do.call(rbind, .)

creatrajs::fire.download("2010-01-01","2020-12-31")
extent.sp <- sf::st_bbox(g) %>% sf::st_as_sfc() %>% as("Spatial")

f <- creatrajs::fire.read(date_from="2010-01-01",
                          date_to="2020-12-31",
                          extent.sp = extent.sp
                          )


g.fire <- g %>%
  sf::st_join(f, left=F) %>%
  as.data.frame()


# saveRDS(g.fire, "fire_count/g.fire.RDS")
# g.fire <- readRDS("fire_count/g.fire.RDS")

g.sum <- g.fire %>%
  group_by(NAME_0, NAME_1,
           date=lubridate::date(acq_date)) %>%
  summarise(count=n(),
            frp=sum(frp, na.rm=T))
saveRDS(g.sum, "fire_count/fire_day.RDS")
g.sum <- readRDS("fire_count/fire_day.RDS")

# Compare with earthengine
gee.sum <- read.csv("fire_count/gee_modis_fire_indonesia.csv") %>%
  mutate(date=strptime(substr(gee.sum$system.index,1,10),format="%Y_%m_%d"),
         year=lubridate::year(date),
         count=sum)

g.sum <- g.sum %>% mutate(year=lubridate::year(date))

ggplot(g.sum) +
  geom_bar(stat="identity",
           aes(factor(year), count)) +
  facet_wrap(~NAME_0,
             scales="free_y") +
  theme_crea() +
  labs(title="Number of fires in South East Asian countries",
       y=NULL,
       x=NULL) +
  scale_x_discrete(breaks=seq(2012,2020))



ggplot(gee.sum %>% filter(ADM0_NAME=="Indonesia")) +
  geom_bar(stat="identity",
           aes(factor(year), count)) +
  facet_wrap(~ADM0_NAME) +
  theme_crea() +
  labs(title="Number of fires in Indonesia",
       y=NULL,
       x=NULL) +
  scale_x_discrete(breaks=seq(2012,2020))

ggplot(g.sum %>% filter(NAME_0=="Indonesia")) +
  geom_bar(stat="identity",
           aes(factor(year), count)) +
  facet_wrap(~NAME_1) +
  theme_crea() +
  labs(title="Number of fires in Indonesia",
       y=NULL,
       x=NULL) +
  scale_x_discrete(breaks=seq(2012,2020))


ggplot(g.sum %>%
         group_by(NAME_0, date) %>%
         summarise(value=sum(count, na.rm=T)) %>%
         mutate(date=lubridate::date(date)) %>%
         rcrea::utils.running_average(30) %>%
         mutate(year=lubridate::year(date),
                date = `year<-`(date,0))) +
  geom_line(aes(date, value, col=factor(year))) +
  facet_wrap(~NAME_0, scales="free_y") +
  theme_crea() +
  labs(title="Number of fires in SEA countries",
       y=NULL,
       x=NULL)
