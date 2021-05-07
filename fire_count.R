library(creatrajs)
library(sp)
library(sf)
library(GADMTools)
library(tidyverse)
library(rcrea)


g <- GADMTools::gadm_sf.loadCountries(c("IDN","VNM","THA","PHL"),2)
g <- g$sf

creatrajs::fire.download("2010-01-01","2020-12-31")
extent.sp <- sf::st_bbox(g) %>% sf::st_as_sfc() %>% as("Spatial")

f <- creatrajs::fire.read(date_from="2010-01-01",
                          date_to="2020-12-31",
                          extent.sp = extent.sp
                          )


saveRDS(f, "fire_count/fires.RDS")
f <- readRDS("fire_count/fires.RDS")

g.fire <- g %>%
  sf::st_join(f, left=F) %>%
  as.data.frame()

names(g.fire)
# saveRDS(g.fire, "fire_count/g.fire.RDS")
# g.fire <- readRDS("fire_count/g.fire.RDS")

g.sum <- g.fire %>%
  group_by(NAME_0, NAME_1, year=lubridate::year(acq_date)) %>%
  summarise(count=n(),
            frp=sum(frp, na.rm=T))
saveRDS(g.sum, "fire_count/g.sum.RDS")
g.sum <- readRDS("fire_count/g.sum.RDS")

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

ggplot(g.sum %>% filter(NAME_0=="Indonesia")) +
  geom_bar(stat="identity",
           aes(factor(year), count)) +
  facet_wrap(~NAME_1) +
  theme_crea() +
  labs(title="Number of fires in Indonesia",
       y=NULL,
       x=NULL) +
  scale_x_discrete(breaks=seq(2012,2020))

