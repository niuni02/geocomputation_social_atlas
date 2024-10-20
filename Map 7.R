# MAP 7 - point pattern analysis on migration levels

# libraries
library(tidyverse)
library(sf)
library(ggplot2)
library(tmap)
library(readr)
library(dplyr)
library(RColorBrewer)
library(janitor)
library(spatstat)
library(spData)

# load in the London MSOAs shapefile
london_MSOA_shp <- st_read("raw/boundaries/MSOA_2011_London_gen_MHW.shp")
# select relevant columns
colnames(london_MSOA_shp)
london_MSOA_shp <- select(london_MSOA_shp, "MSOA11CD", "LAD11CD", "LAD11NM", "geometry")
View(london_MSOA_shp)

# read in migration levels dataset
migration_raw <- read_csv("raw/migration_levels.csv")
migration_sdf <- merge(migration_raw, london_MSOA_shp, 
                           by.x = "GEO_CODE", by.y = "MSOA11CD")
migration_sdf <- st_sf(migration_sdf)
# convert multipolygon points into latitude and longitude points
migration_sdf %>% 
  transmute(lon = list(st_coordinates(.)[, 1]),
            lat = list(st_coordinates(.)[, 2])) %>% 
  unnest(lon, lat) %>% 
  st_drop_geometry()
View(migration_sdf)

# first need to create a ppp object
window <- as.owin(london_MSOA_shp$geometry)
migration_xy <- migration_sdf %>%
  st_coordinates()
migration_ppp <- ppp(x = migration_xy[, 1], 
                     y = migration_xy[, 2], 
                     window = window)
plot(migration_ppp)
# remove illegal points
migration_ppp <- as.ppp(migration_ppp)
plot(migration_ppp)
# remove duplicated points since all events are unique
anyDuplicated(migration_ppp)
  # returned as TRUE
sum(multiplicity(migration_ppp) > 1)
  # returned as 2022
migration_ppp_jitter <- rjitter(migration_ppp, 
                                retry = TRUE, 
                                nsim = 1, 
                                drop = TRUE)
anyDuplicated(migration_ppp_jitter)
  # returned as FALSE
plot(migration_ppp_jitter)

# now creating a kernal density estimation
plot(density.ppp(migration_ppp_jitter, sigma = 100))

# using a 500m bandwith
plot(density.ppp(migration_ppp_jitter, sigma = 500))

plot(density.ppp(migration_ppp_jitter, sigma = 300),
     main = "Kernel Density Estimation (KDE) of Migration Levels in 2011") +
  tm_compass(position = c("RIGHT", "TOP"), size = 2, type = "rose")
# bandwidth 300m helps minimize over/under smoothing