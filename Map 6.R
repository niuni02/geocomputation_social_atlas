# Map 6 - plotting migration level change across London

# libraries
library(tidyverse)
library(sf)
library(ggplot2)
library(tmap)
library(readr)
library(dplyr)
library(RColorBrewer)


# load in the London MSOAs shapefile
london_MSOA_shp <- st_read("raw/boundaries/MSOA_2011_London_gen_MHW.shp")
# select relevant columns
colnames(london_MSOA_shp)
london_MSOA_shp <- select(london_MSOA_shp, "MSOA11CD", "MSOA11NM", "LAD11CD", "LAD11NM", "geometry")
View(london_MSOA_shp)

# read in London Boroughs shapefile
london_boroughs_shp <- st_read("raw/boundaries/London_Borough_Excluding_MHW.shp")
# select relevant columns
colnames(london_boroughs_shp)
london_boroughs_shp <- select(london_boroughs_shp, "NAME", "GSS_CODE", "geometry")
View(london_boroughs_shp)

# read in migration difference dataset
migration_diff <- read_csv("raw/migration_diff.csv", 
                           col_types = cols(migration_2001 = col_integer(), 
                                            migration_2011 = col_integer(), difference = col_integer()))
View(migration_diff)

# combine with shapefile to create a spatial data frame
migration_diff <- merge(migration_diff, london_MSOA_shp, 
                       by.x = "GEO_CODE", by.y = "MSOA11CD")
migration_diff <- st_sf(migration_diff)

# plotting the map
tm_shape(migration_diff) + tm_fill(col = "difference", palette = c("red", "lightgreen"), n = 2) +
  tm_shape(london_boroughs_shp) + tm_borders(col = "black") +
  tm_layout(
    main.title = "Differences in migration levels over 10 years (2001 - 2011)",
    main.title.fontface = 2, fontfamily = "Helvetica", main.title.size = 1,
    legend.outside = FALSE,
    legend.outside.position = "right",
    legend.title.size = 1,
    legend.title.fontface = 2,
    frame = FALSE) +
  tm_compass(type = "rose", position = c("right", "top"), size = 2) +
  tm_scale_bar(breaks = c(0, 5, 10, 15, 20), position = c("left", "bottom"))