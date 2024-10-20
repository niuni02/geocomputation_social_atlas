# MAP 2 - Choropleth map showing life expectancies across London

# libraries
library(tidyverse)
library(sf)
library(ggplot2)
library(tmap)
library(readr)
library(dplyr)
library(rgdal)
library(rgeos)
library(spdep)

# set 'raw' folder as working directory
# read in life expectancy data from the raw folder
life_expectancy_raw <- read_csv("raw/life_expectancy.csv", 
                                col_types = cols(male = col_number(), female = col_number()))
life_expectancy <- mutate(life_expectancy_raw, avg_life_expectancy = (male + female)/2)
sapply(life_expectancy, class)

# read in London MSOAs shapefile
london_MSOA_shp <- st_read("raw/boundaries/MSOA_2011_London_gen_MHW.shp")
# select relevant columns
colnames(london_MSOA_shp)
london_MSOA_shp <- select(london_MSOA_shp, "MSOA11CD", "LAD11CD", "LAD11NM", "geometry")
View(london_MSOA_shp)

# read in London Boroughs shapefile
london_boroughs_shp <- st_read("raw/boundaries/London_Borough_Excluding_MHW.shp")
# select relevant columns
colnames(london_boroughs_shp)
london_boroughs_shp <- select(london_boroughs_shp, "NAME", "GSS_CODE", "geometry")
View(london_boroughs_shp)

# joining the life expectancy data file with the London boroughs shp file
life_expectancy_sdf <- merge(life_expectancy, london_boroughs_shp, 
                                 by.x = "area_code", by.y = "GSS_CODE")
View(life_expectancy_sdf)
sapply(life_expectancy_sdf, class)
life_expectancy_sdf <- st_sf(life_expectancy_sdf)
qtm(life_expectancy_sdf)

# creating choropleth map showing life expectancies
tmap_mode("plot")
qtm(life_expectancy_sdf, fill="avg_life_expectancy", border="black", fill.style="quantile", fill.palette="Oranges", 
    fill.title= "Life expectancy from birth (years)", frame = FALSE) +
  tm_layout(
    main.title = "Average life expectancy from birth in 2011 to 2013",
    legend.outside = TRUE,
    legend.outside.position = "right",
    legend.title.size = 1,
    legend.title.fontface = 2
  ) +
  tm_compass(position = c("RIGHT", "TOP"), size = 3) +
  tm_scale_bar(breaks = c(0, 5, 10, 15, 20), position = c("LEFT", "BOTTOM"), size = 0.75)