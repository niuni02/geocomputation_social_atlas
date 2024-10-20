# Map 8 - comparing wellbeing indicators with main language spoken

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

# load in the London MSOAs shapefile
london_MSOA_shp <- st_read("raw/boundaries/MSOA_2011_London_gen_MHW.shp")
# select relevant columns
colnames(london_MSOA_shp)
london_MSOA_shp <- select(london_MSOA_shp,"MSOA11CD", "MSOA11NM", "LAD11CD", "LAD11NM", "geometry")
View(london_MSOA_shp)

# read in London Boroughs shapefile
london_boroughs_shp <- st_read("raw/boundaries/London_Borough_Excluding_MHW.shp")
# select relevant columns
colnames(london_boroughs_shp)
london_boroughs_shp <- select(london_boroughs_shp, "NAME", "GSS_CODE", "geometry")
View(london_boroughs_shp)

# read in the wellbeing dataset
wellbeing_raw <- read_csv("raw/wellbeing.csv", 
                      col_types = cols(life_satisfaction = col_number(), 
                                       worthwhile = col_number()))
View(wellbeing_raw)
# using the mutate() function to calculate the average score between life satisfaction and worthwhile
wellbeing <- mutate(wellbeing_raw, average = (life_satisfaction + worthwhile)/2)
View(wellbeing)
# combine with shapefile to create a spatial data frame
wellbeing_sdf <- merge(wellbeing, london_boroughs_shp,
                              by.x = "Code", by.y = "GSS_CODE")
wellbeing_sdf <- st_sf(wellbeing_sdf)
View(wellbeing_sdf)

# read in languages dataset
main_language_raw <- read_csv("raw/main_language.csv", 
                          col_types = cols(total = col_integer(), 
                                           english = col_integer()))
View(main_language_raw)
# using the mutate() function to calculate the number of non-english languages as the main language
main_language <- mutate(main_language_raw, non_english = total - english)
View(main_language)
# combine with shapefile to create a spatial data frame
main_language_sdf <- merge(main_language, london_MSOA_shp,
                       by.x = "GEO_CODE", by.y = "MSOA11CD")
main_language_sdf <- st_sf(main_language_sdf)
View(main_language_sdf)

# plot the two indicators together
tm_shape(main_language_sdf) + 
  tm_fill("non_english", palette = "Blues") +
  tm_shape(wellbeing_sdf) + 
  tm_text("average", size = 0.75) +
  tm_shape(london_boroughs_shp) + tm_borders(col = "black", lwd = 0.5) + 
  tm_layout(
    main.title = "Comparing non-English as main language with wellbeing scores acrosss London in 2011",
    main.title.fontface = 2, 
    fontfamily = "Helvetica", 
    main.title.size = 1,
    legend.outside = FALSE,
    legend.outside.position = "right",
    legend.title.size = 1,
    legend.title.fontface = 2,
    frame = FALSE) +
  tm_compass(type = "rose", position = c("right", "top"), size = 2.5) +
  tm_scale_bar(breaks = c(0, 5, 10, 15, 20), position = c("left", "bottom"))