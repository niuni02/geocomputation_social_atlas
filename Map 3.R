# MAP 3 - Distribution of ethnicities across London

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

# read in ethnic groups csv file
ethnic_groups_raw <- read_csv("raw/ethnic_groups.csv", 
                          col_types = cols(total_ethnic_group = col_integer(), 
                                           British = col_integer(), Irish = col_integer(), 
                                           Gypsy = col_integer(), other_White = col_integer(), 
                                           White_Black_Caribbean = col_integer(), 
                                           White_Black_African = col_integer(), 
                                           White_Asian = col_integer(), Other_Mixed = col_integer(), 
                                           Indian = col_integer(), Pakistani = col_integer(), 
                                           Bangladeshi = col_integer(), Chinese = col_integer(), 
                                           Other_Asian = col_integer(), African = col_integer(), 
                                           Caribbean = col_integer(), Other_Black = col_integer(), 
                                           Arab = col_integer(), Any_Other_Ethnic_Group = col_integer()))
View(ethnic_groups_raw)

# using the mutate() function to accumulate and calculate the percentages of each ethnic group
ethnic_groups <- mutate(ethnic_groups_raw, White = (((British + Irish + Gypsy + other_White)/total_ethnic_group) * 100),
                        Mixed = (((White_Black_Caribbean + White_Black_African + White_Asian + Other_Mixed)/total_ethnic_group) * 100),
                        Asian = (((Indian + Pakistani + Bangladeshi + Chinese + Other_Asian)/total_ethnic_group) * 100),
                        Black = (((African + Caribbean + Other_Black)/total_ethnic_group) * 100),
                        Other = (((Arab + Any_Other_Ethnic_Group)/total_ethnic_group) * 100))
View(ethnic_groups)

# load in the London MSOAs shapefile
london_MSOA_shp <- st_read("raw/boundaries/MSOA_2011_London_gen_MHW.shp")
# select relevant columns
colnames(london_MSOA_shp)
london_MSOA_shp <- select(london_MSOA_shp, "MSOA11CD", "LAD11CD", "LAD11NM", "geometry")
View(london_MSOA_shp)

# combining the shapefile with the csv file to create a spatial data frame
ethnic_groups_sdf <- merge(ethnic_groups, london_MSOA_shp, 
                             by.x = "GEO_CODE", by.y = "MSOA11CD")
ethnic_groups_sdf <- st_sf(ethnic_groups_sdf)
View(ethnic_groups_sdf)

# read in London Boroughs shapefile
london_boroughs_shp <- st_read("raw/boundaries/London_Borough_Excluding_MHW.shp")
# select relevant columns
colnames(london_boroughs_shp)
london_boroughs_shp <- select(london_boroughs_shp, "NAME", "GSS_CODE", "geometry")
View(london_boroughs_shp)

# creating a multi-faceted map
map1 <- qtm(ethnic_groups_sdf, fill="White", fill.palette = "Oranges", border = "white", fill.title = "Percentage (%)") + 
  tm_shape(london_boroughs_shp) +
  tm_borders(col = "black") +
  tm_layout(
    title = "White",
    bg.color = "grey85",
    legend.outside = FALSE,
    legend.title.size = 0.75,
    legend.title.fontface = 2) +
  tm_compass(position = c("RIGHT", "TOP"), size = 2, type = "rose") +
  tm_scale_bar(breaks = c(0, 5, 10, 15, 20), position = c("LEFT", "BOTTOM"), size = 0.5)
map2 <- qtm(ethnic_groups_sdf, fill="Mixed", fill.palette = "Blues", border = "white", fill.title = "Percentage (%)") +
  tm_shape(london_boroughs_shp) +
  tm_borders(col = "black") +
  tm_layout(
    title = "Mixed",
    bg.color = "grey85",
    legend.outside = FALSE,
    legend.title.size = 0.75,
    legend.title.fontface = 2) +
  tm_compass(position = c("RIGHT", "TOP"), size = 2, type = "rose") +
  tm_scale_bar(breaks = c(0, 5, 10, 15, 20), position = c("LEFT", "BOTTOM"), size = 0.5)
map3 <- qtm(ethnic_groups_sdf, fill="Asian", fill.palette = "Purples", border = "white", fill.title = "Percentage (%)") +
  tm_shape(london_boroughs_shp) +
  tm_borders(col = "black") +
  tm_layout(
    title = "Asian",
    bg.color = "grey85",
    legend.outside = FALSE, 
    legend.title.size = 0.75,
    legend.title.fontface = 2) +
  tm_compass(position = c("RIGHT", "TOP"), size = 2, type = "rose") +
  tm_scale_bar(breaks = c(0, 5, 10, 15, 20), position = c("LEFT", "BOTTOM"), size = 0.5)
map4 <- qtm(ethnic_groups_sdf, fill="Black", fill.palette = "Greens", border = "white", fill.title = "Percentage (%)") +
  tm_shape(london_boroughs_shp) +
  tm_borders(col = "black") +
  tm_layout(
    title = "Black",
    bg.color = "grey85",
    legend.outside = FALSE,
    legend.title.size = 0.75,
    legend.title.fontface = 2) +
  tm_compass(position = c("RIGHT", "TOP"), size = 2, type = "rose") +
  tm_scale_bar(breaks = c(0, 5, 10, 15, 20), position = c("LEFT", "BOTTOM"), size = 0.5)
map5 <- qtm(ethnic_groups_sdf, fill="Other", fill.palette = "Reds", border = "white", fill.title = "Percentage (%)") +
  tm_shape(london_boroughs_shp) +
  tm_borders(col = "black") +
  tm_layout(
    title = "Other",
    bg.color = "grey85",
    legend.outside = FALSE,
    legend.title.size = 0.75,
    legend.title.fontface = 2) +
  tm_compass(position = c("RIGHT", "TOP"), size = 2, type = "rose") +
  tm_scale_bar(breaks = c(0, 5, 10, 15, 20), position = c("LEFT", "BOTTOM"), size = 0.5)
tmap_arrange(map1, map2, map3, map4, map5, ncol = 3)