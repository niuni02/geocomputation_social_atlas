# MAP 5 - hospital and gp practices locations

# libraries
library(tidyverse)
library(sf)
library(ggplot2)
library(tmap)
library(readr)
library(dplyr)
library(RColorBrewer)
library(janitor)

# load in the London MSOAs shapefile
london_MSOA_shp <- st_read("raw/boundaries/MSOA_2011_London_gen_MHW.shp")
# select relevant columns
colnames(london_MSOA_shp)
london_MSOA_shp <- select(london_MSOA_shp, "MSOA11CD", "LAD11CD", "LAD11NM", "geometry")
View(london_MSOA_shp)

# read in hospitals dataset
hospitals_raw <- read_csv("raw/hospitals.csv")
# using the select() function to extract relevant information from the dataset
hospitals_raw <- select(hospitals_raw, "OrganisationName", "City", "Latitude", "Longitude")
# using the filter() function to select rows to get only the hospitals in London
hospitals_raw <- filter(hospitals_raw, City == "London")
View(hospitals_raw)
# now need to transform data into a spatial data frame
hospitals_df <- read_csv("raw/hospitals.csv") %>%
  dplyr::select("OrganisationName", "City", "Latitude", "Longitude") %>%
  dplyr::filter(City == "London") %>%
  dplyr::select(Longitude, Latitude) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4236) %>%
  st_transform(27700)
View(hospitals_df)
# combine hospitals_raw and hospitals_df
hospitals <- cbind(hospitals_raw, hospitals_df)
hospitals <- st_sf(hospitals)
View(hospitals)

# now do the same as above with gp practices dataset
gp_practices_raw <- read_csv("raw/gp_practices.csv")
gp_practices_raw <- select(gp_practices_raw, "OrganisationName", "City", "Latitude", "Longitude")
gp_practices_raw <- filter(gp_practices_raw, City == "London", !is.na(Longitude), !is.na(Latitude))
View(gp_practices_raw)
# now need to transform data into a spatial data frame
gp_practices_df <- read_csv("raw/gp_practices.csv") %>%
  dplyr::select("OrganisationName", "City", "Latitude", "Longitude") %>%
  dplyr::filter(City == "London" & !is.na(Longitude) & !is.na(Latitude)) %>%
  dplyr::select(Longitude, Latitude) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4236) %>%
  st_transform(27700)
View(gp_practices_df)
gp_practices <- cbind(gp_practices_raw, gp_practices_df)
gp_practices <- st_sf(gp_practices)
View(gp_practices)

# now do the same as above with pharmacy dataset
pharmacy_raw <- read_csv("raw/pharmacy.csv")
pharmacy_raw <- select(pharmacy_raw, "OrganisationName", "City", "Latitude", "Longitude")
pharmacy_raw <- filter(pharmacy_raw, City == "London", !is.na(Longitude), !is.na(Latitude))
View(pharmacy_raw)
# now need to transform data into a spatial data frame
pharmacy_df <- read_csv("raw/pharmacy.csv") %>%
  dplyr::select("OrganisationName", "City", "Latitude", "Longitude") %>%
  dplyr::filter(City == "London" & !is.na(Longitude) & !is.na(Latitude)) %>%
  dplyr::select(Longitude, Latitude) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4236) %>%
  st_transform(27700)
View(pharmacy_df)
pharmacy <- cbind(pharmacy_raw, pharmacy_df)
pharmacy <- st_sf(pharmacy)
View(pharmacy)

# plotting the hospitals, gp practices and pharmacy locations across London
tm_shape(london_MSOA_shp) + tm_fill() +
  tm_shape(hospitals) + tm_dots(col = "red") +
  tm_shape(gp_practices) + tm_dots(col = "blue") +
  tm_shape(pharmacy) + tm_dots(col = "green") +
  tm_layout(
    main.title = "Locations of Hospitals, GP Practices and Pharmacies in London",
    main.title.fontface = 2, fontfamily = "Helvetica", main.title.size = 1,
    legend.outside = FALSE,
    legend.outside.position = "right",
    legend.title.size = 1,
    legend.title.fontface = 2,
    frame = FALSE) +
  tm_add_legend(title = "Legend",
                type = "symbol",
                labels = c("Hospitals", "GP practices", "Pharmacies"),
                col = c("red", "blue", "green"),
                size = 1) +
  tm_compass(type = "rose", position = c("right", "top"), size = 2) +
  tm_scale_bar(breaks = c(0, 5, 10, 15, 20), position = c("left", "bottom")) +
  tm_credits("Source: https://www.nhs.uk/about-us/nhs-website-datasets/",
             position = c("left", "bottom"))