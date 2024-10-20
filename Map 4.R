# MAP 4 - spatial autocorrelation of general health scores

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
library(RColorBrewer)

# read in general health scores data
general_health_scores <- read_csv("raw/general_health_scores.csv", 
                                  col_types = cols(very_bad_health = col_integer(), 
                                                   total_health = col_integer(), very_good_health = col_integer(), 
                                                   good_health = col_integer(), fair_health = col_integer(), 
                                                   bad_health = col_integer()))
View(general_health_scores)

# load in the London MSOAs shapefile
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

# summarise health score data
summary(general_health_scores$good_health)
hist(general_health_scores$good_health)
  # positive skew - makes sense since median is lower than the mean
summary(general_health_scores$bad_health)
hist(general_health_scores$bad_health)
summary(general_health_scores$very_bad_health)
hist(general_health_scores$very_bad_health)
summary(general_health_scores$very_good_health)
summary(general_health_scores$fair_health)

# Using a local spatial test (Local Getis-Ord) to determine hot/cold spots of very bad/bad health
# first combine the very bad and bad health scores as a percentage of total health scores
total_bad_health <- mutate(general_health_scores, total_bad_health = ((very_bad_health + bad_health)/total_health)*100)
View(total_bad_health)

# now combine the total bad health dataset with the London MSOAs shapefile to create a spatial data frame
total_bad_health_sdf <- merge(total_bad_health, london_MSOA_shp,
                              by.x = "GEO_CODE", by.y = "MSOA11CD")
total_bad_health_sdf <- st_sf(total_bad_health_sdf)
View(total_bad_health_sdf)

# then we need to define the spatial weights matrix
badhealth_msoas_neighbours <- total_bad_health_sdf %>%
  poly2nb(., queen = F)

msoas_neighbours_fd <- dnearneigh(st_geometry(st_centroid(total_bad_health_sdf)), 0, 3000)

# calculating the centroids
msoas_centroid <- total_bad_health_sdf %>%
  st_centroid()
plot(badhealth_msoas_neighbours, st_geometry(msoas_centroid), col = "red", pch = 20, cex = 0.5)
# plotting neighbours at Fixed Distance
plot(msoas_neighbours_fd, st_geometry(msoas_centroid), col = "green", pch = 20, cex = 0.5)

# creating a neighbours list
msoas_spatial_weights_fd <- msoas_neighbours_fd %>%
  nb2listw(., style = "B")

# running the local gi test
badhealth_LGO <- total_bad_health_sdf %>%
  pull(total_bad_health) %>%
  as.vector() %>%
  localG(., msoas_spatial_weights_fd)

total_bad_health_sdf_1 <- total_bad_health_sdf %>%
  mutate(badhealth_LGO_G = as.numeric(badhealth_LGO))

GIColours <- rev(brewer.pal(8, "RdBu"))

tm_shape(total_bad_health_sdf_1) +
  tm_polygons("badhealth_LGO_G",
              style = "pretty", palette = GIColours,
              midpoint = 0, title = "Local Gi* statistic", border.col = "white") +
  tm_shape(london_boroughs_shp) +
  tm_borders(col = "black") +
  tm_layout(
    main.title = "Hot/Cold Spot Map of Very Bad/Bad Health Scores in London 2011",
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