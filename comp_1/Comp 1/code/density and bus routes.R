setwd("D:/Queens/Data Analytics/Comp 1")



library(ggplot2)
library(lubridate)
library(tidyverse)
library(dygraphs)
library(readxl)


library(leaflet)
library(sf)
library(rmapshaper)


tmp <- st_read("data/transit-gtfs-routes.shp")


data.p <- tmp %>%
  st_transform(4326) %>%
  rmapshaper::ms_simplify()

data.p <- data.p[,c("route_id", "route_color", "geometry")]


tmp <- st_read("data/neighbourhood-census-profiles-family-housing-mobility.shp")

data.n <- tmp %>%
  st_transform(4326) %>%
  rmapshaper::ms_simplify()




neighborhod_area <- read.csv("data/neighbourhoods.csv",sep = ';')[,1:4]


data.n <- left_join(data.n,neighborhod_area[,c("Neighbourhood.Identifier","Geographic.Area")], by = c("area_id_num" = "Neighbourhood.Identifier"))

data.n$den <- round(data.n$total_popul/(data.n$Geographic.Area/10^5),2)

data.n <- data.n[,c("community_n", "den", "geometry")]
data.n <- data.n[which(data.n$community_n != "Kingston"),]

# temp <- unite(data.n, col ="Name_density", c("community_n","den"), sep = " : ")
data.n$popup <- paste(data.n$community_n," : ",data.n$den," persons/square km")


lng.center <- -76.55
lat.center <- 44.23283
zoom.def <- 12

pal <- colorNumeric(
  palette = "magma",
  domain = data.n$den)


leaflet(data = data.p) %>%
  addProviderTiles("OpenStreetMap.Mapnik", options = providerTileOptions(opacity = 1), group = "Open Street Map") %>%
  setView(lng = lng.center, lat = lat.center, zoom = zoom.def) %>%
  addPolylines(group = 'base',
               fillColor = 'transparent',
               color = "black",
               weight = 3,
               popup = ~route_id) %>%
  addPolygons(data = data.n,
              weight = 1,
              fillColor = ~pal(den), 
              color = "blue",
              group = "base",
              popup = ~popup)

# Exclude rural


data.n_no_sparse <- data.n[which(data.n$den >= 50),]

lng.center <- -76.55
lat.center <- 44.23283
zoom.def <- 12

pal <- colorNumeric(
  palette = "magma",
  domain = data.n_no_sparse$den)


leaflet(data = data.p) %>%
  addProviderTiles("OpenStreetMap.Mapnik", options = providerTileOptions(opacity = 1), group = "Open Street Map") %>%
  setView(lng = lng.center, lat = lat.center, zoom = zoom.def) %>%
  addPolylines(group = 'base',
               fillColor = 'transparent',
               color = "black",
               weight = 3,
               popup = ~paste("route: ",route_id)) %>%
  addPolygons(data = data.n_no_sparse,
              weight = 1,
              fillColor = ~pal(den), 
              color = "blue",
              group = "base",
              popup = ~popup)


library(geosphere)


distm(c(lon1, lat1), c(lon2, lat2), fun = distHaversine)


