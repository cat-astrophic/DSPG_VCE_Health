
library(mapview)
library(openrouteservice)
library(paletteer)
library(ggplot2)
library(tigris)
library(dplyr)
library(sf)
library(leaflet)


# embed data in the output file
mapviewOptions(fgb = FALSE)

vce <- read.csv(paste('vce_agents.csv', sep = ''))
coordinates <- data.frame(lon = vce$Long, lat = as.numeric(vce$Lat))



# Using tigris to get geometries
va.counties <- readRDS(file = "va.counties.Rdata")
us.states <- readRDS(file= "us.states.RData")
# Read Isochrone Data
res <- readRDS(file = "agent_isochrones_3levels.rds") # Isochrone data

values <- levels(factor(res$value))
ranges <- split(res, values)
ranges <- ranges[rev(values)]

names(ranges) <- sprintf("%s min", as.numeric(names(ranges))/60)


# Show the agents distribution
map <- plot(va.counties$geometry , main = 'VCE Agents Distribution')
points(vce$Long, vce$Lat, pch = 18, col = 'red', cex = 2)



# Choice 1
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    data = va.counties$geometry,
    color = "#000000",  # Black boundary color
    weight = 2,         # Increased boundary thickness
    fillOpacity = 0   # Reduced transparency for fill
  ) %>%
  addCircleMarkers(data = coordinates,
                   lng = ~lon,
                   lat = ~lat,
                   color = 'red',
                   radius = 4) %>%
  addPolygons(data = ranges$`60 min`,
              color = "#EF476F",  # color for 60 min
              fillOpacity = 0.1,
              weight = 1) %>%
  addPolygons(data = ranges$`40 min`,
              color = "#EFD166",  # color for 40 min
              fillOpacity = 0.5,
              weight = 1) %>%
  addPolygons(data = ranges$`20 min`,
              color = "#06D6A0",  # color for 20 min
              fillOpacity = 0.5,
              weight = 1)



# Choice 2
mapview(ranges, alpha.regions = 0.2, homebutton = FALSE, legend = FALSE)