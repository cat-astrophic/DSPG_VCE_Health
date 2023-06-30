# This script plots the locations of VCE agents over SDoH data

#Go to traveltime.com, create an account, get API key 

#For the isochrones to run: install the following packages 
#install.packages("remotes")
#remotes::install_github("tlorusso/traveltimeR")

library(traveltimeR)

#Sys.setenv(TRAVELTIME_ID = "YOUR_APP_ID")
#Sys.setenv(TRAVELTIME_KEY = "YOUR_APP_KEY")

# Loading libraries

library(paletteer)
library(ggplot2)
library(tigris)
library(dplyr)
library(sf)
library(leaflet)

# Project directory info

# Reading in data

vce <- read.csv(paste('/Users/nandinidas/Downloads/OneDrive_1_6-4-2023/vce_agents.csv', sep = ''))
hospitals <- read.csv(paste('/Users/nandinidas/Downloads/OneDrive_1_6-4-2023/hospitals.csv', sep = ''))

# Creating county/city level hospital counts

h.counts <- hospitals %>% count(FIPS)
colnames(h.counts) <- c('GEOID', 'Hospitals')
h.counts$GEOID <- as.character(h.counts$GEOID)

# Using tigris to get geometries

va.counties <- counties(state = 'Virginia')
va.tract <- tracts(state = 'Virginia')
va.vote <- voting_districts(state = 'Virginia')

# Merging h.counts and the Virginia shapefile

va.counties <- left_join(va.counties, h.counts, by = 'GEOID')
va.counties$Hospitals[is.na(va.counties$Hospitals)] <- 0

# Setting up plot color settings using paletteer

nColor <- 14
colors <- paletteer_c(palette = 'viridis::mako', n = nColor, direction = -1)

# Plotting only the VCE agents

map <- plot(va.counties$geometry, col = colors[va.counties$Hospitals+1], main = 'VCE Agents and Hospital Counts')
points(vce$Long, vce$Lat, pch = 18, col = 'red', cex = 2)
legend('topleft', col = c(rev(colors)), legend = 13:0, pch = 15)

coordinates <- data.frame(va.counties%>%dplyr::select(INTPTLON,INTPTLAT))

# 
# res1<-ors_isochrones(coordinates[1:5,1:2], profile = "driving-car", range = 1200, output = "sf")
# res2<-ors_isochrones(coordinates[6:10,1:2], profile = "driving-car", range = 1200,output= "sf")
# res3<-ors_isochrones(coordinates[11:15,1:2], profile = "driving-car", range = 1200, output= "sf")
# res4<-ors_isochrones(coordinates[16:20,1:2], profile = "driving-car", range = 1200,output="sf")
# res5<-ors_isochrones(coordinates[21:25,1:2], profile = "driving-car", range = 1200, output = "sf")
# res6<-ors_isochrones(coordinates[26:30,1:2], profile = "driving-car", range = 1200,output= "sf")
# res7<-ors_isochrones(coordinates[31:35,1:2], profile = "driving-car", range = 1200, output= "sf")
# res8<-ors_isochrones(coordinates[36:40,1:2], profile = "driving-car", range = 1200,output="sf")
# res9<-ors_isochrones(coordinates[41:45,1:2], profile = "driving-car", range = 1200, output = "sf")
# res10<-ors_isochrones(coordinates[46:50,1:2], profile = "driving-car", range = 1200,output= "sf")
# 
# 
# 
# res<-rbind(res1,res2,res3,res4,res5,res6,res7,res8,res9,res10,res11)
# res$group_index<-c(1:51)

results <- list()  # Create an empty list to store the results

for (i in 1:27) {
  if (i >= 27) {
    start <- 131
    end <- 133
  } else {
    start <- (i - 1) * 5 + 1
    end <- i * 5
  }
  
  if (start <= 133) {
    res <- ors_isochrones(coordinates[start:min(end, 133), 1:2], profile = "driving-car", range = 900, output = "sf")
    results[[paste0("res", i)]] <- res
  }
}

res <- do.call(rbind, results)  # Combine the results into a single dataframe

res$group_index <- 1:nrow(res)  # Add a new column for the group index

#save(res, file = "shapefiles_15min.Rdata")

leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = va.counties$geometry,
              color="#5f308f",
              weight = 0.5,
              smoothFactor = 0.2,
              fillOpacity = 0.5)  %>% 
  addPolygons(data=res, color= "5f308f",opacity = 1,weight=2,fillColor = "blue", fillOpacity = .5) 

