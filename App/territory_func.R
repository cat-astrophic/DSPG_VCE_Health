library(shiny)
library(shinycssloaders)
library(shinyWidgets)
library(shinythemes)
library(stringr)
library(shinyjs)
library(ggplot2)
library(plotly)
library(rsconnect)
library(rgdal)
library(plyr)
library(tigris)
library(dplyr)
library(leaflet)
library(tidycensus)
library(tidyverse)
library(viridis)
library(readxl)
library(sf) 
options(scipen=999)

setwd("~/DSPG/Repose/DSPG_VCE_Health/App/Data")
#reading in va counties shps
va.counties <- st_read("va_counties.shp")
# Convert columns to appropriate types and rename them
va.counties <- transform(va.counties,
                         GEOID = as.integer(GEOID),
                         NAMELSAD = str_to_title(NAMELSAD) )
# territory data
all_territories <- read_csv("agent_solutions.csv")

territory <- function(territory_type, zscore_type, variable_title) {
  
  temp2 <- all_territories[all_territories$territory_type == territory_type & all_territories$zscore_type == zscore_type, ]
  
  #convert new agent locations to sf
  additional_agent_sf <- temp2 %>% 
    # Convert new agent locations to sf
    st_as_sf(  coords = c("Long", "Lat"), remove = FALSE, crs = 4326, agr = "constant" )
  
  #Join variable data with county geometry data
  territory.counties <- left_join(va.counties, temp2, by = 'NAMELSAD')
  
  #creating color dictionary
  agent_colors <- c(
    "Albemarle" = "lightgoldenrod" ,
    "Amelia" = "red",
    "Amherst"= "forestgreen",
    "Arlington" = "navy",
    "Bedford" = "plum4",
    "Chesapeake City" =  "khaki",
    "Fairfax" = "brown2",
    "Floyd"=  "yellow",
    "Franklin" = "salmon",
    "Gloucester" = "lightgrey",
    "Greensville" = "darkolivegreen",
    "Henrico"= "chartreuse4",
    "King George" = "gold",
    "Lancaster" = "lavenderblush",
    "Lee" = "turquoise",
    "Loudoun" = "mediumvioletred",
    "Louisa" =  "mistyrose",
    "Lynchburg City" = "palegreen",
    "Mecklenburg" = "hotpink4",
    "Newport News City North"  = "purple",
    "Newport News City" = "lightblue",
    "Orange County"= "darkseagreen2",
    "Patrick"=  "lightsteelblue",
    "Petersburg City"= "magenta" ,
    "Pittsylvania" =  "slategray",
    "Pulaski"= "lightcyan",
    "Richmond City" = "darkgrey",
    "Roanoke" = "blue",
    "Rockbridge" = "mediumspringgreen",
    "Rockingham" = "mediumorchid",
    "Spotsylvania" = "pink" ,
    "Virginia Beach City North" = "salmon4",
    "Virginia Beach City" = "burlywood",
    "Warren" = "cadetblue3",
    "Washington"= "honeydew",
    "Northeast District Office" = "orange",
    "Augusta" = "hotpink",
    "Essex" = "chocolate",
    "Frederick" = "tan1",
    "Prince William" = "darkorchid1"
  )
  #pal <- colorFactor(palette = agent_colors, domain= all_territories$Agent)
  pal <- colorFactor(palette = c("lightgoldenrod" , "red","forestgreen","navy","plum4","khaki", "brown2","yellow",
   "salmon", "lightgrey", "darkolivegreen", "chartreuse4", "gold", "lavenderblush",
   "turquoise","mediumvioletred", "mistyrose", "palegreen","hotpink4", "purple",
   "lightblue", "darkseagreen2", "lightsteelblue", "magenta","slategray",
   "lightcyan","darkgrey","blue","mediumspringgreen","mediumorchid", "pink", "salmon4",
   "burlywood","cadetblue3", "honeydew","orange","hotpink","chocolate", "darkorchid1"),
  domain= all_territories$Agent)
  # levels= c( "Albemarle",
  #            "Amelia",
  #            "Amherst",
  #            "Arlington",
  #            "Bedford",
  #            "Chesapeake City",
  #            "Fairfax",
  #            "Floyd",
  #            "Franklin",
  #            "Gloucester",
  #            "Greensville",
  #            "Henrico",
  #            "King George",
  #            "Lancaster",
  #            "Lee",
  #            "Loudoun",
  #            "Louisa",
  #            "Lynchburg City",
  #            "Mecklenburg",
  #            "Newport News City North",
  #            "Newport News City",
  #            "Orange County",
  #            "Patrick",
  #            "Petersburg City",
  #            "Pittsylvania",
  #            "Pulaski",
  #            "Richmond City",
  #            "Roanoke",
  #            "Rockbridge",
  #            "Rockingham",
  #            "Spotsylvania",
  #            "Virginia Beach City North",
  #            "Virginia Beach City",
  #            "Warren",
  #            "Washington",
  #            "Northeast District Office",
  #            "Augusta",
  #            "Essex",
  #            "Frederick",
  #            "Prince William"))

  # Create labels for counties
  county_labels <- sprintf(
    "<strong>%s</strong><br/> Served by Agent From: %s",
    territory.counties$NAMELSAD,
    territory.counties$Agent
  ) %>% lapply(htmltools::HTML)
  
  # Create labels for agents
  agent_labels <- sprintf(
    "<strong>Agent Site </strong><br/>District Office: %s <br/> Agent Name: %s<br/> Contact Info: %s",
    additional_agent_sf $Job.Dept,
    additional_agent_sf $Employee.Name,
    additional_agent_sf $VT.Email
  ) %>% lapply(htmltools::HTML)
  
  
  # Create title for the map
  territory_title = paste("New VCE FCS Agent Territories based on",variable_title, "Z-scores")
  
  #differentiate colors of agents by the new_agent varible
  additional_agent_sf$markerColor <- ifelse(temp2$new_agent == 0, "blue", "red")
  
  # Create leaflet map
  leaflet(data = territory.counties) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(fillColor = ~pal(Agent),
                color = "#BDBDC3",
                weight = 1,
                smoothFactor = 0.2,
                opacity = 1.0,
                fillOpacity = 0.6,
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = TRUE),
                label = county_labels,
                labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                            textsize = "15px",
                                            direction = "auto")) %>%
    addAwesomeMarkers(data = additional_agent_sf, 
                      icon=awesomeIcons(icon='cloud', markerColor = additional_agent_sf$markerColor, iconColor = 'white'),
                      label = agent_labels,
                      labelOptions = labelOptions(noHide = FALSE, direction = "auto", offset=c(0,-10))) %>%
    setView(lng = -78.6568942, lat = 38.2315734, zoom = 7) %>%
    addControl(htmltools::HTML(paste0("<h3 style='margin:3px'>", territory_title, "</h2>")), position = "topright", data = NULL)
}

# va_counties <- counties(state = "VA", cb = TRUE)
# sf::st_write(va_counties, "C:/Users/vivia/Documents/DSPG/Repose/DSPG_VCE_Health/App/Data/va_counties.shp", append= FALSE)
