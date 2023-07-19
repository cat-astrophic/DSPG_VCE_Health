#trying to put icons in the app

# snap territory data
snap_territories <- read.csv("./data/base_agg_snap.csv")

#func draft
snap_territory <- function(territory_type_snaped, zscore_type_snaped) {
  
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
  library(leaflegend)
  library(tidycensus)
  library(tidyverse)
  library(viridis)
  library(readxl)
  library(sf) 
  options(scipen=999)
  library(htmlwidgets)
  
  va.counties <- st_read("./data/va_counties.shp")
  # Convert columns to appropriate types and rename them
  va.counties <- transform(va.counties,
                           GEOID = as.integer(GEOID),
                           NAMELSAD = str_to_title(NAMELSAD),
                           NAME = str_to_title(NAME))
  
  
  temp2 <- snap_territories[snap_territories$snap_territory_type == territory_type_snaped & snap_territories$snap_zscore_type == zscore_type_snaped, ]
  
  #convert new agent locations to sf
  additional_agent_sf <- temp2 %>% 
    
    # Convert new agent locations to sf
    st_as_sf(coords = c("Long", "Lat"), remove = FALSE, crs = 4326, agr = "constant")
  #joining variable data with county geometry data
  territory.counties <- left_join(va.counties, temp2, by = 'NAMELSAD')
  
  #assigning colors for each agent territory
  pal <- colorFactor(palette = c("#004949" , "#fc4e2a","#FDE725FF","#21214f","#332288",
                                 
                                 "#1f77b4", "#018571","#ffffb3","#9a5baf", "#B12A90FF"), 
                     domain= snap_territories$Agent,
                     levels= c( "Arlington","Franklin","Lynchburg City",
                                "Newport News City","Pittsylvania",
                                "Roanoke","Rockbridge","Virginia Beach City",
                                "Washington","Northeast District Office"))
  
  # create labels for counties
  county_labels <- sprintf(
    "<strong>%s</strong><br/> Served by Agent From: %s",
    territory.counties$NAMELSAD,
    territory.counties$Agent
  ) %>% lapply(htmltools::HTML)
  
  snap_agent_labels <- sprintf(
    "<strong>SNAP-Ed Agent Site </strong><br/>District Office: %s <br/> Agent Name: %s<br/> Contact Info: %s",
    additional_agent_sf$Job.Dept,
    additional_agent_sf$Employee.Name,
    additional_agent_sf$VT.Email
  ) %>% lapply(htmltools::HTML)
  
  #creating good title names
  idx2 <- which(unique(snap_territories$snap_zscore_type) == zscore_type_snaped)
  good_title_names <- c("Aggregate", "Obesity", "Diabetes", "Food Insecurity", "Physical Inactivity", "Low Birthweight")
  # create title for the map
  territory_title = paste("Optimized VCE FCS/SNAP-Ed Agent Territories based on",good_title_names[idx2], "Z-scores", sep= " ")
  #territory_title = paste("New VCE FCS Agent Territories based on",variable_title, "Z-scores")
  
  #differentiate colors of agents by the new_agent variable
  additional_agent_sf$markerColor <- ifelse(temp2$new_agent == 0, "orange", "red")
  
  #making icons for different agent service/type
  snap_agents_icons <- makeIcon(
    iconUrl = ifelse(additional_agent_sf$new_agent == 0,
                     "https://tinyurl.com/fcssnapagent.png",
                     "https://tinyurl.com/newfcsagent.png"
    ),
    iconWidth = 38, iconHeight = 50,
    iconAnchorX = 20, iconAnchorY = 50)
  
  #making legend for icons
  html_legend <- "<img src='https://tinyurl.com/fcssnapagent.png'> FCS/SNAP-Ed Agents<br/>
<img src='https://tinyurl.com/newfcsagent.png'> New FCS/SNAP-Ed Agent"
  
  # create leaflet map
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
    addMarkers(lng= ~Long, lat= ~Lat, icon= snap_agents_icons, label= snap_agent_labels) %>% 
    #addAwesomeMarkers(data = additional_agent_sf, 
                      # icon=awesomeIcons(icon='cloud', markerColor = additional_agent_sf$markerColor, iconColor = 'white'),
                      # label = snap_agent_labels,
                      # labelOptions = labelOptions(noHide = FALSE, direction = "auto", offset=c(0,-10))) %>%
    setView(lng = -79.5, lat = 38.2315734, zoom = 6.5) %>%
    addControl(htmltools::HTML(paste0("<h3 style='margin:3px'>", territory_title, "</h2>")), position = "topright", data = NULL) %>% 
    addControl(html = html_legend, position =  "topright")
    #addLegend(colors = c("orange", "red"), labels = c("Existing FCS/SNAP-Ed Agent", "New FCS Agent" ), 
              #position = "topright", title= "Agent Type:")
}