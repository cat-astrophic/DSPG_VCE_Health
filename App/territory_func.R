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
#reading in va counties shps
va.counties <- st_read("./data/va_counties.shp")
# Convert columns to appropriate types and rename them
va.counties <- transform(va.counties,
                         GEOID = as.integer(GEOID),
                         NAMELSAD = str_to_title(NAMELSAD) )
# territory data
all_territories <- read.csv("./data/agent_solutions.csv")
vce2 <- read.csv("./data/vce_agents_with_new_agents.csv")
#convert new agent locations to sf
additional_agent_sf <- st_as_sf(vce2, coords = c("Long", "Lat"), remove = FALSE, crs = 4326, agr = "constant" )
territory <- function(territory_type, Zscore_Type, variable_title) {

    # Filter data for selected year and variable
    territory_type <- territory_type
    temp2 <- all_territories[all_territories$Territory_Type == territory_type & all_territories$Zscore_Type == variable_title, ]
    # # Join variable data with county geometry data
    territory.counties <- left_join(va.counties, temp2, by = 'NAMELSAD')
    #
    # # Identify the index of the selected variable
    # idx <- which(unique(all_var_df$Variable) == variable)

    # Create a color palette function based on the "Value" column
    #problem area!!
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
                        "Northeast District Office" = "orange",
                        "Orange"= "darkseagreen2",
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
                        "Warren" = "dodgerblue",
                        "Washington"= "honeydew",
                        "Frederick" = "tan1",
                        "Augusta" = "hotpink",
                        "Prince William" = "darkorchid1",
                        "Essex" = "chocolate"
                    )
    pal <- colorFactor(palette = agent_colors, domain= territory.counties$Agent)

    # Create labels for counties
    #problem area!!
    county_labels <- sprintf(
      "<strong>%s</strong><br/> Served by Agent From: %s",
      territory.counties$NAMELSAD,
      additional_agent_sf$Agent
    ) %>% lapply(htmltools::HTML)

    # Create labels for agents
    agent_labels <- sprintf(
      "<strong>Agent Site </strong><br/>District Office: %s <br/> Agent Name: %s<br/> Contact Info: %s",
      additional_agent_sf $Job.Dept,
      additional_agent_sf $Employee.Name,
      additional_agent_sf $VT.Email
    ) %>% lapply(htmltools::HTML)

    # Wrap legend title if too long
    # spaces <- gregexpr("\\s", good_names[idx])[[1]]
    # middle_space <- spaces[length(spaces) %/% 2 + 1]
    # legend_title <- paste0(substring(good_names[idx], 1, middle_space-1), "</br>", substring(good_names[idx], middle_space+1))
    #
    # Create title for the map
    territory_title = paste("New VCE FCS Agent Territories based on",variable_title, "Z-scores")

    # Create leaflet map
    leaflet(data = territory.counties) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      #problem area!
      addPolygons(fillColor = pal,
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
      addAwesomeMarkers(data = additional_agent_sf, icon=awesomeIcons(icon='cloud', markerColor = ifelse(all_territories$new_agent == 1, "yellow","blue"), iconColor = 'white'),
                        label = agent_labels,
                        labelOptions = labelOptions(noHide = FALSE, direction = "auto", offset=c(0,-10))) %>%
      #addLegend(colors= agent_colors, labels= agent_colors, title = territory_title, position = "bottomright") %>%
      setView(lng = -78.6568942, lat = 38.2315734, zoom = 7) %>%
      addControl(htmltools::HTML(paste0("<h3 style='margin:3px'>", territory_title, "</h2>")), position = "topright", data = NULL)
  }