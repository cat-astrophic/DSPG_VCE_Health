#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# 1. Set Up------------------------------------------------------------------------ 



## 1.2 Load packages----------------------------------------------------------------
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
library(fontawesome) 


#options(shiny.maxRequestSize = 80*1024^2)

## 1. 1 CODE TO DETECT ORIGIN OF LINK AND CHANGE LOGO ACCORDINGLY -------------
# JavaScript code
jscode <- 'var x = document.getElementsByClassName("navbar-brand");
    var dspgLink = "https://dspg.aaec.vt.edu/";
    var githubLink = "https://github.com/VT-Data-Science-for-the-Public-Good";
    var dspgLogoHTML = \'<a href="\' + dspgLink + \'"><img src="DSPG_black-01.png" alt="VT DSPG" style="height:42px;"></a>\';
    var githubLogoHTML = \'<a href="\' + githubLink + \'"><img src="github_logo.png" alt="GitHub" style="max-height: 30px; max-width: 100%;"></a>\';
    var logosHTML = dspgLogoHTML + githubLogoHTML;
    x[0].innerHTML = x[0].innerHTML + " " + logosHTML;
  '
 
## 1.3 Load the data----------------------------------------------------------------
### 1.3.1 Load the original data----------------------------------------------------------------
  # Reading in map 
  # This is how i download the shapefiles for counties of VA
    ## va_counties <- counties(state = "VA", cb = TRUE)
    ## sf::st_write(va_counties, "data/va_counties.shp")
    ## Read shapefile
    va.counties <- st_read("./data/va_counties.shp")
    # Convert columns to appropriate types and rename them
    va.counties <- transform(va.counties,
                             GEOID = as.integer(GEOID),
                             NAMELSAD = str_to_title(NAMELSAD),
                             NAME = str_to_title(NAME))
    
  # Load and preprocess the health rankings data
  all_var_df <- read.csv("./data/final_variables_16_20.csv") %>%
    transform(GEOID = as.integer(FIPS),
              Value = as.numeric(Value))
  
  # Load and preprocess the VCE agents location data
  agents_sf <- read.csv("./data/vce_agents.csv") %>% st_as_sf(  coords = c("Long", "Lat"), remove = FALSE, crs = 4326, agr = "constant")
  # Prepare labels for varibels of interest
  good_names <- c(   "Percent 65 and over","Percent American Indian or Alaska Native","Percent Asian","Percent Black",
                     "Percent Hispanic","Percent less than 18 years of age","Percent Nonhispanic-White","Percent not Proficient in English",
                     "Gender Pay Gap","Median Household Income","Median Household Income Black","Median Household Income Gap White Black",
                     "Median Household Income Gap White Hispanic","Median Household Income Hispanic","Median Household Income White",
                     "Percent Children in Poverty","Percent Food Insecure","Percent Unemployed","Percent of Adults Reporting Currently Smoking",
                     "Percent of Adults With Obesity","Percent Driving Deaths with Alcohol Involvement","Percent Excessive Drinking",
                     "Percent Physically Inactive","Teen Birth Rate","Life Expectancy","Life Expectancy Black","Life Expectancy Gap",
                     "Life Expectancy White","Percent Low Birthweight","Chlamydia Rate","Dentist Ratio","Drug Mortality Rate",
                     "HIV Prevalence Rate","Mental Health Provider Ratio","Other Primary Care Provider Ratio","Percent of Adults with Diabetes",
                     "Percent Uninsured","Percent of Uninsured Adults","Percent Uninsured Children","Percent Vaccinated",
                     "Percent With Annual Mammogram","Preventable Hospitalization Rate","Primary Care Physicians Ratio",
                     "Juvenile Arrests Rate","Percent With Access to Exercise Opportunities","Percent Insufficient Sleep",
                     "Percent Limited Access to Healthy Foods","Percent Mental Distress","Percent Physical Distress",
                     "Percent Severe Housing Problems","Suicide Rate")
  
  # territory data
  all_territories <- read.csv("./data/all_agent_solutions.csv")
  # snap territory data
  snap_territories <- read.csv("./data/snap_results.csv")

  #convert new agent locations to sf
  additional_agent_sf <- st_as_sf(all_territories, coords = c("Long", "Lat"), remove = FALSE, crs = 4326, agr = "constant" )
  additional_agent_sf$markerColor <- ifelse(additional_agent_sf$new_agent == 0, "blue", "red")

  #load nonsnap terr
  fcs_territories <- read.csv("./data/non_snap_results1.csv")%>%
    transform(Lat = as.numeric(Lat),
              Long = as.numeric(Long))
  
 
  # read in va avg data
  va_avg <- read.csv("./data/with_state_avg.csv") %>% 
    filter(!(Year %in% c(2021, 2022)))
  
  #making icons for maps
  snap_agents_icon <- makeAwesomeIcon(icon = "home", library = "fa", iconColor = 'ivory', markerColor = "lightblue")
  agents_icon <- makeAwesomeIcon(icon = "user", library = "fa", iconColor = 'ivory', markerColor = 'cadetblue')
  new_agent_icon <- makeAwesomeIcon(icon = "star", library = "fa", iconColor = 'ivory', markerColor = "red")

# ## 1.4 Define your functions -------------------------------------------------------
# # Function for health outcomes
  mapping2 <- function(variable, year) {
    
    # Filter data for selected year and variable
    temp <- all_var_df[all_var_df$Year == year & all_var_df$Variable == variable, ]
    
    # Join variable data with county geometry data
    var.counties <- left_join(va.counties, temp, by = 'GEOID')
    
    #separating snap agents
    snap_agents <- agents_sf %>% filter(SNAP == 1)
    non_snap <- agents_sf %>%  filter(SNAP == 0)
    # Identify the index of the selected variable
    idx <- which(unique(all_var_df$Variable) == variable)
    
    # Create a color palette function based on the "Value" column
    pal <- colorNumeric(palette = "viridis", domain = var.counties$Value, na.color= NA )
    
    # Create labels for counties
    county_labels <- sprintf(
      "<strong>%s</strong><br/>%s: %g", 
      var.counties$NAMELSAD, 
      good_names[idx], 
      var.counties$Value
    ) %>% lapply(htmltools::HTML)
    # Create labels for agents
    agent_labels <- sprintf(
      "<strong>Agent Site </strong><br/>District Office: %s <br/> Agent Name: %s<br/> Contact Info: %s",
      agents_sf$Job.Dept,
      agents_sf$Employee.Name,
      agents_sf$VT.Email
    ) %>% lapply(htmltools::HTML)
    
    snap_agent_labels <- sprintf(
      "<strong>Agent Site </strong><br/>District Office: %s <br/> Agent Name: %s<br/> Contact Info: %s <br/> SNAP-Ed Service Provided At: %s",
      snap_agents$Job.Dept,
      snap_agents$Employee.Name,
      snap_agents$VT.Email,
      snap_agents$SNAP.Ed
    ) %>% lapply(htmltools::HTML)
    
    # Wrap legend title if too long
    spaces <- gregexpr("\\s", good_names[idx])[[1]]
    middle_space <- spaces[length(spaces) %/% 2 + 1]
    legend_title <- paste0(substring(good_names[idx], 1, middle_space-1), "</br>", substring(good_names[idx], middle_space+1))
   
    # Create title for the map
    map_title = paste("VCE FCS Agent Sites and",good_names[idx], year, sep= " ")
    
    #making icon set for legend
    icons <- awesomeIconList(
      `FCS Agents` = agents_icon,
      `FCS/SNAP-Ed Agents` = snap_agents_icon)
    
    # group names for legen
    groups <- c("FCS Agents" <- "<div style='position: relative; display: inline-block' class='awesome-marker-icon-cadetblue awesome-marker'><i class='fa fa-user icon-white '></i></div>FCS Agents",
                "FCS/SNAP-Ed Agents" <- "<div style='position: relative ; display: inline-block' class='awesome-marker-icon-lightblue awesome-marker'><i class='fa fa-home icon-white '></i></div>FCS/SNAP-Ed Agents")
    
    # Create leaflet map
    map1 <-leaflet(data = var.counties) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(fillColor = ~pal(Value),
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
      #map title
      addControl(htmltools::HTML(paste0("<h3 style='margin:3px'>", map_title, "</h2>")), position = "topright", data = NULL) %>% 
      #adding fcs agent marker
      addAwesomeMarkers(data = non_snap,
                        lat = non_snap$Lat,
                        lng = non_snap$Long,
                        label = agent_labels,
                        icon = agents_icon) %>%
      addAwesomeMarkers(data= snap_agents,
                        lat =snap_agents$Lat,
                        lng = snap_agents$Long,
                        label = snap_agent_labels,
                        icon = snap_agents_icon) %>%
      addAwesomeMarkers(data= agents_sf, lat= agents_sf$Lat, lng= agents_sf$Long, icon = icons, group= groups, label= agent_labels) %>% 
      addLayersControl(                                                                                                           
        overlayGroups = groups,
        options = layersControlOptions(collapsed = FALSE),
        position= "topleft") %>% 
            htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:left\">Agent Type</label>');
        }
    ") %>% 
      #legend for continious scale
      addLegend(pal = pal, values = ~Value, title = legend_title, position = "topright") %>%
      #addControl(htmltools::HTML( '<div style="background:grey; width: 10px; height: 10px;"></div><div>Missing values</div>'), position = "topright") %>% 
      #setting default map zoom
      setView(lng = -78.6568942, lat = 38.2315734, zoom = 6.8)
}
  
#territory function
  territory <- function(territory_type, zscore_type) {
    
    temp2 <- all_territories[all_territories$territory_type == territory_type & all_territories$zscore_type == zscore_type, ]
    
    #convert new agent locations to sf
    additional_agent_sf <- temp2 %>% 
    
    # Convert new agent locations to sf
    st_as_sf(coords = c("Long", "Lat"), remove = FALSE, crs = 4326, agr = "constant")
    
    #separating snap agents
    snap_agents <- additional_agent_sf %>% filter(SNAP == 1)
    non_snap <- additional_agent_sf %>%  filter(SNAP == 0)
    new_agent <- additional_agent_sf%>%  filter(new_agent == 1)
    #joining variable data with county geometry data
    territory.counties <- left_join(va.counties, temp2, by = 'NAMELSAD')
  
    #assigning colors for each agent territory
    pal <- colorFactor(palette = c("#fee08b" , "#fc4e2a","#35b779","#21214f","#332288",
                                   
                                   "#1f77b4", "#018571","#ffffb3","#e45756", "#B12A90FF", "#4a9848", 
                                   
                                   "#488fc1", "#c2df23", "#004949","#924900" ,"#c44e52", "#fde0dd", 
                                   
                                   "#b3e183","#8e0152", "#8c4f96", "#f98e2b","#a1c9f4", "#1695a3", "#79b8d1", 
                                   
                                   "#e7298a","#5b5b5b","#440154","#af8dc3","#414487","#00ba38","#FDE725FF", 
                                   
                                   "#3b528b", "#b31a1c","#d8b365","#006d2c", "#f0fff0","#ffa500","#ff69b4",
                                   
                                   "#483d8b", "#9a5baf"), 
                       domain= all_territories$Agent,
                       levels= c( "Albemarle","Amelia","Amherst", "Arlington","Bedford",
                                  "Chesapeake City","Fairfax","Floyd","Franklin","Gloucester",
                                  "Greensville","Henrico","King George","Lancaster","Lee",
                                  "Loudoun","Louisa","Lynchburg City","Mecklenburg","Newport News City North",
                                  "Newport News City","Orange","Patrick","Petersburg City","Pittsylvania",
                                  "Pulaski","Richmond City","Roanoke","Rockbridge","Rockingham",
                                  "Spotsylvania","Virginia Beach City North","Virginia Beach City","Warren",
                                  "Washington","Northeast District Office","Augusta","Essex","Frederick",
                                  "Prince William"))
    
    # create labels for counties
    county_labels <- sprintf(
      "<strong>%s</strong><br/> Served by Agent From: %s",
      territory.counties$NAMELSAD,
      territory.counties$Agent
    ) %>% lapply(htmltools::HTML)
    
    snap_agent_labels <- sprintf(
      "<strong>Agent Site </strong><br/>District Office: %s <br/> Agent Name: %s<br/> Contact Info: %s <br/> SNAP-Ed Service Provided At: %s",
      snap_agents$Job.Dept,
      snap_agents$Employee.Name,
      snap_agents$VT.Email,
      snap_agents$SNAP.Ed
    ) %>% lapply(htmltools::HTML)
    
    # create labels for agents
    agent_labels <- sprintf(
      "<strong>Agent Site </strong><br/>District Office: %s <br/> Agent Name: %s<br/> Contact Info: %s",
      non_snap$Job.Dept,
      non_snap$Employee.Name,
      non_snap$VT.Email
    ) %>% lapply(htmltools::HTML)
    
    new_agent_labels <- sprintf(
      "<strong>New Agent Site </strong><br/>District Office: %s",
      additional_agent_sf$Job.Dept
    ) %>% lapply(htmltools::HTML)
    
    #creating good title names
    idx2 <- which(unique(all_territories$zscore_type) == zscore_type)
    good_title_names <- c("Aggregate", "Obesity", "Diabetes", "Food Insecurity", "Physical Inactivity", "Low Birthweight")
    # create title for the map
    territory_title = paste("Optimized VCE FCS Agent Territories based on",good_title_names[idx2], "Z-scores", sep= " ")
    #territory_title = paste("New VCE FCS Agent Territories based on",variable_title, "Z-scores")
    
    #making icon set for legend
    icons <- awesomeIconList(
      `FCS Agents` = agents_icon,
      `FCS/SNAP-Ed Agents` = snap_agents_icon,
      `New Agents` = new_agent_icon)
    #create leaflet map
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
      addAwesomeMarkers(data = non_snap,
                        lat = non_snap$Lat,
                        lng = non_snap$Long,
                        label = agent_labels,
                        icon = agents_icon) %>%
      addAwesomeMarkers(data= snap_agents,
                        lat =snap_agents$Lat,
                        lng = snap_agents$Long,
                        label = snap_agent_labels,
                        icon = snap_agents_icon) %>%
      addAwesomeMarkers(data= new_agent,
                        lat =new_agent$Lat,
                        lng = new_agent$Long,
                        label = new_agent_labels,
                        icon = new_agent_icon) %>%
      addLegendAwesomeIcon(iconSet = icons,
                           orientation = 'vertical',
                           title = htmltools::tags$div(
                             style = 'font-size: 20px;',
                             'Agent Type:'),
                           labelStyle = 'font-size: 14px;') %>% 
      setView(lng = -79.5, lat = 38.2315734, zoom = 6.5) %>%
      addControl(htmltools::HTML(paste0("<h3 style='margin:3px'>", territory_title, "</h2>")), position = "topright", data = NULL)
  }

  # snap territory function
  snap_territory <- function(territory_type_snaped, zscore_type_snaped) {
    
    temp2 <- snap_territories[snap_territories$snap_territory_type == territory_type_snaped & snap_territories$snap_zscore_type == zscore_type_snaped, ]
    
    #convert new agent locations to sf
    additional_agent_sf <- temp2 %>% 
      
      # Convert new agent locations to sf
      st_as_sf(coords = c("Long", "Lat"), remove = FALSE, crs = 4326, agr = "constant")
    #joining variable data with county geometry data
    territory.counties <- left_join(va.counties, temp2, by = 'NAMELSAD')
    
    #separating snap agents
    snap_agents <- additional_agent_sf%>% filter(new_agent == 0)
    new_agent <- additional_agent_sf%>%  filter(new_agent == 1)
    
    #assigning colors for each agent territory
    pal <- colorFactor(palette = c("#004949" , "#fc4e2a","#FDE725FF","#21214f","#332288",
                                   
                                   "#1f77b4", "#018571","#ffffb3","#9a5baf", "#B12A90FF", 
                                   "#ffa500","#e7298a", "#c2df23"), 
                       domain= snap_territories$Agent,
                       levels= c( "Arlington","Franklin","Lynchburg City",
                                  "Newport News City","Pittsylvania",
                                  "Roanoke","Rockbridge","Virginia Beach City",
                                  "Washington","Northeast District Office", "Madison",
                                  "Accomack", "Mathews"))
    
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
    
    new_agent_labels <- sprintf(
      "<strong>New Agent Site </strong><br/>District Office: %s",
      additional_agent_sf$Job.Dept
    ) %>% lapply(htmltools::HTML)
    
    #creating good title names
    idx2 <- which(unique(snap_territories$snap_zscore_type) == zscore_type_snaped)
    good_title_names <- c("Aggregate", "Obesity", "Diabetes", "Food Insecurity", "Physical Inactivity", "Low Birthweight")
    # create title for the map
    territory_title = paste("Optimized VCE FCS/SNAP-Ed Agent Territories based on",good_title_names[idx2], "Z-scores", sep= " ")
    #territory_title = paste("New VCE FCS Agent Territories based on",variable_title, "Z-scores")
    
    #making icon set for legend
    icons <- awesomeIconList(
      `FCS/SNAP-Ed Agents` = snap_agents_icon,
      `New Agents` = new_agent_icon)
    
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
      addAwesomeMarkers(data= snap_agents,
                        lat =snap_agents$Lat,
                        lng = snap_agents$Long,
                        label = snap_agent_labels,
                        icon = snap_agents_icon) %>%
      addAwesomeMarkers(data= new_agent,
                        lat =new_agent$Lat,
                        lng = new_agent$Long,
                        label = new_agent_labels,
                        icon = new_agent_icon) %>%
      addLegendAwesomeIcon(iconSet = icons,
                           orientation = 'vertical',
                           title = htmltools::tags$div(
                             style = 'font-size: 20px;',
                             'Agent Type:'),
                           labelStyle = 'font-size: 14px;') %>% 
      setView(lng = -79.5, lat = 38.2315734, zoom = 6.5) %>%
      addControl(htmltools::HTML(paste0("<h3 style='margin:3px'>", territory_title, "</h2>")), position = "topright", data = NULL)
  }
  # ONLY fcs territory function
  fcs_territory <- function(territory_type_non_snaped, zscore_type_non_snaped) {
    
    temp2 <- fcs_territories[fcs_territories$non_snap_territory_type == territory_type_non_snaped & fcs_territories$non_snap_zscore_type == zscore_type_non_snaped, ]
    
    #convert new agent locations to sf
    additional_agent_sf <- temp2 %>% 
      
      # Convert new agent locations to sf
      st_as_sf(coords = c("Long", "Lat"), remove = FALSE, crs = 4326, agr = "constant")
    #joining variable data with county geometry data
    territory.counties <- left_join(va.counties, temp2, by = 'NAMELSAD')
    
    non_snap <- additional_agent_sf %>% filter(new_agent == 0)
    new_agent <- additional_agent_sf%>%  filter(new_agent == 1)
    #assigning colors for each agent territory
    pal <- colorFactor(palette = c("#fee08b" , "#fc4e2a","#35b779","#21214f","#332288",
                                   
                                   "#ff69b4", "#018571","#ffffb3","#006d2c", "#B12A90FF", "#4a9848", 
                                   
                                   "#488fc1", "#c2df23", "#004949","#924900" ,"#c44e52", "#fde0dd", 
                                   
                                   "#b3e183","#8e0152", "#8c4f96", "#f98e2b","#a1c9f4", "#1695a3", "#b31a1c", 
                                   
                                   "#e7298a","#FDE725FF", "#440154", "#f0fff0", "#3b528b", "#b31a1c" ), 
                       
                       domain= fcs_territories$Agent,
                       levels= c( "Albemarle","Amelia","Amherst", "Bedford",
                                  "Chesapeake City","Fairfax","Floyd","Gloucester",
                                  "Greensville","Henrico","King George","Lancaster","Lee",
                                  "Loudoun","Louisa","Mecklenburg","Newport News City North",
                                  "Orange","Patrick","Petersburg City",
                                  "Pulaski","Richmond City","Rockingham",
                                  "Spotsylvania","Virginia Beach City North","Warren",
                                  "Bland", "Smyth", "Richmond", "Accomack"
                       ))    
    
    # create labels for counties
    county_labels <- sprintf(
      "<strong>%s</strong><br/> Served by Agent From: %s",
      territory.counties$NAMELSAD,
      territory.counties$Agent
    ) %>% lapply(htmltools::HTML)

    
    # create labels for agents
    agent_labels <- sprintf(
      "<strong>Agent Site </strong><br/>District Office: %s <br/> Agent Name: %s<br/> Contact Info: %s",
      additional_agent_sf$Job.Dept,
      additional_agent_sf$Employee.Name,
      additional_agent_sf$VT.Email
    ) %>% lapply(htmltools::HTML)
    
    new_agent_labels <- sprintf(
      "<strong>New Agent Site </strong><br/>District Office: %s",
      additional_agent_sf$Job.Dept
    ) %>% lapply(htmltools::HTML)
    
    #creating good title names
    idx2 <- which(unique(fcs_territories$non_snap_zcore_type) == zscore_type_non_snaped)
    good_title_names <- c("Aggregate", "Obesity", "Diabetes", "Food Insecurity", "Physical Inactivity", "Low Birthweight")
    # create title for the map
    territory_title = paste("Optimized VCE FCS Agent Sites based on",good_title_names[idx2], "Z-scores", sep= " ")
    
    #making icon set for legend
    icons <- awesomeIconList(
      `FCS Agents` = agents_icon,
      `New Agents` = new_agent_icon)
    
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
      addAwesomeMarkers(data = non_snap,
                        lat = non_snap$Lat,
                        lng = non_snap$Long,
                        label = agent_labels,
                        icon = agents_icon) %>%

      addAwesomeMarkers(data= new_agent,
                        lat =new_agent$Lat,
                        lng = new_agent$Long,
                        label = new_agent_labels,
                        icon = new_agent_icon) %>%
      addLegendAwesomeIcon(iconSet = icons,
                           orientation = 'vertical',
                           title = htmltools::tags$div(
                             style = 'font-size: 20px;',
                             'Agent Type:'),
                           labelStyle = 'font-size: 14px;') %>%
      setView(lng = -79.5, lat = 38.2315734, zoom = 6.5) %>%
      addControl(htmltools::HTML(paste0("<h3 style='margin:3px'>", territory_title, "</h2>")), position = "topright", data = NULL)
  }
  #LINE GRAPH FUNCTION
  sdoh_line <- function(va_avg,county1, county2, county3, variable) {
    
    
    va_avg <- read.csv("./data/with_state_avg.csv") %>% 
      filter(!(Year %in% c(2021, 2022)))
   
    # Filter data for the first selected county and variable
    selection1 <- va_avg[va_avg$County2 == county1 & va_avg$Variable == variable, ] %>% 
      na.omit(cols= Value)
    
    # Filter data for the second selected county and variable
    selection2 <- va_avg[va_avg$County2 == county2 & va_avg$Variable == variable, ]%>% 
      na.omit(cols= Value)
    
    # Filter data for the second selected county and variable
    selection3 <- va_avg[va_avg$County2 == county3 & va_avg$Variable == variable, ]%>% 
      na.omit(cols= Value)
    
    # filter va avg data for selected variable
    avg <- va_avg[va_avg$County2 == "Virginia" & va_avg$Variable == variable, ] %>% 
      na.omit(cols= Value)
    
    # Identify the index of the selected variable
    idx <- which(unique(all_var_df$Variable) == variable)
    
    # Wrap legend title if too long
    spaces <- gregexpr("\\s", good_names[idx])[[1]]
    middle_space <- spaces[length(spaces) %/% 2 + 1]
    legend_title <- paste0(substring(good_names[idx], 1, middle_space-1), "</br>", substring(good_names[idx], middle_space+1))
    
    # Create title for the map
    map_title = paste(good_names[idx],"Over Time", sep= " ")
    
    #plot all the selections and average on plotly line graph
    comparison_plot <- plot_ly() %>%
      add_trace(data = selection1, x = ~Year, y = ~Value, name = county1,
                type = "scatter", mode = "lines", 
                line = list(color = "#8B2323", width = 4)) %>%
      add_trace(data = selection2, x = ~Year, y = ~Value, name = county2,
                type = "scatter", mode = "lines", 
                line = list(color = "#D02090", width = 4)) %>%
      add_trace(data = selection3, x = ~Year, y = ~Value, name = county3,
                type = "scatter", mode = "lines", 
                line = list(color = "#D02032", width = 4)) %>%
      add_trace(data = avg, x = ~Year, y = ~Value, name = "State Average",
                type = "scatter", mode = "lines", 
                line = list(color = "#3F4788FF", width = 4)) %>%
      layout(title = map_title, 
             xaxis = list(tickvals= c(2016, 2017, 2018, 2019, 2020),title = 'Years'),
             yaxis = list(title = good_names[idx]),
             legend = list(font = list(size = 15)))
    
    comparison_plot
  }


# 2. Define UI for application ------------------------------------------------------------
ui <- navbarPage(#title = "DSPG 2023",
                 selected = "overview",
                 theme = shinytheme("lumen"),
                 tags$head(tags$style('.selectize-dropdown {z-index: 10000}')), 
                 useShinyjs(),
                 
                 ## 2.1 Tab Overview--------------------------------------------
                 tabPanel("Overview", value = "overview",
                          fluidRow(style = "margin: 12px;",
                                   align = "center",
                                   h1(strong("VCE: Optimizing Extension Agent Services"), style = "font-size: 65px;",
                                      h4("Data Science for the Public Good Program"), style = "font-size: 50px;",
                                      h4("Virginia Tech"), style = "font-size: 50px;",
                                      img(src = "vce_long.jpg", style = "display: inline; margin-right: 5px;", width = "500px;", align = "center"),
                                      br()
                                   )
                          ),
                          fluidRow(style = "margin: 12px;",
                                   align = "justify",
                                   column(6,
                                          h2(strong("Project Background"), align = "center"),
                                          p(strong("Problem:"), 
                                          p("Virginia Cooperative Extension Family (VCE), Consumer Sciences (FCS), and Supplemental Nutrition Assistance Program-Education (SNAP-Ed) agents are essential contributors to the well-being of Virginian individuals, families, and communities. Their commitment to knowledge and resources greatly benefits communities throughout Virginia. However, these dedicated professionals are overextended, which hinders them from delivering their services and educational programs across the entire Commonwealth."),
                                          p(strong("Virginia Coorperative Extensions:"), 
                                            p("Virginia Cooperative Extension (VCE) was established in 1914 and has since been committed to bringing the resources of Virginia Tech and Virginia State University to the people of the Commonwealth. VCE has"),
                                          tags$li("107 offices"),
                                          tags$li("11 Agriculture Research Extension centers"), 
                                          tags$li("6 4-H centers throughout the state"),
                                      
                                          p("VCE agents and volunteers strive to empower youth and Virginian farmers, guide sustainable resource management, and promote public health. VCE accomplishes these goals through programs that put research-based knowledge to work in people’s lives. VCE has a variety of programs like 4-H Youth Development, Family and Consumer Sciences, Community Viability, Agriculture and Natural Resources, Food, Nutrition, and Health, etc. in every county. VCE works on unique challenges Virginians face in partnership with governments and organizations to solve these issues in a way that benefits all people. With the expertise and knowledge from Virginia Tech and Virginia State University, VCE agents are able to tackle issues and foster community growth across the state. "),
                  
                                          p(strong("Family Consumer Sciences:"),
                                          p("This project focuses on VCE's Family and Consumer Sciences Program and the agents supporting it. FCS programming addresses community needs, including Nutrition/Wellness, Family Financial Education, and Family and Human Development. FCS agents collaborate with various organizations to meet local residents' educational needs and make research-based knowledge applicable to their lives. "),
                                          p(strong("Supplemental Nutrition Assistance Program-Education: "),
                                            p("We will also be focusing on SNAP-Ed agents. SNAP-Ed is a federally funded program that operates through partnerships with state and local organizations, including Cooperative Extension offices in Virginia.
                                            Their primary goal is to promote healthy eating habits, improve food choices, and enhance food security among SNAP recipients."),
                                   ))))),
                                   column(6,
                                          h2(strong("Our Work"), align = "center"),
                                          p(strong("Purpose:")),
                                          p("Our team designed an interactive dashboard to aid VCE FCS agents in identifying areas in Virginia that needed more support. The dashboard helped stakeholders gain a better understanding of community needs, enabling agents to identify specific areas of need for each county in Virginia. The resource aimed to support VCE FCS agents in improving the overall health of Virginia communities."),
                                          p("We utilized publicly accessible data, including Virginia health rankings, to provide stakeholders with a comprehensive understanding of the factors influencing family health in Virginia. Our focus was on various variables aligning with the five determinants of health and health outcomes. Additionally, we mapped these variables alongside existing data on Virginia Cooperative Extension (VCE) agents to identify areas where VCE agents could provide additional support to their communities."),
                                          
                                          h2(strong("Dashboard Aims"), align = "center"),
                                         
                                          p(strong("Social Determinants of Health:"),
                                          p("This interactive dashboard will allow agents to gain valuable context regarding the public health landscape of the counties they serve, enabling them to tailor their services accordingly. 
                                            This dashboard displays data on critical health variables that influence the well-being of Virginia's localities and the work of FCS agents. Overall, this project aims to empower FCS agents' leadership with the knowledge and insights necessary to make informed decisions and have an even larger positive impact on the well-being of individuals and families in Virginia.")),
                                          p(strong("Optimizing Agent Territories:"), 
                                          p("This section presents the results of the optimization process, showcasing the agent territories on maps. These visual representations allow stakeholders to identify the areas where they should focus their efforts and explore the locations of newly assigned agents. By providing this interactive feature, stakeholders can make informed decisions based on the visual insights provided by the maps.")),
                                          
                          
                                   ),
                                   
                                   
                          ),
                          
                          fluidRow(align = "center",
                                   p(tags$small(em('Last updated: July 2023')))
                          ) 
                 ),
                 
                 ## 2.2 Tab Health variables --------------------------------------------
                navbarMenu("Social Determinants of Health" ,
                            ### 2.2.0 Subtab Virginia Public Health Overview
                            tabPanel("Public Health Overview",
                                     fluidRow(style = "margin: 12px;",
                                              h1(strong("Public Health in Virginia"), align = "center"),
                                              p("", style = "padding-top:10px;")),
                                     fluidRow(style = "margin: 12px;",
                                              align = "justify",
                                              column(12,
                                                     p("In the past 100 years, Virginia has seen tremendous growth in its public health sector, nevertheless, there are still many areas that are in need of significant improvement. 
                                                       Like other states, Virginia continues to battle multiple epidemics that have decreased the average life expectancy. Epidemics like COVID-19, opioids, gun violence, and motor vehicle crashes have plagued the welfare of the Commonwealth. Due to the contrasting urban and rural regions in Virginia, health varies drastically based on where Virginians reside. 
                                                       In the more wealthy and populated localities, life expectancy surpasses the national average. However, in 2018, the average life expectancy in 80 of Virginia’s 133 counties fell below the national average. 
                                                       The Virginia Public Health Association even found that life expectancy in the state’s capital varies by as much as 20 years. 
                                                       Virginia struggles to provide clean air and water, safe roadways, protection from communicable diseases, and other essential public health services to the entire population of the Commonwealth."),
                                                     p("Virginia’s unfavorable health outcomes can be attributed to the lack of public health funding and poor access to affordable healthcare. 
                                                       The Joint Commission on Health Care found that Virginia ranks in the bottom third of states in public health spending. Spending about $95 per Capita, the Virginia Department 
                                                       of Health’s budget has remained unchanged for the past 20 years, when adjusted for inflation and population growth. Additionally, federal funding sometimes do not match the specific needs of localities. 
                                                       Federal funding often prioritizes diseases that draw the most attention and while this benefits disease prevention, it unintentionally results in the underinvestment of many needed programs that affect the social determinants of health. 
                                                       Moreover, this lack of funding results in public health workforce shortages, and causes workers like VCE FCS agents to be overworked and overwhelmed by the needs of the population. 
                                                       Staffing shortages inhibit local health departments from carrying out their responsibilities and prevent Virginians from getting the best care available.")
                                                     ),
                                              column(6,
                                                    h2("Social Determinants of Health", align = "center"),
                                                    p("The field of public health encompasses various factors that influence the health and well-being of individuals and communities. One crucial aspect that significantly shapes health outcomes is the social determinants of health.
                                                      These determinants as defined by the World Health Organization (WHO) are the social, economic, and environmental conditions in which people are born, grow, live, work, and age.
                                                      They encompass a wide range of factors, including socioeconomic status, education, neighborhood, and physical environment, access to healthcare, social support networks, and cultural norms. Understanding and addressing the social determinants of health is vital for promoting health 
                                                      equity and reducing health disparities among different populations. While individual behaviors and genetics play a role in health outcomes, social determinants profoundly impact an individual's ability to lead a healthy life. 
                                                      They shape opportunities for good health, influence the distribution of resources and power in society, and create conditions that can either support or hinder individual and community health.")
                                                    
                                                    ),
                                              column(6,
                                                     img(src = "sdoh.jpg", style = "display: inline; margin-right: 5px; ", width = "600px;", align = "center"))
                                     
                                               )
                            ),
                 
                         

                            ### 2.2.1 Subtab Health Outcomes--------------------------------------
                           tabPanel("Dashboard",
                                    h1(strong("Health Variables"), align = "center"),
                                    p("Below, you will find various variables that are considered important for understanding the social determinants of health. The variables are grouped into five different categories, each of which allows you to select different variables. The result will include a map displaying the selected variables, as well as the locations of FCS (Food and Consumer Sciences) and SNAP-Ed (Supplemental Nutrition Assistance Program Education) agent sites.", align = "center"),
                                    tabsetPanel(
                                      tabPanel("Demographics", 
                                               fluidRow(style = "margin: 20px;",
                                                        h1(strong("Demographics"), align = "left", style = "font-size: 18px;"),
                                                        p("Demographics are a vital factor to consider as a social determinant of health due to their significant impact on health outcomes and disparities. Demographic factors such as race, ethnicity, gender, age, income level, and education play a crucial role in shaping health disparities and influencing health behaviors, access to healthcare, and social contexts. By examining demographics, we gain insights into the unique challenges and needs of different population groups, allowing for targeted interventions, tailored healthcare services, and the development of policies that address the specific barriers and inequities faced by diverse communities. Understanding the intersectionality of demographics further enhances our understanding of how multiple factors interact to shape health outcomes and informs strategies for promoting health equity and improving overall health for all individuals and populations.", style = "padding-top:10px;")),
                                               fluidRow(style = "margin: 6px;",
                                                        align = "justify",
                                                        column(3, 
                                                               
                                                               h4(strong("Summary")),
                                                               textOutput("DemographicsDefinition")
                                                               
                                                        ) ,
                                                        column(9, 
                                                               selectInput("demographics", "Select Variable:", width = "50%", choices = c(
                                                                 "Less 18 years old" = "per_less_than_18_years_of_age",
                                                                 "More than 65 years old" = "per_65_and_over",
                                                                 "Black" = "per_black",
                                                                 "American Indian or Alaska Native" = "per_american_indian_or_alaska_native",
                                                                 "Asian" = "per_asian",
                                                                 "Hispanic" = "per_hispanic",
                                                                 "Nonhispanic White" = "per_nonhispanic_white",
                                                                 "Not Proficient in English" = "per_not_proficient_in_english"
                                                               )
                                                               ),
                                                               radioButtons(inputId = "yearSelect_demo", label = "Select Year: ", 
                                                                            choices = c("2016","2017", "2018", "2019", "2020"), 
                                                                            selected = "2020", inline = TRUE),
                                                               withSpinner(leafletOutput("demographicsvar", height = "500px")),
                                                        )
                                                        
                                               ),
                                               fluidRow(style = "margin: 12px;",
                                                        align = "justify",
                                                        column(3,
                                                               h4(strong("Line Graph")),
                                                               selectInput("county11", "Select County 1", choices = unique(va_avg$County2)),
                                                               selectInput("county12", "Select County 2", choices = unique(va_avg$County2), selected = "Richmond City"),
                                                               
                                                        ),
                                                        column(9,
                                                               plotlyOutput("comparison_plot_demo", height = "500px")
                                                        )),
                                               
                                               
                                      ),
                                      
                                      tabPanel("Health Outcomes", 
                                               fluidRow(style = "margin: 20px;",
                                                        h1(strong("Health Outcomes"), align = "left", style = "font-size: 18px;"),
                                                        p("Health outcomes provide insights into the average lifespan and the physical and mental well-being experienced by individuals within a community. These outcomes are shaped by various factors, including access to clean water, affordable housing, quality medical care, and the availability of good employment opportunities. Local, state, and federal programs and policies play a significant role in influencing these factors. Communities often exhibit significant disparities based on geographical location, income levels, and racial or ethnic backgrounds. To uncover these disparities, data is often disaggregated based on people's characteristics or their geographical location. This breakdown of data helps reveal hidden inequalities and enables a better understanding of why and where health outcomes differ across different areas within a county. It also sheds light on how various health factors interact to influence these outcomes. Furthermore, analyzing data in this manner allows us to evaluate how policies and programs either support or limit opportunities for achieving health equity for all individuals within a community.", style = "padding-top:10px;")),
                                               fluidRow(style = "margin: 12px;",
                                                        align = "justify",
                                                        column(3,
                                                               h4(strong("Summary")),
                                                               textOutput("VariableDefinition")),
                                                        column(9,
                                                               selectInput("Health_Outcomes", "Select Variable:", width = "50%", choices = c(
                                                                 "Low Birthweight" = "per_low_birthweight",
                                                                 "Life Expectancy" = "life_expectancy",
                                                                 "Life Expectancy Gap" = "life_expectancy_gap",
                                                                 "Life Expectancy Black" = "life_expectancy_black",
                                                                 "Life Expectancy White" = "life_expectancy_white")
                                                               ),
                                                               radioButtons(inputId = "yearSelect_outcomes", label = "Select Year: ",
                                                                            choices = c("2017", "2018", "2019", "2020"),
                                                                            selected = "2020", inline = TRUE, width = "50%"),
                                                               withSpinner(leafletOutput("outcomes", height = "500px"))
                                                        )),
                                               fluidRow(style = "margin: 12px;",
                                                        align = "justify",
                                                        column(3,
                                                               h4(strong("Line Graph")),
                                                               selectInput("county1", "Select County 1", choices = unique(va_avg$County2)),
                                                               selectInput("county2", "Select County 2", choices = unique(va_avg$County2), selected = "Richmond City"),
                                                               
                                                        ),
                                                        column(9,
                                                               plotlyOutput("comparison_plot", height = "500px")
                                                        )),
                                      ),
                                      
                          
                            
                            
                      
                          
                            ### 2.2.2 Subtab Healthcare Access and Quality--------------------------------------
                            tabPanel("Healthcare Access and Quality", 
                                     fluidRow(style = "margin: 20px;",
                                              h1(strong("Healthcare Access and Quality Variables"), align = "left", style = "font-size: 18px;"),
                                              p("Accessible and affordable healthcare plays a critical role in promoting physical, social, and mental well-being. While health insurance facilitates access to essential medical services, it alone does not guarantee accessibility. It is equally vital for healthcare providers to offer affordable care, be accessible to patients, and be located conveniently.
                                                In the context of VCE FCS agents' work, their efforts can contribute to improving healthcare accessibility for individuals and families. By addressing community-specific needs and collaborating with local healthcare providers, FCS agents can support initiatives that enhance access to quality healthcare. They can facilitate partnerships between healthcare providers and community organizations, advocate for affordable healthcare options, and educate individuals on navigating the healthcare system effectively. Additionally, FCS agents can provide valuable resources and information on health insurance options, enrollment assistance, and healthcare rights.
                                                Through their work, VCE FCS agents can play a pivotal role in fostering collaborations between healthcare providers and communities, promoting health equity, and ensuring that individuals and families have the necessary tools and support to access affordable, quality healthcare services.", style = "padding-top:10px;")),
                                     fluidRow(style = "margin: 12px;",
                                              align = "justify",
                                              column(3, 
                                                    
                                                     h4(strong("Summary")),
                                                     textOutput("HealthAccessVariableDefinition")
                                                    
                                              ) ,
                                              column(9, 
                                                       selectInput("Health_Access", "Select Variable:", width = "50%", choices = c(
                                                       "Percent Uninsured" = "per_uninsured",
                                                       "Children without Insurance" = "per_uninsured_children",
                                                       "Dentist Ratio" = "dentist_ratio",
                                                       "Mental Health Provider Ratio" = "mental_health_provider_ratio",
                                                       "Primary Care Physicians Ratio" = "primary_care_physicians_ratio",
                                                        "Vaccination Rate" = "per_vaccinated",
                                                       "Preventable Hospitalization Rate" = "preventable_hospitalization_rate",
                                                       "Annual Mammograms" = "per_with_annual_mammogram",
                                                       "Diabetes Rate" = "per_adults_with_diabetes",
                                                       "Chlamydia Rate" = "chlamydia_rate"
                                                       )
                                                     ),
                                                     radioButtons(inputId = "yearSelect_access", label = "Select Year: ", 
                                                                  choices = c("2016","2017", "2018", "2019", "2020"), 
                                                                  selected = "2020", inline = TRUE),
                                                     withSpinner(leafletOutput("healthaccess", height = "500px")),
                                                    )
                                              ),
                                     fluidRow(style = "margin: 12px;",
                                              align = "justify",
                                              column(3,
                                                     h4(strong("Line Graph")),
                                                     selectInput("county3", "Select County 1", choices = unique(va_avg$County2)),
                                                     selectInput("county4", "Select County 2", choices = unique(va_avg$County2), selected = "Richmond City"),
                                                     
                                              ),
                                              column(9,
                                                     plotlyOutput("comparison_plot_access", height = "500px")
                                              )),
                            ),
                            
                             
                            ### 2.2.3 Subtab Economic Stability--------------------------------------
                            tabPanel("Economic Stability", 
                                     fluidRow(style = "margin: 20px;",
                                              h1(strong("Economic Stability"), align = "left", style = "font-size: 18px;"),
                                              p("Economic factors have a profound impact on health outcomes and overall well-being within communities. Socioeconomic status, income inequality, and access to resources are all key determinants of health disparities. 
                                                One crucial aspect of economic influence is income and poverty. Low-income individuals and families often struggle to afford basic necessities like nutritious food, stable housing, and healthcare services. 
                                                This can result in higher rates of malnutrition, inadequate living conditions, and limited access to essential healthcare, leading to a range of health challenges. VCE agents can play a pivotal role in addressing these issues by providing community members with resources on financial literacy, budgeting, and connecting them with public assistance programs. By promoting financial stability and providing guidance on accessing available resources, agents can help improve health outcomes for vulnerable populations.
                                                Furthermore, employment and job opportunities significantly impact health and well-being. Unemployment or underemployment can contribute to chronic stress, mental health issues, and limited access to healthcare services. 
                                                Additionally, job insecurity and stressful work environments can negatively affect physical health. "),
                                              p("VCE agents can collaborate with local workforce development agencies, businesses, and educational institutions to provide job training, skills development programs, and support in finding employment. 
                                                By helping individuals gain meaningful employment and stable income, agents contribute to improved health and overall quality of life within their communities.
                                                Education is another important economic factor that influences health outcomes. Educational attainment is strongly associated with better health and economic prospects. 
                                                VCE agents can support educational initiatives by providing workshops, training programs, and resources on topics such as career development, vocational skills, and entrepreneurship. By equipping community members with the necessary skills and knowledge, agents empower individuals to pursue better job opportunities, increase income potential, and ultimately enhance their health and well-being.
                                                Access to affordable and quality healthcare is crucial for maintaining good health. Economic factors significantly influence healthcare access, as individuals with limited financial resources may struggle to afford insurance coverage and healthcare services. VCE agents can raise awareness about healthcare resources, insurance options, and preventative care programs available in the community. They can provide information on navigating the healthcare system, understanding insurance coverage, and connecting community members with local healthcare providers. By promoting access to affordable and quality healthcare, agents contribute to better health outcomes and reduce health disparities within their communities.", style = "padding-top:10px;")),
                                     fluidRow(style = "margin: 12px;",
                                              align = "justify",
                                              column(3, 
                                                     
                                                     h4(strong("Summary")),
                                                     textOutput("EconomicStabilityVariableDefinition")
                                                     
                                              ) ,
                                              column(9, 
                                                     selectInput("econ_stab", "Select Variable:", width = "50%", choices = c(
                                                       "Unemployment Rate" = "per_unemployed",
                                                       "Children in Poverty" = "per_children_in_poverty",
                                                       "Food Insecurity" = "per_food_insecure",
                                                       "Median Household Income" = "median_household_income",
                                                        "Median Household Income (Black)" = "median_household_income_black",
                                                        "Median Household Income (White)" = "median_household_income_white",
                                                         "Median Household Income (Hispanic)" = "median_household_income_hispanic"
                                                        
                                                     )
                                                     ),
                                                     radioButtons(inputId = "yearSelect_econ", label = "Select Year: ", 
                                                                  choices = c("2016","2017", "2018", "2019", "2020"), 
                                                                  selected = "2020", inline = TRUE),
                                                     withSpinner(leafletOutput("econstability", height = "500px")),
                                              )
                                     
                            
                                     ),
                                     fluidRow(style = "margin: 12px;",
                                              align = "justify",
                                              column(3,
                                                     h4(strong("Line Graph")),
                                                     selectInput("county5", "Select County 1", choices = unique(va_avg$County2)),
                                                     selectInput("county6", "Select County 2", choices = unique(va_avg$County2), selected = "Richmond City"),
                                                     
                                              ),
                                              column(9,
                                                     plotlyOutput("comparison_plot_econ", height = "500px")
                                              )),
                            ),
                            ### 2.2.4 Subatb Health Behaviors-------
                            tabPanel("Health Behaviors", 
                                     fluidRow(style = "margin: 20px;",
                                              h1(strong("Health Behaviors"), align = "left", style = "font-size: 18px;"),
                                              p("The Virginia Cooperative Extension and Family Consumer Services can play a crucial role in addressing health behaviors and promoting positive health outcomes within communities. Through extensive outreach and educational initiatives, the Virginia Cooperative Extension can provide valuable information and resources to individuals and families. They can raise awareness about the importance of health behaviors, such as maintaining a balanced diet and engaging in regular physical exercise, and offer guidance on making healthy choices. The Family Consumer Services division can specifically contribute to improving health behaviors by focusing on areas such as nutrition education, budgeting for healthy food options, meal planning, and promoting physical activity. They can provide workshops, classes, and demonstrations that empower community members with the knowledge and skills necessary to make informed choices about their health and well-being. Furthermore, the Virginia Cooperative Extension and Family Consumer Services can collaborate with local organizations, policymakers, and community leaders to advocate for policies and programs that support reliable access to nutritious food and exercise opportunities. They can participate in community partnerships to address food insecurity, promote community gardens and farmers markets, and advocate for the development of safe and accessible spaces for physical activity.", style = "padding-top:10px;")),
                                     fluidRow(style = "margin: 6px;",
                                              align = "justify",
                                              column(3, 
                                                     
                                                     h4(strong("Summary")),
                                                     textOutput("HealthBehaviorsVariableDefinition")
                                                     
                                              ) ,
                                              column(9, 
                                                     selectInput("health_behaviors", "Select Variable:", width = "50%", choices = c(
                                                       "Smoking Rate" = "per_adults_reporting_currently_smoking",
                                                       "Excessive Drinking" = "per_excessive_drinking",
                                                       "Driving Deaths Involving Alcohol" = "per_driving_deaths_with_alcohol_involvement",
                                                       "Physical Inactivity" = "per_physically_inactive",
                                                       "Teen Birth Rate" = "teen_birth_rate",
                                                       "Obesity Rate" = "per_adults_with_obesity"
                                                     )
                                                     ),
                                                     radioButtons(inputId = "yearSelect_healthbehaviors", label = "Select Year: ", 
                                                                  choices = c("2016","2017", "2018", "2019", "2020"), 
                                                                  selected = "2020", inline = TRUE),
                                                     withSpinner(leafletOutput("healthbehaviors", height = "500px")),
                                              )
                                              
                                     ),
                                     fluidRow(style = "margin: 12px;",
                                              align = "justify",
                                              column(3,
                                                     h4(strong("Line Graph")),
                                                     selectInput("county7", "Select County 1", choices = unique(va_avg$County2)),
                                                     selectInput("county8", "Select County 2", choices = unique(va_avg$County2), selected = "Richmond City"),
                                                     
                                              ),
                                              column(9,
                                                     plotlyOutput("comparison_plot_behavior", height = "500px")
                                              )),
                            ),
                            ### 2.2.5 Subtab Neighborhood and Built Envr------
                            tabPanel("Neighborhood and Built Environment", 
                                     fluidRow(style = "margin: 20px;",
                                              h1(strong("Neighborhood and Built Environment"), align = "left", style = "font-size: 18px;"),
                                              p("The physical environment encompasses the spaces where individuals reside, acquire knowledge, work, and engage in recreational activities. It includes factors such as housing, transportation, and the overall built environment.
                                             Stable and affordable housing plays a significant role in providing a safe environment for families to thrive and prosper. However, housing costs often constitute a substantial portion of a family's expenses. When rent or mortgage payments become unaffordable, 
                                             families are compelled to make challenging trade-offs, such as prioritizing housing over other essential needs like utilities, food, transportation, or medical care.", style = "padding-top:10px;")),
                                     fluidRow(style = "margin: 6px;",
                                              align = "justify",
                                              column(3, 
                                                     
                                                     h4(strong("Summary")),
                                                     textOutput("EnvrVariableDefinition")
                                                     
                                              ) ,
                                              column(9, 
                                                     selectInput("neighbor_envr", "Select Variable:", width = "50%", choices = c(
                                                       "Physical Distress" = "per_physical_distress",
                                                       "Mental Distress" = "per_mental_distress",
                                                       #"Access to Exercise Opportunity" = "per_with_access_to_exercise_opportunities",
                                                        "Juvenile Arrest Rate" = "juvenile_arrests_rate",
                                                        "Insufficient Sleep" = "per_insufficient_sleep",
                                                        "Housing Problems" = "per_severe_housing_problems"
                                                     )
                                                     ),
                                                     radioButtons(inputId = "yearSelect_envr", label = "Select Year: ", 
                                                                  choices = c("2016","2017", "2018", "2019", "2020"), 
                                                                  selected = "2020", inline = TRUE),
                                                     withSpinner(leafletOutput("envr", height = "500px")),
                                              
                                     )
                                    
                                 ),
                                 fluidRow(style = "margin: 12px;",
                                          align = "justify",
                                          column(3,
                                                 h4(strong("Line Graph")),
                                                 selectInput("county9", "Select County 1", choices = unique(va_avg$County2)),
                                                 selectInput("county10", "Select County 2", choices = unique(va_avg$County2), selected = "Richmond City"),
                                                 
                                          ),
                                          column(9,
                                                 plotlyOutput("comparison_plot_envr", height = "500px")
                                          )),
                            ),
                            
                            ### 2.2.6 Subtab Demographics-----

                 ),
                 ),
                ),
                 
                 ## 2.4 Tab Agent Optimization Programming------
                navbarMenu("Agent Optimization",
                           tabPanel("Methodology",
                                    fluidRow(style = "margin: 12px;",
                                             h1(strong("Agent Optimization Process"), align = "center")),
                                    fluidRow(style = "margin: 12px;",
                                             column(12,
                                                    titlePanel(strong("Overview")),
                                                    p("Our primary objective is to enhance the efficiency of FCS (Food and Consumer Service) agent efforts by strategically determining optimal territories for these agents to cover. Given that not all counties currently have FCS agents, our aim is to find the most effective way to allocate their efforts across different areas. 
                                                        We want to avoid situations where some agents are serving relatively well-off counties while others are burdened with multiple counties, some of which may be challenging to access due to long travel times."),
                                                        p("To achieve this goal, we will spatially optimize the distribution of existing agents, ensuring a balanced and equitable allocation of their services. This means identifying areas where FCS agents can make the most significant impact and where their services are most needed.
                                                        Furthermore, we will also explore the possibility of establishing new FCS agent locations in areas where there is a high demand for their services. By identifying regions with the greatest need and potential impact, we can strategically deploy new agents to address the specific challenges faced by underserved communities."),
                                                        p("In addition to optimizing the allocation of FCS agents, our methodology will extend to SNAP-Ed (Supplemental Nutrition Assistance Program Education) agents. Since SNAP-Ed agents work with families who have limited financial means, we aim to provide them with valuable insights into where their resources should be allocated most effectively. By focusing on specific groups with income levels below $2,000, SNAP-Ed agents can tailor their efforts to serve those who need assistance the most. By considering both SNAP-Ed and FCS agents together, we can achieve a comprehensive approach that optimizes the impact of their collective efforts. This integrated approach will enable us to better address the diverse needs of various communities and create a more efficient and equitable distribution of services."),
                                                        p("Ultimately, our objective is to empower agents with the necessary tools and data-driven insights to make informed decisions about resource allocation. By strategically positioning FCS and SNAP-Ed agents and identifying areas with the highest potential for impact, we can work towards improving the overall well-being of communities and promoting health and nutrition for those who need it most."),
                                                        p(strong("Workflow:")),
                                                        tags$li("Perform a literature review to understand the role of FCS and identify which SDoH variables are relevant to the work that FCS does"),
                                                        tags$li("Identify health outcomes that FCS agents can affect"),
                                                        tags$li("Create an aggregate measure of need within communities through a health index: aggregate z-score"),
                                                        tags$li("Use current FCS agent locations and isochrone maps to determine how accessible FCS agents are to their communities"),
                                                        tags$li("Use a mathematical programming model to optimize the assignment of counties to FCS agents"),
                                                        tags$li("Use a mathematical programming model to determine the best locations for adding new FCS agents to Virginia"),
                                                        h1(strong("Programming"), align = "center"),
                                                        fluidRow(style = "margin: 12px;",
                                                             column(6,
                                                                    h1(strong("Objective Function"), align = "center"),
                                                                    img(src = "equation.png", style = "display: inline; margin-right: 5px; ", width = "500px;", align = "center"),
                                                                    p("The objective of this model is to maximize the population-weighted need of each county, which serves as a measure of the overall demand for services in a given area. By maximizing this objective, the model aims to allocate resources in a way that addresses the varying needs of different counties effectively. To ensure a realistic and practical allocation, the model incorporates four constraints that capture the challenges faced by agents. These constraints are designed to limit the workload of agents and consider the constraints they encounter in their service provision."),
                                                                    tags$li(strong("Population:")),
                                                                    p("The population factor plays a crucial role in this mathematical program as it addresses the challenges faced by agents in serving counties with varying population densities. By setting a limit of 1.2 million people for each agent, we ensure that the workload is distributed fairly and that no agent becomes overwhelmed with an excessively large population to serve. This constraint helps to balance the distribution of agents across counties, considering their respective populations."),
                                                                    tags$li(strong("Distance:")),
                                                                    p("The distance constraint is another critical component of this model as it addresses the challenges related to geographical distances that agents must cover in their service provision. By imposing a constraint on the maximum distance an agent can travel, we ensure that the service coverage is feasible and practical in terms of travel time and logistics. The distance constraint acknowledges that agents have limitations on how far they can travel to reach the counties they serve. This constraint helps to account for the time and resources required for agents to travel between counties, ensuring that they can provide timely and efficient services to the populations in need."),
                                                                    tags$li(strong("Need:")),
                                                                    p("The need factor is specifically determined by the unique needs of each county. These needs are based on various social determinants of health variables. Given that each county requires different services, 
                                                                      it is impractical for agents to handle all the problems alone. To address this issue, we developed a health index by aggregating z-scores. These z-scores are calculated using five key social determinants of health variables: obesity, food insecurity, diabetes, low birthweight, and physical inactivity. Although there are many other variables to consider, we believe that focusing on these health variables allows FCS agents to make a significant impact."),
                                                                      h1(strong("Constraints"), align = "center"),
                                                                      tags$li(strong("Mean commute time less than 120 minutes")),
                                                                      p("The time constraint is implemented to ensure that agents do not have to endure excessive travel times exceeding 120 minutes. This constraint aims to optimize efficiency by minimizing the commuting burden placed on agents. By limiting travel distances, agents can allocate more time to engage with and serve their assigned communities effectively. This constraint helps maintain a reasonable work-life balance for agents, allowing them to maximize their availability and dedicate their efforts to fulfilling their responsibilities within a manageable time."),
                                                                      tags$li(strong("Unique assignment")),
                                                                      p("The county assignment constraint ensures that every county is allocated to an agent, leaving no county without coverage. This constraint guarantees that each county receives the attention and support of an assigned agent. By assigning agents to specific counties, we can ensure that the unique needs and characteristics of each county are addressed, providing tailored services and resources to the communities within them. This approach promotes comprehensive coverage and equitable distribution of support across all counties, leaving no county overlooked or underserved."),
                                                                      tags$li(strong("Population served less than 1.2 million")),
                                                                      p("The population constraint serves as an important mechanism to ensure that agents are not overwhelmed with excessive workloads. By considering the varying population density, particularly in rural and urban areas, we can identify imbalances where some agents may be burdened with more individuals to assist, while others have a lighter workload. 
                                                                      This constraint helps maintain a fair distribution of responsibilities among agents, ensuring that they can effectively and efficiently serve the population within their capacity."),
                                                                      tags$li(strong("District(agent) = district(county)")),
                                                                      p("The district-based constraint ensures that agents operate within their designated districts as established by the Virginia Cooperative Extension (VCE). 
                                                                      VCE divides Virginia into five distinct districts, and it is essential for agents to adhere to this division. By working exclusively within their assigned districts, 
                                                                      agents can effectively focus on the specific needs and priorities of their respective communities. This constraint enables efficient coordination, ensures localized expertise, 
                                                                      and enhances the delivery of targeted services within each district.")
                                                                           ),
                                                        column(6, align = "center",
                                                               h1("Figure 1: Isochrone Map"),
                                                               p("Figure 1 illustrates an isochrone map that represents the travel distance for agents. The map displays three key time thresholds: 60 minutes, 40 minutes, and 20 minutes. However, there are certain counties where agents would need to travel for more than one hour to reach them. These counties pose greater logistical challenges due to their distance from the agents' locations, requiring additional time and resources for travel."),
                                                               img(src = "isochrone.png", style = "display: inline; margin-right: 5px; ", width = "500px"),
                                                               h1("Figure 2: Aggregate Z-scores Map"),
                                                               p("Figure 2 displays a map depicting the aggregated z-scores calculated for each county. The color intensity on the map indicates the magnitude of the z-scores, with darker colors representing lower z-scores. This signifies that the county is significantly below the average in terms of the five health variables considered. 
                                                        Counties with lower z-scores require more assistance from agents as they exhibit greater needs across these health factors."),
                                                        img(src = "zscore_map.png", style = "display: inline; margin-right: 5px; ", width = "700px"),
                                                        h1("Figure 3: VCE Districts"),
                                                        p("Figure 3 shows the different VCE districts."),
                                                        img(src = "vce_districts.jpg", style = "display: inline; margin-right: 5px;", width = "700px")
                                                        ))
                                                           )
                                                               )
                                                                 ),
                                                            
                            
                 
                            ### 2.4.2 Subtab Results ----
                            tabPanel("Where Should New Agents Be?",
                                     tabsetPanel(
                                       tabPanel("FCS and SNAP-Ed Agents",
                                                fluidRow(
                                                  style = "margin: 12px;",
                                                  h1(strong("Results"), align = "center"),
                                                  column(12,
                                                         p("Please submit different choices to the Agents/Health dropdowns to see a new map! ", style = "padding-top:20px;")
                                                  )
                                                ),
                                                
                                                fluidRow(
                                                  style = "margin: 12px;",
                                                  column(4,
                                                         selectInput("territory_type", "Agents",
                                                                     choices = c("No New Agents" = "base",
                                                                                 "One New Agent" = "one",
                                                                                 "Two New Agents" = "two"),
                                                                     selected = "base"
                                                         ),
                                                         selectInput("zscore_type", "Health Variables",
                                                                     choices = c("Aggregate" = "aggregate",
                                                                                 "Food Insecurity" = "food",
                                                                                 "Obesity" = "obese",
                                                                                 "Low Birthweight" = "lowbirth",
                                                                                 "Physical Inactivity" = "inactivity",
                                                                                 "Diabetes" = "diabetes"),
                                                                     selected = "aggregate"
                                                         ),
                                                         h4(strong("What Does the Map Show?")),
                                                         textOutput("territorydescription")
                                                  ),
                                                  
                                                  column(8,
                                                         h4(strong("Map")),  # Add the heading for the map
                                                         leafletOutput("map", width = "100%", height = "700px")
                                                  )
                                                )
                                       ),
                                       #results for snaped----
                                      tabPanel("SNAP-Ed Agents",
                                                fluidRow(
                                                  style = "margin: 12px;",
                                                  h1(strong("Results"), align = "center"),
                                                  column(12,
                                                         p("Please submit different choices to the Agents/Health dropdowns to see a new map! ", style = "padding-top:20px;")
                                                  )
                                                ),

                                                fluidRow(
                                                  style = "margin: 12px;",
                                                  column(4,
                                                         selectInput("territory_type_snaped", "Agents",
                                                                     choices = c("No New Agents" = "base",
                                                                                 "One New Agent" = "one",
                                                                                 "Two New Agents" = "two"),
                                                                     selected = "base"
                                                         ),
                                                         selectInput("zscore_type_snaped", "Health Index",
                                                                     choices = c("Aggregate" = "aggregate",
                                                                                 "Food Insecurity" = "food",
                                                                                 "Obesity" = "obese",
                                                                                 "Low Birthweight" = "lowbirth",
                                                                                 "Physical Inactivity" = "inactivity",
                                                                                 "Diabetes" = "diabetes"),
                                                                     selected = "aggregate"
                                                         ),
                                                         h4(strong("What Does the Map Show?")),
                                                         textOutput("territorydescription_snaped")
                                                  ),

                                                  column(8,
                                                         h4(strong("Map")),  # Add the heading for the map
                                                         leafletOutput("map_snaped", width = "100%", height = "700px")
                                                  )
                                                )
                                       ),
                                       # #results for nonsnaped----
                                      tabPanel("FCS Agents",
                                               fluidRow(
                                                 style = "margin: 12px;",
                                                 h1(strong("Results"), align = "center"),
                                                 column(12,
                                                        p("Please submit different choices to the Agents/Health dropdowns to see a new map! ", style = "padding-top:20px;")
                                                 )
                                               ),

                                               fluidRow(
                                                 style = "margin: 12px;",
                                                 column(4,
                                                        selectInput("territory_type_non_snaped", "Agents",
                                                                    choices = c("No New Agents" = "base",
                                                                                "One New Agent" = "one",
                                                                                "Two New Agents" = "two"),
                                                                    selected = "base"
                                                        ),
                                                        selectInput("zscore_type_non_snaped", "Health Index",
                                                                    choices = c("Aggregate" = "aggregate",
                                                                                "Food Insecurity" = "food",
                                                                                "Obesity" = "obese",
                                                                                "Low Birthweight" = "lowbirth",
                                                                                "Physical Inactivity" = "inactivity",
                                                                                "Diabetes" = "diabetes"),
                                                                    selected = "aggregate"
                                                        ),
                                                        h4(strong("What Does the Map Show?")),
                                                        textOutput("territorydescription_nonsnaped")
                                                 ),

                                                 column(8,
                                                        h4(strong("Map")),  # Add the heading for the map
                                                        leafletOutput("map_non_snaped", width = "100%", height = "700px")
                                                 )
                                               )
                                      )
                                     )
                            )),
                            
                 
                 
                 # ## 2.5 Tab Takeawayss --------------------------------------------
                 # tabPanel("Takeaways", value = "conclusion", 
                 #          fluidRow(style = "margin: 6px;",
                 #                   h1(strong("Project Findings and Predictions"), align = "center"),
                 #                   p("", style = "padding-top:10px;"),
                 #                   p("findings on maps"),
                 #                   fluidRow(style = "margin: 6px;", align = "justify",
                 #                            h4(strong("VCE")),
                 #                            
                 #                   ), 
                 #                   
                 #                   
                 #                   
                 #          )),
                 
                 
                 ## 2.6 Tab Data Sources --------------------------------------------
                 tabPanel("Data Sources", 
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Data Sources"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   fluidRow(style = "margin: 6px;", align = "left",
                                            column(4,
                                                   img(src = "county_health_rankings.jpg", style = "display: inline; float: left;", width = "230px"),
                                                   p(strong("County Health Rankings & Roadmaps"), "The County Health Rankings & Raodmaps (CHR&R) a program of the University of Wisconsin Population Health Institute. 
                                                   The CHR&R program provides data, evidence, guidance, and examples to build awareness of the multiple factors that influence health and support leaders in growing community power to improve health equity. This project utilizes CHR&R
                                                    to obtain 2016/2020 county-level data to explore and visualize the Commonwealth's health characteristics.")),
                                            column(4,
                                                   img(src = "vce_logo.jpg", style = "display: inline; float: left;", width = "200px"),
                                                   p(strong("Virginia Cooperative Extension Administrative Data"), "Virginia Cooperative Extension (VCE) provided us with office and agent data which allowed us to gain a better understanding of where Family Consumer Science
                                                     agents operate. The team used this data to create visualizations, specifically focusing on the distribution of optimized agent territories across localities.")),
                                            column(4,
                                                   img(src = "data-acs.jpg", style = "display: inline; float: left;", width = "230px"),
                                                   p(strong("American Community Survey (ACS)"),"We retrieve ACS data to examine demographic and socioeconomic characteristics of our target population. ACS is an ongoing yearly survey conducted by the U.S Census Bureau that samples 
                                                            households to compile 1-year and 5-year datasets. We used the most recently available 1-year/5-year estimates, to analyze localities social determinants of health.")),
                                            ), 
                                            
                                            fluidRow(style = "margin: 6px;", align = "left",
                                                     column(4,
                                                            img(src = "brfss.jpg", style = "display: inline; float: left;", width = "230px"),
                                                            p(strong("Behavioral Risk Factor Surveillance System (BRFSS)"),"BRFSS stands as the leading national platform for health-related telephone surveys. Its primary objective is to gather state-level data on health-related risk 
                                                            behaviors, chronic health conditions, and the utilization of preventive services among U.S. residents.")),
                                                     column(4,
                                                            img(src = "cdc_stat.jpg", style = "display: inline; float: left;", width = "230px"),
                                                            p(strong("CDC National Center for Health Statistics(NCHS)"),"The NCHS is responsible for the collection, analysis, and dissemination of health data and statistics. NCHS focuses on providing timely, relevant, and accurate information 
                                                            that informs the public and assists in making program and policy decisions aimed at enhancing the health of our nation. Through its products and services, NCHS strives to contribute to the improvement of public health.
                                                            Our team used this data to dive deeper into the public health landscape of the Commonwealth.")),
                                                     column(4,
                                                            img(src = "census.jpg", style = "display: inline; float: left;", width = "230px"),
                                                            p(strong("United States Census Bureau"),"The primary objective of the Census Bureau is to fulfill its role as the foremost provider of high-quality data concerning the people and economy of the nation. Their mission centers 
                                                              around delivering data that encompasses an ideal combination of timeliness, relevance, quality, and cost-effectiveness. Our team used insightful data on the demographics of Virginia to enhance our dashboard's descriptiveness")),
                                                            
                                             ),
                                            

                                            
                                            
                                  

                                             fluidRow(style = "margin: 6px;", align = "left",
                                                      column(4,
                                                             img(src = "usda_ers.jpg", style = "display: inline; float: left;", width = "230px"),
                                                             p(strong("USDA Economic Research Service (ERS)"),"The mission of the ERS, a part of the U.S. Department of Agriculture, is to proactively identify trends and emerging issues in agriculture, 
                                                               food, the environment, and rural America. Through conducting rigorous and unbiased economic research, the ERS aims to provide valuable insights that inform and enrich decision-making processes 
                                                               for both public and private sectors. The team used ERS data on the food environment in Virginia to help Agents better tailor their FCS programming .")),
                                                      column(4,
                                                             img(src = "bureau_labor_stat.jpg", style = "display: inline; float: left;", width = "230px"),
                                                             p(strong("Bureau of Labor Statistics (BLS)"),"The BLS is responsible for assessing labor market activity, working conditions, price fluctuations, and productivity 
                                                               within the U.S. economy. Their primary objective is to provide essential data that supports decision-making processes in both public and private sectors.By improving working conditions, expanding opportunities for meaningful employment, and 
                                                               ensuring the provision of work-related benefits and rights, they contribute to fostering the welfare of individuals and promoting their economic stability. The team looked at BLS data to analyze how economic stability in Virginians lives effects
                                                               their overall well-being and access to healthcare.")),
                                                      
                                                      
                                   ),
                          )),
                 

                 
                 
                 ## 2.5 Tab Team --------------------------------------------
                 tabPanel("Meet the Team", 
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                   align = "center",
                                   h1(strong("Meet the Team")),
                                   br(),
                                   h4(strong("VT Data Science for the Public Good")),
                                   p("The", a(href = 'https://dspg.aaec.vt.edu/', 'Data Science for the Public Good (DSPG) Young Scholars program', target = "_blank"),
                                     "is a summer immersive program held at the", a(href = 'https://aaec.vt.edu/index.html', 'Virginia Tech Department of Agricultural'), "and", a(href = 'https://ext.vt.edu/','Applied Economics and the Virginia Cooperative Extension Service.'),
                                     "In its third year, the program engages students from across the country to work together on projects that address state, federal, and local government challenges around critical
                                social issues relevant in the world today. DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences to determine how 
                                information generated within every community can be leveraged to improve quality of life and inform public policy. For more information on program highlights, how to apply,
                                and our annual symposium, please visit", 
                                     a(href = 'https://dspg.aaec.vt.edu/', 'the official VT DSPG website.', target = "_blank")),
                                   p("", style = "padding-top:10px;")
                          ),
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                   column(6, align = "center",
                                          h4(strong("DSPG Undergraduate Interns")),
                                          img(src = "Sareth_Moy.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "500px"),
                                          img(src = "Vivian_Peregrino.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "500px"),
                                          br(),
                                          p(a(href = 'https://www.linkedin.com/in/sarethmoy90/', 'Sareth Moy', target = '_blank'), "(Berea College, Undergraduate in Economics, minor in Mathematics);",
                                            br(), 
                                            a(href = 'https://www.linkedin.com/in/vivian-peregrino/', 'Vivian Peregrino', target = '_blank'), "(Virginia Tech, Undergraduate in Environmental Economics, Management, and Policy)"),
                                            br(), 
                                            p("", style = "padding-top:10px;"),
                                          
                                          h4(strong("DSPG PhD Student Fellow")),
                                          img(src = "Yang_Cheng.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "500px"),
                                          br(),
                                          p(a(href = 'https://www.linkedin.com/in/yangcheng2019/', 'Yang Cheng', target = '_blank'), "(Virginia Tech, PhD Student Fellow in Agricultural Economics)"),
                                          p("", style = "padding-top:10px;")
                                   ),
                                   column(6, align = "center",
                                          h4(strong("VT Faculty Members")),
                                          img(src = "SusanChen.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "500px"),
                                          img(src = "Michael_Cary.JPG", style = "display: inline; border: 1px solid #C0C0C0;", width = "500px"),
                                          br(),
                                          p(a(href = "https://www.linkedin.com/in/susanchenja/", 'Dr. Susan Chen', target = '_blank'), "(Associate Professor of Agricultural and Applied Economics);",
                                            br(), 
                                            a(href = 'https://aaec.vt.edu/academics/undergraduate/dspg/team.html', 'Dr. Michael Cary', target = '_blank'), "(Research Assistant Professor of Agricultural and Applied Economics)" ),
                                          br(),
                                          p("", style = "padding-top:10px;"),
                                          
                                          h4(strong("Project Stakeholders")),
                                          img(src = "Kathy_Hosig.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "300px"),
                                          p(a(href = "https://vetmed.vt.edu/people/faculty/hosig-kathy.html", 'Kathy Hosig', target = '_blank'), "(Virginia Cooperative Extension)",
                                            br(), 
                                          #   a(href = 'https://goochland.ext.vt.edu/staff/Maxwell-Charlotte.html', 'Nichole Shuman', target = '_blank'), "(Virginia Cooperative Extension, Goochland County)."),
                                          # p("", style = "padding-top:10px;"),
                                          
                                          
                                   )
                                   
                          )) ,
                 inverse = T))


# 3. Define server logic  ------------------------------------------------------------
server <- function(input, output) {
  # Execute the JavaScript code when the app is loaded--> for the DSPG logo
  shinyjs::runjs(jscode)
  
  ## 3.1 Health Outcomes ------------------------------------------
  # Create a reactive expression for health outcomes drop down
  
  temp_outcome <- reactive({
    input$Health_Outcomes
  })
  
  temp_year <- reactive({
    as.integer(input$yearSelect_outcomes)
  })
  
  output$outcomes <- renderLeaflet({
    mapping2(temp_outcome(), temp_year())
  })
  
  comparison_plot_reactive <- reactive({
    county1 <- input$county1
    county2 <- input$county2
    
    if (temp_outcome() == "per_low_birthweight") {
      comparison_plot <- sdoh_line(va_avg, county1, county2, temp_outcome())
      return(comparison_plot)
    } else if (temp_outcome() == "life_expectancy"){
      comparison_plot <- sdoh_line(va_avg, county1, county2, temp_outcome())
      return(comparison_plot)
    } else if (temp_outcome() == "life_expectancy_gap"){
      comparison_plot <- sdoh_line(va_avg, county1, county2, temp_outcome())
      return(comparison_plot)
    } else if (temp_outcome() == "life_expectancy_black"){
      comparison_plot <- sdoh_line(va_avg, county1, county2, temp_outcome())
      return(comparison_plot)
    } else if (temp_outcome() == "life_expectancy_white"){
      comparison_plot <- sdoh_line(va_avg, county1, county2, temp_outcome())
      return(comparison_plot)
    } else {
    return(NULL)
    }
  })
  
  output$comparison_plot <- renderPlotly({
    comparison_plot_reactive()
  })
  
  output$VariableDefinition <- renderText({
    if (input$Health_Outcomes == "per_low_birthweight") {
      "% Low Birthweight: Percentage of live births with low birthweight (< 2,500 grams).Low birthweight is a significant public health indicator that reflects various factors related to maternal health, nutrition, healthcare delivery, and poverty. It is primarily attributed to two main causes: preterm births and intrauterine growth restrictions. Both of these conditions are associated with increased risks of infant morbidity and mortality.
      Preterm births, which occur before 37 weeks of gestation, contribute to low birthweight. Given the far-reaching consequences of low birthweight, it is crucial to address the underlying factors contributing to it. This involves efforts to improve access to quality prenatal care, promote proper nutrition, address maternal stress, reduce exposure to pollution, and provide support for substance misuse prevention and treatment during pregnancy. By addressing these factors, we can work towards reducing the occurrence of low birthweight and improving the long-term health outcomes for infants and their families."
    } else if (input$Health_Outcomes == "life_expectancy") {
      "Life Expectancy: Average number of years a person can expect to live. 
      Life expectancy is a vital metric for assessing health outcomes as it provides valuable insights into the well-being of a population and the factors influencing lifespan. This measure not only informs us about the overall state of the population's health but also sheds light on the various determinants that can impact longevity. By analyzing life expectancy, we gain a comprehensive understanding of both the health status of a community and the underlying elements that shape the length of an individual's life.
      In 2020, the life expectancy at birth for the total population of Virginia was 78.3 years, as reported by the Virginia Department of Health (VDH). 
      However, this figure marked a decrease of 1.4 years compared to the previous year. The decline in life expectancy can be attributed to several factors, including the significant impact of the COVID-19 pandemic, which accounted for a large number of deaths. Additionally, unintentional injuries, heart disease, chronic liver disease and cirrhosis, diabetes, and Alzheimer's disease were significant contributors to the decrease in life expectancy. These causes reflect the complex nature of health outcomes and highlight the need for comprehensive efforts to address these factors and improve population health."
    } else if (input$Health_Outcomes == "life_expectancy_gap") {
      "Life Expectancy Gap: The life expectancy gap is a measure obtained by calculating the disparity between the life expectancies of white and black populations. This disparity serves as a significant indicator of health inequalities between these two racial groups. Analyzing the life expectancy gap provides valuable insights into the existing disparities in health outcomes between white and black populations. By examining this gap, we can gain a deeper understanding of the prevailing health disparities and the need for targeted interventions to address the unequal health experiences of these racial groups."
    } else if (input$Health_Outcomes == "life_expectancy_black") {
      "Life Expectancy Black: Avergae number of years a black person can expect to live. In 2020, the average life expectancy for individuals of the black race, as reported by the Virginia Department of Health, was 73.9 years. This figure represented a decrease of 2.4 years compared to the previous year, 2019. The life expectancy of the black population is an important measure that provides insights into the health outcomes and well-being of this specific racial group. By monitoring changes in life expectancy over time, we can identify trends and address the factors contributing to the decrease, aiming to improve the health and longevity of individuals belonging to the black race."
    } else if (input$Health_Outcomes == "life_expectancy_white") {
      "Life Expectancy White: Avergae number of years a white person can expect to live. 
      In 2020, based on data from the Virginia Department of Health, the life expectancy was recorded as 78.9 years, indicating a decrease of 1.1 years compared to the previous year, 2019. This measure provides valuable information about the expected average lifespan within the population. By tracking changes in life expectancy over time, we can gain insights into health trends and factors that might contribute to the observed decrease, leading to potential interventions aimed at improving overall population health and well-being."
    } else {
      "Please select a health outcome."
    } 
  })  

  ## 3.2 Healthcare Access -----
  #create a reactive expression for healthcare access variables
  temp_healthaccess <- reactive({
    input$Health_Access
  })
  
  temp_healthaccessyear <- reactive({
    as.integer(input$yearSelect_access)
  })
  
  output$healthaccess <- renderLeaflet({
    mapping2(temp_healthaccess(), temp_healthaccessyear())
    
  })
 
  
  comparison_plot_access_reactive <- reactive({
    county3 <- input$county3
    county4 <- input$county4
    
    if (temp_healthaccess() == "per_uninsured") {
      comparison_plot_access <- sdoh_line(va_avg, county3, county4, temp_healthaccess())
      return(comparison_plot_access)
    } else if (temp_healthaccess() == "dentist_ratio"){
      comparison_plot_access <- sdoh_line(va_avg, county3, county4, temp_healthaccess())
      return(comparison_plot)
    } else if (temp_healthaccess() == "mental_health_provider_ratio"){
      comparison_plot_access <- sdoh_line(va_avg, county3, county4, temp_healthaccess())
      return(comparison_plot)
    } else if (temp_healthaccess() == "primary_care_physicians_ratio"){
      comparison_plot_access <- sdoh_line(va_avg, county3, county4, temp_healthaccess())
      return(comparison_plot_access)
    } else if (temp_healthaccess() == "per_vaccinated"){
      comparison_plot_access <- sdoh_line(va_avg, county3, county4, temp_healthaccess())
      return(comparison_plot_access)
    } else if (temp_healthaccess() == "per_with_annual_mammogram"){
      comparison_plot_access <- sdoh_line(va_avg, county3, county4, temp_healthaccess())
      return(comparison_plot_access)
    } else if (temp_healthaccess() == "preventable_hospitalization_rate"){
      comparison_plot_access <- sdoh_line(va_avg, county3, county4, temp_healthaccess())
      return(comparison_plot_access)
    } else if (temp_healthaccess() == "per_uninsured_children"){
      comparison_plot_access <- sdoh_line(va_avg, county3, county4, temp_healthaccess())
      return(comparison_plot_access)
    } else if (temp_healthaccess() == "per_adults_with_diabetes"){
      comparison_plot_access <- sdoh_line(va_avg, county3, county4, temp_healthaccess())
      return(comparison_plot_access)
    } else if (temp_healthaccess() == "chlamydia_rate"){
      comparison_plot_access <- sdoh_line(va_avg, county3, county4, temp_healthaccess())
      return(comparison_plot_access)
    } else {
      return(NULL)
    }
  })
  
  output$comparison_plot_access <- renderPlotly({
    comparison_plot_access_reactive()
  })
  
  output$HealthAccessVariableDefinition <- renderText({
    if (input$Health_Access == "per_uninsured") {
      "% Uninsured: Percentage of population under age 65 without health insurance.
      The absence of health insurance coverage presents a notable obstacle in accessing essential healthcare services and maintaining 
      financial stability. According to a report by the Kaiser Family Foundation, individuals without insurance face significant health 
      consequences as they receive less preventive care, and delayed treatment often leads to severe illnesses or other health complications. 
      Moreover, being uninsured can have substantial financial implications, with many individuals unable to afford their medical expenses, 
      leading to the accumulation of medical debt."
    } else if (input$Health_Access == "dentist_ratio") {
      "Dentist Ratio: Ratio of population to dentists.
      Neglected dental diseases can result in significant health consequences, such as pain, infection, and tooth loss. 
      While the inadequacy of dental providers represents just one of the barriers to accessing oral healthcare, a substantial 
      portion of the nation faces shortages in this field. According to the Health Resources and Services Administration, as of December 
      2022, there were 7,313 designated Dental Health Professional Shortage Areas (HPSAs), encompassing a total population of 70 million 
      individuals affected by these shortages."
    } else if (input$Health_Access == "mental_health_provider_ratio") {
      "Mental Health Provider Ratio: Ratio of population to mental health providers.
      Accessing healthcare involves more than just financial coverage; it also necessitates access to healthcare providers. 
      Approximately thirty percent of the population resides in a county designated as a Mental Health Professional Shortage Area, 
      indicating significant deficiencies in mental health providers. With the mental health parity provisions of the Affordable Care Act 
      expanding coverage for mental health services, there is growing concern about exacerbated workforce shortages in this field."
    } else if (input$Health_Access == "primary_care_physicians_ratio") {
      "Primary Care Physicians Ratio: Ratio of population to primary care physicians
      Access to healthcare is not solely reliant on financial coverage; it also requires access to healthcare providers. 
      While an abundance of specialist physicians has been linked to increased utilization of services, including potentially unnecessary 
      ones, having an adequate number of primary care physicians is crucial for delivering preventive and primary care. 
      Additionally, primary care providers play a vital role in referring patients to appropriate specialty care when necessary. 
      Thus, ensuring sufficient availability of primary care physicians is essential for facilitating timely and appropriate 
      healthcare services."
    } else if (input$Health_Access == "per_vaccinated"){
      "% Vaccinated: Percentage of fee-for-service (FFS) Medicare enrollees that had an annual flu vaccination.
      Influenza is a potentially severe illness that can result in hospitalization and death. 
      Each year, millions of people experience influenza infections, hundreds of thousands require hospitalization due to the flu, 
      and thousands lose their lives to the disease.
      The most effective method to prevent influenza and lower the chances of flu-related illness, hospitalization,
      and death is through an annual flu vaccine. It is recommended that individuals aged 6 months and older receive a seasonal 
      flu vaccine every year. Specifically, individuals over the age of 65 are strongly encouraged to get vaccinated as they face a 
      higher risk of developing severe complications from the flu."
    } else if (input$Health_Access == "per_with_annual_mammogram"){
      "% with Annual Mammogram: Percentage of female Medicare enrollees ages 65-74 that received an annual mammography screening.
      Research indicates that undergoing mammography screening can significantly reduce breast cancer mortality, particularly among older 
      women. The recommendation or referral from a physician, along with satisfaction with healthcare providers, plays a significant role 
      in encouraging breast cancer screening. Presently, women aged 45-54 are advised to undergo mammograms annually, while women aged 55 
      and older are recommended to have mammograms every two years."
    } else if (input$Health_Access == "preventable_hospitalization_rate"){
      "Preventable Hospitalization Rate: Rate of hospital stays for ambulatory-care sensitive conditions per 100,000 Medicare enrollees. 
      When people are hospitalized for conditions that could have been treated in outpatient settings, 
      it suggests that they did not have access to quality healthcare outside of hospitals. 
      This could also mean that they relied heavily on emergency rooms and urgent care centers instead of regular healthcare providers. 
      Preventable hospital stays can be seen as a measure of both the quality of care and the ability to access primary 
      healthcare services."
    } else if (input$Health_Access == "chlamydia_rate"){
      "Chlamydia_rate: Number of newly diagnosed chlamydia cases per 100,000 population.
      Unsafe sexual activity is linked to the incidence rates of chlamydia, which is the most prevalent bacterial sexually transmitted infection (STI) in North America. Chlamydia can lead to serious health consequences such as tubal infertility, ectopic pregnancy, pelvic inflammatory disease, and chronic pelvic pain. The impact of STIs, including chlamydia, extends beyond physical health and can result in increased morbidity and mortality, with higher risks of cervical cancer, infertility, and premature death. Moreover, the economic burden on society due to STIs is significant.
      Addressing chlamydia rates is crucial for promoting health equity, as underserved communities, particularly adolescent minority women, are disproportionately affected by this infection. Taking measures to prevent and treat chlamydia in these vulnerable populations is essential to ensure equal access to reproductive health care and improve overall health outcomes."  
    } else if (input$Health_Access == "per_uninsured_adults"){
      "% Uninsured Adults: Percentage of adults under age 65 without health insurance."
    } else if (input$Health_Access == "per_uninsured_children"){
      "% Uninsured Children: Percentage of children under age 19 without health insurance. 
      The absence of health insurance coverage poses a substantial obstacle to accessing necessary healthcare and maintaining 
      financial stability. being uninsured can have severe financial implications, with many individuals unable to afford 
      their medical expenses, resulting in the accumulation of medical debt. This issue is particularly notable among uninsured children, 
      who are less likely to receive timely preventive care, such as vaccinations and well-child visits. "
    } else if (input$Health_Access == "per_adults_with_diabetes"){
      "% Adults with Diabetes- Percentage of adults aged 20 and above with diagnosed diabetes (age-adjusted).
      Diabetes is a chronic condition known to have broad impacts on physical, social, and mental well-being, and causes significant 
      morbidity and mortality in the United States.The Centers for Disease Control and Prevention (CDC) reports that people with diabetes are at high
      risk of heart disease, stroke, and other complications such as kidney failure, blindness, and amputation of a foot, toe, or leg."
    } else {
      "nothing :)"
    } 
  }) 
  
  
  
  ## 3.3 Economic Stability ----
  temp_econ <- reactive({
    input$econ_stab
  })
  
  temp_econyear <- reactive({
    as.integer(input$yearSelect_econ)
  })
  
  output$econstability <- renderLeaflet({
    mapping2(temp_econ(), temp_econyear())
  })
  
  comparison_plot_econ_reactive <- reactive({
    county5 <- input$county5
    county6 <- input$county6
    
    if (temp_econ() == "per_unemployed") {
     comparison_plot_econ <- sdoh_line(va_avg, county5, county6, temp_econ())
      return(comparison_plot_econ)
    } else if (temp_econ() == "per_children_in_poverty"){
     comparison_plot_econ <- sdoh_line(va_avg, county5, county6, temp_econ())
      return(comparison_plot_econ)
    } else if (temp_econ() == "per_food_insecure"){
     comparison_plot_econ <- sdoh_line(va_avg, county5, county6, temp_econ())
      return(comparison_plot_econ)
    } else if (temp_econ() == "median_household_income"){
     comparison_plot_econ <- sdoh_line(va_avg, county5, county6, temp_econ())
      return(comparison_plot_econ)
    } else if (temp_econ() == "median_household_income_black"){
      comparison_plot_econ <- sdoh_line(va_avg, county5, county6, temp_econ())
      return(comparison_plot_econ)
    } else if (temp_econ() == "median_household_income_white"){
      comparison_plot_econ <- sdoh_line(va_avg, county5, county6, temp_econ())
      return(comparison_plot_econ)
    } else if (temp_econ() == "median_household_income_hispanic"){
      comparison_plot_econ <- sdoh_line(va_avg, county5, county6, temp_econ())
      return(comparison_plot_econ)
    } else {
      return(NULL)
    }
  })
  
  output$comparison_plot_econ <- renderPlotly({
    comparison_plot_econ_reactive()
  })
  

  
  output$EconomicStabilityVariableDefinition <- renderText({
    if (input$econ_stab == "per_unemployed") {
      "Unemployment rate: The unemployment rate reflects the economic and social conditions influencing an individual’s well-being. Employment provides economic stability. Unemployment, on the other hand, is a stressor that could worsen one’s health. It not only limits people’s access to quality healthcare but also is a burden on mental health as people tend to feel more depressed without a job."
    } else if (input$econ_stab == "per_children_in_poverty") {
      "% Children in poverty: This variable measures the percentage of people under the age of 18 in poverty. Children living in poverty have less access to health resources. Understanding this variable could tell us a lot more about the overall health and well-being of a community. It is an important indicator of socioeconomic disparities and can provide insights into the level of economic inequality and social support systems.
      A high percentage of children in poverty suggests that a significant portion of the younger population is living in disadvantaged conditions. These children may face challenges in accessing quality education, nutritious food, stable housing, and healthcare services. Living in poverty can have long-lasting effects on a child's physical and mental health, educational attainment, and overall development. "
    } else if (input$econ_stab == "per_food_insecure") {
      "% Food Insecure: Percentage of population who lack adequate access to food. Lack of consistent access to food is associated with adverse health consequences, including weight gain and premature mortality. This issue extends beyond simply having enough food and encompasses the ability of individuals and families to provide balanced meals, including fruits and vegetables. By considering the accessibility and affordability of nutritious food, we can address barriers to healthy eating and improve overall health outcomes.
      When examining food insecurity, it is essential to go beyond assessing whether individuals had a continuous food supply in the past year. This measure also takes into account the challenges individuals face in acquiring and preparing well-rounded meals that meet their nutritional needs. It recognizes that access to healthy food options, such as fresh fruits and vegetables, can be limited for individuals and families experiencing food insecurity."
    } else if (input$econ_stab == "median_household_income") {
      "Median Household Income: Income impacts health outcomes in many ways. It is one of the most important factors that affect other factors such as housing, education, and food.  Higher income provides individuals with greater access to healthcare services such as health insurance, medical treatments, and medication. Higher income also affects one’s eating behaviors. Having money to buy healthier food can decrease the risk of nutrition-related health conditions such as obesity, diabetes, and heart disease."
    } else if (input$econ_stab == "median_household_income_black") {
        "Median Household Income Black: " 
    } else if (input$econ_stab == "median_household_income_white") {
      "Median Household Income White: "  
    } else if (input$econ_stab == "median_household_income_hispanic") {
      "Median Household Income Hispanic: "   
    } else {
      "Please select a health variable."
    } 
  }) 
  ## 3.4 Health Behaviors-----
  temp_healthbehaviors <- reactive({
    input$health_behaviors
  })
  temp_behavioryear <- reactive({
    as.integer(input$yearSelect_healthbehaviors)
  })
  
  output$healthbehaviors <- renderLeaflet({
    mapping2(temp_healthbehaviors(), temp_behavioryear())
  })
  
  comparison_plot_behavior_reactive <- reactive({
    county7 <- input$county7
    county8 <- input$county8
    
    if (temp_healthbehaviors() == "per_adults_reporting_currently_smoking") {
      comparison_plot_behavior <- sdoh_line(va_avg, county7, county8, temp_healthbehaviors())
      return(comparison_plot_behavior)
    } else if (temp_healthbehaviors() == "per_excessive_drinking"){
      comparison_plot_behavior <- sdoh_line(va_avg, county7, county8, temp_healthbehaviors())
      return(comparison_plot_behavior)
    } else if (temp_healthbehaviors() == "per_driving_deaths_with_alcohol_involvement"){
      comparison_plot_behavior <- sdoh_line(va_avg, county7, county8, temp_healthbehaviors())
      return(comparison_plot_behavior)
    } else if (temp_healthbehaviors() == "per_physically_inactive"){
      comparison_plot_behavior <- sdoh_line(va_avg, county7, county8, temp_healthbehaviors())
      return(comparison_plot_behavior)
    } else if (temp_healthbehaviors() == "per_adults_with_obesity"){
      comparison_plot_behavior <- sdoh_line(va_avg, county7, county8, temp_healthbehaviors())
      return(comparison_plot_behavior)
    } else if (temp_healthbehaviors() == "teen_birth_rate"){
      comparison_plot_behavior <- sdoh_line(va_avg, county7, county8, temp_healthbehaviors())
      return(comparison_plot_behavior)
    } else {
      return(NULL)
    }
  })
  
  output$comparison_plot_behavior <- renderPlotly({
    comparison_plot_behavior_reactive()
  })
  output$HealthBehaviorsVariableDefinition <- renderText({
    if (input$health_behaviors == "per_adults_reporting_currently_smoking") {
      "% Adults Reporting Currently Smoking: Percentage of adults who are current smokers (age-adjusted).
      According to the County Health Rankings, every year, approximately 480,000 premature deaths are directly linked to smoking. Cigarette smoking is a known cause of several 
      cancers, cardiovascular disease, respiratory conditions, and adverse health outcomes, including low birthweight. 
      Monitoring the prevalence of tobacco use in the population is crucial as it serves as an indicator of potential health risks. 
      It helps communities identify the need for cessation programs and evaluate the effectiveness of existing tobacco control initiatives."
    } else if (input$health_behaviors == "per_excessive_drinking") {
      "% Excessive Drinking: Percentage of adults reporting binge or heavy drinking (age-adjusted).
      Nearly 1 in 6 American adults are considered binge drinkers. 
      Excessive alcohol consumption poses a significant risk for various adverse health outcomes. These include alcohol poisoning, 
      hypertension, acute myocardial infarction, sexually transmitted infections, unintended pregnancy, fetal alcohol syndrome, sudden 
      infant death syndrome, suicide, interpersonal violence, and motor vehicle crashes."
    } else if (input$health_behaviors == "per_driving_deaths_with_alcohol_involvement") {
      "% Driving Deaths with Alcohol Involvement: Percentage of driving deaths with alcohol involvement.
      This variable directly measures the relationship between alcohol and motor vehicle crash deaths. 
      Alcohol is a substance that reduces the function of the brain, impairing thinking, reasoning, and muscle coordination, 
      which are essential to operating a vehicle safely. In 2018, approximately 10,500 Americans were killed in alcohol-related motor 
      vehicle crashes. The annual cost of alcohol-related crashes totals more than $44 billion. Drivers between the ages of 21 and 24 
      cause 27% of all alcohol-impaired deaths."
    } else if (input$health_behaviors == "per_physically_inactive") {
      "% Physically Inactive: Percentage of adults age 18 and over reporting no leisure-time physical activity (age-adjusted).
      Physical inactivity is highly associated with increased risk of health conditions such as Type 2 diabetes, cancer, stroke, hypertension, cardiovascular disease, and shortened life expectancy. Physical activity is associated with improved sleep, cognitive ability, bone, and musculoskeletal health, and reduced risk of dementia."
    } else if (input$health_behaviors == "per_adults_with_obesity") {
      "% Adults with Obesity: This variable measures the percentage of the adult population (age 18 and older) that reports a body mass index (BMI) greater than or equal to 30 kg/m2 (age-adjusted).
      Adult obesity is a persistent condition that raises the likelihood of various health risks, including hypertension, heart disease, type 2 diabetes, respiratory issues, chronic inflammation, mental illness, and certain cancers.The development of obesity is influenced by a combination of environmental and individual factors. Environmental factors, such as the availability and affordability of nutrient-rich foods, the extent of fast-food advertising, and societal attitudes regarding weight stigma, can significantly impact the prevalence and risk of obesity."
    } else if (input$health_behaviors == "teen_birth_rate") {
      "Teen Birth Rate: Number of births per 1,000 female population ages 15-19.
      Teenage pregnancy has been linked to detrimental health outcomes for both the mother and child, with impacts extending to partners, 
      family members, and the wider community. The negative impacts of early childbearing on children and mothers can primarily be 
      attributed to social disadvantage and adversity. Adolescent mothers face obstacles in pursuing education beyond high school and 
      experience heightened mental and physical stress, along with a chronic lack of community support. Access to affordable, 
      high-quality childcare and suitable transportation can pose additional challenges, further limiting their educational and 
      employment opportunities."
    } else {
      "Please select a health variable."
    } 
  }) 
  ## 3.5 Neighborhood and Built Environment------
  temp_envr <- reactive({
    input$neighbor_envr
  })
  temp_envryear <- reactive({
    as.integer(input$yearSelect_envr)
  })
  
  output$envr <- renderLeaflet({
    mapping2(temp_envr(), temp_envryear())
  })
  
  comparison_plot_envr_reactive <- reactive({
    county9 <- input$county9
    county10 <- input$county10
    
    if (temp_envr() == "per_physical_distress") {
      comparison_plot_envr <- sdoh_line(va_avg, county9, county10, temp_envr())
      return(comparison_plot_envr)
    } else if (temp_envr() == "per_mental_distress"){
      comparison_plot_envr <- sdoh_line(va_avg, county9, county10, temp_envr())
      return(comparison_plot_envr)
    } else if (temp_envr() == "per_with_access_to_exercise_opportunities"){
      comparison_plot_envr <- sdoh_line(va_avg, county9, county10, temp_envr())
      return(comparison_plot_envr)
    } else if (temp_envr() == "suicide_rate"){
      comparison_plot_envr <- sdoh_line(va_avg, county9, county10, temp_envr())
      return(comparison_plot_envr)
    } else if (temp_envr() == "juvenile_arrests_rate"){
      comparison_plot_envr <- sdoh_line(va_avg, county9, county10, temp_envr())
      return(comparison_plot_envr)
    } else if (temp_envr() == "per_insufficient_sleep"){
      comparison_plot_envr <- sdoh_line(va_avg, county9, county10, temp_envr())
      return(comparison_plot_envr)
    } else if (temp_envr() == "per_severe_housing_problem"){
      comparison_plot_envr <- sdoh_line(va_avg, county9, county10, temp_envr())
      return(comparison_plot_envr)
    } else {
      return(NULL)
    }
  })
  
  output$comparison_plot_envr <- renderPlotly({
    comparison_plot_envr_reactive()
  })
  output$EnvrVariableDefinition <- renderText({
    if (input$neighbor_envr == "per_physical_distress") {
      "% Physical Distress: Percentage of adults reporting 14 or more days of poor physical health per month (age-adjusted).
      This variable offers valuable information on the overall well-being of adults in a community. 
      Physical health is important for disease prevention, mental health, energy levels, independence, social engagement, and longevity."
    } else if (input$neighbor_envr == "per_mental_distress") {
      "% Mental Distress: Percentage of adults reporting 14 or more days of poor mental health per month (age-adjusted).
      Mental health is a fundamental aspect of our overall well-being. It encompasses our emotional, psychological, and social well-being, and it affects how we think, feel, and act. Good mental health allows us to cope with the daily stresses of life, form positive relationships, make meaningful contributions to society, and navigate challenges effectively. Poor mental health can have detrimental effects on physical health, contributing to the development or exacerbation of various health conditions, including cardiovascular disease, weakened immune system, chronic pain, and digestive disorders."
    } else if (input$neighbor_envr == "per_access_to_exercise_opportunities") {
      "% Access to Exercise Opportunities: Percentage of the population with adequate access to locations for physical activity.
      Engaging in more physical activity has been linked to reduced risks of various health conditions, including type 2 diabetes, cancer, stroke, hypertension, cardiovascular disease, and premature mortality. The built environment plays a crucial role in promoting physical activity, as individuals residing in close proximity to amenities such as sidewalks, parks, and gyms are more inclined to engage in regular exercise."
    } else if (input$neighbor_envr == "suicide_rate") {
      "Suicide Rate: The number of deaths due to suicide per 100,000 population (age-adjusted)
      Suicide rates provide information on the mental health of a community. Suicide has an overwhelming effect on the mental health of surviving community members, family members, and friends."
    } else if(input$neighbor_envr == "per_limited_access_to_healthy_foods"){
      "% Limited Access to Healthy Foods: Percentage of population who are low-income and do not live close to a grocery store.
      Extensive evidence indicates a robust correlation between living in a food desert and experiencing a higher prevalence of obesity and premature death. Supermarkets have traditionally been known to offer healthier food choices compared to convenience stores or smaller grocery stores. Moreover, limited access to fresh fruits and vegetables is directly linked to premature mortality."
    } else if(input$neighbor_envr == "juvenile_arrests_rate"){  
      "Juvenile Arrests Rate: Rate of delinquency cases per 1,000 juveniles.
      Juvenile arrests are the result of many factors such as policing strategies, local laws, community and family support, and individual behaviors. Youth who are arrested and incarcerated experience lower self-reported health, higher rates of infectious disease and stress-related illnesses, and higher body mass indices."
    } else if(input$neighbor_envr == "per_insufficient_sleep"){
      "% Insufficient Sleep: Percentage of adults who report fewer than 7 hours of sleep on average (age-adjusted).
      Sleep plays a vital role in maintaining a healthy lifestyle, and insufficient sleep can have significant adverse effects on both personal health and the well-being of others. Persistent sleep deprivation has been associated with various chronic health conditions, including heart disease, kidney disease, high blood pressure, and stroke. It is also linked to psychiatric disorders such as depression and anxiety, as well as risky behavior and an increased risk of suicide. Recognizing the importance of adequate sleep is crucial for promoting overall well-being."
    } else if(input$neighbor_envr == "per_severe_housing_problem"){
      "% Severe Housing Problems: Percentage of households with at least 1 of 4 housing problems: overcrowding, high housing costs, lack of kitchen facilities, or lack of plumbing facilities.
      Life in the built environment, ought to be safe for a person to grow and develop in a healthy way. Inadequate housing can make negative contributions to health like infectious and chronic diseases, injuries, and poor child development. Households experiencing severe cost burdens are likely to face tradeoffs in meeting other basic needs which in turn can lead to mental/emotional strain."
      } else {
      "Please select a health variable."
    } 
  }) 
  ## 3.6 Demographics-----
  temp_demo <- reactive({
    input$demographics
  })
  temp_demoyear <- reactive({
    as.integer(input$yearSelect_demo)
  })
  
  output$demographicsvar <- renderLeaflet({
    mapping2( temp_demo(), temp_demoyear())
  })
  comparison_plot_demo_reactive <- reactive({
    county11 <- input$county11
    county12 <- input$county12
    
    if (temp_demo() == "per_less_than_18_years_of_age") {
      comparison_plot_demo <- sdoh_line(va_avg, county11, county12, temp_demo())
      return(comparison_plot_demo)
    } else if (temp_demo() == "per_65_and_over"){
      comparison_plot_demo <- sdoh_line(va_avg, county11, county12, temp_demo())
      return(comparison_plot_demo)
    } else if (temp_demo() == "per_hispanic"){
      comparison_plot_demo <- sdoh_line(va_avg, county11, county12, temp_demo())
      return(comparison_plot_demo)
    } else if (temp_demo() == "per_asian"){
      comparison_plot_demo <- sdoh_line(va_avg, county11, county12, temp_demo())
      return(comparison_plot_demo)
    } else if (temp_demo() == "per_nonhispanic_white"){
      comparison_plot_demo <- sdoh_line(va_avg, county11, county12, temp_demo())
      return(comparison_plot_demo)
    } else if (temp_demo() == "per_american_indian_or_alaska_native"){
      comparison_plot_demo <- sdoh_line(va_avg, county11, county12, temp_demo())
      return(comparison_plot_demo)
    } else if (temp_demo() == "per_black"){
      comparison_plot_demo <- sdoh_line(va_avg, county11, county12, temp_demo())
      return(comparison_plot_demo)
    } else if (temp_demo() == "per_not_proficient_in_english"){
      comparison_plot_demo <- sdoh_line(va_avg, county11, county12, temp_demo())
      return(comparison_plot_demo)
    } else {
      return(NULL)
    }
  })
  
  output$comparison_plot_demo <- renderPlotly({
    comparison_plot_demo_reactive()
  })
  output$DemographicsDefinition <- renderText({
    if (input$demographics == "per_less_than_18_years_of_age") {
      " % Less than 18 Years of Age: Percentage of population below 18 years of age.
      Measuring the percentage of the population in different age groups is crucial for healthcare planning, understanding population dynamics, economic projections, social policy development, and public safety considerations. It enables policymakers, researchers, and service providers to make informed decisions and develop targeted strategies that address the unique needs and challenges of specific age cohorts within a population. Policymakers can anticipate the demand for healthcare services, such as pediatric care, geriatric care, and specialized services for specific age-related conditions."
    } else if (input$demographics == "per_65_and_over") {
      "% 65 and Over:Percentage of the population ages 65 and older.
      Measuring the percentage of the population in different age groups is crucial for healthcare planning, understanding population dynamics, economic projections, social policy development, and public safety considerations. It enables policymakers, researchers, and service providers to make informed decisions and develop targeted strategies that address the unique needs and challenges of specific age cohorts within a population.  Policymakers can anticipate the demand for healthcare services, such as pediatric care, geriatric care, and specialized services for specific age-related conditions."
    } else if (input$demographics == "per_hispanic") {
      "% Hispanic: Percentage of the population self-identifying as Hispanic.
      Collecting data on ethnicity helps identify disparities and inequalities that may exist among different ethnic groups. By measuring and analyzing this information, policymakers and organizations can identify areas where certain ethnic groups may face discrimination, bias, or disadvantage. This data can guide the development of targeted policies and interventions aimed at reducing inequality and promoting equal opportunities for all ethnic groups."
    } else if (input$demographics == "per_asian") {
      "% Asian: Percentage of the population self-identifying as Asian.
      Collecting data on ethnicity helps identify disparities and inequalities that may exist among different ethnic groups. By measuring and analyzing this information, policymakers and organizations can identify areas where certain ethnic groups may face discrimination, bias, or disadvantage. This data can guide the development of targeted policies and interventions aimed at reducing inequality and promoting equal opportunities for all ethnic groups."
    } else if (input$demographics == "per_nonhispanic_white") {
      "% Nonhispanic White: Percentage of population self-identifying as non-Hispanic white.
      Collecting data on ethnicity helps identify disparities and inequalities that may exist among different ethnic groups. By measuring and analyzing this information, policymakers and organizations can identify areas where certain ethnic groups may face discrimination, bias, or disadvantage. This data can guide the development of targeted policies and interventions aimed at reducing inequality and promoting equal opportunities for all ethnic groups."
    } else if (input$demographics == "per_american_indian_or_alaska_native"){
      "% American Indian or Alaska Native: Percentage of population self-identifying as American Indian or Alaska Native.
      Collecting data on ethnicity helps identify disparities and inequalities that may exist among different ethnic groups. By measuring and analyzing this information, policymakers and organizations can identify areas where certain ethnic groups may face discrimination, bias, or disadvantage. This data can guide the development of targeted policies and interventions aimed at reducing inequality and promoting equal opportunities for all ethnic groups."
    } else if (input$demographics == "per_black") {
      "% Black: Percentage of the population self-identifying as non-Hispanic Black or African American.
    Collecting data on ethnicity helps identify disparities and inequalities that may exist among different ethnic groups. By measuring and analyzing this information, policymakers and organizations can identify areas where certain ethnic groups may face discrimination, bias, or disadvantage. This data can guide the development of targeted policies and interventions aimed at reducing inequality and promoting equal opportunities for all ethnic groups."
    } else if (input$demographics == "per_not_proficient_in_english") {
      "% Not Proficient in English: Percentage of the population aged 5 and over who reported speaking English less than 'well'. Understanding the number of individuals who are not proficient in English helps identify the language needs of a population. It allows policymakers, educators, and service providers to develop appropriate strategies and resources to ensure effective communication and equal access to essential services, such as healthcare, education, legal services, employment, and government programs. By recognizing language diversity and addressing language barriers, societies can promote inclusivity and equitable access to opportunities and resources.
      "
    } else {
      "Please select a health variable."
    } 
  }) 
 ## 3.7 server territory maps----
  observe({
    territory_type <- input$territory_type
    zscore_type <- input$zscore_type
    map <- territory(territory_type, zscore_type)
    
    output$map <- renderLeaflet({
      map
    })
  })
  
  output$territorydescription <- renderText({
  if (input$territory_type == "base" & input$zscore_type == "food") {
    "The map shows the
        ideal territories assigned to FCS agents. These territories have been
        determined using data from food insecurity z-scores to optimize
        their effectiveness. 
    
    These territories have also been created in regard
        to the model’s constraints on population, commute time, and VCE districts.
        Each color on the map corresponds to a distinct agent's territory. By
        hovering your cursor over a county, you can easily identify the agent
        responsible for serving that particular area. Furthermore, you can hover
        over the cloud icons to access relevant information regarding the FCS
        agent's contact details and home office."
  } else if (input$territory_type == "base" & input$zscore_type == "obese") {
    "The map shows the
        ideal territories assigned to FCS agents. These territories have been
        determined using data from obesity insecurity z-scores to optimize
        their effectiveness. These territories have also been created in regard
        to the model’s constraints on population, commute time, and VCE districts.
        Each color on the map corresponds to a distinct agent's territory. By
        hovering your cursor over a county, you can easily identify the agent
        responsible for serving that particular area. Furthermore, you can hover
        over the cloud icons to access relevant information regarding the FCS
        agent's contact details and home office."
  } else if (input$territory_type == "base" && input$zscore_type == "inactivity") {
    "The map shows the
        ideal territories assigned to FCS agents. These territories have been
        determined using data from physical inactivity insecurity z-scores to optimize
        their effectiveness. These territories have also been created in regard
        to the model’s constraints on population, commute time, and VCE districts.
        Each color on the map corresponds to a distinct agent's territory. By
        hovering your cursor over a county, you can easily identify the agent
        responsible for serving that particular area. Furthermore, you can hover
        over the cloud icons to access relevant information regarding the FCS
        agent's contact details and home office. "
  } else if (input$territory_type == "base" & input$zscore_type == "aggregate") {
    "The map shows the
        ideal territories assigned to FCS agents. These territories have been
        determined using data from aggregate z-scores to optimize
        their effectiveness. These territories have also been created in regard
        to the model’s constraints on population, commute time, and VCE districts.
        Each color on the map corresponds to a distinct agent's territory. By
        hovering your cursor over a county, you can easily identify the agent
        responsible for serving that particular area. Furthermore, you can hover
        over the cloud icons to access relevant information regarding the FCS
        agent's contact details and home office. "
  } else if (input$territory_type == "base" & input$zscore_type == "lowbirth") {
    "The map shows the
        ideal territories assigned to FCS agents. These territories have been
        determined using data from low birthweight insecurity z-scores to optimize
        their effectiveness. These territories have also been created in regard
        to the model’s constraints on population, commute time, and VCE districts.
        Each color on the map corresponds to a distinct agent's territory. By
        hovering your cursor over a county, you can easily identify the agent
        responsible for serving that particular area. Furthermore, you can hover
        over the cloud icons to access relevant information regarding the FCS
        agent's contact details and home office. "
  } else if (input$territory_type == "base" & input$zscore_type == "diabetes") {
    "The map shows the
        ideal territories assigned to FCS agents. These territories have been
        determined using data from diabetes insecurity z-scores to optimize
        their effectiveness. These territories have also been created in regard
        to the model’s constraints on population, commute time, and VCE districts.
        Each color on the map corresponds to a distinct agent's territory. By
        hovering your cursor over a county, you can easily identify the agent
        responsible for serving that particular area. Furthermore, you can hover
        over the cloud icons to access relevant information regarding the FCS
        agent's contact details and home office. "
  } else if (input$territory_type == "one" & input$zscore_type == "food") {
    "The map shows the
        ideal territories assigned to FCS agents. These territories have been
        determined using data from food insecurity z-scores to optimize
        their effectiveness. These territories have also been created in regard to
        the model’s constraints on population, commute time, and VCE districts.
        Each color on the map corresponds to a distinct agent's territory. By hovering
        your cursor over a county, you can easily identify the agent responsible for
        serving that particular area.

        Furthermore, you can hover over the cloud icons to access relevant information
        about the FCS agent's contact details and home office. The blue cloud icons
        signify existing agents, while the red cloud icons indicate optimal new agent
        sites. The map now includes a newly designated agent location in Prince William County
        ."
  } else if (input$territory_type == "one" & input$zscore_type == "obese") {
    "The map shows the
        ideal territories assigned to FCS agents. These territories have been
        determined using data from obesity insecurity z-scores to optimize
        their effectiveness. These territories have also been created in regard to
        the model’s constraints on population, commute time, and VCE districts.
        Each color on the map corresponds to a distinct agent's territory. By hovering
        your cursor over a county, you can easily identify the agent responsible for
        serving that particular area.

        Furthermore, you can hover over the cloud icons to access relevant information
        about the FCS agent's contact details and home office. The blue cloud icons
        signify existing agents, while the red cloud icons indicate optimal new agent
        sites. The map now includes a newly designated agent location in Frederick County
        .Feel free to explore different choices in the Agents/Health dropdowns to
        generate a new map!"
  } else if (input$territory_type == "one" & input$zscore_type == "inactivity") {
    "The map shows the
        ideal territories assigned to FCS agents. These territories have been
        determined using data from physical inactivity z-scores to optimize
        their effectiveness. These territories have also been created in regard to
        the model’s constraints on population, commute time, and VCE districts.
        Each color on the map corresponds to a distinct agent's territory. By hovering
        your cursor over a county, you can easily identify the agent responsible for
        serving that particular area.

        Furthermore, you can hover over the cloud icons to access relevant information
        about the FCS agent's contact details and home office. The blue cloud icons
        signify existing agents, while the red cloud icons indicate optimal new agent
        sites. The map now includes a newly designated agent location in Augusta County
        .Feel free to explore different choices in the Agents/Health dropdowns to
        generate a new map!"
  } else if (input$territory_type == "one" & input$zscore_type == "aggregate") {
    "The map shows the
        ideal territories assigned to FCS agents. These territories have been
        determined using data from aggregate z-scores to optimize
        their effectiveness. These territories have also been created in regard to
        the model’s constraints on population, commute time, and VCE districts.
        Each color on the map corresponds to a distinct agent's territory. By hovering
        your cursor over a county, you can easily identify the agent responsible for
        serving that particular area.

        Furthermore, you can hover over the cloud icons to access relevant information
        about the FCS agent's contact details and home office. The blue cloud icons
        signify existing agents, while the red cloud icons indicate optimal new agent
        sites. The map now includes a newly designated agent location in Frederick County
        .Feel free to explore different choices in the Agents/Health dropdowns to
        generate a new map!"
  } else if (input$territory_type == "one" & input$zscore_type == "lowbirth") {
    "The map shows the
        ideal territories assigned to FCS agents. These territories have been
        determined using data from low birthweight z-scores to optimize
        their effectiveness. These territories have also been created in regard to
        the model’s constraints on population, commute time, and VCE districts.
        Each color on the map corresponds to a distinct agent's territory. By hovering
        your cursor over a county, you can easily identify the agent responsible for
        serving that particular area.

        Furthermore, you can hover over the cloud icons to access relevant information
        about the FCS agent's contact details and home office. The blue cloud icons
        signify existing agents, while the red cloud icons indicate optimal new agent
        sites. The map now includes a newly designated agent location in Frederick County
        .Feel free to explore different choices in the Agents/Health dropdowns to
        generate a new map!."
  } else if (input$territory_type == "one" & input$zscore_type == "diabetes") {
    "The map shows the
        ideal territories assigned to FCS agents. These territories have been
        determined using data from diabetes z-scores to optimize
        their effectiveness. These territories have also been created in regard to
        the model’s constraints on population, commute time, and VCE districts.
        Each color on the map corresponds to a distinct agent's territory. By hovering
        your cursor over a county, you can easily identify the agent responsible for
        serving that particular area.

        Furthermore, you can hover over the cloud icons to access relevant information
        about the FCS agent's contact details and home office. The blue cloud icons
        signify existing agents, while the red cloud icons indicate optimal new agent
        sites. The map now includes a newly designated agent location in Frederick County
        .Feel free to explore different choices in the Agents/Health dropdowns to
        generate a new map!"
  } else if (input$territory_type == "two" & input$zscore_type == "food") {
    "The map shows the
        ideal territories assigned to FCS agents. These territories have been
        determined using data from food insecurity z-scores to optimize
        their effectiveness. These territories have also been created in regard to
        the model’s constraints on population, commute time, and VCE districts.
        Each color on the map corresponds to a distinct agent's territory. By hovering
        your cursor over a county, you can easily identify the agent responsible for
        serving that particular area.

        Furthermore, you can hover over the cloud icons to access relevant information
        about the FCS agent's contact details and home office. The blue cloud icons
        signify existing agents, while the red cloud icons indicate optimal new agent
        sites. The map now includes newly designated agent locations in Prince William County
        and Essex County .Feel free to explore different choices in the Agents/Health dropdowns to
        generate a new map!"
  } else if (input$territory_type == "two" & input$zscore_type == "obese") {
    "The map shows the
        ideal territories assigned to FCS agents. These territories have been
        determined using data from obesity z-scores to optimize
        their effectiveness. These territories have also been created in regard to
        the model’s constraints on population, commute time, and VCE districts.
        Each color on the map corresponds to a distinct agent's territory. By hovering
        your cursor over a county, you can easily identify the agent responsible for
        serving that particular area.

        Furthermore, you can hover over the cloud icons to access relevant information
        about the FCS agent's contact details and home office. The blue cloud icons
        signify existing agents, while the red cloud icons indicate optimal new agent
        sites. The map now includes newly designated agent locations in Frederick County
        and Augusta County.Feel free to explore different choices in the Agents/Health dropdowns to
        generate a new map!"
  } else if (input$territory_type == "two" & input$zscore_type == "inactivity") {
    "The map shows the
        ideal territories assigned to FCS agents. These territories have been
        determined using data from physical inactivity z-scores to optimize
        their effectiveness. These territories have also been created in regard to
        the model’s constraints on population, commute time, and VCE districts.
        Each color on the map corresponds to a distinct agent's territory. By hovering
        your cursor over a county, you can easily identify the agent responsible for
        serving that particular area.

        Furthermore, you can hover over the cloud icons to access relevant information
        about the FCS agent's contact details and home office. The blue cloud icons
        signify existing agents, while the red cloud icons indicate optimal new agent
        sites. The map now includes newly designated agent locations in Frederick County
        and Augusta County.Feel free to explore different choices in the Agents/Health dropdowns to
        generate a new map!"
  } else if (input$territory_type == "two" & input$zscore_type == "aggregate") {
    "The map shows the
        ideal territories assigned to FCS agents. These territories have been
        determined using data from aggregate z-scores to optimize
        their effectiveness. These territories have also been created in regard to
        the model’s constraints on population, commute time, and VCE districts.
        Each color on the map corresponds to a distinct agent's territory. By hovering
        your cursor over a county, you can easily identify the agent responsible for
        serving that particular area.

        Furthermore, you can hover over the cloud icons to access relevant information
        about the FCS agent's contact details and home office. The blue cloud icons
        signify existing agents, while the red cloud icons indicate optimal new agent
        sites. The map now includes newly designated agent locations in Frederick County
        and Augusta County.Feel free to explore different choices in the Agents/Health dropdowns to
        generate a new map!"
  } else if (input$territory_type == "two" & input$zscore_type == "lowbirth") {
    "The map shows the
        ideal territories assigned to FCS agents. These territories have been
        determined using data from low birthweight z-scores to optimize
        their effectiveness. These territories have also been created in regard to
        the model’s constraints on population, commute time, and VCE districts.
        Each color on the map corresponds to a distinct agent's territory. By hovering
        your cursor over a county, you can easily identify the agent responsible for
        serving that particular area.

        Furthermore, you can hover over the cloud icons to access relevant information
        about the FCS agent's contact details and home office. The blue cloud icons
        signify existing agents, while the red cloud icons indicate optimal new agent
        sites. The map now includes newly designated agent locations in Frederick County
        and Augusta County.Feel free to explore different choices in the Agents/Health dropdowns to
        generate a new map!"
  } else if (input$territory_type == "two" & input$zscore_type == "diabetes") {
    "The map shows the
        ideal territories assigned to FCS agents. These territories have been
        determined using data from diabetes z-scores to optimize
        their effectiveness. These territories have also been created in regard to
        the model’s constraints on population, commute time, and VCE districts.
        Each color on the map corresponds to a distinct agent's territory. By hovering
        your cursor over a county, you can easily identify the agent responsible for
        serving that particular area.

        Furthermore, you can hover over the cloud icons to access relevant information
        about the FCS agent's contact details and home office. The blue cloud icons
        signify existing agents, while the red cloud icons indicate optimal new agent
        sites. The map now includes newly designated agent locations in Frederick County
        and Augusta County."
  } else {
    "This map shows territories for no new agents."
  }
  })
  #snaped territory server-----
  observe({
    territory_type_snaped <- input$territory_type_snaped
    zscore_type_snaped <- input$zscore_type_snaped
    map_snaped <- snap_territory(territory_type_snaped, zscore_type_snaped)

    output$map_snaped <- renderLeaflet({
      map_snaped
    })
  })
 # nonsnaped terr server-----
  observe({
    territory_type_non_snaped <- input$territory_type_non_snaped
    zscore_type_non_snaped <- input$zscore_type_non_snaped
    map_non_snaped <- fcs_territory(territory_type_non_snaped, zscore_type_non_snaped)

    output$map_non_snaped <- renderLeaflet({
      map_non_snaped
    })
  })

  
}

# 4. Run the application-------------------------------------------------------------------
shinyApp(ui = ui, server = server)
