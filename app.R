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
library(tidycensus)
library(tidyverse)
library(viridis)
library(readxl)
library(sf) 
options(scipen=999)


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
                             NAMELSAD = str_to_title(NAMELSAD) )
    
  # Load and preprocess the health rankings data
  all_var_df <- read.csv("./data/final_variables.csv") %>%
    transform(GEOID = as.integer(FIPS),
              Value = as.numeric(Value))
  
  # Load and preprocess the VCE agents location data
  agents_sf <- read.csv("./data/vce_agents.csv") %>% st_as_sf(  coords = c("Long", "Lat"), remove = FALSE, crs = 4326, agr = "constant")
  
  # Prepare labels for varibels of interest
  good_names <- c("Percent Low Birthweight", "Percent of Adults Reporting Currently Smoking","Percent Population with Access to Exercise Opportunities", "Percent Excessive Drinking",
                  "Percent Driving Deaths with Alcohol Involvement", "Dentist Ratio", "Mental Health Provider Ratio", "Teen Birth Rate","Percent Unemployed", "Percent Children in Poverty", "Chlamydia Rate", "Percent Uninsured","Primary Care Physicians Ratio", "Preventable Hospitalization Rate", "Percent With Annual Mammogram",
                  "Percent Vaccinated", "Life Expectancy", "Life Expectancy Black", "Life Expectancy White",
                  "Life Expectancy Gap", "Percent of Uninsured Adults", "Percent Uninsured Children", "Other Primary Care Provider Ratio","Drug Mortality Rate", "Percent of Adults With Obesity", "Percent Physically Inactive", "Percent of Adults with Diabetes", "HIV Prevalence Rate","Percent Food Insecure", "Percent Physical Distress", "Percent Physical Distress", "Percent Mental Distress", "Percent Severe Housing Problems", "Percent Insufficient Sleep","Suicide Rate", "Percent Access to Exercise Opportunities","Percent Limited Access to Healthy Foods", "Juvenile Arrests Rate","Percent less than 18 years of age", "Percent 65 and over", "Percent Black", "Percent American Indian or Alaska Native", "Percent Asian","Percent Hispanic","Percent Nonhispanic-White","Percent not Proficient in English","Percent Household Income Required for Child Care Expenses","Gender Pay Gap","Median Household Income Black", "Median Household Income White","Median Household Income Hispanic","Median Household Income Gap White Black","Median Household Income Gap White Hispanic", "Median Household Income")
  
  ### 1.3.2 Process the data---------------------------------------------------------------- 
  
  
  
  ### 1.3.3 Merging the data(Geo)---------------------------------------------------------------- 



# ## 1.4 Define your functions -------------------------------------------------------
# # Function for health outcomes
mapping2 <- function(variable, year) {
  
  # Filter data for selected year and variable
  temp <- all_var_df[all_var_df$Year == year & all_var_df$Variable == variable, ]
  
  # Join variable data with county geometry data
  var.counties <- left_join(va.counties, temp, by = 'GEOID')
  
  # Identify the index of the selected variable
  idx <- which(unique(all_var_df$Variable) == variable)
  
  # Create a color palette function based on the "Value" column
  pal <- colorNumeric(palette = "viridis", domain = var.counties$Value)
  
  # Create labels for counties
  county_labels <- sprintf(
    "<strong>%s</strong><br/>%s: %g", 
    var.counties$NAMELSAD, 
    good_names[idx], 
    var.counties$Value
  ) %>% lapply(htmltools::HTML)
  
  # Create labels for agents
  agent_labels <- sprintf(
    "<strong>Agent Site</strong><br/>Job Department:<br/> Agent Name:<br/> Contact Info: %s", 
    agents_sf$Job.Dept,
    agents_sf$Employee.Name,
    agents_sf$VT.Email
  ) %>% lapply(htmltools::HTML)
  
  # Wrap legend title if too long
  spaces <- gregexpr("\\s", good_names[idx])[[1]]
  middle_space <- spaces[length(spaces) %/% 2 + 1]
  legend_title <- paste0(substring(good_names[idx], 1, middle_space-1), "</br>", substring(good_names[idx], middle_space+1))
  
  # Create title for the map
  map_title = paste("VCE FCS Agent Sites and",good_names[idx], year, sep= " ")
  
  # Create leaflet map
  leaflet(data = var.counties) %>%
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
    addAwesomeMarkers(data = agents_sf, icon=awesomeIcons(icon='cloud', markerColor = 'red', iconColor = 'white'),
                      label = agent_labels, 
                      labelOptions = labelOptions(noHide = FALSE, direction = "auto", offset=c(0,-10))) %>%
    addLegend(pal = pal, values = ~Value, title = legend_title, position = "bottomright") %>%
    setView(lng = -78.6568942, lat = 38.2315734, zoom = 7) %>% 
    addControl(htmltools::HTML(paste0("<h3 style='margin:3px'>", map_title, "</h2>")), position = "topright", data = NULL)
}
  #territory function
territory <- function(territory_type, Zscore_Type, variable_title) {
    
    # Filter data for selected year and variable
    temp2 <- all_territories[all_territories$Territory_Type == territory_type & all_territories$Zscore_Type == variable, ]
    # 
    # # Join variable data with county geometry data
    # var.counties <- left_join(va.counties, temp, by = 'GEOID')
    # 
    # # Identify the index of the selected variable
    # idx <- which(unique(all_var_df$Variable) == variable)
    
    # Create a color palette function based on the "Value" column
    pal <- colorNumeric(palette = "viridis", domain = var.counties$Value)
    
    # Create labels for counties
    county_labels <- sprintf(
      "<strong>%s</strong><br/>%s: %g", 
      all_territories$Agen, 
      good_names[idx], 
      var.counties$Value
    ) %>% lapply(htmltools::HTML)
    
    # Create labels for agents
    agent_labels <- sprintf(
      "<strong>Agent Site</strong><br/>Job Department:</strong><br/>Agent Contact Info: %s", 
      all_territories$`Job Dept`,
      all_territories$`Employee Name`,
      all_territories$`VT Email`
    ) %>% lapply(htmltools::HTML)
    
    # Wrap legend title if too long
    # spaces <- gregexpr("\\s", good_names[idx])[[1]]
    # middle_space <- spaces[length(spaces) %/% 2 + 1]
    # legend_title <- paste0(substring(good_names[idx], 1, middle_space-1), "</br>", substring(good_names[idx], middle_space+1))
    # 
    # Create title for the map
    territory_title = paste("New VCE FCS Agent Territories based on",variable_title, "Z-scores")
    
    # Create leaflet map
    leaflet(data = all_territories) %>%
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
      addAwesomeMarkers(data = with_new_agents, icon=awesomeIcons(icon='cloud', markerColor = with_new_agents$NewAgent, iconColor = 'white'),
                        label = agent_labels, 
                        labelOptions = labelOptions(noHide = FALSE, direction = "auto", offset=c(0,-10))) %>%
      addLegend(pal = pal, values = ~Value, title = legend_title, position = "bottomright") %>%
      setView(lng = -78.6568942, lat = 38.2315734, zoom = 7) %>% 
      addControl(htmltools::HTML(paste0("<h3 style='margin:3px'>", territory_title, "</h2>")), position = "topright", data = NULL)
  }  

## 1.5 Statistic analysis---------
  a <- "Statistics for per_low_birthweight"
  b <- "Statistics for second varible"

# 2. Define UI for application ------------------------------------------------------------
ui <- navbarPage(#title = "DSPG 2023",
                 selected = "overview",
                 theme = shinytheme("lumen"),
                 tags$head(tags$style('.selectize-dropdown {z-index: 10000}')), 
                 useShinyjs(),
                 
                 ## 2.1 Tab Overview--------------------------------------------
                 tabPanel("Overview", value = "overview",
                          fluidRow(style = "margin: 2px;",
                                   align = "center",
                                   h1(strong("VCE: Optimizing Extension Agent Services"),
                                      h4("Data Science for the Public Good Program"),
                                      h4("Virginia Tech"),
                                      br()
                                   )
                          ),
                          fluidRow(style = "margin: 6px;",
                                   align = "justify",
                                   column(4,
                                          h2(strong("Project Background")),
                                          p(strong("Virginia Coorperative Extensions:"), "Virginia Cooperative Extension (VCE) was established in 1914 and has since been committed to bringing the resources of Virginia Tech and Virginia State University to the people of the Commonwealth. VCE has"),
                                          tags$li("107 offices"),
                                          tags$li("11 Agriculture Research Extension centers"), 
                                          tags$li("6 4-H centers throughout the state"),
                                          p("VCE agents and volunteers strive to empower youth and Virginian farmers, guide sustainable resource management, and promote public health. VCE accomplishes these goals through programs that put research-based knowledge to work in people’s lives. VCE has a variety of programs like 4-H Youth Development, Family and Consumer Sciences, Community Viability, Agriculture and Natural Resources, Food, Nutrition, and Health, etc. in every county. VCE works on unique challenges Virginians face in partnership with governments and organizations to solve these issues in a way that benefits all people. With the expertise and knowledge from Virginia Tech and Virginia State University, VCE agents are able to tackle issues and foster community growth across the state. "),
                  
                                          p("For the purpose of this project, we will be focusing on VCE’s Family and Consumer Sciences Program and the agents that support this program. FCS programming is tied to community needs and directed toward families and individuals. Many counties’ FCS programs look different from one another, however, there are core specialty areas every program has. The specialty areas include: Nutrition/Wellness, Family Financial Education, and Family and Human Development. FCS agents are responsible for partnering and collaborating with other VCE agents, agencies, nonprofits/ other organizations, and the public to meet the educational needs of local residents. Agents are tasked with determining program goals and needs by monitoring trends and issues. FCS agents essentially help Virginian families make more healthy and smart decisions by applying research-based knowledge to work in people’s lives. However, this is easier said than done. A big reason why every county’s FCS programs look different is because of the unique populations and challenges every county has. This unfortunately creates a difficult job for FCS agents. They are overextended and commit a lot more time and effort than what seems to fit into the 3 FCS specialty areas. Today, FCS agents are doing a lot more than what was originally expected of them as VCE extends their work to be more public health focused."),
                                  
                                   ),
                                   column(4,
                                          h2(strong("Our Work")),
                                          p("Our team seeks to design an interactive dashboard that will aid VCE FCS agents in identifying which areas of Virginia are in need of more support. This dashboard will help our stakeholders gain a better understanding of the needs of the community, as well as their current health demographics. Agents will be able to put our research into practice by using our dashboard to identify specific areas of need for every county in Virginia. We hope that this resource will support VCE FCS agents in improving the overall health of Virginia communities."),
                                          p("We will utilize publicly accessible data, including Virginia health rankings, to offer our stakeholders a comprehensive comprehension of the factors influencing the health of families in Virginia. Our primary focus will be on various variables that align with the five determinants of health as well as health outcomes. Additionally, we will map these variables alongside the existing data on Virginia Cooperative Extension (VCE) agents. These maps will aid in identifying areas where VCE agents can provide further support to their communities."),

                                          p("The resulting analysis and findings will be presented in an interactive dashboard, which will serve as a valuable resource for the Virginia Cooperative Extension. Through this dashboard, VCE agents will be equipped with the necessary information to enhance their understanding of community health determinants and identify areas where their expertise can have a meaningful impact."),
                          
                                          p("This dashboard compiles our findings and allows stakeholders and other users to explore the information interactively."),
                                   ),
                                   column(4,
                                          h2(strong("Agents Opitmization")),
                                          p(strong("Overview")),
                                          p("Our goal is to try to optimize FCS agent efforts by determining optimal territories for these agents to cover. Since not all counties have FCS agents (there are well over 100 cities and counties in Virginia but only 34 FCS agents), we want to determine how FCS agents can allocate their efforts across space so that we do not have some agents serving one well-off county while other agents serve several counties, many of which may be inaccessible in the sense that they take several hours to reach by car. In addition to spatially optimizing existing agents, we also want to identify the locations where new agents could have the largest impact."),
                                          p(strong("Workflow:")),
                                          tags$li("Identify where FCS Agents are"),
                                          tags$li("Identify health outcomes that FCS agents can affect"),
                                          tags$li("Create a health index: z-score aggregation"),
                                          tags$li("Determining accessibility definition"),
                                          tags$li("Solving mathematical programs"),
                                          tags$li("Mapping optimized territories"),
                                          p(""),
                                        
                                          br(),
                                          img(src='vce.jpg'),
                
                                   )
                          ),
                          fluidRow(align = "center",
                                   p(tags$small(em('Last updated: August 2023')))
                          ) 
                 ),
                 
                 ## 2.2 Tab Health variables --------------------------------------------
                 navbarMenu("Health Variables" ,
                            ### 2.2.0 Subtab Virginia Public Health Overview
                            tabPanel("Public Health Overview",
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Public Health in Virginia"), align = "center"),
                                              p("", style = "padding-top:10px;")),
                                     fluidRow(style = "margin: 6px;",
                                              align = "justify",
                                     )
                            ),
                         

                            ### 2.2.1 Subtab Health Outcomes--------------------------------------
                            tabPanel("Health Outcomes",
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Health Outcomes"), align = "center"),
                                              p("", style = "padding-top:10px;")),
                                     fluidRow(style = "margin: 6px;",
                                              align = "justify",
                                              column(3,
                                                     h4(strong("Summary Statistics")),
                                                     textOutput("VariableDefinition"),

                                              ) ,
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
                                                                  selected = "2020", inline = TRUE , width = "50%"),
                                                     withSpinner(leafletOutput("outcomes", height = "500px")),

                                              )
                                     )
                            ),
                            ### 2.2.2 Subtab Healthcare Access and Quality--------------------------------------
                            tabPanel("Healthcare Access and Quality", 
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Healthcare Access and Quality Variables"), align = "center"),
                                              p("", style = "padding-top:10px;")),
                                     fluidRow(style = "margin: 6px;",
                                              align = "justify",
                                              column(3, 
                                                    
                                                     h4(strong("Summary Statistics")),
                                                     textOutput("HealthAccessVariableDefinition")
                                                    
                                              ) ,
                                              column(9, 
                                                       selectInput("Health_Access", "Select Variable:", width = "50%", choices = c(
                                                       "Percent Uninsured" = "per_uninsured",
                                                       "Aults without Insurance" = "per_uninsured_adults",
                                                       "Children without Insurance" = "per_uninsured_children",
                                                       "Dentist Ratio" = "dentist_ratio",
                                                       "Mental Health Provider Ratio" = "mental_health_provider_ratio",
                                                       "Primary Care Physicians Ratio" = "primary_care_physicians_ratio",
                                                       "Other Primary Care Provider Ratio" = "other_primary_care_provider_ratio",
                                                       "Vaccination Rate" = "per_vaccinated",
                                                       "Preventable Hospitalization Rate" = "preventable_hospitalization_rate",
                                                       "Annual Mammograms" = "per_with_annual_mammogram"
                                                       )
                                                     ),
                                                     radioButtons(inputId = "yearSelect_access", label = "Select Year: ", 
                                                                  choices = c("2016","2017", "2018", "2019", "2020"), 
                                                                  selected = "2020", inline = TRUE),
                                                     withSpinner(leafletOutput("healthaccess", height = "500px")),
                                                    )
                                              )
                                     
                            ), 
                            ### 2.2.3 Subtab Economic Stability--------------------------------------
                            tabPanel("Economic Stability", 
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Economic Stability Variables"), align = "center"),
                                              p("", style = "padding-top:10px;")),
                                     fluidRow(style = "margin: 6px;",
                                              align = "justify",
                                              column(3, 
                                                     
                                                     h4(strong("Summary Statistics")),
                                                     textOutput("EconomicStabilityVariableDefinition")
                                                     
                                              ) ,
                                              column(9, 
                                                     selectInput("econ_stab", "Select Variable:", width = "50%", choices = c(
                                                       "Unemployment Rate" = "per_unemployed",
                                                       "Children in Poverty" = "per_children_in_poverty",
                                                       "Food Insecurity" = "per_food_insecure",
                                                       "Median Household Income" = "median_household_income"
                                                     )
                                                     ),
                                                     radioButtons(inputId = "yearSelect_econ", label = "Select Year: ", 
                                                                  choices = c("2016","2017", "2018", "2019", "2020"), 
                                                                  selected = "2020", inline = TRUE),
                                                     withSpinner(leafletOutput("econstability", height = "500px")),
                                              )
                                     
                            
                                     )
                            ),
                            ### 2.2.4 Subatb Health Behaviors-------
                            tabPanel("Health Behaviors", 
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Health Behaviors"), align = "center"),
                                              p("", style = "padding-top:10px;")),
                                     fluidRow(style = "margin: 6px;",
                                              align = "justify",
                                              column(3, 
                                                     
                                                     h4(strong("Summary Statistics")),
                                                     textOutput("HealthBehaviorsVariableDefinition")
                                                     
                                              ) ,
                                              column(9, 
                                                     selectInput("health_behaviors", "Select Variable:", width = "50%", choices = c(
                                                       "Smoking Rate" = "per_adults_reporting_currently_smoking",
                                                       "Excessive Drinking" = "per_excessive_drinking",
                                                       "Driving Deaths Involving Alcohol" = "per_driving_deaths_with_alcohol_involvement",
                                                       "Physical Inactivity" = "per_physically_inactive"
                                                     )
                                                     ),
                                                     radioButtons(inputId = "yearSelect_healthbehaviors", label = "Select Year: ", 
                                                                  choices = c("2016","2017", "2018", "2019", "2020"), 
                                                                  selected = "2020", inline = TRUE),
                                                     withSpinner(leafletOutput("healthbehaviors", height = "500px")),
                                              )
                                              
                                     )
                            ),
                            ### 2.2.5 Subtab Neighborhood and Built Envr------
                            tabPanel("Neighborhood and Built Environment", 
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Neighborhood and Built Environment"), align = "center"),
                                              p("", style = "padding-top:10px;")),
                                     fluidRow(style = "margin: 6px;",
                                              align = "justify",
                                              column(3, 
                                                     
                                                     h4(strong("Summary Statistics")),
                                                     textOutput("EnvrVariableDefinition")
                                                     
                                              ) ,
                                              column(9, 
                                                     selectInput("neighbor_envr", "Select Variable:", width = "50%", choices = c(
                                                       "Physical Distress" = "per_physical_distress",
                                                       "Mental Distress" = "per_mental_distress",
                                                       "Access to Exercise Opportunity" = "per_access_to_exercise_opportunities",
                                                       "Suicide Rate" = "suicide_rate"
                                                     )
                                                     ),
                                                     radioButtons(inputId = "yearSelect_envr", label = "Select Year: ", 
                                                                  choices = c("2016","2017", "2018", "2019", "2020"), 
                                                                  selected = "2020", inline = TRUE),
                                                     withSpinner(leafletOutput("envr", height = "500px")),
                                              
                                     )
                                    
                                 )
                            ),
                            
                            ### 2.2.6 Subtab Demographics-----
                            tabPanel("Demographics", 
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Demographics"), align = "center"),
                                              p("", style = "padding-top:10px;")),
                                     fluidRow(style = "margin: 6px;",
                                              align = "justify",
                                             
                                              )
                                                                           
                            ),
                            
                            
                 ),
                 ## 2.4 Tab Agent Optimization Programming------
                 navbarMenu("Mathematical Programming" ,
                            ### 2.4.1 Subtab Overview -----
                            tabPanel("Programming Overview",
                                     fluidRow(style = "margin: 6px;",
                                              p("", style = "padding-top:10px;")),
                                     fluidRow(style = "margin: 6px;",
                                              align = "justify",
                                              
                                              
                                     )
                            ),
                            ### 2.4.2 Subtab Results
                            tabPanel("Results",
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Results"), align = "center"),
                                              p("", style = "padding-top:10px;")),
                                     fluidRow(style = "margin: 6px;",
                                              p("", style = "padding-top:10px;")),
                                     fluidRow(style = "margin: 6px;",
                                              align = "justify",
                                              tabsetPanel(
                                                tabPanel("Aggregate Z-score",
                                                         # Content for the Aggregate Z-score tab goes here
                                                ),
                                                tabPanel("Food Insecurity Z-score",
                                                         # Content for the z-score tab goes here
                                                ),
                                                tabPanel("Low Birthweight Z-score",
                                                         # Content for the Z-score tab goes here
                                                ),
                                                tabPanel("Obesity Z-score",
                                                         # Content for the Z-score tab goes here
                                                ),
                                                tabPanel("Physical Inactivity Z-score",
                                                         # Content for the  Z-score tab goes here
                                                )
                                               
                                              ),
                                             
                                     )
                            )
                 ),
                            
                 
                 
                 ## 2.5 Tab Takeawayss --------------------------------------------
                 tabPanel("Takeaways", value = "conclusion", 
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Project Findings and Predictions"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   p("findings on maps"),
                                   fluidRow(style = "margin: 6px;", align = "justify",
                                            h4(strong("VCE")),
                                            
                                   ), 
                                   
                                   
                                   
                          )),
                 
                 
                 ## 2.6 Tab Data Sources --------------------------------------------
                 tabPanel("Data Sources", 
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Data Sources"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   fluidRow(style = "margin: 6px;", align = "justify",
                                            column(4,
                                                   img(src = "county_health_rankings.jpg", style = "display: inline; float: left;", width = "230px"),
                                                   p(strong("County Health Rankings & Roadmaps"), "The County Health Rankings & Raodmaps (CHR&R) a program of the University of Wisconsin Population Health Institute. 
                                                   The CHR&R program provides data, evidence, guidance, and examples to build awareness of the multiple factors that influence health and support leaders in growing community power to improve health equity. This project utilizes CHR&R
                                                    to obtain 2016/2020 county-level data to explore and visualize the Commonwealth's health characteristics.")),
                                            column(4,
                                                   img(src = "vce.jpg", style = "display: inline; float: left;", width = "230px"),
                                                   p(strong("Virginia Cooperative Extension Administrative Data"), "Virginia Cooperative Extension (VCE) provided us with office and agent data which allowed us to gain a better understanding of where Family Consumer Science
                                                     agents operate. The team used this data to create visualizations, specifically focusing on the distribution of optimized agent territories across localities.")),
                                            )),
                 ),
                 
                 
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
  output$VariableDefinition <- renderText({
    if (input$Health_Outcomes == "per_low_birthweight") {
      a
    } else if (input$Health_Outcomes == "life_expectancy") {
      b
    } else if (input$Health_Outcomes == "life_expectancy_gap") {
      "Statistics for life_expectancy_gap"
    } else if (input$Health_Outcomes == "life_expectancy_black") {
      "Statistics for life_expectancy_black"
    } else if (input$Health_Outcomes == "life_expectancy_white") {
      "Statistics for life_expectancy_white"
    } else {
      "Please select a health outcome."
    } 
  })
  
  ## 3.2 Healthcare Access -----
  #create a rective expression for healthcare access variables
  temp_healthaccess <- reactive({
    input$Health_Access
  })
  temp_healthaccessyear <- reactive({
    as.integer(input$yearSelect_access)
  })
  
  output$healthaccess <- renderLeaflet({
    mapping2(temp_healthaccess(), temp_healthaccessyear())
  })
  output$HealthAccessVariableDefinition <- renderText({
    if (input$Health_Access == "per_uninsured") {
      "stats for unsinsured"
    } else if (input$Health_Access == "dentist_ratio") {
      "stats for "
    } else if (input$Health_Access == "mental_health_provider_ratio") {
      "Statistics for "
    } else if (input$Health_Access == "primary_care_physicians_ratio") {
      "Statistics for"
      
    } else {
      "Please select a health outcome."
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
  output$EconomicStabilityVariableDefinition <- renderText({
    if (input$econ_stab == "per_unemployed") {
      "stats for unsinsured"
    } else if (input$econ_stab == "per_children_in_poverty") {
      "stats for "
    } else if (input$econ_stab == "per_food_insecure") {
      "Statistics for "
    } else if (input$econ_stab == "median_household_income") {
      "Statistics for"
      
    } else {
      "Please select a health outcome."
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
  output$HealthBehaviorsVariableDefinition <- renderText({
    if (input$health_behaviors == "per_adults_reporting_currently_smoking") {
      "stats for "
    } else if (input$health_behaviors == "per_excessive_drinking") {
      "stats for "
    } else if (input$health_behaviors == "per_driving_deaths_with_alcohol_involvement") {
      "Statistics for "
    } else if (input$health_behaviors == "per_physically_inactive") {
      "Statistics for"
      
    } else {
      "Please select a health outcome."
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
  output$EnvrVariableDefinition <- renderText({
    if (input$neighbor_envr == "per_physical_distress") {
      "stats for "
    } else if (input$neighbor_envr == "per_mental_distress") {
      "stats for "
    } else if (input$neighbor_envr == "per_access_to_exercise_opportunities") {
      "Statistics for "
    } else if (input$neighbor_envr == "suicide_rate") {
      "Statistics for"
    } else {
      "Please select a health outcome."
    } 
  }) 
 
}

# 4. Run the application-------------------------------------------------------------------
shinyApp(ui = ui, server = server)
