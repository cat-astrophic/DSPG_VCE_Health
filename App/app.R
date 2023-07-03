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
    "<strong>Agent Site</strong><br/>Job Department: %s", 
    agents_sf$Job.Dept
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
                                                       "Life Expectancy Black" = "life_expectancy_black")
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
                                              column(4, 
                                                     h4(strong("County Background")),
                                                     p(""),
                                                     
                                                     h4(strong("Summary Statistics")),
                                                    
                                              ) ,
                                              column(8, 
                                                     h4(strong("Sociodemographics")),
                                                     selectInput("powhatan_soc", "Select Variable:", width = "100%", choices = c(
                                                       "Age Distribution of Population" = "page",
                                                       "Employment by Industry" = "pind",
                                                       "Income Distribution" = "pinc",
                                                       "Median Earnings By Educational Attainment (Age > 25 years)" = "pedu")
                                                     ),
                                                     radioButtons(inputId = "yearSelect_psoc", label = "Select Year: ", 
                                                                  choices = c("2017", "2018", "2019", "2020"), 
                                                                  selected = "2020", inline = TRUE),
                                                     plotOutput("psoc", height = "500px"),
                                                     h4(strong("Visualization Summaries")),
                                                    )),
                                     column(12, 
                                            h4("References: "), 
                                            p("", style = "padding-top:10px;"))  , 
                            ), 
                            ### 2.2.3 Subtab Economic Stability--------------------------------------
                            tabPanel("Economic Stability", 
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Economic Stability Variables"), align = "center"),
                                              p("", style = "padding-top:10px;")),
                                     fluidRow(style = "margin: 6px;",
                                              align = "justify",
                            
                                     )
                            ),
                            ### 2.2.4 Subtab Healh Behaviors -----
                            tabPanel("Health Behaviors", 
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Health Behaviors"), align = "center"),
                                              p("", style = "padding-top:10px;")),
                                     fluidRow(style = "margin: 6px;",
                                              align = "justify",
                                              
                                     )
                            ),
                            
                 ),
                 ## 2.4 Tab Agent Optimization Programming------
                 navbarMenu("Mathematical Programming" ,
                            ### 2.4.0 Subtab Results
                            tabPanel("Results",
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Results"), align = "center"),
                                              p("", style = "padding-top:10px;")),
                                     fluidRow(style = "margin: 6px;",
                                              align = "justify",
                                     )
                            ),
                 ),
                            
                          
                 
                 
                 ## 2.5 Tab Takeawayss --------------------------------------------
                 tabPanel("Takeaways", value = "conclusion", 
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Project Findings and Predictions"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   p("Given the rich agricultural histories of the two counties, we are interested in how agricultural land has changed over the last several years. 
                                     This research uses quantitative tools to understand how some key natural and social factors affect the parcellation and conversion with administrative data and county-level geospatial data."),
                                   fluidRow(style = "margin: 6px;", align = "justify",
                                            h4(strong("Goochland")),
                                            p("In Goochland, agricultural land was converted to residential, mainly single-family residential urban, and suburban. 
                                              There were also 5 parcels (about 671 acres) of large agricultural lands that have been parcellated into smaller agricultural plots."),
                                            p("Parcellation is occurring predominantly in the southeast of Goochland County near Richmond, around the U.S. Routes I64, 250, and 288. This pattern might reflect the urban influence on the county. 
                                              This pattern might also imply some correlation between parcellation and transportation. On the crop and land type map, those Routes are labeled as “Developed.” 
                                              High traffic volumes can also be seen along those Routes."),
                                            br(),
                                            h4(strong("Powhatan")),
                                            p("Large amounts of agricultural land were converted to 
                                              residential-suburban uses during the decade in Powhatan (including recurrences). Parcellation among agricultural land 
                                              is also noticeable, as 28 parcels (about 5,750 acres) of large agricultural lands have been parcellated
                                              into smaller agricultural plots."),
                                            p("Parcellation is occurring predominantly in the heart of Powhatan County, around the U.S. Routes 60 and 522. 
                                              On the east end near Richmond, high parcellation rates are seen along the U.S. Routes 60 and 288 within 
                                              the county and this might reflect the urban influence on the county. The high parcellation around 
                                              those Routes might imply some correlation between parcellation and transportation. On the map of crop and land type, 
                                              those Routes are labeled as “Developed”. High traffic volumes can also be seen along U.S. Routes 60 and 288. Hence the 
                                              correlation between parcellation and those Routes is also a correlation between parcellation and developed areas (traffic volumes)."),
                                            p("There is no obvious sign that poor soil quality can be a driver of land conversion out of agriculture from the maps."),
                                            p("In addition to the univariate spatial analysis, we also conducted a statistical analysis that examined the association between land conversion out of 
                                              agriculture and the characteristics of the land parcel, which include parcel acreage, whether the owner lives in the county, distance to the city of Richmond, the traffic volume and the soil class. 
                                              The analysis was conducted for Powhatan County only due to data availability. The findings from a logistic regression model show that the probability of converting out of agriculture: 
                                              decreases as the size of the parcel increases, decreases if the land owner lives in Powhatan, decreases with distance from Richmond. The association with traffic volume shows a U shaped impact 
                                              on the probability of conversion. Soil quality is not significantly associated with land conversion. Note these are not causal effects. They are associations."),
                                   ), 
                                   
                                   
                                   
                          )),
                 
                 
                 ## 2.6 Tab Data Sources --------------------------------------------
                 tabPanel("Data Sources", 
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Data Sources"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   fluidRow(style = "margin: 6px;", align = "justify",
                                            column(4,
                                                   img(src = "data-acs.png", style = "display: inline; float: left;", width = "180px"),
                                                   p(strong("American Community Survey"), "The American Community Survey (ACS) is an demographics survey conducted by the U.S Census Bureau. The ACS samples households to compile 1-year and 5-year datasets 
                                      providing information on social and economic characteristics including employment, education, and income. This project utilizes ACS 2016/2020 5-year
                                      estimates to obtain county- and census tract-level data to explore Goochland and Powhatan Counties' resident characteristics.")),
                                            column(4,
                                                   img(src = "goochland.jpg", style = "display: inline; float: left;", width = "150px"),
                                                   p(strong("Goochland County Administrative Data"), "Goochland County provided us with parcel/property data which allowed us to gain a better understanding of the different land uses and parcellation
                                            that has occured over a 5 year period (2018 - 2022). The team used this data to create visualizations, specifically focusing on the distribution and change in land use in the county.")),
                                            column(4,
                                                   img(src = "powhatan.jpg", style = "display: inline; float: left;", width = "150px"),
                                                   p(strong("Powhatan County Administrative Data"), "Powhatan County provided us with parcel/property data which allowed us to gain a better understanding of the different land uses and parcellation
                                            that has occured over a 8 year period (2014 - 2021). The team used this data to create visualizations, specifically focusing on the distribution and change in land use in the county.")),
                                   ),
                                   
                                   fluidRow(style = "margin: 6px;", align = "justify",
                                            column(4,
                                                   img(src = "nass.jpg", style = "display: inline; float: left;", width = "130px"),
                                                   p(strong("USDA National Agricultural Statistics Service"), "The National Agricultural Statistics Service (NASS) under the United States Department of Agriculture (USDA) provides statistics on a wide variety
                                                    of agricultural topics. This project specifically relies on crop layer data to create maps and to conduct a statistical analysis on the probablity of land use conversion.")),
                                            column(4,
                                                   img(src = "ncss.jpg", style = "display: inline; float: left;", width = "150px"),
                                                   p(strong("USDA National Cooperative Soil Survey"), "The National Cooperative Soil Survey (NCSS) under the USDA provides soil data which was used to generate soil quality maps for both counties. 
                                            The data was also used for our statistical analysis to predict the occurrence of land use conversion.")),
                                            column(4,
                                                   img(src = "vdot_crop.png", style = "display: inline; float: left;", width = "180px"),
                                                   p(strong("VDOT Traffic Data"), "The Virginia Department of Transportation (VDOT) is responsible for building, maintaining and operating the state's roads, bridges and tunnels. VDOT also conducts 
                                          a program where traffic data are gathered from sensors in or along streets and highways and other sources.  This data includes estimates of the average number of vehicles that traveled each segment
                                          of road and daily vehicle miles traveled for specific groups of facilities and vehicle types are calculated. This project utilizes VDOT data to create traffic volume and commute maps for both counties."))
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
                                          img(src = "Rachel Inman.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "John Malla.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          br(), 
                                          img(src = "Christopher Vest.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          p(a(href = 'https://www.linkedin.com/in/rachelinman21/', 'Rachel Inman', target = '_blank'), "(Virginia Tech, Undergraduate in Smart and Sustainable Cities and Minoring in Landscape Architecture);",
                                            br(), 
                                            a(href = 'https://www.linkedin.com/in/john-malla-4b03b0232/', 'John Malla', target = '_blank'), "(Virginia Tech, Undergraduate in Computational Modeling and Data Analytics);",
                                            br(), 
                                            a(href = 'https://www.linkedin.com/in/christophercvest', 'Christopher Vest', target = '_blank'), "(Jacksonville State University, Undergraduate in Finance)."),
                                          p("", style = "padding-top:10px;"),
                                          
                                          h4(strong("DSPG Graduate Student Fellows and Research Assistants")),
                                          img(src = "Nazmul Huda.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "Samantha Rippley.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          br(), 
                                          img(src = "yuanyuanWen.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px",height="100px"),
                                          p(a(href = 'https://www.linkedin.com/in/nazmulpeyal/', 'Nazmul Huda', target = '_blank'), "(Virginia Tech, Graduate Student Fellow in Geography);",
                                            br(), 
                                            a(href = 'https://www.linkedin.com/in/samantha-rippley-58846119b/', 'Samantha Rippley', target = '_blank'), "(Virgina Tech, Graduate Student Fellow in Agricultural Economics);",
                                            br(), 
                                            a(href = 'https://www.linkedin.com/in/yuanyuan-wen-811227246', 'Yuanyuan Wen', target = '_blank'), "(Virginia Tech, Graduate Research Assistant in Agricultural & Applied Economics)."),
                                          p("", style = "padding-top:10px;")
                                   ),
                                   column(6, align = "center",
                                          h4(strong("VT Faculty Member")),
                                          img(src = "SusanChen.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          p(a(href = "https://www.linkedin.com/in/susanchenja/", 'Dr. Susan Chen', target = '_blank'), "(Associate Professor of Agricultural and Applied Economics);",
                                          ),
                                          p("", style = "padding-top:10px;"),
                                          
                                          h4(strong("Project Stakeholders")),
                                          p(a(href = "https://www.linkedin.com/in/rachel-henley-335a0345/", 'Rachel Henley', target = '_blank'), "(Virginia Cooperative Extension, Powhatan County);",
                                            br(), 
                                            a(href = 'https://goochland.ext.vt.edu/staff/Maxwell-Charlotte.html', 'Nichole Shuman', target = '_blank'), "(Virginia Cooperative Extension, Goochland County)."),
                                          p("", style = "padding-top:10px;"),
                                          
                                          
                                   )
                                   
                          )) ,
                 inverse = T)


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
  
  # # Update the choices for cyl input with labels
  # observe({
  #   updateSelectInput(session, "cyl", choices = choices_cyl, selected = input$cyl, label = names(choices_cyl))
  # })
  
  # output$outcomes <- renderPlotly({
  #  mapping(temp_outcome(), temp_year())
  # })
  output$outcomes <- renderLeaflet({
   mapping2(temp_outcome(), temp_year())
  })
  output$VariableDefinition <- renderText({
    if (input$Health_Outcomes == "per_low_birthweight") {
      "Statistics for per_low_birthweight"
    } else if (input$Health_Outcomes == "life_expectancy") {
      "Statistics for life_expectancy"
    } else if (input$Health_Outcomes == "life_expectancy_gap") {
      "Statistics for life_expectancy_gap"
    } else if (input$Health_Outcomes == "life_expectancy_black") {
      "Statistics for life_expectancy_black"
    } else {
      "Please select a health outcome."
    } 
  }) 
     
}

# 4. Run the application-------------------------------------------------------------------
shinyApp(ui = ui, server = server)
