#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# 1. Set Up------------------------------------------------------------------------ 

## 1. 1 CODE TO DETECT ORIGIN OF LINK AND CHANGE LOGO ACCORDINGLY -------------
# JavaScript code
jscode <- '
    var x = document.getElementsByClassName("navbar-brand");
    var dspgLink = "https://dspg.aaec.vt.edu/";
    var githubLink = "https://github.com/VT-Data-Science-for-the-Public-Good";
    var dspgLogoHTML = \'<a href="\' + dspgLink + \'"><img src="DSPG_black-01.png" alt="VT DSPG" style="height:42px;"></a>\';
    var githubLogoHTML = \'<a href="\' + githubLink + \'"><img src="github_logo.png" alt="GitHub" style="max-height: 30px; max-width: 100%;"></a>\';
    var logosHTML = dspgLogoHTML + githubLogoHTML;
    x[0].innerHTML = x[0].innerHTML + " " + logosHTML;
  '

## 1.2 Load packages----------------------------------------------------------------
library(shiny)
library(shinycssloaders)
library(shinyWidgets)
library(shinythemes)
library(stringr)
library(shinyjs) # run the java code for the DSPG logo
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
library(RColorBrewer)
library(highcharter)
library(sf) #for importing shp file
library(highcharter) #for transition matrix
library(htmlwidgets) #for transition matrix
library(png)
library(slickR)
options(scipen=999)
library(readxl)
library(tigris)
library(paletteer)
library(readr)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(maps)
library(tools)
library(shinycssloaders)
#options(shiny.maxRequestSize = 80*1024^2)

## 1.3 Load the data----------------------------------------------------------------
### 1.3.1 Load the original data----------------------------------------------------------------
#County Health Rankings 2023 Data
ranked23_measured_data <- read_excel("./data/2023 County Health Rankings Virginia Data.xlsx", sheet = 4, skip= 1)

#FCS Agent Location Data
agents_df <- read.csv(paste("./data/vce_agents.csv", sep = ''))
na.omit(agents_df)

#Map Built-in
vc <- counties(state= "Virginia")
# cleaning data for ONLY counties and selected variables
ranked <- ranked23_measured_data %>%
  select(County,FIPS,`% Low Birthweight`, `# Mental Health Providers`, `Dentist Rate`, 
         `Severe Housing Cost Burden`, `% Children in Poverty`,`Years of Potential Life Lost Rate`)
names(ranked)[names(ranked) == 'FIPS'] <- 'GEOID'
ranked23_counties<- ranked[2:134 , ]


### 1.3.2 Process the data---------------------------------------------------------------- 
#loading low birhtweight rate
low_birth_23 <- ranked23_counties %>%
  select(County,GEOID,`% Low Birthweight`) %>%
  na.omit()
names(low_birth_23)[3] = "per_low_birthweight"


### 1.3.3 Merging the data(Geo)---------------------------------------------------------------- 
#merging with agents
low_birth_23$GEOID <- as.character(low_birth_23$GEOID)
va.low.birth <- left_join(vc, low_birth_23, by = 'GEOID')



#importing the world map built-in
world <- ne_countries(scale = "medium", returnclass = "sf")
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

#mapping va counties
counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))
counties <- counties[2785:2884,]

#making agents and removing missing values
agents_df <- agents_df %>%
  slice(-n())

#converting agents into sf
agents_sf <- st_as_sf(agents_df, coords = c("Long", "Lat"), remove = FALSE, 
                      crs = 4326, agr = "constant")


## 1.4 Define your functions -------------------------------------------------------
# Comments: you can write your own functions if you are tired of copying and paste same code over and over again. It is optional.
# 1) write you functions in this file, or
# 2) write them in a seperate file and call them in this app.R. Example: https://stackoverflow.com/questions/13548266/define-all-functions-in-one-r-file-call-them-from-another-r-file-how-if-pos



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
                 
                 ## 2.2 Tab sociodemographics --------------------------------------------
                 navbarMenu("Health Variables" ,
                            ### 2.2.0 Virginia Public Health
                            tabPanel("Public Health Overview", 
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Public Health in Virginia"), align = "center"),
                                              p("", style = "padding-top:10px;")), 
                                     fluidRow(style = "margin: 6px;",
                                              align = "justify",
                                     )
                            ),
                            
                            ### 2.2.1 Subtab Goochland--------------------------------------
                            tabPanel("Health Outcomes", 
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Health Outcomes"), align = "center"),
                                              p("", style = "padding-top:10px;")), 
                                     fluidRow(style = "margin: 6px;",
                                              align = "justify",
                                              column(4, 
                                                     h4(strong("Background")),
                                                     p("Goochland County is located in the Piedmont of the Commonwealth of Virginia. It covers 281.42 square miles and is 71st in the state in size. The county is known for its fertile land and mineral deposits. 
                                                       The James River flows along the county's southern border, supplying water to farmlands and mills. Coal was mined in the east and gold in the west. Today, agriculture is still important to the county's economy. 
                                                       Goochland has updated its voting districts in 2022 to better represent the population of all 5 districts [1]. Goochland County also has a vast summer program with plenty of activities. The activities are located 
                                                       across the county at different facilities, including the skate park, gymnasium, baseball fields, weight room, trails, and many more [2]."),
                                                     h4(strong("Summary Statistics")),
                                                     p("Goochland County’s population is 23,472, which is split between 49.8% male (11,698), and 50.2% female (11,774) [3]. 23,524 identify as one race, where 19,302 are white, 3,267 are African American, 
                                                       75 are American Indian and Alaska Native, 494 are Asian, 3 are Native Hawaiian and Other Pacific Islander, and 383 are some other race [4]." ),
                                                     p("57.9% of the population within Goochland County is employed. The unemployment rate is 3.7% [5]."),
                                                     p("There are 11,001 civilian citizens, with 418 employed in agriculture, forestry, fishing and hunting, and mining [6]."),
                                                     p("There are a total of 8,711 households in Goochland County. The median income is $97,146, with a margin of error of around 8,582. Approximately 24.1% of the 6,600 households have one earner, while 46.1% have two earners [7]. 
                                                       The largest proportion (20.5%) of earners in Goochland make between $100,000 to $149,999. 18.4% of earners in Goochland earn over $200,000 [8]."),
                                                     p("Nearly 93.1% of the population 25 and over have graduated high school and pursued further training. The highest level of education is a graduate or professional degree attained by around 3,531 people, or 20.1% of the population 
                                                       over 25 years old [9]."),
                                                     p("According to the 2017 Agricultural Census, there were approximately 355 farms with an average farm size of 160 acres. This makes the total land coverage of farms to be 56,739 acres. $11,740,000 was generated from agricultural products sold. 
                                                       46% of farms sold less than $2,500, and 3% of farms sold over $100,000. Grains, oilseeds, dry beans, and dry peas were the main crops produced for sale ($2,846,000). 
                                                       Milk (dairy) and poultry products sold were also significant ($4,936,000) [1]."),
                                                     p("1.0% of Goochland's population moved within the county, 8.4% moved into the county from a different county in VA, 0.7% moved from a completely different state, and 0.3% moved from abroad [10]."),
                                              ) ,
                                              column(8, 
                                                     h4(strong("Category 1")),
                                                     selectInput("goochland_soc", "Select Variable:", width = "100%", choices = c(
                                                       "Low Birthweight" = "gage",
                                                       "Life Expectancy" = "gind",
                                                       "Life Expectancy Gap" = "ginc",
                                                       "Median Earnings By Educational Attainment (Age > 25 years)" = "gedu")
                                                     ),
                                                     radioButtons(inputId = "yearSelect_gsoc", label = "Select Year: ", 
                                                                  choices = c("2017", "2018", "2019", "2020"), 
                                                                  selected = "2020", inline = TRUE),
                                                     #here is the graoh Output for----
                                                     withSpinner(plotlyOutput("gsoc", height = "500px")) ,
                                                     fluidRow(style = "margin: 6px;",
                                                              align = "justify",
                                                              h4(strong("Visualization Summaries")),
                                                              p("The", strong("age distribution"), "graphs show that the categories consisting of age groups 45 and above have consistently been the largest in the county, making up more than 30% of the population."),
                                                              p("The", strong("employment"), "graphs indicates that the education, health, and social services industry group has been the largest by a wide margin, and specifically saw a large 
                                                       increase between 2017 and 2018. The agricultural, forestal, fishing, hunting, and mining industry group has consistently been the smallest, employing less than 5% of 
                                                       the population every year."),
                                                              p("The" ,strong("income distribution"), "graphs illustrate the consistent growth in individuals and households earning at least $100,000 each year. This growth has been accompanied 
                                                       by a general decrease in earnings below $75,000. It is also notable that earnings above $100,000 and below $35,000 are the largest categories throughout all years."),
                                                              p("The" ,strong("median earnings"), "graphs highlight the fact that those who have gone through some college or attained an associates degree earn the most. The median earnings for this 
                                                       group were significantly higher than others in 2017 and 2018, but saw a significant decrease to $65,890 in 2019. This number goes back up to $75,313 in 2020; still much lower than the first two years.")),
                                                     
                                              ),
                                     ),
                                     column(12, 
                                            h4("References: "), 
                                            p(tags$small("[1] United States Department of Agriculture. Goochland County Virginia - county profile. National Agricultural Statistics Survey. Retrieved July 6, 2022, from https://www.nass.usda.gov/Publications/AgCensus/2017/Online_Resources/County_Profiles/Virginia/cp51075.pdf", tags$br(),
                                                         "[2] Goochland County. (n.d.). Parks &amp;&nbsp;recreation. Goochland County, VA - Official Website. Retrieved July 25, 2022, from https://www.goochlandva.us/236/Parks-Recreation", tags$br(), 
                                                         "[3] U.S. Census Bureau (2022). Age and sex, 2020: ACS 5-Year estimates subject tables. Retrieved July 18, 2022, from https://data.census.gov/cedsci/table?t=Populations%20and%20People&g=0500000US51075&tid=ACSST5Y2020.S0101.", tags$br(), 
                                                         "[4] U.S. Census Bureau (2022). Race, 2020: DEC redistricting data (PL 94-171). Retrieved July 18, 2022, from https://data.census.gov/cedsci/table?t=Populations%20and%20People&g=0500000US51075." , tags$br(),
                                                         "[5] U.S. Census Bureau (2022). Employment status, 2020: ACS 5-Year estimates subject tables. Retrieved July 18, 2022, from https://data.census.gov/cedsci/table?t=Employment%3AEmployment%20and%20Labor%20Force%20Status&g=0500000US51075&y=2020&tid=ACSST5Y2020.S2301&moe=false." , tags$br(),
                                                         "[6] U.S. Census Bureau (2022). Industry by occupation for the civilian employed population 16 years and over, 2020: ACS 5-Year estimates subject tables. Retrieved July 25, 2022, from https://data.census.gov/cedsci/table?t=Occupation&g=0500000US51075&y=2020&tid=ACSST5Y2020.S2405", tags$br(),
                                                         "[7] U.S. Census Bureau (2022). Median income in the past 12 months (in 2020 inflation-adjusted dollars), 2020: ACS 5-Year estimates subject tables. Retrieved July 25, 2022, from https://data.census.gov/cedsci/table?t=Income%20%28Households,%20Families,%20Individuals%29&g=0500000US51075&y=2020&tid=ACSST5Y2020.S1903", tags$br(),
                                                         "[8] U.S. Census Bureau (2022). Income in the past 12 months (in 2020 inflation-adjusted dollars), 2020: ACS 5-Year estimates subject tables. Retrieved July 25, 2022, from https://data.census.gov/cedsci/table?t=Income%20%28Households,%20Families,%20Individuals%29&g=0500000US51075&y=2020", tags$br(),
                                                         "[9] U.S. Census Bureau (2022). Educational attainment, 2020: ACS 5-Year estimates subject tables. Retrieved July 25, 2022, from https://data.census.gov/cedsci/table?t=Education&g=0500000US51075&y=2020", tags$br(),
                                                         "[10] U.S. Census Bureau (2022). Geographic mobility by selected characteristics in the United States, 2020: ACS 5-Year estimates subject tables. Retrieved July 25, 2022, from https://data.census.gov/cedsci/table?t=Residential%20Mobility&g=0500000US51075&y=2020")),
                                            p("", style = "padding-top:10px;")) 
                            ), 
                            ### 2.2.2 Subtab Powhatan--------------------------------------
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
                                            p(tags$small("[1] About Powhatan. About Powhatan | Powhatan County, VA - Official Website. (n.d.). Retrieved July 15, 2022, from http://www.powhatanva.gov/317/About-Powhatan", tags$br(),
                                                         "[2] Powhatan WMA. Virginia Department of Wildlife Resources. (n.d.). Retrieved July 15, 2022, from https://dwr.virginia.gov/wma/powhatan/", tags$br(),
                                                         "[3] U.S. Census Bureau (2022). Age and sex, 2020: ACS 5-Year estimates subject tables. Retrieved July 18, 2022, from https://data.census.gov/cedsci/table?t=Populations%20and%20People&g=0500000US51145&y=2020&tid=ACSST5Y2020.S0101", tags$br(), 
                                                         "[4] U.S. Census Bureau (2022). Race, 2020: DEC redistricting data (PL 94-171). Retrieved July 18, 2022, from https://data.census.gov/cedsci/table?g=0500000US51145&y=2020&tid=DECENNIALPL2020.P1", tags$br(),
                                                         "[5] U.S. Census Bureau (2022). Employment status, 2020: ACS 5-Year estimates subject tables. Retrieved July 18, 2022, from https://data.census.gov/cedsci/table?t=Employment%3AEmployment%20and%20Labor%20Force%20Status&g=0500000US51145&y=2020&tid=ACSST5Y2020.S2301" , tags$br(),
                                                         "[6] U.S. Census Bureau (2022). Industry by occupation for the civilian employed population 16 years and over, 2020: ACS 5-Year estimates subject tables. Retrieved July 25, 2022, from https://data.census.gov/cedsci/table?t=Occupation&g=0500000US51145&y=2020&tid=ACSST5Y2020.S2405", tags$br(),
                                                         "[7] U.S. Census Bureau (2022). Median income in the past 12 months (in 2020 inflation-adjusted dollars), 2020: ACS 5-Year estimates subject tables. Retrieved July 25, 2022, from https://data.census.gov/cedsci/table?t=Income%20%28Households,%20Families,%20Individuals%29&g=0500000US51145&y=2020&tid=ACSST5Y2020.S1903", tags$br(),
                                                         "[8] U.S. Census Bureau (2022). Income in the past 12 months (in 2020 inflation-adjusted dollars), 2020: ACS 5-Year estimates subject tables. Retrieved July 25, 2022, from https://data.census.gov/cedsci/table?t=Income%20%28Households,%20Families,%20Individuals%29&g=0500000US51145&y=2020&tid=ACSST5Y2020.S1901", tags$br(),
                                                         "[9] U.S. Census Bureau (2022). Educational attainment, 2020: ACS 5-Year estimates subject tables. Retrieved July 25, 2022, from https://data.census.gov/cedsci/table?t=Education&g=0500000US51145&y=2020&tid=ACSST5Y2020.S1501", tags$br(),
                                                         "[10] United States Department of Agriculture. Powhatan County Virginia - county profile. National Agricultural Statistics Survey. Retrieved July 25, 2022, from https://www.nass.usda.gov/Publications/AgCensus/2017/Online_Resources/County_Profiles/Virginia/cp51145.pdf", tags$br(), 
                                                         "[11] U.S. Census Bureau (2022). Geographic mobility by selected characteristics in the United States, 2020: ACS 5-Year estimates subject tables. Retrieved July 25, 2022, from https://data.census.gov/cedsci/table?t=Residential%20Mobility&g=0500000US51145&y=2020&tid=ACSST5Y2020.S0701")),
                                            p("", style = "padding-top:10px;")) 
                                     , 
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
                 ),
                 ## 2.4 Tab Agent Optimization Programming
                 tabPanel("Mathematical Programming",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Optimization Program"), align = "Left"),
                                   p("", style = "padding-top:10px;"),
                                   fluidRow(style = "margin: 6px;", align = "justify"),
                          )
                 ),
                 
                 
                 ## 2.5 Tab Findings --------------------------------------------
                 tabPanel("Findings", value = "conclusion", 
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
  
  output$gsoc <- renderPlotly({
    
    low_birth__ggplot <- ggplot(data = world) +
      geom_sf(data = states, color= "grey60", fill= "ivory1") +
      geom_sf(data = counties, fill = NA) +
      geom_sf(data= va.low.birth, aes(fill=per_low_birthweight, text= NAME))+
      scale_fill_viridis_c(trans= "sqrt", alpha= .4, direction= -1) +
      labs(fill= "% Low Birthweight")+
      geom_sf(data = agents_sf, aes( color = "red")) +
      guides(color= guide_legend(title= "Agent Sites")) +
      coord_sf(xlim = c(-84, -75), ylim = c(36, 40), expand = FALSE) +
      xlab("Longitude") + ylab("Latitude") +
      ggtitle("VCE FCS Agent Sites and Percent Low Birthweight of 2023") +
      theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.5),
            panel.background = element_rect(fill = "azure1"))

    low_birth__ggplot %>% ggplotly()
  })
     
}

# 4. Run the application-------------------------------------------------------------------
shinyApp(ui = ui, server = server)
