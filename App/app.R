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
                  "Percent Driving Deaths with Alcohol Involvement", "Dentist Ratio", "Mental Health Provider Ratio", "Teen Birth Rate","Percent Unemployed", "Percent Children in Poverty", 
                  "Chlamydia Rate", "Percent Uninsured","Primary Care Physicians Ratio", "Preventable Hospitalization Rate", "Percent With Annual Mammogram",
                  "Percent Vaccinated", "Life Expectancy", "Life Expectancy Black", "Life Expectancy White", "Life Expectancy Gap", "Percent of Uninsured Adults", "Percent Uninsured Children", "Other Primary Care Provider Ratio","Drug Mortality Rate", 
                  "Percent of Adults With Obesity", "Percent Physically Inactive", "Percent of Adults with Diabetes", "HIV Prevalence Rate","Percent Food Insecure", "Percent Physical Distress", "Percent Mental Distress", "Percent Severe Housing Problems", 
                  "Percent Insufficient Sleep","Suicide Rate", "Percent Access to Exercise Opportunities","Percent Limited Access to Healthy Foods", 
                  "Juvenile Arrests Rate","Percent less than 18 years of age", "Percent 65 and over", "Percent Black", "Percent American Indian or Alaska Native", 
                  "Percent Asian","Percent Hispanic","Percent Nonhispanic-White","Percent not Proficient in English","Percent Household Income Required for Child Care Expenses",
                  "Gender Pay Gap","Median Household Income Black", "Median Household Income White","Median Household Income Hispanic","Median Household Income Gap White Black","Median Household Income Gap White Hispanic", "Median Household Income")
  # territory data
  all_territories <- read.csv("./data/agent_solutions.csv")
  
  #convert new agent locations to sf
  additional_agent_sf <- st_as_sf(all_territories, coords = c("Long", "Lat"), remove = FALSE, crs = 4326, agr = "constant" )
  ## reading in data for territory function
  #reading in va counties shps
  va.counties <- st_read("./data/va_counties.shp")
  # Convert columns to appropriate types and rename them
  va.counties <- transform(va.counties,
                           GEOID = as.integer(GEOID),
                           NAMELSAD = str_to_title(NAMELSAD) )
  # territory data
  all_territories <- read.csv("./data/all_agent_solutions.csv")


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
    pal <- colorNumeric(palette = "viridis", domain = var.counties$Value, na.color= NA )
    
    #floating text for NA values 
    textbox_content <- "<div id='floating-textbox'>*grey fill indicates no data*</div>"
    # Create labels for counties
    county_labels <- sprintf(
      "<strong>%s</strong><br/>%s: %g", 
      var.counties$NAMELSAD, 
      good_names[idx], 
      var.counties$Value
    ) %>% lapply(htmltools::HTML)
    
    # Create labels for agents
    agent_labels <- sprintf(
      "<strong>Agent Site </strong><br/>District Office: %s <br/> Agent Name: %s<br/> Contact Info: %s <br/> SNAP Ed location: %s",
      agents_sf$Job.Dept,
      agents_sf$Employee.Name,
      agents_sf$VT.Email,
      agents_sf$SNAP.Ed
    ) %>% lapply(htmltools::HTML)
    
    # Wrap legend title if too long
    spaces <- gregexpr("\\s", good_names[idx])[[1]]
    middle_space <- spaces[length(spaces) %/% 2 + 1]
    legend_title <- paste0(substring(good_names[idx], 1, middle_space-1), "</br>", substring(good_names[idx], middle_space+1))
    css_fix <- "div.info.legend.leaflet-control br {clear: both;}" # CSS to correct spacing
    html_fix <- htmltools::tags$style(type = "text/css", css_fix)  # Convert CSS to HTML
    #m %<>% htmlwidgets::prependContent(html_fix)                   # Insert into leaflet HTML code
    
    #floating text bow for missing values
    textbox_content <- "<div id='floating-textbox'>*grey fill indicates no data*</div>"
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
      addControl(htmltools::HTML( '<div style="background:grey; width: 10px; height: 10px;"></div><div>Missing values</div>'), position = "bottomright") %>%
      addAwesomeMarkers(data = agents_sf, icon=awesomeIcons(icon='cloud', markerColor = 'red', iconColor = 'white'),
                        label = agent_labels,
                        labelOptions = labelOptions(noHide = FALSE, direction = "auto", offset=c(0,-10))) %>%
      addLegend(pal = pal, values = ~Value, title = legend_title, position = "bottomright") %>%
      setView(lng = -78.6568942, lat = 38.2315734, zoom = 7)  %>%
      addControl(htmltools::HTML(paste0("<h3 style='margin:3px'>", map_title, "</h2>")), position = "topright", data = NULL)
}
  
#territory function
  territory <- function(territory_type, zscore_type) {
    
    temp2 <- all_territories[all_territories$territory_type == territory_type & all_territories$zscore_type == zscore_type, ]
    
    #convert new agent locations to sf
    additional_agent_sf <- temp2 %>% 
    # Convert new agent locations to sf
    st_as_sf(coords = c("Long", "Lat"), remove = FALSE, crs = 4326, agr = "constant")
    
    #joining variable data with county geometry data
    territory.counties <- left_join(va.counties, temp2, by = 'NAMELSAD')
  
    #assigning colors for each agent territory
    pal <- colorFactor(palette = c("lightgoldenrod" , "red","forestgreen","navy","grey21",
                                   "dodgerblue", "aquamarine4","yellow","salmon", "firebrick4", "darkolivegreen1", 
                                   "royalblue", "greenyellow", "gold","turquoise","mediumvioletred", "mistyrose", 
                                   "palegreen","hotpink4", "purple", "sienna1","lightblue", "darkcyan", "lightsteelblue", 
                                   "magenta","slategray","darkorange3","violet","blue","mediumspringgreen","mediumorchid", 
                                   "pink", "salmon4","burlywood","darkgreen", "honeydew","orange","hotpink",
                                   "darkslateblue", "darkorchid1"),
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
    
    # create labels for agents
    agent_labels <- sprintf(
      "<strong>Agent Site </strong><br/>District Office: %s <br/> Agent Name: %s<br/> Contact Info: %s <br/> SNAP Ed location: %s",
      additional_agent_sf$Job.Dept,
      additional_agent_sf$Employee.Name,
      additional_agent_sf$VT.Email,
      additional_agent_sf$SNAP.Ed
    ) %>% lapply(htmltools::HTML)
    
    #creating good title names
    idx2 <- which(unique(all_territories$zscore_type) == zscore_type)
    good_title_names <- c("Aggregate", "Obesity", "Diabetes", "Food Insecurity", "Physical Inactivity", "Low Birthweight")
    # create title for the map
    territory_title = paste("New VCE FCS Agent Sites based on",good_title_names[idx2], "Z-scores", sep= " ")
    #territory_title = paste("New VCE FCS Agent Territories based on",variable_title, "Z-scores")
    
    #differentiate colors of agents by the new_agent variable
    additional_agent_sf$markerColor <- ifelse(temp2$new_agent == 0, "blue", "red")
    
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
      addAwesomeMarkers(data = additional_agent_sf, 
                        icon=awesomeIcons(icon='cloud', markerColor = additional_agent_sf$markerColor, iconColor = 'white'),
                        label = agent_labels,
                        labelOptions = labelOptions(noHide = FALSE, direction = "auto", offset=c(0,-10))) %>%
      setView(lng = -78.6568942, lat = 38.2315734, zoom = 7) %>%
      addControl(htmltools::HTML(paste0("<h3 style='margin:3px'>", territory_title, "</h2>")), position = "topright", data = NULL)
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
                                   h1(strong("VCE: Optimizing Extension Agent Services"),
                                      h4("Data Science for the Public Good Program"),
                                      h4("Virginia Tech"),
                                      br()
                                   )
                          ),
                          fluidRow(style = "margin: 12px;",
                                   align = "justify",
                                   column(6,
                                          h2(strong("Project Background")),
                                          p(strong("Virginia Coorperative Extensions:"), "Virginia Cooperative Extension (VCE) was established in 1914 and has since been committed to bringing the resources of Virginia Tech and Virginia State University to the people of the Commonwealth. VCE has"),
                                          tags$li("107 offices"),
                                          tags$li("11 Agriculture Research Extension centers"), 
                                          tags$li("6 4-H centers throughout the state"),
                                          p("VCE agents and volunteers strive to empower youth and Virginian farmers, guide sustainable resource management, and promote public health. VCE accomplishes these goals through programs that put research-based knowledge to work in people’s lives. VCE has a variety of programs like 4-H Youth Development, Family and Consumer Sciences, Community Viability, Agriculture and Natural Resources, Food, Nutrition, and Health, etc. in every county. VCE works on unique challenges Virginians face in partnership with governments and organizations to solve these issues in a way that benefits all people. With the expertise and knowledge from Virginia Tech and Virginia State University, VCE agents are able to tackle issues and foster community growth across the state. "),
                  
                                          p("For the purpose of this project, we will be focusing on VCE’s Family and Consumer Sciences Program and the agents that support this program. FCS programming is tied to community needs and directed toward families and individuals. Many counties’ FCS programs look different from one another, however, there are core specialty areas every program has. The specialty areas include: Nutrition/Wellness, Family Financial Education, and Family and Human Development. FCS agents are responsible for partnering and collaborating with other VCE agents, agencies, nonprofits/ other organizations, and the public to meet the educational needs of local residents. Agents are tasked with determining program goals and needs by monitoring trends and issues. FCS agents essentially help Virginian families make more healthy and smart decisions by applying research-based knowledge to work in people’s lives. However, this is easier said than done. A big reason why every county’s FCS programs look different is because of the unique populations and challenges every county has. This unfortunately creates a difficult job for FCS agents. They are overextended and commit a lot more time and effort than what seems to fit into the 3 FCS specialty areas. Today, FCS agents are doing a lot more than what was originally expected of them as VCE extends their work to be more public health focused."),
                                  
                                   ),
                                   column(6,
                                          h2(strong("Our Work")),
                                          p("Our team seeks to design an interactive dashboard that will aid VCE FCS agents in identifying which areas of Virginia are in need of more support. This dashboard will help our stakeholders gain a better understanding of the needs of the community, as well as their current health demographics. Agents will be able to put our research into practice by using our dashboard to identify specific areas of need for every county in Virginia. We hope that this resource will support VCE FCS agents in improving the overall health of Virginia communities."),
                                          p("We will utilize publicly accessible data, including Virginia health rankings, to offer our stakeholders a comprehensive comprehension of the factors influencing the health of families in Virginia. Our primary focus will be on various variables that align with the five determinants of health as well as health outcomes. Additionally, we will map these variables alongside the existing data on Virginia Cooperative Extension (VCE) agents. These maps will aid in identifying areas where VCE agents can provide further support to their communities."),

                                          p("The resulting analysis and findings will be presented in an interactive dashboard, which will serve as a valuable resource for the Virginia Cooperative Extension. Through this dashboard, VCE agents will be equipped with the necessary information to enhance their understanding of community health determinants and identify areas where their expertise can have a meaningful impact."),
                          
                                          p("This dashboard compiles our findings and allows stakeholders and other users to explore the information interactively."),
                                   ),
                                   
                          ),
                          fluidRow(align = "center",
                                   p(tags$small(em('Last updated: August 2023')))
                          ) 
                 ),
                 
                 ## 2.2 Tab Health variables --------------------------------------------
                 navbarMenu("Health Variables" ,
                            ### 2.2.0 Subtab Virginia Public Health Overview
                            tabPanel("Public Health Overview",
                                     fluidRow(style = "margin: 12px;",
                                              h1(strong("Public Health in Virginia"), align = "center"),
                                              p("", style = "padding-top:10px;")),
                                     fluidRow(style = "margin: 12px;",
                                              align = "justify",
                                              column(6,
                                                     p("In the past 100 years, Virginia has seen tremendous growth in its public health sector, nevertheless, there are still many areas that are in need of significant improvement. Like other states, Virginia continues to battle multiple epidemics that have decreased the average life expectancy. Epidemics like COVID-19, opioids, gun violence, and motor vehicle crashes have plagued the welfare of the Commonwealth. Due to the contrasting urban and rural regions in Virginia, health varies drastically based on where Virginians reside. In the more wealthy and populated localities, life expectancy surpasses the national average. However, in 2018, the average life expectancy in 80 of Virginia’s 133 counties fell below the national average. The Virginia Public Health Association even found that life expectancy in the state’s capital varies by as much as 20 years. Virginia struggles to provide clean air and water, safe roadways, protection from communicable diseases, and other essential public health services to the entire population of the Commonwealth."),
                                                     p("Virginia’s unfavorable health outcomes can be attributed to the lack of public health funding and poor access to affordable healthcare. The Joint Commission on Health Care found that Virginia ranks in the bottom third of states in public health spending. Spending about $95 per Capita, the Virginia Department of Health’s budget has remained unchanged for the past 20 years, when adjusted for inflation and population growth. Additionally, federal funding sometimes do not match the specific needs of localities. Federal funding often prioritizes diseases that draw the most attention and while this benefits disease prevention, it unintentionally results in the underinvestment of many needed programs that affect the social determinants of health. Moreover, this lack of funding results in public health workforce shortages, and causes workers like VCE FCS agents to be overworked and overwhelmed by the needs of the population. Staffing shortages inhibit local health departments from carrying out their responsibilities and prevent Virginians from getting the best care available."),
                                    
                                                    h2("Social Determinants of Health Overview"),
                                                    p("The field of public health encompasses various factors that influence the health and well-being of individuals and communities. One crucial aspect that significantly shapes health outcomes is the social determinants of health. These determinants as defined by the World Health Organization (WHO) are the social, economic, and environmental conditions in which people are born, grow, live, work, and age. They encompass a wide range of factors, including socioeconomic status, education, neighborhood, and physical environment, access to healthcare, social support networks, and cultural norms. Understanding and addressing the social determinants of health is vital for promoting health equity and reducing health disparities among different populations. While individual behaviors and genetics play a role in health outcomes, social determinants profoundly impact an individual's ability to lead a healthy life. They shape opportunities for good health, influence the distribution of resources and power in society, and create conditions that can either support or hinder individual and community health.")
                                                    )
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
                                                     h4(strong("Summary")),
                                                     textOutput("VariableDefinition"),

                                              ) ,
                                              column(8,

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
                                              p("Accessible and affordable healthcare plays a critical role in promoting physical, social, and mental well-being. While health insurance facilitates access to essential medical services, it alone does not guarantee accessibility. It is equally vital for healthcare providers to offer affordable care, be accessible to patients, and be located conveniently.
                                                In the context of VCE FCS agents' work, their efforts can contribute to improving healthcare accessibility for individuals and families. By addressing community-specific needs and collaborating with local healthcare providers, FCS agents can support initiatives that enhance access to quality healthcare. They can facilitate partnerships between healthcare providers and community organizations, advocate for affordable healthcare options, and educate individuals on navigating the healthcare system effectively. Additionally, FCS agents can provide valuable resources and information on health insurance options, enrollment assistance, and healthcare rights.
                                                Through their work, VCE FCS agents can play a pivotal role in fostering collaborations between healthcare providers and communities, promoting health equity, and ensuring that individuals and families have the necessary tools and support to access affordable, quality healthcare services.", style = "padding-top:10px;")),
                                     fluidRow(style = "margin: 6px;",
                                              align = "justify",
                                              column(3, 
                                                    
                                                     h4(strong("Summary")),
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
                                              )
                                     
                            ), 
                            ### 2.2.3 Subtab Economic Stability--------------------------------------
                            tabPanel("Economic Stability", 
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Economic Stability Variables"), align = "center"),
                                              p("Economic factors have a profound impact on health outcomes and overall well-being within communities. Socioeconomic status, income inequality, and access to resources are all key determinants of health disparities. One crucial aspect of economic influence is income and poverty. Low-income individuals and families often struggle to afford basic necessities like nutritious food, stable housing, and healthcare services. This can result in higher rates of malnutrition, inadequate living conditions, and limited access to essential healthcare, leading to a range of health challenges. VCE agents can play a pivotal role in addressing these issues by providing community members with resources on financial literacy, budgeting, and connecting them with public assistance programs. By promoting financial stability and providing guidance on accessing available resources, agents can help improve health outcomes for vulnerable populations.
                                                Furthermore, employment and job opportunities significantly impact health and well-being. Unemployment or underemployment can contribute to chronic stress, mental health issues, and limited access to healthcare services. Additionally, job insecurity and stressful work environments can negatively affect physical health. VCE agents can collaborate with local workforce development agencies, businesses, and educational institutions to provide job training, skills development programs, and support in finding employment. By helping individuals gain meaningful employment and stable income, agents contribute to improved health and overall quality of life within their communities.
                                                Education is another important economic factor that influences health outcomes. Educational attainment is strongly associated with better health and economic prospects. VCE agents can support educational initiatives by providing workshops, training programs, and resources on topics such as career development, vocational skills, and entrepreneurship. By equipping community members with the necessary skills and knowledge, agents empower individuals to pursue better job opportunities, increase income potential, and ultimately enhance their health and well-being.
                                                Access to affordable and quality healthcare is crucial for maintaining good health. Economic factors significantly influence healthcare access, as individuals with limited financial resources may struggle to afford insurance coverage and healthcare services. VCE agents can raise awareness about healthcare resources, insurance options, and preventative care programs available in the community. They can provide information on navigating the healthcare system, understanding insurance coverage, and connecting community members with local healthcare providers. By promoting access to affordable and quality healthcare, agents contribute to better health outcomes and reduce health disparities within their communities.", style = "padding-top:10px;")),
                                     fluidRow(style = "margin: 6px;",
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
                                                     
                                                     h4(strong("Summary")),
                                                     textOutput("EnvrVariableDefinition")
                                                     
                                              ) ,
                                              column(9, 
                                                     selectInput("neighbor_envr", "Select Variable:", width = "50%", choices = c(
                                                       "Physical Distress" = "per_physical_distress",
                                                       "Mental Distress" = "per_mental_distress",
                                                       "Access to Exercise Opportunity" = "per_access_to_exercise_opportunities",
                                                       "Suicide Rate" = "suicide_rate",
                                                       
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
                                    
                                 )
                            ),
                            
                            ### 2.2.6 Subtab Demographics-----
                            tabPanel("Demographics", 
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Demographics"), align = "center"),
                                              p("", style = "padding-top:10px;")),
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
                                                                           
                            )
                            
                            
                 ),
                 ),
                 ## 2.4 Tab Agent Optimization Programming------
                 navbarMenu("Agents Territories",
                            tabPanel("Methodology",
                                     fluidRow(
                                       style = "margin: 12px;",
                                       h1(strong("Agent Optimization Process"), align = "center")
                                     ),
                                     fluidRow(
                                       style = "margin: 12px;",
                                       column(12,
                                              titlePanel(strong("Overview")),
                                              p("Our goal is to optimize FCS agent efforts by determining optimal territories for these agents to cover. Since not all counties have FCS agents, we want to determine how FCS agents can allocate their efforts across space so that we do not have some agents serving one well-off county while other agents serve several counties, many of which may be inaccessible in the sense that they take several hours to reach by car. In addition to spatially optimizing existing agents, we also want to identify the locations where new agents could have the largest impact."),
                                              p(strong("Workflow:")),
                                              tags$li("Identify where FCS Agents are"),
                                              tags$li("Identify health outcomes that FCS agents can affect"),
                                              tags$li("Create a health index: z-score aggregation"),
                                              tags$li("Determining accessibility definition"),
                                              tags$li("Solving mathematical programs"),
                                              tags$li("Mapping optimized territories")
                                       )
                                     )
                            ),
                            tabPanel("Programming Overview",
                                     fluidRow(
                                       style = "margin: 12px;",
                                       h1(strong("Mathematical Programming"), align = "center")
                                     ),
                                     fluidRow(
                                       style = "margin: 12px;",
                                       align = "justify",
                                       column(6,
                                              img(src = "equation.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "300px"),
                                              p("The objective of this model is to maximize the population-weighted need of each county, which serves as a measure of the overall demand for services in a given area. By maximizing this objective, the model aims to allocate resources in a way that addresses the varying needs of different counties effectively. To ensure a realistic and practical allocation, the model incorporates four constraints that capture the challenges faced by agents. These constraints are designed to limit the workload of agents and consider the constraints they encounter in their service provision."),
                                              tags$li("Population:"),
                                              p("The population constraint plays a crucial role in this mathematical program as it addresses the challenges faced by agents in serving counties with varying population densities. By setting a limit of 1.2 million people for each agent, we ensure that the workload is distributed fairly and that no agent becomes overwhelmed with an excessively large population to serve. This constraint helps to balance the distribution of agents across counties, considering their respective populations."),
                                              tags$li("Distance:"),
                                              p("The distance constraint is another critical component of this model as it addresses the challenges related to geographical distances that agents must cover in their service provision. By imposing a constraint on the maximum distance an agent can travel, we ensure that the service coverage is feasible and practical in terms of travel time and logistics. The distance constraint acknowledges that agents have limitations on how far they can travel to reach the counties they serve. This constraint helps to account for the time and resources required for agents to travel between counties, ensuring that they can provide timely and efficient services to the populations in need.")
                                       )
                                     )
                            ),
                            
                 
                 
                 
                 
                            ### 2.4.2 Subtab Results ----
                            tabPanel("Results",
                                     fluidRow(
                                       
                                       h1(strong("Results"), align = "center"),
                                       column(12,
                                              p("The map displayed on the right provides a visual representation of the ideal territories assigned to FCS agents.
                                                These territories have been determined using data from aggregate (z_score_type)  z-scores to optimize their effectiveness. 
                                                These territories have also been created in regard to population, accessibility, and VCE districts. Each color on the map corresponds to a distinct agent's territory. 
                                                By hovering your cursor over a county, you can easily identify the agent responsible for serving that particular area. 
                                                Furthermore, you can hover over the cloud icons to access relevant information regarding the FCS agent's contact details and home office. 
                                                Feel free to explore different choices in the Agents/Health dropdowns to generate a new map.", style = "padding-top:20px;"),
                                              
                                              p("Please submit different choices to the Agents/Health dropdowns to see a new map! ", style = "padding-top:20px;"),
                                       )
                                     ),
                                     
                                     fluidRow(
                                       column(5,
                                              selectInput("territory_type", "Agents",
                                                          choices = c("No New Agents" = "base", 
                                                                      "One New Agent" = "one", 
                                                                      "Two New Agents" = "two"), 
                                                          selected = "base"
                                              ),
                                              selectInput("zscore_type", "Health Index",
                                                          choices = c("Aggregate" = "aggregate", 
                                                                      "Food Insecurity" = "food", 
                                                                      "Obesity" = "obese", 
                                                                      "Low Birthweight" = "lowbirth", 
                                                                      "Physical Inactivity" = "inactivity", 
                                                                      "Diabetes" = "diabetes"), 
                                                          selected = "aggregate"
                                              ),
                                              actionButton("submit_btn", "Submit"),
                                              h4(strong("Description")),
                                              textOutput("territorydescription"),
                                              
                                       ),
                                       
                                       column(7,
                                              h4(strong("Map")),  # Add the heading for the map
                                              leafletOutput("map", width = "100%", height = "800px"),

                                       )))),
                 
                 
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
      "% Low Birthweight: Percentage of live births with low birthweight (< 2,500 grams).Low birthweight is a significant public health indicator that reflects various factors related to maternal health, nutrition, healthcare delivery, and poverty. It is primarily attributed to two main causes: preterm births and intrauterine growth restrictions. Both of these conditions are associated with increased risks of infant morbidity and mortality.
      Preterm births, which occur before 37 weeks of gestation, contribute to low birthweight. Given the far-reaching consequences of low birthweight, it is crucial to address the underlying factors contributing to it. This involves efforts to improve access to quality prenatal care, promote proper nutrition, address maternal stress, reduce exposure to pollution, and provide support for substance misuse prevention and treatment during pregnancy. By addressing these factors, we can work towards reducing the occurrence of low birthweight and improving the long-term health outcomes for infants and their families."
    } else if (input$Health_Outcomes == "life_expectancy") {
      "verage number of years a person can expect to live. "
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
      "Percent Uninsured: Percentage of population under age 65 without health insurance.
The absence of health insurance coverage presents a notable obstacle in accessing essential healthcare services and maintaining financial stability. According to a report by the Kaiser Family Foundation, individuals without insurance face significant health consequences as they receive less preventive care, and delayed treatment often leads to severe illnesses or other health complications. Moreover, being uninsured can have substantial financial implications, with many individuals unable to afford their medical expenses, leading to the accumulation of medical debt."
    } else if (input$Health_Access == "dentist_ratio") {
      "Dentist Ratio: Ratio of population to dentists.
Neglected dental diseases can result in significant health consequences, such as pain, infection, and tooth loss. While the inadequacy of dental providers represents just one of the barriers to accessing oral healthcare, a substantial portion of the nation faces shortages in this field. According to the Health Resources and Services Administration, as of December 2022, there were 7,313 designated Dental Health Professional Shortage Areas (HPSAs), encompassing a total population of 70 million individuals affected by these shortages."
    } else if (input$Health_Access == "mental_health_provider_ratio") {
      "Mental Health Provider Ratio: Ratio of population to mental health providers.
Accessing healthcare involves more than just financial coverage; it also necessitates access to healthcare providers. Approximately thirty percent of the population resides in a county designated as a Mental Health Professional Shortage Area, indicating significant deficiencies in mental health providers. With the mental health parity provisions of the Affordable Care Act expanding coverage for mental health services, there is growing concern about exacerbated workforce shortages in this field."
    } else if (input$Health_Access == "primary_care_physicians_ratio") {
      "Primary Care Physicians Ratio: Ratio of population to primary care physicians
Access to healthcare is not solely reliant on financial coverage; it also requires access to healthcare providers. While an abundance of specialist physicians has been linked to increased utilization of services, including potentially unnecessary ones, having an adequate number of primary care physicians is crucial for delivering preventive and primary care. Additionally, primary care providers play a vital role in referring patients to appropriate specialty care when necessary. Thus, ensuring sufficient availability of primary care physicians is essential for facilitating timely and appropriate healthcare services."
    } else if (input$Health_Access == "per_vaccinated"){
      "% Vaccinated: Percentage of fee-for-service (FFS) Medicare enrollees that had an annual flu vaccination.
Influenza is a potentially severe illness that can result in hospitalization and death. Each year, millions of people experience influenza infections, hundreds of thousands require hospitalization due to the flu, and thousands lose their lives to the disease. The most effective method to prevent influenza and lower the chances of flu-related illness, hospitalization, and death is through an annual flu vaccine. It is recommended that individuals aged 6 months and older receive a seasonal flu vaccine every year. Specifically, individuals over the age of 65 are strongly encouraged to get vaccinated as they face a higher risk of developing severe complications from the flu."
    } else if (input$Health_Access == "per_with_annual_mammogram"){
      "% with Annual Mammogram: Percentage of female Medicare enrollees ages 65-74 that received an annual mammography screening.
Research indicates that undergoing mammography screening can significantly reduce breast cancer mortality, particularly among older women. The recommendation or referral from a physician, along with satisfaction with healthcare providers, plays a significant role in encouraging breast cancer screening. Presently, women aged 45-54 are advised to undergo mammograms annually, while women aged 55 and older are recommended to have mammograms every two years."
    } else if (input$Health_Access == "preventable_hospitalization_rate"){
      "Preventable Hospitalization Rate: Rate of hospital stays for ambulatory-care sensitive conditions per 100,000 Medicare enrollees. 
When people are hospitalized for conditions that could have been treated in outpatient settings, it suggests that they did not have access to quality healthcare outside of hospitals. This could also mean that they relied heavily on emergency rooms and urgent care centers instead of regular healthcare providers. Preventable hospital stays can be seen as a measure of both the quality of care and the ability to access primary healthcare services."
    } else if (input$Health_Access == "other_primary_care_provider_ratio"){
    "Other Primary Care Provider Ratio: Ratio of population to primary care providers other than physicians. Primary healthcare is not exclusively provided by physicians. Other healthcare professionals, such as nurse practitioners (NP), physician assistants (PA), and clinical nurse specialists, can also offer routine and preventive care. According to the Health Resources and Services Administration, the primary care NP and PA workforces are projected to grow at a much faster rate compared to physicians in the next decade. This growth has the potential to help address healthcare provider shortages as demand for primary care services continues to increase."  
    } else if (input$Health_Access == "per_uninsured_adults"){
      "% Uninsured Adults: Percentage of adults under age 65 without health insurance."
    } else if (input$Health_Access == "per_uninsured_children"){
      "% Uninsured Children: Percentage of children under age 19 without health insurance. 
The absence of health insurance coverage poses a substantial obstacle to accessing necessary healthcare and maintaining financial stability. According to a report by the Kaiser Family Foundation, individuals without insurance face significant health consequences as they receive limited preventive care, and delayed treatment often leads to severe illnesses or other health complications. Additionally, being uninsured can have severe financial implications, with many individuals unable to afford their medical expenses, resulting in the accumulation of medical debt. This issue is particularly notable among uninsured children, who are less likely to receive timely preventive care, such as vaccinations and well-child visits."
    } else if (input$Health_Access == "per_adults_with_diabetes"){
      "% Adults with Diabetes- Percentage of adults aged 20 and above with diagnosed diabetes (age-adjusted).
Diabetes is a chronic condition known to have broad impacts on physical, social, and mental well-being, and causes significant morbidity and mortality in the United States."
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
  output$HealthBehaviorsVariableDefinition <- renderText({
    if (input$health_behaviors == "per_adults_reporting_currently_smoking") {
      "% Adults Reporting Currently Smoking- This variable measures the percentage of adults who are current smokers (age-adjusted).
        Every year, approximately 480,000 premature deaths are directly linked to smoking. Cigarette smoking is a known cause of several cancers, cardiovascular disease, respiratory conditions, and adverse health outcomes, including low birthweight. Monitoring the prevalence of tobacco use in the population is crucial as it serves as an indicator of potential health risks. It helps communities identify the need for cessation programs and evaluate the effectiveness of existing tobacco control initiatives."
    } else if (input$health_behaviors == "per_excessive_drinking") {
      "% Excessive Drinking- This variable is the percentage of adults reporting binge or heavy drinking (age-adjusted).
      Nearly 1 in 6 American adults are considered binge drinkers. Excessive alcohol consumption poses a significant risk for various adverse health outcomes. These include alcohol poisoning, hypertension, acute myocardial infarction, sexually transmitted infections, unintended pregnancy, fetal alcohol syndrome, sudden infant death syndrome, suicide, interpersonal violence, and motor vehicle crashes."
    } else if (input$health_behaviors == "per_driving_deaths_with_alcohol_involvement") {
      "% Driving Deaths with Alcohol Involvement- This variable represents the percentage of driving deaths with alcohol involvement.
      This variable directly measures the relationship between alcohol and motor vehicle crash deaths. Alcohol is a substance that reduces the function of the brain, impairing thinking, reasoning, and muscle coordination, which are essential to operating a vehicle safely. In 2018, approximately 10,500 Americans were killed in alcohol-related motor vehicle crashes. The annual cost of alcohol-related crashes totals more than $44 billion. Drivers between the ages of 21 and 24 cause 27% of all alcohol-impaired deaths."
    } else if (input$health_behaviors == "per_physically_inactive") {
      "% Physically Inactive- Percentage of adults age 18 and over reporting no leisure-time physical activity (age-adjusted).
      Physical inactivity is highly associated with increased risk of health conditions such as Type 2 diabetes, cancer, stroke, hypertension, cardiovascular disease, and shortened life expectancy. Physical activity is associated with improved sleep, cognitive ability, bone, and musculoskeletal health, and reduced risk of dementia."
    } else if (input$health_behaviors == "per_adults_with_obesity") {
      "% Adults with Obesity- This variable measures the percentage of the adult population (age 18 and older) that reports a body mass index (BMI) greater than or equal to 30 kg/m2 (age-adjusted).
      Adult obesity is a persistent condition that raises the likelihood of various health risks, including hypertension, heart disease, type 2 diabetes, respiratory issues, chronic inflammation, mental illness, and certain cancers.The development of obesity is influenced by a combination of environmental and individual factors. Environmental factors, such as the availability and affordability of nutrient-rich foods, the extent of fast-food advertising, and societal attitudes regarding weight stigma, can significantly impact the prevalence and risk of obesity."
    } else if (input$health_behaviors == "teen_birth_rate") {
      "Teen Birth Rate- This variable represents the number of births per 1,000 female population ages 15-19.
      Teenage pregnancy has been linked to detrimental health outcomes for both the mother and child, with impacts extending to partners, family members, and the wider community. The negative impacts of early childbearing on children and mothers can primarily be attributed to social disadvantage and adversity. Adolescent mothers face obstacles in pursuing education beyond high school and experience heightened mental and physical stress, along with a chronic lack of community support. Access to affordable, high-quality childcare and suitable transportation can pose additional challenges, further limiting their educational and employment opportunities."
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
  output$EnvrVariableDefinition <- renderText({
    if (input$neighbor_envr == "per_physical_distress") {
      "This variable represents the percentage of adults reporting 14 or more days of poor physical health per month (age-adjusted).
      This variable offers valuable information on the overall well-being of adults in a community. Physical health is important for disease prevention, mental health, energy levels, independence, social engagement, and longevity."
    } else if (input$neighbor_envr == "per_mental_distress") {
      "This variable represents the percentage of adults reporting 14 or more days of poor mental health per month (age-adjusted).
      Mental health is a fundamental aspect of our overall well-being. It encompasses our emotional, psychological, and social well-being, and it affects how we think, feel, and act. Good mental health allows us to cope with the daily stresses of life, form positive relationships, make meaningful contributions to society, and navigate challenges effectively. Poor mental health can have detrimental effects on physical health, contributing to the development or exacerbation of various health conditions, including cardiovascular disease, weakened immune system, chronic pain, and digestive disorders."
    } else if (input$neighbor_envr == "per_access_to_exercise_opportunities") {
      "This variable is the percentage of the population with adequate access to locations for physical activity.
      Engaging in more physical activity has been linked to reduced risks of various health conditions, including type 2 diabetes, cancer, stroke, hypertension, cardiovascular disease, and premature mortality. The built environment plays a crucial role in promoting physical activity, as individuals residing in close proximity to amenities such as sidewalks, parks, and gyms are more inclined to engage in regular exercise."
    } else if (input$neighbor_envr == "suicide_rate") {
      "This variable measures the number of deaths due to suicide per 100,000 population (age-adjusted)
      Suicide rates provide information on the mental health of a community. Suicide has an overwhelming effect on the mental health of surviving community members, family members, and friends."
    } else if(input$neighbor_envr == "per_limited_access_to_healthy_foods"){
      "This variable represents the percentage of population who are low-income and do not live close to a grocery store.
      Extensive evidence indicates a robust correlation between living in a food desert and experiencing a higher prevalence of obesity and premature death. Supermarkets have traditionally been known to offer healthier food choices compared to convenience stores or smaller grocery stores. Moreover, limited access to fresh fruits and vegetables is directly linked to premature mortality."
    } else if(input$neighbor_envr == "juvenile_arrests_rate"){  
      "This variable represents the rate of delinquency cases per 1,000 juveniles.
      Juvenile arrests are the result of many factors such as policing strategies, local laws, community and family support, and individual behaviors. Youth who are arrested and incarcerated experience lower self-reported health, higher rates of infectious disease and stress-related illnesses, and higher body mass indices."
    } else if(input$neighbor_envr == "per_insufficient_sleep"){
      " This variable represents the percentage of adults who report fewer than 7 hours of sleep on average (age-adjusted).
        Sleep plays a vital role in maintaining a healthy lifestyle, and insufficient sleep can have significant adverse effects on both personal health and the well-being of others. Persistent sleep deprivation has been associated with various chronic health conditions, including heart disease, kidney disease, high blood pressure, and stroke. It is also linked to psychiatric disorders such as depression and anxiety, as well as risky behavior and an increased risk of suicide. Recognizing the importance of adequate sleep is crucial for promoting overall well-being."
    } else if(input$neighbor_envr == "per_severe_housing_problem"){
      "% Severe Housing Problems - This variable is the percentage of households with at least 1 of 4 housing problems: overcrowding, high housing costs, lack of kitchen facilities, or lack of plumbing facilities.
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
  output$DemographicsDefinition <- renderText({
    if (input$demographics == "per_less_than_18_years_of_age") {
      " Percentage of population below 18 years of age.
      Measuring the percentage of the population in different age groups is crucial for healthcare planning, understanding population dynamics, economic projections, social policy development, and public safety considerations. It enables policymakers, researchers, and service providers to make informed decisions and develop targeted strategies that address the unique needs and challenges of specific age cohorts within a population. Policymakers can anticipate the demand for healthcare services, such as pediatric care, geriatric care, and specialized services for specific age-related conditions."
    } else if (input$demographics == "per_65_and_over") {
      "Percentage of the population ages 65 and older.
      Measuring the percentage of the population in different age groups is crucial for healthcare planning, understanding population dynamics, economic projections, social policy development, and public safety considerations. It enables policymakers, researchers, and service providers to make informed decisions and develop targeted strategies that address the unique needs and challenges of specific age cohorts within a population.  Policymakers can anticipate the demand for healthcare services, such as pediatric care, geriatric care, and specialized services for specific age-related conditions."
    } else if (input$demographics == "per_hispanic") {
      " Percentage of the population self-identifying as Hispanic.
      Collecting data on ethnicity helps identify disparities and inequalities that may exist among different ethnic groups. By measuring and analyzing this information, policymakers and organizations can identify areas where certain ethnic groups may face discrimination, bias, or disadvantage. This data can guide the development of targeted policies and interventions aimed at reducing inequality and promoting equal opportunities for all ethnic groups."
    } else if (input$demographics == "per_asian") {
      "Percentage of the population self-identifying as Asian.
      Collecting data on ethnicity helps identify disparities and inequalities that may exist among different ethnic groups. By measuring and analyzing this information, policymakers and organizations can identify areas where certain ethnic groups may face discrimination, bias, or disadvantage. This data can guide the development of targeted policies and interventions aimed at reducing inequality and promoting equal opportunities for all ethnic groups."
    } else if (input$demographics == "per_nonhispanic_white") {
      "Percentage of population self-identifying as non-Hispanic white.
      Collecting data on ethnicity helps identify disparities and inequalities that may exist among different ethnic groups. By measuring and analyzing this information, policymakers and organizations can identify areas where certain ethnic groups may face discrimination, bias, or disadvantage. This data can guide the development of targeted policies and interventions aimed at reducing inequality and promoting equal opportunities for all ethnic groups."
    } else if (input$demographics == "per_american_indian_or_alaska_native"){
      " Percentage of population self-identifying as American Indian or Alaska Native.
      Collecting data on ethnicity helps identify disparities and inequalities that may exist among different ethnic groups. By measuring and analyzing this information, policymakers and organizations can identify areas where certain ethnic groups may face discrimination, bias, or disadvantage. This data can guide the development of targeted policies and interventions aimed at reducing inequality and promoting equal opportunities for all ethnic groups."
    } else if (input$demographics == "per_black") {
      " Percentage of the population self-identifying as non-Hispanic Black or African American.
    Collecting data on ethnicity helps identify disparities and inequalities that may exist among different ethnic groups. By measuring and analyzing this information, policymakers and organizations can identify areas where certain ethnic groups may face discrimination, bias, or disadvantage. This data can guide the development of targeted policies and interventions aimed at reducing inequality and promoting equal opportunities for all ethnic groups."
    } else if (input$demographics == "per_not_proficient_in_english") {
      "% Not Proficient in English- Percentage of the population aged 5 and over who reported speaking English less than 'well'. Understanding the number of individuals who are not proficient in English helps identify the language needs of a population. It allows policymakers, educators, and service providers to develop appropriate strategies and resources to ensure effective communication and equal access to essential services, such as healthcare, education, legal services, employment, and government programs. By recognizing language diversity and addressing language barriers, societies can promote inclusivity and equitable access to opportunities and resources.
      "
    } else {
      "Please select a health variable."
    } 
  }) 
 ## 3.7 server territory maps----
  observeEvent(input$submit_btn, {
    territory_type <- input$territory_type
    zscore_type <- input$zscore_type
    map <- territory(territory_type, zscore_type)
    
    output$map <- renderLeaflet({
      map
  })
  })
  
  output$territorydescription <- renderText({
  if (input$territory_type == "base" & input$zscore_type == "food") {
    "The map displayed on the right provides a visual representation of the
        ideal territories assigned to FCS agents. These territories have been
        determined using data from food insecurity z-scores to optimize
        their effectiveness. These territories have also been created in regard
        to the model’s constraints on population, commute time, and VCE districts.
        Each color on the map corresponds to a distinct agent's territory. By
        hovering your cursor over a county, you can easily identify the agent
        responsible for serving that particular area. Furthermore, you can hover
        over the cloud icons to access relevant information regarding the FCS
        agent's contact details and home office. Feel free to explore different
        choices in the Agents/Health dropdowns to generate a new map!"
  } else if (input$territory_type == "base" & input$zscore_type == "obese") {
    "The map displayed on the right provides a visual representation of the
        ideal territories assigned to FCS agents. These territories have been
        determined using data from obesity insecurity z-scores to optimize
        their effectiveness. These territories have also been created in regard
        to the model’s constraints on population, commute time, and VCE districts.
        Each color on the map corresponds to a distinct agent's territory. By
        hovering your cursor over a county, you can easily identify the agent
        responsible for serving that particular area. Furthermore, you can hover
        over the cloud icons to access relevant information regarding the FCS
        agent's contact details and home office. Feel free to explore different
        choices in the Agents/Health dropdowns to generate a new map!"
  } else if (input$territory_type == "base" && input$zscore_type == "inactivity") {
    "The map displayed on the right provides a visual representation of the
        ideal territories assigned to FCS agents. These territories have been
        determined using data from physical inactivity insecurity z-scores to optimize
        their effectiveness. These territories have also been created in regard
        to the model’s constraints on population, commute time, and VCE districts.
        Each color on the map corresponds to a distinct agent's territory. By
        hovering your cursor over a county, you can easily identify the agent
        responsible for serving that particular area. Furthermore, you can hover
        over the cloud icons to access relevant information regarding the FCS
        agent's contact details and home office. Feel free to explore different
        choices in the Agents/Health dropdowns to generate a new map!"
  } else if (input$territory_type == "base" & input$zscore_type == "aggregate") {
    "The map displayed on the right provides a visual representation of the
        ideal territories assigned to FCS agents. These territories have been
        determined using data from aggregate z-scores to optimize
        their effectiveness. These territories have also been created in regard
        to the model’s constraints on population, commute time, and VCE districts.
        Each color on the map corresponds to a distinct agent's territory. By
        hovering your cursor over a county, you can easily identify the agent
        responsible for serving that particular area. Furthermore, you can hover
        over the cloud icons to access relevant information regarding the FCS
        agent's contact details and home office. Feel free to explore different
        choices in the Agents/Health dropdowns to generate a new map!"
  } else if (input$territory_type == "base" & input$zscore_type == "lowbirth") {
    "The map displayed on the right provides a visual representation of the
        ideal territories assigned to FCS agents. These territories have been
        determined using data from low birthweight insecurity z-scores to optimize
        their effectiveness. These territories have also been created in regard
        to the model’s constraints on population, commute time, and VCE districts.
        Each color on the map corresponds to a distinct agent's territory. By
        hovering your cursor over a county, you can easily identify the agent
        responsible for serving that particular area. Furthermore, you can hover
        over the cloud icons to access relevant information regarding the FCS
        agent's contact details and home office. Feel free to explore different
        choices in the Agents/Health dropdowns to generate a new map!"
  } else if (input$territory_type == "base" & input$zscore_type == "diabetes") {
    "The map displayed on the right provides a visual representation of the
        ideal territories assigned to FCS agents. These territories have been
        determined using data from diabetes insecurity z-scores to optimize
        their effectiveness. These territories have also been created in regard
        to the model’s constraints on population, commute time, and VCE districts.
        Each color on the map corresponds to a distinct agent's territory. By
        hovering your cursor over a county, you can easily identify the agent
        responsible for serving that particular area. Furthermore, you can hover
        over the cloud icons to access relevant information regarding the FCS
        agent's contact details and home office. Feel free to explore different
        choices in the Agents/Health dropdowns to generate a new map!"
  } else if (input$territory_type == "one" & input$zscore_type == "food") {
    "The map displayed on the right provides a visual representation of the
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
        .Feel free to explore different choices in the Agents/Health dropdowns to
        generate a new map!"
  } else if (input$territory_type == "one" & input$zscore_type == "obese") {
    "The map displayed on the right provides a visual representation of the
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
    "The map displayed on the right provides a visual representation of the
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
    "The map displayed on the right provides a visual representation of the
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
    "The map displayed on the right provides a visual representation of the
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
    "The map displayed on the right provides a visual representation of the
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
    "The map displayed on the right provides a visual representation of the
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
    "The map displayed on the right provides a visual representation of the
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
    "The map displayed on the right provides a visual representation of the
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
    "The map displayed on the right provides a visual representation of the
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
    "The map displayed on the right provides a visual representation of the
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
    "The map displayed on the right provides a visual representation of the
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
        and Augusta County.Feel free to explore different choices in the Agents/Health dropdowns to
        generate a new map!"
  } else {
    "This map shows territories for no new agents."
  }
  })
  
  
}

# 4. Run the application-------------------------------------------------------------------
shinyApp(ui = ui, server = server)
