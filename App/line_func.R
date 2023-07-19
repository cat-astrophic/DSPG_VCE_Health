# SDoH line graph function
sdoh_line <- function(county1, county2, variable) {
  
  good_names2 <- c( "Chlamydia Rate","Dentist Ratio","Drug Mortality Rate",
                    "Gender Pay Gap","HIV Prevalence Rate","Median Household Income Black",
                    "Median Household Income White","Juvenile Arrests Rate","Life Expectancy",
                    "Life Expectancy Black","Life Expectancy White","Life Expectancy Gap",
                    "Median Household Income","Median Household Income Gap White Black",
                    "Median Household Income Gap White Hispanic","Median Household Income Hispanic",
                    "Mental Health Provider Ratio","Other Primary Care Provider Ratio",
                    "Percent 6 and over","Percent Population with Access to Exercise Opportunities",
                    "Percent of Adults Reporting Currently Smoking","Percent of Adults with Diabetes",
                    "Percent of Adults With Obesity","Percent American Indian or Alaska Native",
                    "Percent Asian", "Percent Black","Percent Children in Poverty",
                    "Percent Driving Deaths with Alcohol Involvement","Percent Excessive Drinking",
                    "Percent Food Insecure","Percent Hispanic","Percent Household Income Required for Child Care Expenses",
                    "Percent Insufficient Sleep","Percent less than 18 years of age","Percent Limited Access to Healthy Foods",
                    "Percent Low Birthweight","Percent Mental Distress","Percent Nonhispanic-White",
                    "Percent not Proficient in English","Percent Physical Distress","Percent Physically Inactive",
                    "Percent Severe Housing Problems","Percent Unemployed","Percent Uninsured","Percent of Uninsured Adults",
                    "Percent Uninsured Children","Percent Vaccinated","Percent Access to Exercise Opportunities",
                    "Percent With Annual Mammogram","Preventable Hospitalization Rate","Primary Care Physicians Ratio",
                    "Suicide Rate","Teen Birth Rate")
  # read in va avg data
  va_avg <- read.csv("with_state_avg.csv") %>% 
    filter(Year != 2021 & 2022)
  # Filter data for the first selected county and variable
  selection1 <- va_avg[va_avg$County2 == county1 & va_avg$Variable == variable, ] %>% 
    na.omit(cols= Value)
  
  # Filter data for the second selected county and variable
  selection2 <- va_avg[va_avg$County2 == county2 & va_avg$Variable == variable, ]%>% 
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
  map_title = paste(good_names2[idx],"Over Time", sep= " ")
  
  #plot all the selections and average on plotly line graph
  comparison_plot <- plot_ly() %>%
    add_trace(data = selection1, x = ~Year, y = ~Value, name = county1,
              type = "scatter", mode = "lines", 
              line = list(color = "#33638DFF", width = 4)) %>%
    add_trace(data = selection2, x = ~Year, y = ~Value, name = county2,
              type = "scatter", mode = "lines", 
              line = list(color = "#440154FF", width = 4)) %>%
    add_trace(data = avg, x = ~Year, y = ~Value, name = "State Average",
              type = "scatter", mode = "lines", 
              line = list(color = "#3F4788FF", width = 4)) %>%
    layout(title = map_title, 
           xaxis = list(tickvals= c(2016, 2017, 2018, 2019, 2020),title = 'Years'),
           yaxis = list(title = good_names2[idx]),
           legend = list(font = list(size = 15)))
  
  comparison_plot
}