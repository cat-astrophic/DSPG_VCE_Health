# SDoH line graph function
sdoh_line <- function(county1, county2, variable) {
  
  # Filter data for the first selected county and variable
  selection1 <- all_var_df[all_var_df$County2 == county1 & all_var_df$Variable == variable, ]
  
  # Filter data for the second selected county and variable
  selection2 <- all_var_df[all_var_df$County2 == county2 & all_var_df$Variable == variable, ]
  
  # read in va avg data
  va_avg <-
  
  # filter va avg data for selected variable
  avg <- va_avg[va_avg$Variable == variable]
  
  # Identify the index of the selected variable
  idx <- which(unique(all_var_df$Variable) == variable)
  
  # Wrap legend title if too long
  spaces <- gregexpr("\\s", good_names[idx])[[1]]
  middle_space <- spaces[length(spaces) %/% 2 + 1]
  legend_title <- paste0(substring(good_names[idx], 1, middle_space-1), "</br>", substring(good_names[idx], middle_space+1))
  
  # Create title for the map
  map_title = paste(good_names[idx],"Over Time", sep= " ")
  
  #plot all the selections and average on plotly line graph
  drug_class_plot <- plot_ly(data= temp1, x= ~Year, y= ~Variable, name= county1,
                             type= "scatter", mode= "lines", 
                             line= list(color= "#33638DFF",
                                        width= 2 )) %>%
    layout(title= paste(good_names[idx],"Over Time", sep= " ")), 
           annotation = subtitle_annotation,
           xaxis= list(title='Year'),
           yaxis= list(title= good_names[idx] )) %>% 
           layout(legend = list(font = list(size = 15)))
           %>% add_trace(x= temp2$Year, y= temp2$Variable, name= county2, line= list(color= "#440154FF", width= 2))
           %>% add_trace(x= avg$Year, y= avg$Variable, name= "State Average", line= list(color= "#3F4788FF", width= 2))
}