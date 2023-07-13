#results printing function
results_printed <- function(territory_type, zscore_type){
  temp2 <- all_territories[all_territories$territory_type == territory_type & all_territories$zscore_type == zscore_type, ],
  if(territory_type == "base" & zscore_type == "aggregate") {
    print(paste("The map displayed on the right provides a visual representation of the 
          ideal territories assigned to FCS agents. These territories have been 
          determined using data from aggregate aggregate z-scores to optimize 
          their effectiveness. These territories have also been created in regard 
          to the model’s constraints on population, commute time, and VCE districts. 
          Each color on the map corresponds to a distinct agent's territory. By 
          hovering your cursor over a county, you can easily identify the agent 
          responsible for serving that particular area. Furthermore, you can hover
          over the cloud icons to access relevant information regarding the FCS
          agent's contact details and home office. Feel free to explore different 
          choices in the Agents/Health dropdowns to generate a new map!"))}
  else if(territory_type == "base" & zscore_type == "food") {
    print(paste("The map displayed on the right provides a visual representation of the 
          ideal territories assigned to FCS agents. These territories have been 
          determined using data from aggregate food insecurity z-scores to optimize 
          their effectiveness. These territories have also been created in regard 
          to the model’s constraints on population, commute time, and VCE districts. 
          Each color on the map corresponds to a distinct agent's territory. By 
          hovering your cursor over a county, you can easily identify the agent 
          responsible for serving that particular area. Furthermore, you can hover
          over the cloud icons to access relevant information regarding the FCS
          agent's contact details and home office. Feel free to explore different 
          choices in the Agents/Health dropdowns to generate a new map!")}
  else if(territory_type == "base" & zscore_type == "diabetes") {
    print(paste("The map displayed on the right provides a visual representation of the 
          ideal territories assigned to FCS agents. These territories have been 
          determined using data from aggregate diabetes insecurity z-scores to optimize 
          their effectiveness. These territories have also been created in regard 
          to the model’s constraints on population, commute time, and VCE districts. 
          Each color on the map corresponds to a distinct agent's territory. By 
          hovering your cursor over a county, you can easily identify the agent 
          responsible for serving that particular area. Furthermore, you can hover
          over the cloud icons to access relevant information regarding the FCS
          agent's contact details and home office. Feel free to explore different 
          choices in the Agents/Health dropdowns to generate a new map!"))}
  else if(territory_type == "base" & zscore_type == "obese") {
    print(paste("The map displayed on the right provides a visual representation of the 
          ideal territories assigned to FCS agents. These territories have been 
          determined using data from aggregate obesity insecurity z-scores to optimize 
          their effectiveness. These territories have also been created in regard 
          to the model’s constraints on population, commute time, and VCE districts. 
          Each color on the map corresponds to a distinct agent's territory. By 
          hovering your cursor over a county, you can easily identify the agent 
          responsible for serving that particular area. Furthermore, you can hover
          over the cloud icons to access relevant information regarding the FCS
          agent's contact details and home office. Feel free to explore different 
          choices in the Agents/Health dropdowns to generate a new map!"))}
  else if(territory_type == "base" & zscore_type == "lowbirth") {
    print(paste("The map displayed on the right provides a visual representation of the 
          ideal territories assigned to FCS agents. These territories have been 
          determined using data from aggregate low birthweight insecurity z-scores to optimize 
          their effectiveness. These territories have also been created in regard 
          to the model’s constraints on population, commute time, and VCE districts. 
          Each color on the map corresponds to a distinct agent's territory. By 
          hovering your cursor over a county, you can easily identify the agent 
          responsible for serving that particular area. Furthermore, you can hover
          over the cloud icons to access relevant information regarding the FCS
          agent's contact details and home office. Feel free to explore different 
          choices in the Agents/Health dropdowns to generate a new map!"))}
  else if(territory_type == "base" & zscore_type == "inactivity") {
    print(paste("The map displayed on the right provides a visual representation of the 
          ideal territories assigned to FCS agents. These territories have been 
          determined using data from aggregate physical inactivity insecurity z-scores to optimize 
          their effectiveness. These territories have also been created in regard 
          to the model’s constraints on population, commute time, and VCE districts. 
          Each color on the map corresponds to a distinct agent's territory. By 
          hovering your cursor over a county, you can easily identify the agent 
          responsible for serving that particular area. Furthermore, you can hover
          over the cloud icons to access relevant information regarding the FCS
          agent's contact details and home office. Feel free to explore different 
          choices in the Agents/Health dropdowns to generate a new map!"))}
}