#results printing function
results_printed <- function(territory_type, zscore_type){
  temp2 <- all_territories[all_territories$territory_type == territory_type & all_territories$zscore_type == zscore_type, ],
  if(territory_type == "base" & zscore_type == "aggregate") {
    print("The map displayed on the right provides a visual representation of the 
          ideal territories assigned to FCS agents. These territories have been 
          determined using data from aggregate z-scores to optimize 
          their effectiveness. These territories have also been created in regard 
          to the model’s constraints on population, commute time, and VCE districts. 
          Each color on the map corresponds to a distinct agent's territory. By 
          hovering your cursor over a county, you can easily identify the agent 
          responsible for serving that particular area. Furthermore, you can hover
          over the cloud icons to access relevant information regarding the FCS
          agent's contact details and home office. Feel free to explore different 
          choices in the Agents/Health dropdowns to generate a new map!")}
  else if(territory_type == "base" & zscore_type == "food") {
    print("The map displayed on the right provides a visual representation of the 
          ideal territories assigned to FCS agents. These territories have been 
          determined using data from food insecurity z-scores to optimize 
          their effectiveness. These territories have also been created in regard 
          to the model’s constraints on population, commute time, and VCE districts. 
          Each color on the map corresponds to a distinct agent's territory. By 
          hovering your cursor over a county, you can easily identify the agent 
          responsible for serving that particular area. Furthermore, you can hover
          over the cloud icons to access relevant information regarding the FCS
          agent's contact details and home office. Feel free to explore different 
          choices in the Agents/Health dropdowns to generate a new map!")}
  else if(territory_type == "base" & zscore_type == "diabetes") {
    print("The map displayed on the right provides a visual representation of the 
          ideal territories assigned to FCS agents. These territories have been 
          determined using data from diabetes insecurity z-scores to optimize 
          their effectiveness. These territories have also been created in regard 
          to the model’s constraints on population, commute time, and VCE districts. 
          Each color on the map corresponds to a distinct agent's territory. By 
          hovering your cursor over a county, you can easily identify the agent 
          responsible for serving that particular area. Furthermore, you can hover
          over the cloud icons to access relevant information regarding the FCS
          agent's contact details and home office. Feel free to explore different 
          choices in the Agents/Health dropdowns to generate a new map!")}
  else if(territory_type == "base" & zscore_type == "obese") {
    print("The map displayed on the right provides a visual representation of the 
          ideal territories assigned to FCS agents. These territories have been 
          determined using data from obesity insecurity z-scores to optimize 
          their effectiveness. These territories have also been created in regard 
          to the model’s constraints on population, commute time, and VCE districts. 
          Each color on the map corresponds to a distinct agent's territory. By 
          hovering your cursor over a county, you can easily identify the agent 
          responsible for serving that particular area. Furthermore, you can hover
          over the cloud icons to access relevant information regarding the FCS
          agent's contact details and home office. Feel free to explore different 
          choices in the Agents/Health dropdowns to generate a new map!")}
  else if(territory_type == "base" & zscore_type == "lowbirth") {
    print("The map displayed on the right provides a visual representation of the 
          ideal territories assigned to FCS agents. These territories have been 
          determined using data from low birthweight insecurity z-scores to optimize 
          their effectiveness. These territories have also been created in regard 
          to the model’s constraints on population, commute time, and VCE districts. 
          Each color on the map corresponds to a distinct agent's territory. By 
          hovering your cursor over a county, you can easily identify the agent 
          responsible for serving that particular area. Furthermore, you can hover
          over the cloud icons to access relevant information regarding the FCS
          agent's contact details and home office. Feel free to explore different 
          choices in the Agents/Health dropdowns to generate a new map!")}
  else if(territory_type == "base" & zscore_type == "inactivity") {
    print("The map displayed on the right provides a visual representation of the 
          ideal territories assigned to FCS agents. These territories have been 
          determined using data from physical inactivity insecurity z-scores to optimize 
          their effectiveness. These territories have also been created in regard 
          to the model’s constraints on population, commute time, and VCE districts. 
          Each color on the map corresponds to a distinct agent's territory. By 
          hovering your cursor over a county, you can easily identify the agent 
          responsible for serving that particular area. Furthermore, you can hover
          over the cloud icons to access relevant information regarding the FCS
          agent's contact details and home office. Feel free to explore different 
          choices in the Agents/Health dropdowns to generate a new map!")}
  else if(territory_type == "one" & zscore_type == "aggregate") {
    print("The map displayed on the right provides a visual representation of the 
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
          generate a new map!")}
  else if(territory_type == "one" & zscore_type == "food") {
    print("The map displayed on the right provides a visual representation of the 
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
          generate a new map!")}
  else if(territory_type == "one" & zscore_type == "obese") {
    print("The map displayed on the right provides a visual representation of the 
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
          generate a new map!")}
  else if(territory_type == "one" & zscore_type == "lowbirth") {
    print("The map displayed on the right provides a visual representation of the 
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
          generate a new map!")}
  else if(territory_type == "one" & zscore_type == "inactivity") {
    print("The map displayed on the right provides a visual representation of the 
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
          generate a new map!")}
  else if(territory_type == "one" & zscore_type == "diabetes") {
    print("The map displayed on the right provides a visual representation of the 
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
          generate a new map!")}
  else if(territory_type == "two" & zscore_type == "aggregate") {
    print("The map displayed on the right provides a visual representation of the 
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
          generate a new map!")}
  else if(territory_type == "two" & zscore_type == "food") {
    print("The map displayed on the right provides a visual representation of the 
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
          generate a new map!")}
  else if(territory_type == "two" & zscore_type == "obese") {
    print("The map displayed on the right provides a visual representation of the 
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
          generate a new map!")}
  else if(territory_type == "two" & zscore_type == "lowbirth") {
    print("The map displayed on the right provides a visual representation of the 
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
          generate a new map!")}
  else if(territory_type == "two" & zscore_type == "inactivity") {
    print("The map displayed on the right provides a visual representation of the 
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
          generate a new map!")}
  else if(territory_type == "two" & zscore_type == "diabetes") {
    print("The map displayed on the right provides a visual representation of the 
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
          generate a new map!")}
}