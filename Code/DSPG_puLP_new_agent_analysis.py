# This script solves the DSPG Health optimization problem for adding new agents

# Importing required modules

import pulp as pulp
import pandas as pd
import numpy as np
import itertools
from matplotlib import pyplot as plt

# Reading in data

data = pd.read_csv('C:/Users/Michael/Documents/Data/VCE_Optimization/data/program_data.csv')
ndata = pd.read_csv('C:/Users/Michael/Documents/Data/VCE_Optimization/data/program_data_new.csv')
a_districts = pd.read_csv('C:/Users/Michael/Documents/Data/VCE_Optimization/data/district_agent_matrix.csv')
na_districts = pd.read_csv('C:/Users/Michael/Documents/Data/VCE_Optimization/data/district_new_agent_matrix.csv')
c_districts = pd.read_csv('C:/Users/Michael/Documents/Data/VCE_Optimization/data/district_county_matrix.csv')

# Extracting data

a = data.a_long
c = data.c_long
d = data.d_long
p = data.p_long
z = data.z_long
z_obese = data.z_obese_long
z_diabetes = data.z_diabetes_long
z_food = data.z_food_long
z_inactive = data.z_inactive_long
z_low_bw = data.z_low_bw_long

a2 = ndata.a_long
c2 = ndata.c_long
d2 = ndata.d_long
p2 = ndata.p_long
z2 = ndata.z_long
z2_obese = ndata.z_obese_long
z2_diabetes = ndata.z_diabetes_long
z2_food = ndata.z_food_long
z2_inactive = ndata.z_inactive_long
z2_low_bw = ndata.z_low_bw_long

agent_districts = [list(a_districts[a_districts.Agent == x][['Central', 'Northeast', 'Northwest', 'Southeast', 'Southwest']].reset_index(drop = True).iloc[0]) for x in a]
new_agent_districts = [list(na_districts[na_districts.County == x][['Central', 'Northeast', 'Northwest', 'Southeast', 'Southwest']].reset_index(drop = True).iloc[0]) for x in a2]
county_districts = [list(c_districts[c_districts.County == x][['Central', 'Northeast', 'Northwest', 'Southeast', 'Southwest']].reset_index(drop = True).iloc[0]) for x in c]

# Setting some parameters

n = len(c_districts.County.unique()) # counties
m = len(a_districts.Agent.unique()) # agents
m2 = len(na_districts.County.unique()) # new agents
T = 60 # commute threshold of 2 hours
P = 1000000 # population served upper bound

# Transforming / creating data

baseline_choice_vars = [a[i].replace(' ', '_') + '__' + c[i].replace(' ', '_') for i in range(len(a))] # choice variables
new_choice_vars = [a2[i].replace(' ', '_') + '__' + c2[i].replace(' ', '_') for i in range(len(a2))] # more potential choice variables
d = [1/x for x in d]
d2 = [1/x for x in d2]
#d = [1/np.sqrt(x) for x in d]
#d2 = [1/np.sqrt(x) for x in d2]
p = [np.log(x) for x in p]
p2 = [np.log(x) for x in p2]
z = [max(z) - x + 1 for x in z]
z_obese = [max(z_obese) - x + 1 for x in z_obese]
z_diabetes = [max(z_diabetes) - x + 1 for x in z_diabetes]
z_food = [max(z_food) - x + 1 for x in z_food]
z_inactive = [max(z_inactive) - x + 1 for x in z_inactive]
z_low_bw = [max(z_low_bw) - x + 1 for x in z_low_bw]
z2 = [max(z2) - x + 1 for x in z2]
z2_obese = [max(z2_obese) - x + 1 for x in z2_obese]
z2_diabetes = [max(z2_diabetes) - x + 1 for x in z2_diabetes]
z2_food = [max(z2_food) - x + 1 for x in z2_food]
z2_inactive = [max(z2_inactive) - x + 1 for x in z2_inactive]
z2_low_bw = [max(z2_low_bw) - x + 1 for x in z2_low_bw]

new_agent_lists = []

for num_agents in range(1,3):
    
    tmp_list = []
    
    for subset in itertools.combinations(list(a2.unique()), num_agents):
        
        tmp_list.append(list(subset))
        
    new_agent_lists.append(tmp_list)

# Baseline model objective function optimized value

#baseline_of_val = 7083277.21434252
baseline_of_val = 857.03802575
z_lists = [z, z_obese, z_diabetes, z_food, z_inactive, z_low_bw]
z2_lists = [z2, z2_obese, z2_diabetes, z2_food, z2_inactive, z2_low_bw]
new_of_vals = [[0, 0]]*len(z_lists)
optimal_new_agents = [[None, None]]*len(z_lists)
model_names = ['aggregate', 'obesity', 'diabetes', 'food insecurity', 'physical inactivity', 'low birthweight']
of_vals = []

# Main loop

for num_agents in range(1,3):
    
    for z_score in range(len(z_lists)):
        
        good_z = z_lists[z_score]
        good_z2 = z2_lists[z_score]
        all_of_vals = []
        
        for k in range(len(new_agent_lists[num_agents-1])):
            
            # Status update
            
            print('Optimizing ' + model_names[z_score] + ' model with new agents at :: ' + str(new_agent_lists[num_agents-1][k]))
            
            # Update choice variable set
                    
            new_indices = [q for q in range(len(a2)) for qq in range(len(new_agent_lists[num_agents-1][k])) if a2[q] == new_agent_lists[num_agents-1][k][qq]]
            more_agents = [new_choice_vars[q] for q in new_indices]
            choice_vars = baseline_choice_vars + more_agents
            
            # Updating the problem data
            
            d2x = [d2[q] for q in new_indices]
            p2x = [p2[q] for q in new_indices]
            z2x = [good_z2[q] for q in new_indices]
            
            dd = d + d2x
            pp = p + p2x
            zz = good_z + z2x
            
            more_agent_districts = [new_agent_districts[q] for q in new_indices]
            more_county_districts = [county_districts[q] for q in range(n)]*int(len(more_agent_districts)/n)
            
            x_agent_districts = agent_districts + more_agent_districts
            x_county_districts = county_districts + more_county_districts
            
            travel_constraint = [q <= T for q in dd] # travel distance constraint
            
            # Setting up the program
            
            problem = pulp.LpProblem('Agent Optimization Problem', pulp.LpMaximize)
            
            # Initialize a list of choice variables
            
            x = [pulp.LpVariable(cv, lowBound = 0, upBound = 1, cat = 'Integer') for cv in choice_vars]
            
            # Define the objective function
            
            problem += pulp.lpSum([dd[i]*pp[i]*zz[i]*x[i] for i in range(len(x))])
            
            # Constraints
            
            # Unique assignment of counties to agents
            
            for i in range(n):
                
                problem += pulp.lpSum([x[j] for j in range(len(x)) if j%n == i]) <= 1
            
            # Travel distance threshold
            
            for i in range(len(x)):
                
                problem += pulp.lpSum(x[i] - travel_constraint[i]) <= 0
            
            # Same region threshold
            
            for i in range(len(x)):
                
                problem += pulp.lpSum(x[i] - np.matmul(x_agent_districts[i], x_county_districts[i])) <= 0
            
            # Population upper bound threshold
            
            for i in range(m + num_agents):
                
                problem += pulp.lpSum([pp[j]*x[i] for j in range(len(pp)) if j%m == i]) <= P
            
            # Solve this problem
            
            problem.solve()
            
            # Extracting the results
            
            obj_fx_val = problem.objective.value()
            all_of_vals.append(obj_fx_val)
            
            solution = []
            
            for var in problem.variables():
                
                if var.varValue > 0:
                    
                    solution.append(var)
            
            # If this is the best solution (so far) for num_agents number of new agents, store the results
            
            #if len(solution) == n and obj_fx_val > new_of_vals[z_score][num_agents-1]:
                
                #new_of_vals[z_score][num_agents-1] = obj_fx_val
                #optimal_new_agents[z_score][num_agents-1] = new_agent_lists[num_agents-1][k]
                
        # Store the new_of_vals
        
        of_vals.append(all_of_vals)

# Generate outputs for the optimal combinations

baseline_of_val
new_of_vals
optimal_new_agents
















"""


# Visualizing the number of counties per agent

ans = []

for i in range(m):
    
    count = 0
    
    for s in solution:
        
        if str(s).split('__')[0].replace('_', ' ') == a[133*i+1]:
            
            count += 1
            
    ans.append(count)

plt.hist(ans, bins = max(ans) - min(ans))

# Extracting the territories served by each agent

territories = []

for i in range(m):
    
    tmp = []
    
    for s in solution:
        
        if str(s).split('__')[0].replace('_', ' ') == a[133*i+1]:
            
            tmp.append(str(s).split('__')[1].replace('_', ' '))
            
    territories.append(tmp)

# Some descriptive statistics on the agents

def agent_data_function(locs, param, oper_mean, is_z, is_d, is_pz, ax):
    
    val = 0
    
    for loc in locs:
        
        tmp = data[data.c_long == loc].reset_index(drop = True)
        
        if is_z == True:
            
            val += (max(data.z_long) + 1 - tmp[param][0])
            
        elif is_d == True:
            
            tmp2 = tmp[tmp.a_long == ax].reset_index(drop = True)
            val += tmp2[param][0]
            
        elif is_pz == True:
            
            val += tmp['p_long'][0] * (max(data.z_long) + 1 - tmp['z_long'][0])
            
        else:
            
            val += tmp[param][0]
            
    if oper_mean == True:
        
        val = val / len(locs)
    
    return val

agent_p = [agent_data_function(t, 'p_long', False, False, False, False, list(a.unique())[territories.index(t)]) for t in territories]
agent_z = [agent_data_function(t, 'z_long', True, True, False, False, list(a.unique())[territories.index(t)]) for t in territories]
agent_d = [agent_data_function(t, 'd_long', True, False, True, False, list(a.unique())[territories.index(t)]) for t in territories]
agent_pz = [agent_data_function(t, '', False, False, False, True, list(a.unique())[territories.index(t)]) for t in territories]

agent_p = pd.Series(agent_p, name = 'Agent Population Served')
agent_z = pd.Series(agent_z, name = 'Agent Mean z-score Served')
agent_d = pd.Series(agent_d, name = 'Agent Mean Commute Time')
agent_pz = pd.Series(agent_pz, name = 'Agent Population Weighted Need Served')

agents_df = pd.concat([pd.Series(a.unique(), name = 'Agent'), agent_p, agent_z, agent_d, agent_pz, pd.Series(territories, name = 'Territory')], axis = 1)

# Save the descriptive statistics on the agents

agents_df.to_csv('C:/Users/Michael/Documents/Data/VCE_Optimization/agent_descriptive_statistics_________.csv', index = False)












percentage improvement in OF relative to baseline for (1,2,3)-optimal new agent models


"""