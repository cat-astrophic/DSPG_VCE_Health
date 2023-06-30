# This script creates a single file to feed into python for the mathematical programming models

# Project directory info

direc <- 'C:/Users/macary/Documents/DSPG/Health_2023/data/'

# Reading in the data

data <- read.csv(paste(direc, 'final_variables.csv', sep = ''))
popdata <- read.csv(paste(direc, 'popdata.csv', sep = ''))
distdata <- read.csv(paste(direc, 'distance_matrix.csv', sep = ''))
distdata2 <- read.csv(paste(direc, 'distance_matrix_new.csv', sep = ''))
vce <- read.csv(paste(direc, 'vce_agents.csv', sep = ''))
new <- read.csv(paste(direc, 'new_agent_locations.csv', sep = ''))

# Splitting distdata and distdata2

distdata <- distdata[,2:dim(distdata)[2]]
distdata2 <- distdata2[,2:dim(distdata2)[2]]

# Making the Value column numeric

data$Value <- as.numeric(data$Value)

# Defining a function for z-scores

z.score <- function(var,year,loc) {
  
  tmp <- data[which(data$Variable == var & data$Year == year),]
  x <- tmp[which(tmp$County2 == loc),]$Value
  mu <- mean(tmp$Value, na.rm = TRUE)
  sigma <- sd(tmp$Value, na.rm = TRUE)
  z <- (x - mu) / sigma
  
  return(z)
  
}

# Creating z-scores using 2020 data

z.obese <- c()
z.diabetes <- c()
z.food <- c()
z.inactive <- c()
z.low.bw <- c()
z.overdose <- c()
z.vacc <- c()

for (loc in unique(popdata$County)) {
  
  z.obese <- c(z.obese, z.score('per_adults_with_obesity', 2020, loc))
  z.diabetes <- c(z.diabetes, z.score('per_adults_with_diabetes', 2020, loc))
  z.food <- c(z.food, z.score('per_food_insecure', 2020, loc))
  z.inactive <- c(z.inactive, z.score('per_physically_inactive', 2020, loc))
  z.low.bw <- c(z.low.bw, z.score('per_low_birthweight', 2020, loc))
  z.overdose <- c(z.overdose, z.score('drug_overdose_mortality_rate', 2020, loc))
  z.vacc <- c(z.vacc, -1*z.score('per_vaccinated', 2020, loc))
  
}

z <- z.obese + z.diabetes + z.food + z.inactive + z.low.bw 

# Create long data for current agent analysis

z_long <- z
for (i in 2:36) {z_long <- c(z_long, z)}

p_long <- popdata$Population
for (i in 2:36) {p_long <- c(p_long, popdata$Population)}

d_long <- c(as.matrix(distdata))

c_long <- popdata$County
for (i in 2:36) {c_long <- c(c_long, popdata$County)}

a_long <- c()
for (i in 1:36) {a_long <- c(a_long, rep(vce$County[i], 133))}

long_df <- as.data.frame(cbind(a_long, c_long, d_long, p_long, z_long))

write.csv(long_df, 'C:/Users/macary/Documents/DSPG/Health_2023/data/program_data.csv', row.names = FALSE)

# Creating long data for new agent analysis

z_long <- z
for (i in 2:75) {z_long <- c(z_long, z)}

p_long <- popdata$Population
for (i in 2:75) {p_long <- c(p_long, popdata$Population)}

d_long <- c(as.matrix(distdata2))

c_long <- popdata$County
for (i in 2:75) {c_long <- c(c_long, popdata$County)}

a_long <- c()
for (i in 1:75) {a_long <- c(a_long, rep(vce$County[i], 133))}

long_df <- as.data.frame(cbind(a_long, c_long, d_long, p_long, z_long))

write.csv(long_df, 'C:/Users/macary/Documents/DSPG/Health_2023/data/program_data_new.csv', row.names = FALSE)

