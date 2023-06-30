# This script gets the drive time between county centroids and agent locations

# Loading libraries

library(gmapsdistance)

# API key

key <- 'YOU NEED TO OBTAIN A GOOGLE API KEY'

# Read in the location data sets

vce <- read.csv('C:/Users/macary/Documents/DSPG/Health_2023/data/vce_agents.csv')
new <- read.csv('C:/Users/macary/Documents/DSPG/Health_2023/data/new_agent_locations.csv')
counties <- read.csv('C:/Users/macary/Documents/DSPG/Health_2023/data/centroids.csv')

# Create origins and destinations for current agent analysis

origins <- c()

for (i in 1:dim(vce)[1]) {
  
  coords <- paste(vce$Lat[i], vce$Long[i], sep = ' ')
  origins <- c(origins, coords)
  
}

destinations <- c()

for (i in 1:dim(counties)[1]) {
  
  coords <- paste(counties$Lat[i], counties$Long[i], sep = ' ')
  destinations <- c(destinations, coords)
  
}

# Getting times

dists <- c()
times <- c()
stats <- c()

for (i in 1:dim(vce)[1]) {
  
  print(i)
  
  for (j in 1:dim(counties)[1]) {
    
    test <- gmapsdistance(origins[i], destinations[j], combinations = 'all', mode = 'driving', key = key, shape = 'wide')
    dists <- c(dists, test$Distance)
    times <- c(times, test$Time)
    stats <- c(stats, test$Status)
    
  }
  
}

# Storing results

dists.df <- dists[1:133]
times.df <- times[1:133]

for (i in 2:36) {
  
  start <- 1+(133*(i-1))
  end <- 133*i
  
  d.col <- dists[start:end]
  t.col <- times[start:end]
  
  dists.df <- cbind(dists.df, d.col)
  times.df <- cbind(times.df, t.col)
  
}

colnames(dists.df) <- vce$County
colnames(times.df) <- vce$County

rownames(dists.df) <- counties$County
rownames(times.df) <- counties$County

# Converting times to minutes from seconds

for (i in 1:dim(times.df)[1]) {
  
  for (j in 1:dim(times.df)[2]) {
    
    times.df[i,j] <- times.df[i,j] / 60
    
    if (times.df[i,j] < 10) {
      
      times.df[i,j] <- 10 
      
    }
    
  }
  
}

# Saving results

write.csv(dists.df, 'C:/Users/macary/Documents/DSPG/Health_2023/data/distance_matrix2.csv', row.names = TRUE)
write.csv(times.df, 'C:/Users/macary/Documents/DSPG/Health_2023/data/distance_matrix.csv', row.names = TRUE)

# Create origins for new agent analysis

origins <- c()

for (i in 1:dim(new)[1]) {
  
  coords <- paste(new$Lat[i], new$Long[i], sep = ' ')
  origins <- c(origins, coords)
  
}

# Getting times

dists <- c()
times <- c()
stats <- c()

for (i in 1:dim(new)[1]) {
  
  print(i)
  
  for (j in 1:dim(counties)[1]) {
    
    test <- gmapsdistance(origins[i], destinations[j], combinations = 'all', mode = 'driving', key = key, shape = 'wide')
    dists <- c(dists, test$Distance)
    times <- c(times, test$Time)
    stats <- c(stats, test$Status)
    
  }
  
}

# Storing results

dists.df <- dists[1:133]
times.df <- times[1:133]

for (i in 2:75) {
  
  start <- 1+(133*(i-1))
  end <- 133*i
  
  d.col <- dists[start:end]
  t.col <- times[start:end]
  
  dists.df <- cbind(dists.df, d.col)
  times.df <- cbind(times.df, t.col)
  
}

colnames(dists.df) <- new$County
colnames(times.df) <- new$County

rownames(dists.df) <- counties$County
rownames(times.df) <- counties$County

# Converting times to minutes from seconds

for (i in 1:dim(times.df)[1]) {
  
  for (j in 1:dim(times.df)[2]) {
    
    times.df[i,j] <- times.df[i,j] / 60
    
    if (times.df[i,j] < 10) {
      
      times.df[i,j] <- 10 
      
    }
    
  }
  
}

# Saving results

write.csv(dists.df, 'C:/Users/macary/Documents/DSPG/Health_2023/data/distance_matrix2_new.csv', row.names = TRUE)
write.csv(times.df, 'C:/Users/macary/Documents/DSPG/Health_2023/data/distance_matrix_new.csv', row.names = TRUE)

