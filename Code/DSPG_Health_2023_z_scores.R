# This script creates a figure based on aggregate z-scores

# Loading libraries

library(ggplot2)
library(tigris)
library(plotly)

# Project directory info

direc <- 'C:/Users/vivia/Documents/DSPG/Repose/DSPG_VCE_Health/Data/'

# Reading in the data

data <- read.csv(paste(direc, 'final_variables.csv', sep = ''))
vce <- read.csv(paste(direc, 'vce_agents.csv', sep = ''))

# Making the Value column numeric

data$Value <- as.numeric(data$Value)

# Defining a function for z-scores

z.score <- function(var,year,loc) {
  
  tmp <- data[which(data$Variable == var & data$Year == year),]
  x <- tmp[which(tmp$County == loc),]$Value
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

for (loc in unique(data$County)) {
  
  z.obese <- c(z.obese, z.score('per_adults_with_obesity', 2020, loc))
  z.diabetes <- c(z.diabetes, z.score('per_adults_with_diabetes', 2020, loc))
  z.food <- c(z.food, z.score('per_food_insecure', 2020, loc))
  z.inactive <- c(z.inactive, z.score('per_physically_inactive', 2020, loc))
  z.low.bw <- c(z.low.bw, z.score('per_low_birthweight', 2020, loc))
  z.overdose <- c(z.overdose, z.score('drug_overdose_mortality_rate', 2020, loc))
  z.vacc <- c(z.vacc, -1*z.score('per_vaccinated', 2020, loc))
  
}

z <- z.obese + z.diabetes + z.food + z.inactive + z.low.bw 
zdf <- as.data.frame(cbind(unique(data$County), unique(data$FIPS), z))
colnames(zdf) <- c('County', 'GEOID', 'z')
zdf$z <- -1*as.numeric(zdf$z)

# Loading a Virginia shapefile with tigris

va.counties <- counties(state = 'Virginia')

# Merging z-scores and the Virginia shapefile

va.counties <- left_join(va.counties, zdf, by = 'GEOID')

# Plotting the aggregate z-score

p <- ggplot(data = va.counties) + 
  scale_fill_gradient2('z-score', low = 'blue', mid = 'orange', high = 'red', midpoint = 0) +
  geom_sf(color = NA, aes(geometry = geometry, fill = as.numeric(z), text = County)) +
  geom_point(data = vce, aes(x = Long, y = Lat), color = 'black') +
  ggtitle('Aggregate Z-scores') +
  theme(plot.title = element_text(hjust = 0.5))

ggplotly(p)

