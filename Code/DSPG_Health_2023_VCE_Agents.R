# This script plots the locations of VCE agents over SDoH data

# Loading libraries

library(paletteer)
library(ggplot2)
library(tigris)
library(dplyr)
library(sf)

# Project directory info

path1 <- "C:/Users/vivia/Documents/DSPG/Data/"

# Reading in data

vce <- read.csv(paste(path, 'vce_agents.csv', sep = ''))
hospitals <- read.csv(paste(path, 'hospitals.csv', sep = ''))

# Creating county/city level hospital counts

h.counts <- hospitals %>% count(FIPS)
colnames(h.counts) <- c('GEOID', 'Hospitals')
h.counts$GEOID <- as.character(h.counts$GEOID)

# Using tigris to get geometries

va.counties <- counties(state = 'Virginia')
va.tract <- tracts(state = 'Virginia')
va.vote <- voting_districts(state = 'Virginia')

# Merging h.counts and the Virginia shapefile

va.counties <- left_join(va.counties, h.counts, by = 'GEOID')

# Setting up plot color settings using paletteer

nColor <- 13
colors <- paletteer_c(palette = 'ggthemes::Gray', n = nColor, direction = 1)

# Plotting only the VCE agents

plot(va.counties$geometry, col = colors[va.counties$Hospitals], main = 'VCE Agents and Hospital Counts')
points(vce$Long, vce$Lat, pch = 18, col = 'red', cex = 2)






