snap <- read.csv("snap_agents.csv")
base_snap <- read.csv("base_agg_snap.csv")
snap_agents <- left_join(snap, base_snap, by= "Agent" )
write.csv(snap_agents, "snap.csv")


#creating icon map
library(leaflet)
library(sf)

# generate example data
library(leaflet)
NHL <- makeIcon(
  iconUrl = "https://www-league.nhlstatic.com/images/logos/league-dark/133-flat.svg",
  iconWidth = 31*215/230,
  iconHeight = 31, 
  iconAnchorY = 16,
  iconAnchorX = 31*215/230/2)
MLB <- makeIcon(
  iconUrl = "https://www.mlbstatic.com/team-logos/league-on-dark/1.svg",
  iconWidth = 31*215/230,
  iconHeight = 31, 
  iconAnchorY = 16,
  iconAnchorX = 31*215/230/2)
MLS <-makeIcon(
  iconUrl = "https://league-mp7static.mlsdigital.net/styles/non-retina_desktop_logo/s3/logo25-77x77_0.png?LzMdhn2DU4GXKEjKfJ2QYWMaQKQIk7VQ&itok=ZtYZ58tI",
  iconWidth = 31*215/230,
  iconHeight = 31, 
  iconAnchorY = 16,
  iconAnchorX = 31*215/230/2)
NBA <-makeIcon(
  iconUrl = "https://seeklogo.net/wp-content/uploads/2014/09/NBA-logo.png",
  iconWidth = 31*215/230,
  iconHeight = 31, 
  iconAnchorY = 16,
  iconAnchorX = 31*215/230/2)
NFL <-makeIcon(
  iconUrl = "https://static.nfl.com/static/content/public/static/wildcat/assets/img/application-shell/shield/default.svg",
  iconWidth = 31*215/230,
  iconHeight = 31, 
  iconAnchorY = 16,
  iconAnchorX = 31*215/230/2)
set.seed(2020)
venues <- c('NHL', 'MLB', 'MLS', 'NBA', 'NFL')
nc <- st_read(system.file("shape/nc.shp", package="sf"))

df <- st_sample(nc, 5) %>%
  st_coordinates() %>%
  as.data.frame 
df$league <- venues

df

#>           X        Y league
#> 1 -78.58785 35.94350    NHL
#> 2 -80.82830 35.88732    MLB
#> 3 -78.83967 36.11236    MLS
#> 4 -80.09532 35.01562    NBA
#> 5 -83.72636 35.33204    NFL
#> # create iconSet
iconSet <- iconList(NHL= NHL,
                    MLB =MLB, 
                    MLS = MLS,
                    NBA = NBA,
                    NFL = NFL)

# map
leaflet(df) %>%
  addTiles() %>%
  addMarkers(lng=~X, lat=~Y, icon = ~iconSet[league]) 