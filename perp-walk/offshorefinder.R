library(tidyverse)
library(rgeos)
library(rgdal)
library(sf)
library(maps)
library(maptools)

# df <- read_csv("./data/all_inc.csv") %>%
#   filter(grepl("NA", ILOC) | grepl("Municipality", ILOC))%>%
#   select(LOCATION_LONGITUDE,LOCATION_LATITUDE)%>%
#   head(50)%>%
#   rename(X = 1, Y = 2)
# 
# 
# offshoreFinder(df)
# locCounty(lon = df$X,lat = df$Y)
# 
# all <- cbind(locCounty(lon = df$X,lat = df$Y), offshoreFinder(df) )

# x <- all.inc %>%
#   filter(grepl("NA", ILOC) | grepl("Municipality", ILOC))%>%
#   head(20)%>%
#   mutate(ILOC = glue(lat = LOCATION_LATITUDE,lon = LOCATION_LONGITUDE))

glue <- function(lat, lon){
  state <- locState(lat = lat, lon = lon)
  county <- locCounty(lat = lat, lon = lon)
  paste0(county, ", " ,state)
}


locState <- function(lat, lon){
  ### states
  #load state data
  states <- readOGR("./data/gis/us_states.shp")
  #set up projections
  x <- data.frame(X=lon, Y = lat)
  #equidistant conic
  projStr <- "+proj=longlat +datum=WGS84"
  crs <- CRS(projStr)
  coordinates(x) <- c("X","Y")
  proj4string(x) <- crs
  #x <- spTransform(x, crs)
  statesProj <- spTransform(states, crs)
  #stateList <- states$NAME
  # view(cbind(stateList, st_distance(st_as_sf(x[1,]), st_as_sf(statesProj), by_element = T)))
  # view(cbind(stateList, gDistance((x[1,]), (statesProj), byid = T)))
  ## Set up containers for results
  n <- length(x)
  nearState <- character(n)
  distState <- numeric(n)
  
  #other format things
  xSf <- st_as_sf(x)
  statesSf <- st_as_sf(statesProj)

  #loop
  for (i in seq_along(nearState)) {
    gDists <- st_distance(xSf[i,], statesSf, by_element=T)
    nearState[i] <- statesSf$NAME[which.min(gDists)]
    distState[i] <- min(gDists)
  }
  
  # state_converter <- data.frame(abb = state.abb, name = str_to_upper(state.name))
  # nearState <- data.frame(name = nearState) 
  # nearState <- left_join(nearState, state_converter)
  # nearState <- nearState$abb
  state.abb[match(nearState,state.name)]
}

#mutate(ILOC = if_else(grepl()))
locCounty <- function(lat, lon){
  x <- data.frame(X = lon, Y = lat)
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per county
  counties <- maps::map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
  counties_sp <- map2SpatialPolygons(counties, IDs=IDs,
                                     proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(x, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, counties_sp)
  
  # Return the county names of the Polygons object containing each point
  countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
  counties <- countyNames[indices]

  counties <- replace_na(counties, "Offshore")
  counties <- sub(".*,","",counties)
  counties <- if_else(counties == "Offshore", counties, paste(counties, "County"))
  counties <- str_to_title(counties)
  
  counties
  
}

