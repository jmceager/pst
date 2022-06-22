library(tidyverse)
library(rgeos)
library(rgdal)
library(sf)
library(maps)
library(maptools)


cleanLoc <- function(df, col, lat, lon, org = NULL){
  
  #clean bad lat longs 
  df <- df %>%
    mutate(LOCATION_LONGITUDE = if_else(.data[[lon]] < -180, 
                                        .data[[lon]]/100000,
                                        .data[[lon]]))%>%
    mutate(LOCATION_LONGITUDE = if_else(.data[[lon]] > 0, 
                                        .data[[lon]]*-1,
                                        .data[[lon]]))
  
  #separate good locations
  goodLoc <- df %>%
    filter(!grepl("NA", .data[[col]]) 
           & !grepl("Municipality", .data[[col]]) 
           & !grepl(" Miles", .data[[col]])) %>%
    mutate(cleanLoc = .data[[col]])
  
  #run gis on bad location
  if(is.null(org)){
    badLoc <- df %>%
      filter(grepl("NA", .data[[col]]) 
             | grepl("Municipality", .data[[col]]) 
             | grepl(" Miles", .data[[col]])) %>%
      mutate(cleanLoc = glue(lat = .data[[lat]], lon = LOCATION_LONGITUDE))
  }
  else{
    badLoc <- df %>%
      filter(grepl("NA", .data[[col]]) 
             | grepl("Municipality", .data[[col]]) 
             | grepl(" Miles", .data[[col]])) %>%
      mutate(cleanLoc = glue(lat = .data[[lat]], lon = LOCATION_LONGITUDE, org = .data[[org]]))
  }
  
  #return clean DF
  return( rbind(goodLoc, badLoc) )
}


glue <- function(lat, lon, org = NULL){
  state <- locState(lat = lat, lon = lon)
  if(is.null(org)){
    county <- locCounty(lat = lat, lon = lon)
  }
  else{
    county <- locCounty(lat = lat, lon = lon, org = org)
  }
  place <- rep("",length(state))
  #naming places based on origin 
  #most state waters get a county assigned already
  for(i in 1:length(place)){
    if(str_detect("Outer Continental Shelf",county)){
      place[i] <- paste0(county, ", near ", state)
    }
    else if(str_detect("State Waters", county)){
      place[i] <- paste0(state, " ", county)
    }
    else{
      place[i] <- paste0(county, ", ", state)
    }
  }
  place
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
locCounty <- function(lat, lon, org = NULL){
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
  
  if(is.null(org)){
    offshore <- rep("Offshore", length(counties))
  }
  else{
    offshore <- org %>%
      str_replace("ON THE OUTER CONTINENTAL SHELF", "Outer Continental Shelf") %>%
      str_remove(" \\s*\\([^\\)]+\\)")%>% # remove (OCS) from string
      str_replace("IN STATE WATERS", "State Waters")
  }

  counties <- coalesce(counties, offshore)
  counties <- sub(".*,","",counties)
  counties <- if_else(counties %in% offshore, counties, paste(counties, "County"))
  counties <- str_to_title(counties)
  
  counties
  
}

