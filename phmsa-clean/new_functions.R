gt.sample <- gt.full %>% slice_sample(n = 50)

lon <- gt.sample$LOCATION_LONGITUDE
lat <- gt.sample$LOCATION_LATITUDE
org <- gt.sample$OFF_ACCIDENT_ORIGIN
loc <- gt.sample$ILOC
state <- coalesce(gt.sample$ONSHORE_STATE_ABBREVIATION, gt.sample$OFFSHORE_STATE_ABBREVIATION)

bad_location_names = c("NA", "N/A", "a Municipality", " Miles", "[[:digit:]]")

#load census state and county data
land <- st_read("data/gis/census_county/cb_2021_us_county_5m.shp") %>%
  # select useful cols 
  select(GEOID, NAME, STATE_NAME)
#load us water reference
water <- st_read("data/gis/hydro/us_hydro.shp") %>%
  #keep names, create abrev. that match mileage data 
  select(Name1) %>%
  mutate(OCS_ABV = case_when(Name1 == "Pacific Ocean" ~ "OCSP",
                             Name1 == "Atlantic Ocean" ~ "OCSAT",
                             Name1 == "Gulf of Mexico" ~ "OCSG")) %>%
  st_transform(4269)

# plot(states)
# plot(x_sf, add = T)

points <- tranData %>%
  st_as_sf(agr = NA_agr_,
           coords = c("LOCATION_LONGITUDE", "LOCATION_LATITUDE"),
           crs = crs,
           dim = "XY",
           remove = TRUE,
           na.fail = TRUE,
           sf_column_name = NULL
  )%>%
  #transform crs to same as census 
  st_transform(4269)

# fix improperly documented lats and longs 
latlongizer <- function( vals, type = c("lat", "lon")){
  if(type == "lat"){
    fix <- sapply(vals, 
                  function(x) if_else(x < 0, 
                                      x *-1, 
                                      x))
  }
  else if (type == "lon"){
    fix <- sapply(vals, 
                  function(x) if_else(between(x, -50, 130), 
                                      x *-1, 
                                      x))
  }
  else{
    fix = "Error: need lat or lon as type"
  }
  return(fix)
}

#vectorize original locCleaner
#assume lat and lon are already cleaned 
#state and origina null to handle offshore / OCS incidents
locCleaner <- function(loc, lat, lon, 
                       on_state = NULL, off_state = NULL, org = NULL){
  state = ifelse(is.null(on_state), off_state, on_state)
  xy <- tibble( x = lon, y = lat, state = state)
  locFix = NULL
  #bad location names
  if(grepl(paste(bad_location_names, collapse = "|"), loc)){
    # check origin 
    if(grepl("OCS", org)){
      
    }
    else if(grepl("STATE")){
      
    }
    else{
      
    }
  }
  #good location names  
  else{
    locFix = loc
  }
  
}



