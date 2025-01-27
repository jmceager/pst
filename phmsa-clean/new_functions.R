library(tidyverse)
library(sf)
library(maps)

# gt.sample <- gt.full %>% slice_sample(n = 100)
# 
# lon <- gt.sample$LOCATION_LONGITUDE
# lat <- gt.sample$LOCATION_LATITUDE
# org <- gt.sample$OFF_ACCIDENT_ORIGIN
# loc <- gt.sample$ILOC
# state <- gt.sample$STATE
# op <- gt.sample$OPERATOR_ID
# rn <- gt.sample$REPORT_NUMBER
# sys <- rep("GT", nrow(gt.sample))

bad_location_names = c("NA", "N/A", "Municipality", " Miles", "[[:digit:]]", "Near", "Outer Continental Shelf")

#load census state and county data
land <- st_read("data/gis/census_county/cb_2021_us_county_5m.shp") %>%
  # select useful cols 
  select(GEOID, NAME, STATE_NAME) %>%
  mutate(
    stab = state.abb[match(STATE_NAME, state.name)],
    STATE_ABB = case_when(
      !is.na(stab) ~ stab,
      STATE_NAME == "Puerto Rico" ~ "PR",
      STATE_NAME == "District of Columbia" ~ "DC",
      .default = NA
    )
  ) %>%
  select(!stab)
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

# points <- tranData %>%
#   st_as_sf(agr = NA_agr_,
#            coords = c("LOCATION_LONGITUDE", "LOCATION_LATITUDE"),
#            crs = crs,
#            dim = "XY",
#            remove = TRUE,
#            na.fail = TRUE,
#            sf_column_name = NULL
#   )%>%
#   #transform crs to same as census 
#   st_transform(4269)

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

ocs_fixer <- function(df, xy){
  
  #get name of nearest ocs water
  nearest_water_name <- st_join( xy, water,join = st_nearest_feature)[["Name1"]]
  nearest_water_shp <- water %>% filter(Name1 == nearest_water_name)
  #filter for points in that water
  df_sf <- df%>%
    st_as_sf(
    agr = NA_agr_,
    coords = c("LOCATION_LONGITUDE", "LOCATION_LATITUDE"),
    crs = crs,
    dim = "XY",
    remove = FALSE,
    na.fail = TRUE,
    sf_column_name = NULL
  ) %>%
    st_transform(4269) %>%
    filter(grepl("OCS", OFF_ACCIDENT_ORIGIN)) %>%
    st_filter(.,nearest_water_shp)
  
  #get distance from df points
  dist = st_distance(xy, df_sf)
  #get closest few indices
  closest = order(dist)[1:6]
  #get average lat longs
  df_lat <- df_sf %>% st_drop_geometry() %>% pull(LOCATION_LATITUDE)
  df_lon <- df_sf %>% st_drop_geometry() %>% pull(LOCATION_LONGITUDE)
  new_lat = mean(df_lat[closest], na.rm = T)
  new_lon = mean(df_lon[closest], na.rm = T)
  
  #get new point (maybe fixed who knows)
  new_xy <- tibble(
    x = new_lon,
    y = new_lat, 
    state = xy$state,
    op=xy$op,
    rn = xy$rn
  ) %>%
    st_as_sf(
      agr = NA_agr_,
      coords = c("x", "y"),
      crs = crs,
      dim = "XY",
      remove = TRUE,
      na.fail = TRUE,
      sf_column_name = NULL
    ) %>%
    st_transform(4269)
  
  return(new_xy)
  
}
# 
# idx = 67
# loc = gt.sample$ILOC[[idx]]
# lat = gt.sample$LOCATION_LATITUDE[[idx]]
# lon = gt.sample$LOCATION_LONGITUDE[[idx]]
# state = gt.sample$STATE[[idx]]
# org = gt.sample$OFF_ACCIDENT_ORIGIN[[idx]]
# op = gt.sample$OPERATOR_ID[[idx]]
# rn = gt.sample$REPORT_NUMBER[[idx]]
# 
# 
# test <- gt.sample %>%
#   mutate(newloc = locCleaner(loc, lat, lon, state, org))
# 
# pmap(list(loc, lat,lon, op, rn, state, org, sys), locCleaner)
#vectorize original locCleaner
#assume lat and lon are already cleaned 
#state and origina null to handle offshore / OCS incidents
#odf is original data frame for some ocs functioning 
# rn is report number for troubleshooting 
#op is operator id number 
#ssy is to pick odf
locCleaner <- function(loc, lat, lon, 
                       op, rn, 
                       state, org = NULL, sys){
  if(sys == "GT"){
    o_df <- tranData
  }
  else if(sys == "HL"){
    o_df <- hzrdData
  }
  else{
    o_df <- distData
  }
  #get the important location details and turn it into an sf 
  xy <- tibble( x = lon, y = lat, state = state, op = op, rn = rn)%>%
    st_as_sf(
      agr = NA_agr_,
      coords = c("x", "y"),
      crs = crs,
      dim = "XY",
      remove = TRUE,
      na.fail = TRUE,
      sf_column_name = NULL
    ) %>%
    st_transform(4269)
  
  locFix = NULL
  #bad location names
  if(grepl(paste(bad_location_names, collapse = "|"), loc )|| 
           !is.na(org)){
    #grab state shapefile
    state_shape <- land %>% filter(STATE_ABB == state)
    # check origin 
    #bad loc name and OCS
    if(grepl("OCS", org)){
      locFix = paste0(st_join(xy, water, join = st_within, left = T)[["Name1"]])
      if(is.na(locFix)){
        new_xy = ocs_fixer(df =o_df, xy = xy)
        locFix = paste0(st_join(new_xy, water, join = st_within, left = T)[["Name1"]])
      }
      else{}
    }
    #bad loc name and state waters
    else if(grepl("STATE", org)){
      locator = st_join(xy, land, join = st_nearest_feature, left = TRUE)
      locFix = paste0(locator$NAME, " County Waters, ", locator$STATE_ABB)
    }
    #bad loc name, onshore
    else{
      #get county NAME from st_join and paste into loc name
      locFix = paste0(st_join(xy, state_shape, join = st_within, left = T)[["NAME"]], 
                      " County, ", state)
    }
  }
  #good location names need no fixin' (hopefully)
  else{
    locFix = loc
  }
  return(locFix)
}

#function built to get proportion of mileage relevant to a primary op 
#basically: if OP A becomes the submitting op for pipe X on 3/1, 
#how do we handle that when it comes to incident rates? 
#in the cases where the operator doesnt have the mileage for a full year 
#maybe this is dumb idk 
day_yr <- function(x){
  ly = leap_year(x)
  dy = if_else(ly, 366, 365)
  (dy - yday(x))/dy
}


