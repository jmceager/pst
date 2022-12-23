library(tidyverse)
library(sf)
library(maps)

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

#retreive clean state and location names
locCleaner <- function(df, loc, lat, lon, org, state){
  #set up phmsa points
  x <- data.frame(X=df[[lon]], Y = df[[lat]], O = df[[org]], 
                  I = df[[loc]], S = df[[state]]) %>%
    mutate(run = case_when((grepl("NA", I) 
                            | grepl("N/A", I) 
                            | grepl("Municipality", I) 
                            | grepl(" Miles", I)
                            | grepl("[[:digit:]]", I)
                            | is.na(I)) ~ T,
                           TRUE ~ F))
  #equidistant conic
  projStr <- "+proj=longlat +datum=WGS84"
  crs <- CRS(projStr)
  # transform to simple features
  x_sf <- st_as_sf(x,
                   agr = NA_agr_,
                   coords = c("X", "Y"),
                   crs = crs,
                   dim = "XY",
                   remove = TRUE,
                   na.fail = TRUE,
                   sf_column_name = NULL
  )%>%
    #transform crs to same as census 
    st_transform(4269)
  
  
  ## main loop: probably has to be a way to vectorize / conditionalize 
  for (i in 1:nrow(x_sf)){
    if (x[i, "run"] == T){
      # for on land county finding 
      if (is.na(x[i, "O"])){
        x_sf[i, ] <- st_join(x_sf[i, ], land, join = st_within, left = T) %>%
          mutate(I = paste0(NAME, " County, ", state.abb[match(STATE_NAME, state.name)]),
                 S = coalesce(S, STATE_NAME))%>%
          select(setdiff(colnames(x_sf), colnames(land)))
      }
      # for OCS incidents
      else if (grepl("OCS", x[i, "O"])){
        x_sf[i, ] <- st_join(x_sf[i, ], water, join = st_within, left = TRUE) %>%
          mutate(I = paste0("Outer Continental Shelf: ", 
                            case_when(OCS_ABV == "OCSG" ~ "Gulf of Mexico",
                                      OCS_ABV == "OCSAT" ~ "Atlantic",
                                      OCS_ABV == "OCSP" ~ "Pacific")),
                 S = OCS_ABV
          )%>%
          select(setdiff(colnames(x_sf), colnames(water)))
      }
      # for offshore state waters that fell through cracks 
      else{
        x_sf[i, ] <- st_join(x_sf[i, ], land, join = st_nearest_feature, left = TRUE)%>%
          mutate(I = paste0(STATE_NAME, " State Waters"))%>%
          select(setdiff(colnames(x_sf), colnames(land)))
      }
    }
  }
  
  #only return loc and state, clean df and colnames, bind to df
  cbind(df, select(x_sf, I, S)) %>%
    mutate(cleanLoc = I,
           STATE = S)%>%
    select(-c(I,S))
}