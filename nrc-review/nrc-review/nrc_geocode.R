library(ggmap)
library(tidyverse)
library(readxl)
library(lubridate)

#list of interested materials
materials <- c("CRUDE", "GAS", "PETROLEUM","KEROSENE","ETHYLENE","DIESEL", "OIL")

#sizes for systems 
gasBreaks <- c(0, 40, 1000, 8000, 600000)
liqBreaks <- c(0, 20, 100, 1000, 5000000)
breakLab <- c(3,6,9,12,15)

nrcGeo <- function(path){
  temp = tempfile(fileext = ".xlsx")
  download.file(path, destfile=temp, mode='wb')
  
  pipes <- read_xlsx(temp, sheet = 2) %>%
    mutate(
      INCIDENT_DATE_TIME = parse_date_time(INCIDENT_DATE_TIME, orders = "mdY HM"),
      INC_DATE = dmy(paste(day(INCIDENT_DATE_TIME), month(INCIDENT_DATE_TIME), year(INCIDENT_DATE_TIME), sep = "-")),
      LOC_FIRST = coalesce(LOCATION_NEAREST_CITY, LOCATION_COUNTY),
      LOC_FULL = paste(LOC_FIRST, LOCATION_STATE, sep = ", ")
    ) %>%
   # filter(INC_DATE >= ymd()) %>%
    left_join( read_xlsx(temp, sheet = 3), by = "SEQNOS") %>% 
    filter(TYPE_OF_INCIDENT == "PIPELINE") %>%
    mutate(human = if_else( ANY_EVACUATIONS == "Y" | 
                              ANY_INJURIES == "Y" |
                              ANY_FATALITIES == "Y",
                            "H",
                            "NA"),
           dmg = if_else(ANY_DAMAGES == "Y"|
                           FIRE_INVOLVED == "Y",
                         "D",
                         "NA"),
           hd = paste0(human, dmg),
           hd = str_replace(hd, "NANA", "None"),
           hd = str_remove(hd, "NA"))   %>%    
    left_join( read_xlsx(temp, sheet = 3), by = "SEQNOS", suffix = c("",".x"))%>%    #incident details
    left_join( read_xlsx(temp, sheet = 4), by = "SEQNOS", suffix = c("",".y"))%>%    #incident details
    left_join( read_xlsx(temp, sheet = 5), by = "SEQNOS", suffix = c("",".z"))%>%   # material details 
    left_join( read_xlsx(temp, sheet = 1) %>% select(SEQNOS, RESPONSIBLE_COMPANY), #operator info 
               by = "SEQNOS", suffix = c("",".c"))%>%
    filter((hd != "None" | MEDIUM_DESC == "WATER") & 
             grepl(paste(materials, collapse="|"), NAME_OF_MATERIAL, fixed = F)) %>%
    mutate(NUMBER_EVACUATED = replace_na(NUMBER_EVACUATED,0),
           NUMBER_INJURED = replace_na(NUMBER_INJURED, 0),
           NUMBER_FATALITIES = replace_na(NUMBER_FATALITIES, 0),
           PIPELINE_TYPE = str_trunc(PIPELINE_TYPE,8),
           BODY_OF_WATER = replace_na(BODY_OF_WATER, "None"),
           protocol = if_else(grepl("///",DESCRIPTION_OF_INCIDENT),"Y","Z"),
           SEQNOS = str_sub(as.character(SEQNOS), -3))%>%
    arrange(INC_DATE, protocol)%>%
    distinct(LOC_FULL,INC_DATE, .keep_all = T)
  
  pipes_geocode <- geocode(pipes$LOC_FULL)
  pipes_geocode <- cbind(pipes, pipes_geocode)
  pipes_geocode <- pipes_geocode %>%
    mutate(lat = if_else(is.na(lat),
                         LAT_DEG + (LAT_MIN/60) + (LAT_SEC/3600),
                         lat),
           lon = if_else(is.na(lon),
                         LONG_DEG + (LONG_MIN/60) + (LONG_SEC/3600),
                         lon),
           AMOUNT_OF_MATERIAL = if_else(UNIT_OF_MEASURE == "BARREL(S)",
                                        AMOUNT_OF_MATERIAL*42,
                                        AMOUNT_OF_MATERIAL),
           UNIT_OF_MEASURE = if_else(UNIT_OF_MEASURE == "BARREL(S)",
                                     "GALLON(S)",
                                     UNIT_OF_MEASURE),
           size = if_else(SYS == "Gas", 
                          cut(AMOUNT_OF_MATERIAL, breaks = gasBreaks, labels = breakLab),
                          cut(AMOUNT_OF_MATERIAL, breaks = liqBreaks, labels = breakLab))
           )
  
  
  return(pipes_geocode)
}