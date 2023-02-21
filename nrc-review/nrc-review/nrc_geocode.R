library(ggmap)
library(tidyverse)
library(readxl)
library(lubridate)

#list of interested materials
materials <- c("CRUDE", "GAS", "PETROLEUM","KEROSENE","ETHYLENE","DIESEL", "OIL")

#wanted cols
cols <- c("SEQNOS", "LOC_FULL", "RESPONSIBLE_COMPANY","INC_DATE", "INCIDENT_DATE_TIME",
          "INCIDENT_CAUSE","NAME_OF_MATERIAL", "AMOUNT_OF_MATERIAL","UNIT_OF_MEASURE",
          "MEDIUM_DESC","ADDITIONAL_MEDIUM_INFO","ANY_DAMAGES","DAMAGE_AMOUNT",
          "FIRE_INVOLVED","ANY_INJURIES","NUMBER_INJURED","NUMBER_HOSPITALIZED",
          "ANY_FATALITIES","NUMBER_FATALITIES","NUMBER_EVACUATED",
          "lat","lon", "SYS", "size", "DESCRIPTION_OF_INCIDENT")


# current df 
current <- read_csv("current.csv")
curDate <- max(current$INC_DATE)

# get year and table name 
curYr <- year(curDate) - 2000
nameCY <- paste0("CY", curYr, ".xlsx")


nrcGeo <- function(df){
  if(nrow(df > 0)){
    pipes_geocode <- geocode(df$LOC_FULL)
    pipes_geocode <- cbind(df, pipes_geocode)
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
             SEQNOS = parse_number(SEQNOS),
             SYS = if_else(grepl("LIQUEFIED NATURAL GAS", NAME_OF_MATERIAL),
                           "LNG",
                           if_else(grepl("NATURAL GAS", NAME_OF_MATERIAL),
                                   "Gas",
                                   "Liquid"
                           ) #end second ifelse
             ),# end first ifelse
             size = if_else(SYS == "Gas", 
                            cut(AMOUNT_OF_MATERIAL, breaks = gasBreaks, labels = breakLab),
                            cut(AMOUNT_OF_MATERIAL, breaks = liqBreaks, labels = breakLab)),
             INCIDENT_DATE_TIME = format(INCIDENT_DATE_TIME, format = "%H:%M:%S"),
             UNIT_OF_MEASURE = if_else(UNIT_OF_MEASURE == "GALLON(S)",
                                       "Gal",
                                       if_else(grepl("UNKNOWN", UNIT_OF_MEASURE),
                                               "Unknown",
                                               if_else(grepl("CF", UNIT_OF_MEASURE),
                                                       "MSCF",
                                                       "NA")
                                       )
             )
      )
    
    
    return(pipes_geocode)
  }
  else {
    return()
  }
  
}



dfUpdate <- function(p, c, d){ #path, current, curDate 
  #set current.xlsx var
  curxl <- "current.xlsx"
  
  #load up the excel 
  temp = tempfile(fileext = ".xlsx")
  download.file(paste0("https://nrc.uscg.mil/FOIAFiles/", nameCY), destfile=curxl, mode='wb')
  
  read_xlsx(curxl, sheet = 2) %>% 
    mutate(
      INCIDENT_DATE_TIME = parse_date_time(INCIDENT_DATE_TIME, orders = "mdY HM"),
      INC_DATE = dmy(paste(day(INCIDENT_DATE_TIME), month(INCIDENT_DATE_TIME), year(INCIDENT_DATE_TIME), sep = "-")),
      LOC_FIRST = coalesce(LOCATION_NEAREST_CITY, LOCATION_COUNTY),
      LOC_FULL = paste(LOC_FIRST, LOCATION_STATE, sep = ", ")
    ) %>%
    filter(INC_DATE > d)%>%
    left_join( read_xlsx(curxl, sheet = 3), by = "SEQNOS") %>% 
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
    left_join( read_xlsx(curxl, sheet = 3), by = "SEQNOS", suffix = c("",".x"))%>%    #incident details
    left_join( read_xlsx(curxl, sheet = 4), by = "SEQNOS", suffix = c("",".y"))%>%    #incident details
    left_join( read_xlsx(curxl, sheet = 5), by = "SEQNOS", suffix = c("",".z"))%>%   # material details 
    left_join( read_xlsx(curxl, sheet = 1) %>% select(SEQNOS, RESPONSIBLE_COMPANY), #operator info 
               by = "SEQNOS", suffix = c("",".c"))%>%
    filter((hd != "None" | MEDIUM_DESC == "WATER") & 
             grepl(paste(materials, collapse="|"), NAME_OF_MATERIAL, fixed = F)) %>%
    mutate(NUMBER_EVACUATED = replace_na(NUMBER_EVACUATED,0),
           NUMBER_INJURED = replace_na(NUMBER_INJURED, 0),
           NUMBER_FATALITIES = replace_na(NUMBER_FATALITIES, 0),
           PIPELINE_TYPE = str_trunc(PIPELINE_TYPE,8),
           protocol = if_else(grepl("///",DESCRIPTION_OF_INCIDENT),"Y","Z"),
           SEQNOS = str_sub(as.character(SEQNOS), -3),
           DESCRIPTION_OF_INCIDENT = gsub('[\r\n]', '', DESCRIPTION_OF_INCIDENT))%>%
    arrange(INC_DATE, protocol)%>%
    distinct(LOC_FULL,INC_DATE, .keep_all = T) %>%
    nrcGeo()%>%
    rbind(c)
}

dfUpdate("current.csv", c = current, d = curDate) %>%
  write_csv(  "current.csv")




