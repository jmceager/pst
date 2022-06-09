library(tidyverse)
library(lutz)
library(lubridate)
library(readxl)

source("offshorefinder.R")

#columns for gas
gas_cols <- c("NAME","OPERATOR_ID", "SIGNIFICANT", "IYEAR","LOCAL_DATETIME" , 
              "LOCATION_LATITUDE","LOCATION_LONGITUDE",
              "UNINTENTIONAL_RELEASE", "INTENTIONAL_RELEASE", "FATALITY_IND","FATAL",
              "INJURY_IND","INJURE","EXPLODE_IND","IGNITE_IND" ,  "NUM_PUB_EVACUATED", 
              "TOTAL_COST_CURRENT","CAUSE","COMMODITY_RELEASED_TYPE", "NARRATIVE")
#join
gd.clean <- read_xlsx("./data/incidents/gd2010toPresent.xlsx", sheet = 2) %>%
  select(all_of(  append(gas_cols,c("LOCATION_CITY_NAME","LOCATION_STATE_ABBREVIATION"))))%>%
  mutate(SYSTEM_TYPE = "GD (Gas Distribution)")%>%
  mutate(UNINTENTIONAL_RELEASE = replace_na(UNINTENTIONAL_RELEASE,0), 
         INTENTIONAL_RELEASE = replace_na(INTENTIONAL_RELEASE,0),
         UNITS = "mscf",
         TOTAL_RELEASE = UNINTENTIONAL_RELEASE + INTENTIONAL_RELEASE,
         TOTAL_COST_CURRENT = replace_na(parse_number(TOTAL_COST_CURRENT), 0),
         EXPLODE_IND = replace_na(EXPLODE_IND, "NO"),
         IGNITE_IND = replace_na(IGNITE_IND, "NO"),
         FATAL = replace_na(FATAL, 0),
         INJURE = replace_na(INJURE, 0),
         MDY = date(LOCAL_DATETIME),
         IMONTH = month(MDY),
         MSYS = "Gas",
         ILOC = paste(str_to_title(LOCATION_CITY_NAME),LOCATION_STATE_ABBREVIATION, sep = ", "))%>%
  select(!c(LOCATION_CITY_NAME,LOCATION_STATE_ABBREVIATION))
  

all.gas <- read_xlsx("./data/incidents/gtggungs2010toPresent.xlsx", sheet = 2) %>%
  select(all_of(c(gas_cols, "SYSTEM_TYPE", "ON_OFF_SHORE",
                          "ONSHORE_CITY_NAME","OFFSHORE_COUNTY_NAME",
                          "ONSHORE_STATE_ABBREVIATION", "OFFSHORE_STATE_ABBREVIATION")))%>%
  mutate(UNINTENTIONAL_RELEASE = replace_na(UNINTENTIONAL_RELEASE,0), 
         INTENTIONAL_RELEASE = replace_na(INTENTIONAL_RELEASE,0),
         UNITS = "mscf",
         TOTAL_RELEASE = UNINTENTIONAL_RELEASE + INTENTIONAL_RELEASE,
         TOTAL_COST_CURRENT = replace_na(parse_number(TOTAL_COST_CURRENT), 0),
         EXPLODE_IND = replace_na(EXPLODE_IND, "NO"),
         IGNITE_IND = replace_na(IGNITE_IND, "NO"),
         FATAL = replace_na(FATAL, 0),
         INJURE = replace_na(INJURE, 0),
         MDY = date(LOCAL_DATETIME),
         IMONTH = month(MDY),
         MSYS = "Gas",
         ILOC =  if_else(ON_OFF_SHORE == "ONSHORE", 
                         paste(str_to_title(ONSHORE_CITY_NAME), ONSHORE_STATE_ABBREVIATION,
                               sep = ", "),
                         paste(paste(str_to_title(OFFSHORE_COUNTY_NAME), "County Waters", sep = " "),
                               OFFSHORE_STATE_ABBREVIATION,
                               sep = ", ")
         )
  )%>%
  select(!c("ON_OFF_SHORE","ONSHORE_CITY_NAME","OFFSHORE_COUNTY_NAME",
            "ONSHORE_STATE_ABBREVIATION", "OFFSHORE_STATE_ABBREVIATION"))%>%
  rbind( gd.clean)
  
  

#fix hl to be similar
#cols
hl_cols <- c("NAME","OPERATOR_ID", "SIGNIFICANT", "IYEAR","LOCAL_DATETIME" , 
              "LOCATION_LATITUDE","LOCATION_LONGITUDE", 
              "ON_OFF_SHORE","ONSHORE_CITY_NAME","OFFSHORE_COUNTY_NAME",
             "ONSHORE_STATE_ABBREVIATION", "OFFSHORE_STATE_ABBREVIATION",
              "UNINTENTIONAL_RELEASE_BBLS", "INTENTIONAL_RELEASE_BBLS", "FATALITY_IND","FATAL",
              "INJURY_IND","INJURE","EXPLODE_IND","IGNITE_IND" ,  "NUM_PUB_EVACUATED", 
              "TOTAL_COST_CURRENT","CAUSE","COMMODITY_RELEASED_TYPE", "NARRATIVE")
#cleaning
all.hl <- read_xlsx("./data/incidents/hl2010toPresent.xlsx", sheet = 2) %>% 
  select(hl_cols)%>%
  mutate(SYSTEM_TYPE = "HL (Hazardous Liquids)")%>%
  rename( INTENTIONAL_RELEASE = INTENTIONAL_RELEASE_BBLS,
          UNINTENTIONAL_RELEASE = UNINTENTIONAL_RELEASE_BBLS)%>%
  mutate(UNINTENTIONAL_RELEASE = replace_na(UNINTENTIONAL_RELEASE,0), 
         INTENTIONAL_RELEASE = replace_na(INTENTIONAL_RELEASE,0),
         UNITS = "US BBLs",
         TOTAL_RELEASE = UNINTENTIONAL_RELEASE + INTENTIONAL_RELEASE,
         TOTAL_COST_CURRENT = replace_na(parse_number(TOTAL_COST_CURRENT), 0),
         EXPLODE_IND = replace_na(EXPLODE_IND, "NO"),
         IGNITE_IND = replace_na(IGNITE_IND, "NO"),
         FATAL = replace_na(FATAL, 0),
         INJURE = replace_na(INJURE, 0),
         MDY = date(LOCAL_DATETIME),
         IMONTH = month(MDY),
         MSYS = "HL",
         ILOC =  if_else(ON_OFF_SHORE == "ONSHORE", 
                         paste(str_to_title(ONSHORE_CITY_NAME), ONSHORE_STATE_ABBREVIATION,
                               sep = ", "),
                         paste(paste(str_to_title(OFFSHORE_COUNTY_NAME), "County Waters", sep = " "),
                               OFFSHORE_STATE_ABBREVIATION,
                               sep = ", ")
                         )
        ) %>%
  select(!c("ON_OFF_SHORE","ONSHORE_CITY_NAME","OFFSHORE_COUNTY_NAME",
            "ONSHORE_STATE_ABBREVIATION", "OFFSHORE_STATE_ABBREVIATION"))
  

all.inc <- rbind(all.gas, all.hl) %>%
  mutate(LOCATION_LONGITUDE = if_else(LOCATION_LONGITUDE < -180, 
                                      LOCATION_LONGITUDE/100000,
                                      LOCATION_LONGITUDE))%>%
  mutate(LOCATION_LONGITUDE = if_else(LOCATION_LONGITUDE > 0, 
                                      LOCATION_LONGITUDE*-1,
                                      LOCATION_LONGITUDE))%>%
  mutate( MoYr = my(paste(IMONTH,IYEAR, sep = "-"))) %>%
  mutate(STATE = str_sub(ILOC, start = -2, end = -1))


#location mutations, might as well
# to add: fix all x miles east places without fixing literal eight mile, al 
#in other words replace x miles east with y county, state
goodLoc <- all.inc %>%
  filter(!grepl("NA", ILOC) & !grepl("Municipality", ILOC))
  

badLoc <- all.inc %>%
  filter(grepl("NA", ILOC)| grepl("Municipality", ILOC))%>%
  mutate(ILOC = glue(lat = LOCATION_LATITUDE, lon = LOCATION_LONGITUDE))


## getting mileage data 
mileCols <- c("system", "Calendar.Year", "State.Abbreviation", "Operator.ID", 
              "Total.Miles.by.Decade", "Total.By.Decade.Miles")

miles <- read.csv("data/GD_MilesDecadeAge.csv") %>%
  mutate(system = "GD (Gas Distribution)") %>%
  select(any_of(mileCols))%>%
  rbind(select(
    read.csv("data/GT_MilesDecadeAge.csv"), 
    any_of(mileCols)) %>% 
      mutate(system = "GT (Gas Transmission)") %>%
      rename(Total.Miles.by.Decade = Total.By.Decade.Miles)
        )%>%
  rbind(select(
    read.csv("data/HL_MilesDecadeAge.csv"), 
    any_of(mileCols)) %>% 
      mutate(system = "HL (Hazardous Liquids)")
    )%>%
  rename(mileage = Total.Miles.by.Decade,
         SYSTEM_TYPE = system,
         OPERATOR_ID = Operator.ID)%>%
  filter(Calendar.Year == max(Calendar.Year))%>%
  group_by(OPERATOR_ID, SYSTEM_TYPE)%>%
  summarise(mileage = sum(mileage, na.rm = T))


new.inc <- rbind(goodLoc, badLoc) %>%
  left_join(miles, by = c("OPERATOR_ID", "SYSTEM_TYPE"))%>%
  mutate(mileage = replace_na(mileage, 0))


#need mileage


#csv for joined all incidents
write_csv(new.inc, "./data/all_inc.csv")


#Q's for perp walk: what about smaller events that result in more direct impact? smaller spill w/ death vs larger spill without? 

#build walk of shame csv
#all.inc %>%
#  group_by(MoYr, MSYS)%>%
#  slice(which.max(TOTAL_RELEASE))%>%
#  write_csv("./data/perp_walk.csv")

