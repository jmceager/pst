library(tidyverse)
library(lutz)
library(lubridate)
library(readxl)

source("offshorefinder.R")

#### Data Setup ####
## TODO: download zip direct from PHMSA website
## TODO: unpack zip to raw/ directory and continue cleaning 
## TODO: replace 2022 miles with 2021 numbers 

temp <- tempfile()
download.file("https://www.phmsa.dot.gov/sites/phmsa.dot.gov/files/data_statistics/pipeline/PHMSA_Pipeline_Safety_Flagged_Incidents.zip", temp)
distData <- read_xlsx(unzip(temp, files = "gd2010toPresent.xlsx"), sheet = 2)
tranData <- read_xlsx(unzip(temp, files = "gtggungs2010toPresent.xlsx"), sheet = 2)
hzrdData <- read_xlsx(unzip(temp, files = "hl2010toPresent.xlsx"), sheet = 2)

#### MILEAGE DATA ####

## getting mileage data 
mileCols <- c("Operator.ID", "Operator.Business.Name",
              "system", "Calendar.Year", "State.Abbreviation", 
              "Total.Miles.by.Decade", "Total.By.Decade.Miles")

miles <- read.csv("data/raw/GD_MilesDecadeAge.csv") %>% 
  mutate(system = "GD (Gas Distribution)") %>% 
  select(any_of(mileCols))%>%
  rbind(select(
    read.csv("data/raw/GT_MilesDecadeAge.csv"), 
    any_of(mileCols)) %>% 
      mutate(system = "GT (Gas Transmission)") %>%
      rename(Total.Miles.by.Decade = Total.By.Decade.Miles)
  )%>%
  rbind(select(
    read.csv("data/raw/HL_MilesDecadeAge.csv"), 
    any_of(mileCols)) %>% 
      mutate(system = "HL (Hazardous Liquids)")
  )%>%
  #make colnames match incident data
  rename(mileage = Total.Miles.by.Decade, 
         SYSTEM_TYPE = system,
         OPERATOR_ID = Operator.ID,
         STATE = State.Abbreviation,
         IYEAR = Calendar.Year,
         NAME = Operator.Business.Name)%>%
  #give snapshots of operator's yearly mileage to match incident years
  group_by(OPERATOR_ID, NAME, SYSTEM_TYPE, STATE, IYEAR)%>% 
  summarise(mileage = sum(mileage, na.rm = T))%>%
  mutate(SYS = str_sub(SYSTEM_TYPE, 1,2)) %>% 
  ungroup()

miles <- miles %>%
  rbind(miles %>% filter(IYEAR == max(IYEAR)) %>% mutate(IYEAR = max(IYEAR)+1) )

## note: possible to add mileage for install_decade? 
## note: possible handling of on/offshore mileage 

#### OPERATOR DATA ####
# from safety program 
safe <- read_csv("data/raw/Safety_Program_Data.csv", skip = 2)


ops<-safe %>%
  mutate(sub.id = SUBMITTING_OPID,
         sub.name = SUBMITTING_OPID_NAME,
         sub.status = SUBMITTING_OPID_STATUS,
         sub.sys = SYSTEM_TYPE,
         pri.id = `D&A_PRIMARY_OPID`,
         pri.id = coalesce(OME_PRIMARY_OPID, `D&A_PRIMARY_OPID`, 
                           IM_PRIMARY_OPID, OPA_PRIMARY_OPID,
                           PA_PRIMARY_OPID, DP_PRIMARY_OPID, 
                           OQ_PRIMARY_OPID, CRM_PRIMARY_OPID),
         pri.name = coalesce(OME_PRIMARY_NAME, `D&A_PRIMARY_NAME`, 
                             IM_PRIMARY_NAME, OPA_PRIMARY_NAME,
                             PA_PRIMARY_NAME, DP_PRIMARY_NAME, 
                             OQ_PRIMARY_NAME, CRM_PRIMARY_NAME),
         pri.status = coalesce(OME_STATUS, `D&A_STATUS`, 
                               IM_STATUS, OPA_STATUS,
                               PA_STATUS, DP_STATUS, 
                               OQ_STATUS, CRM_STATUS)
  )%>%
  select(sub.id,sub.name,sub.sys,sub.status,pri.id,pri.name, pri.status) %>%
  distinct() %>%
  mutate(pri.name = if_else(str_detect(pri.name,regex("enbridge",ignore_case = TRUE)),
                            "Enbridge",pri.name
  ),
  pri.name = if_else(str_detect(pri.name,regex("BP",ignore_case = TRUE)),
                     "BP",pri.name
  ),
  pri.name = if_else(str_detect(pri.name,regex("kinder morgan",ignore_case = TRUE)),
                     "Kinder Morgan",pri.name
  )
  )

ops.simple <- ops %>%
  select(sub.id, sub.name, pri.id, pri.name)

#### INCIDENT DATA ####

# gd big 
gd.full <- distData %>% 
  mutate(SYSTEM_TYPE = "Gas Distribution", 
         SYS = "GD",
         UNINTENTIONAL_RELEASE = replace_na(UNINTENTIONAL_RELEASE,0), 
         INTENTIONAL_RELEASE = replace_na(INTENTIONAL_RELEASE,0),
         ON_OFF_SHORE = "ONSHORE",
         UNITS = "mscf",
         TOTAL_RELEASE = UNINTENTIONAL_RELEASE + INTENTIONAL_RELEASE,
         TOTAL_COST_CURRENT = replace_na(parse_number(TOTAL_COST_CURRENT), 0),
         EXPLODE_IND = replace_na(EXPLODE_IND, "NO"),
         IGNITE_IND = replace_na(IGNITE_IND, "NO"),
         FATAL = replace_na(FATAL, 0),
         INJURE = replace_na(INJURE, 0),
         MDY = date(LOCAL_DATETIME),
         IMONTH = month(MDY),
         MoYr = my(paste(IMONTH,IYEAR, sep = "-")),
         MSYS = "Gas",
         ILOC = paste(str_to_title(LOCATION_CITY_NAME),LOCATION_STATE_ABBREVIATION, sep = ", "),
         STATE = LOCATION_STATE_ABBREVIATION) %>%
  locCleaner(., "ILOC", lat= "LOCATION_LATITUDE", lon = "LOCATION_LONGITUDE")%>%
  left_join(miles, by = c("OPERATOR_ID", "SYS", "STATE","IYEAR"))%>%
  mutate(mileage = replace_na(mileage, 0)) %>%
  left_join(ops.simple, by = c("OPERATOR_ID" = "sub.id"))%>% 
  distinct(NARRATIVE, .keep_all = T)%>%
  rename(NAME = NAME.x)


## add safety data to mileage for consistency 
miles <- left_join(miles, ops.simple, by = c("OPERATOR_ID" = "sub.id")) 

# gt big
gt.full <- tranData %>%
  mutate(SYS = gsub( " .*$", "", SYSTEM_TYPE ), #accounts for UNGS and GD
         UNINTENTIONAL_RELEASE = replace_na(UNINTENTIONAL_RELEASE,0), 
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
         SYSTEM_TYPE = gsub("[\\(\\)]", "", 
                            regmatches(SYSTEM_TYPE, 
                                       gregexpr("\\(.*?\\)", SYSTEM_TYPE))),
         MoYr = my(paste(IMONTH,IYEAR, sep = "-")),
         MSYS = "Gas",
         ILOC =  if_else(ON_OFF_SHORE == "ONSHORE", 
                         paste(str_to_title(ONSHORE_CITY_NAME), ONSHORE_STATE_ABBREVIATION,
                               sep = ", "),
                         if_else(is.na(OFFSHORE_COUNTY_NAME),
                                 "NA",
                                 paste(paste(str_to_title(OFFSHORE_COUNTY_NAME), "County Waters", sep = " "),
                                       OFFSHORE_STATE_ABBREVIATION,
                                       sep = ", "))
                         
                          ),
         STATE = coalesce(ONSHORE_STATE_ABBREVIATION, OFFSHORE_STATE_ABBREVIATION),
         STATE = if_else(is.na(STATE) & grepl("OCS", OFF_ACCIDENT_ORIGIN),
                         "OCS",
                         STATE),
         STATE = if_else(is.na(STATE),locState(LOCATION_LATITUDE, LOCATION_LONGITUDE),STATE)  
    )%>%
  locCleaner(.,"ILOC","LOCATION_LATITUDE","LOCATION_LONGITUDE", "OFF_ACCIDENT_ORIGIN")%>%
  left_join(miles, by = c("OPERATOR_ID", "SYS", "STATE","IYEAR"))%>%
  mutate(mileage = replace_na(mileage, 0))%>%
  left_join(ops.simple, by = c("OPERATOR_ID" = "sub.id"))%>% 
  distinct(NARRATIVE, .keep_all = T)%>%
  rename(pri.id = pri.id.x,
         pri.name = pri.name.x,
         NAME = NAME.x)
  

# hl big
hl.full <- hzrdData %>% 
  mutate(SYSTEM_TYPE = "Hazardous Liquids",
         SYS = "HL")%>%
  rename( INTENTIONAL_RELEASE = INTENTIONAL_RELEASE_BBLS,
          UNINTENTIONAL_RELEASE = UNINTENTIONAL_RELEASE_BBLS)%>%
  mutate(UNINTENTIONAL_RELEASE = replace_na(UNINTENTIONAL_RELEASE,0)*42, 
         INTENTIONAL_RELEASE = replace_na(INTENTIONAL_RELEASE,0)*42,
         UNITS = "US Gal.",
         TOTAL_RELEASE = UNINTENTIONAL_RELEASE + INTENTIONAL_RELEASE,
         TOTAL_COST_CURRENT = replace_na(parse_number(TOTAL_COST_CURRENT), 0),
         EXPLODE_IND = replace_na(EXPLODE_IND, "NO"),
         IGNITE_IND = replace_na(IGNITE_IND, "NO"),
         FATAL = replace_na(FATAL, 0),
         INJURE = replace_na(INJURE, 0),
         MDY = date(LOCAL_DATETIME),
         IMONTH = month(MDY),
         MoYr = my(paste(IMONTH,IYEAR, sep = "-")),
         MSYS = "HL",
         ILOC =  if_else(ON_OFF_SHORE == "ONSHORE", 
                         paste(str_to_title(ONSHORE_CITY_NAME), ONSHORE_STATE_ABBREVIATION,
                               sep = ", "),
                         if_else(is.na(OFFSHORE_COUNTY_NAME),
                                 "NA",
                                 paste(paste(str_to_title(OFFSHORE_COUNTY_NAME), "County Waters", sep = " "),
                                       OFFSHORE_STATE_ABBREVIATION,
                                       sep = ", "))
                         
                         ),
         STATE = coalesce(ONSHORE_STATE_ABBREVIATION, OFFSHORE_STATE_ABBREVIATION),
         STATE = if_else(is.na(STATE) & grepl("OCS", OFF_ACCIDENT_ORIGIN),
                         "OCS",
                         STATE),
         STATE = if_else(is.na(STATE),locState(LOCATION_LATITUDE, LOCATION_LONGITUDE),STATE) 
        )%>%
  locCleaner(.,"ILOC","LOCATION_LATITUDE","LOCATION_LONGITUDE", "OFF_ACCIDENT_ORIGIN")%>%
  left_join(miles, by = c("OPERATOR_ID", "SYS", "STATE","IYEAR"))%>%
  mutate(mileage = replace_na(mileage, 0))%>%
  left_join(ops.simple, by = c("OPERATOR_ID" = "sub.id"))%>% 
  distinct(NARRATIVE, .keep_all = T)%>%
  rename(pri.id = pri.id.x,
         pri.name = pri.name.x,
         NAME = NAME.x)


#### JOINS, BINDS ####

#columns for abridged incidents
short_cols <- c( "REPORT_NUMBER", "NAME","OPERATOR_ID",  #basic characteristics
                 "IYEAR","MDY","MoYr" ,"LOCAL_DATETIME" ,   #temporal char
                 "LOCATION_LATITUDE","LOCATION_LONGITUDE", "cleanLoc", "STATE", "ON_OFF_SHORE",#location
                 "SYS","MSYS" ,"SIGNIFICANT", "SERIOUS","COMMODITY_RELEASED_TYPE",#inc summary
                 "UNINTENTIONAL_RELEASE", "INTENTIONAL_RELEASE", "TOTAL_RELEASE","UNITS", #releases
                 "FATALITY_IND","FATAL", "INJURY_IND","INJURE", #human impact
                 "EXPLODE_IND","IGNITE_IND" ,  "NUM_PUB_EVACUATED", "TOTAL_COST_CURRENT",#impact 2
                 "INSTALLATION_YEAR", "SYSTEM_PART_INVOLVED", #inc char 
                 "CAUSE","CAUSE_DETAILS", "NARRATIVE", #inc char 
                 "mileage", "pri.id","pri.name")  #joined char

#abridged all inc  
all.inc <- rbind(select(hl.full, all_of(short_cols)), 
                 select(gt.full, all_of(short_cols)), 
                 select(gd.full, all_of(short_cols))) 



#### WRITING EXPORTS ####
  
#csvs for each full table
write_csv(gt.full, "data/clean/gt_inc.csv")
write_csv(gd.full, "data/clean/gd_inc.csv")
write_csv(hl.full, "data/clean/hl_inc.csv")


#csv for abridged all incidents
write_csv(all.inc, "data/clean/all_inc.csv")

#csv for mileage numbers 
write_csv(miles, "data/clean/sys_miles.csv")

#csv for op match 
write.csv(ops, "data/clean/operator_id_match.csv")
