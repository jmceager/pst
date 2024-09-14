library(tidyverse)
library(lutz)
library(lubridate)
library(readxl)
library(fuzzyjoin)

library(httr)

httr::set_config(config(http_version = 1.1))
source("new_functions.R")

crs = 4326

#### Data Setup ####
## TODO: unpack zip to raw/ directory and continue cleaning 
## TODO: replace 2022 miles with 2021 numbers 

temp <- tempfile()
download.file("https://www.phmsa.dot.gov/sites/phmsa.dot.gov/files/data_statistics/pipeline/PHMSA_Pipeline_Safety_Flagged_Incidents.zip", destfile =  temp, method = "curl")
distData <- read_xlsx(unzip(temp, files = "gd2010toPresent.xlsx"), sheet = 2)
tranData <- read_xlsx(unzip(temp, files = "gtggungs2010toPresent.xlsx"), sheet = 2)
hzrdData <- read_xlsx(unzip(temp, files = "hl2010toPresent.xlsx"), sheet = 2)
lngData <- read_xlsx(unzip(temp, files = "lng2011toPresent.xlsx"), sheet = 2)

#### MILEAGE DATA ####

## getting mileage data 
mileCols <- c("Operator.ID", "Operator.Business.Name",
              "system", "Calendar.Year", "State.Abbreviation", 
              "Total.Miles.by.Decade", "Total.By.Decade.Miles")

miles <- read.csv("data/raw/GD_MilesDecadeAge.csv") %>% 
  mutate(system = "GD (Gas Distribution)",
         services = select(., Unknown.services:X2020.2029.Number.of.Services) %>% rowSums(na.rm = T),
         Total.Miles.by.Decade = Total.Miles.by.Decade + (services * 0.0134) )%>%
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
safe <- read_xlsx("data/raw/Safety_Program_Data.xlsx", skip = 2, sheet = 2)


ops<-safe %>%
  mutate(sub.id = SUBMITTING_OPID,
         sub.name = SUBMITTING_OPID_NAME,
         sub.status = SUBMITTING_OPID_STATUS,
         sub.sys = SYSTEM_TYPE,
         pri.id = coalesce(#OME_PRIMARY_OPID, `D&A_PRIMARY_OPID`, 
                           IM_PRIMARY_OPID#, OPA_PRIMARY_OPID,
                           #PA_PRIMARY_OPID, DP_PRIMARY_OPID, 
                           #OQ_PRIMARY_OPID, CRM_PRIMARY_OPID
                           ),
         pri.name = coalesce(#OME_PRIMARY_NAME, `D&A_PRIMARY_NAME`, 
                             IM_PRIMARY_NAME#, OPA_PRIMARY_NAME,
                             # PA_PRIMARY_NAME, DP_PRIMARY_NAME, 
                             # OQ_PRIMARY_NAME, CRM_PRIMARY_NAME
                             ),
         pri.status = coalesce(#OME_STATUS, `D&A_STATUS`, 
                               IM_STATUS#, OPA_STATUS,
                               # PA_STATUS, DP_STATUS, 
                               # OQ_STATUS, CRM_STATUS
                               ),
         start = if_else(is.na(IM_ENTER_DATE), ymd("1200-12-12"),
                              ymd(IM_ENTER_DATE)),
         end = if_else(is.na(IM_EXIT_DATE), ymd(Sys.Date()),
                      ymd(IM_EXIT_DATE))
  )%>%
  select(sub.id,sub.name,sub.sys,sub.status,pri.id,pri.name, pri.status, start,end) %>%
  distinct() %>% 
  mutate(pri.name = if_else(str_detect(pri.name,regex("enbridge",ignore_case = TRUE)),
                            "Enbridge",pri.name
  ),
  pri.name = if_else(str_detect(pri.name,regex("BP",ignore_case = TRUE)),
                     "BP",pri.name
  ),
  pri.name = if_else(str_detect(pri.name,regex("kinder morgan",ignore_case = TRUE)),
                     "Kinder Morgan",pri.name
  ),
  pri.name = if_else(str_detect(pri.name,regex("PGandE",ignore_case = TRUE)),
                     "PG&E",pri.name
  ),
  sub.sys = str_extract_all(sub.sys, pattern = "[A-Z]") %>% #extract capital letters
    sapply(.,paste,collapse = "")%>%   #paste list into sys abbreviations
    str_replace(.,"PF",""), # fix lngpf to lng,
  end = if_else(pri.status == "Current",ymd(Sys.Date()), end)
  ) %>%
  filter(!is.na(pri.status))

ops.simple <- ops %>%
  select(sub.id, sub.name, sub.sys, pri.id, pri.name)

write_csv(ops, "Op_IM_Dates.csv")

write_csv(ops.simple, "Op_IM_Simple.csv")

#### INCIDENT DATA ####

# gd big 
gd.full <- distData %>% 
  #assorted nonsense I thought was useful early on
  #mostly cleaning things up to make more sense
  #always wondering if changing Y/N cols to true bools would make more sense too
  mutate(SYSTEM_TYPE = "Gas Distribution", 
         SYS = "GD",
         UNINTENTIONAL_RELEASE = replace_na(UNINTENTIONAL_RELEASE,0), 
         INTENTIONAL_RELEASE = replace_na(INTENTIONAL_RELEASE,0),
         ON_OFF_SHORE = "ONSHORE",
         UNITS = "mscf",
         INTER_INTRA = "INTRASTATE",
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
         STATE = LOCATION_STATE_ABBREVIATION,
         OFF_ACCIDENT_ORIGIN = NA)%>%
  #clean location data
  mutate( cleanLoc = pmap(list(ILOC, LOCATION_LATITUDE, LOCATION_LONGITUDE,
                               OPERATOR_ID, REPORT_NUMBER, STATE,
                               OFF_ACCIDENT_ORIGIN, SYS), 
                          locCleaner) %>% unlist()) %>%
  select(!OFF_ACCIDENT_ORIGIN)%>%
  left_join(miles, by = c("OPERATOR_ID", "SYS", "STATE","IYEAR"))%>%
  mutate(mileage = replace_na(mileage, 0))%>%
  ## add safety data
  #many to many left join
  left_join(filter(ops, sub.sys == "GT"), 
            by = c("OPERATOR_ID" = "sub.id"),
            suffix = c("", ".o"))%>%
  #determine valid records by checking date of incident by dates in safety rec
  mutate(valid = between(MDY, start, end-days(1)) ,
         valid = replace_na(valid, T))%>%
  group_by(REPORT_NUMBER)%>%
  mutate(rep = n())%>%
  ungroup()%>%
  filter(valid  | rep == 1)%>%
  #clean up 
  rename(NAME = NAME.x)



## add safety data to mileage for consistency 
#miles <- left_join(miles, ops.simple, by = c("OPERATOR_ID" = "sub.id")) 

# gt big
gt.full <- tranData %>%
 # slice_sample(n=50)%>%
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
                         paste0(str_to_title(ONSHORE_CITY_NAME), ", ", ONSHORE_STATE_ABBREVIATION),
                         if_else(grepl("STATE WATERS", OFF_ACCIDENT_ORIGIN, ignore.case= T) ,
                                 paste0(state.name[match(OFFSHORE_STATE_ABBREVIATION, state.abb)], " State Waters"),
                                 "Outer Continental Shelf" )),
         STATE = coalesce(ONSHORE_STATE_ABBREVIATION, OFFSHORE_STATE_ABBREVIATION),
         STATE = if_else(grepl("OCS", OFF_ACCIDENT_ORIGIN),
                         "OCS",
                         STATE)
    )%>%
  mutate( cleanLoc = pmap(list(ILOC, LOCATION_LATITUDE, LOCATION_LONGITUDE,
                               OPERATOR_ID, REPORT_NUMBER, STATE,
                               OFF_ACCIDENT_ORIGIN, SYS), 
                          locCleaner) %>% unlist()) %>%
  left_join(miles, by = c("OPERATOR_ID", "SYS", "STATE","IYEAR"))%>%
  mutate(mileage = replace_na(mileage, 0))%>%
    ## add safety data
    #many to many left join
    left_join(filter(ops, sub.sys == "GT"), 
              by = c("OPERATOR_ID" = "sub.id"),
              suffix = c("", ".o"))%>%
    #determine valid records by checking date of incident by dates in safety rec
    mutate(valid = between(MDY, start, end-days(1)) ,
           valid = replace_na(valid, T))%>%
    group_by(REPORT_NUMBER)%>%
    mutate(rep = n())%>%
    ungroup()%>%
    filter(valid  | rep == 1)%>%
    select(!c("rep", "valid")) %>%
    #repeat step to catch any overlapping dates, 
    #typically occurs when one primary is suprseded and one current
    group_by(REPORT_NUMBER)%>%
    mutate(rep2 = n())%>%
    ungroup()%>%
    filter(ifelse(rep2 > 1,pri.status != "Current", TRUE ) ) %>%
  rename(INTER_INTRA = PIPE_FACILITY_TYPE)
    
  


#{if(rep > 1) filter(pri.status != "Current")}%>%
# filter(!(rep>1 & pri.status != "Current"))%>%
# gt.op <- tibble()

# TODO: wtf is going wrong here 
# hl big
hl.full <- hzrdData %>% 
  mutate(SYSTEM_TYPE = "Hazardous Liquids",
         SYS = "HL")%>%
  rename( INTENTIONAL_RELEASE = INTENTIONAL_RELEASE_BBLS,
          UNINTENTIONAL_RELEASE = UNINTENTIONAL_RELEASE_BBLS,
          INTER_INTRA = PIPE_FACILITY_TYPE)%>%
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
                         
                         paste0(str_to_title(ONSHORE_CITY_NAME), ", ", ONSHORE_STATE_ABBREVIATION),
                         if_else(grepl("STATE WATERS", OFF_ACCIDENT_ORIGIN, ignore.case= T) ,
                                 paste0(state.name[match(OFFSHORE_STATE_ABBREVIATION, state.abb)], " State Waters"),
                                 "Outer Continental Shelf" )),
         STATE = coalesce(ONSHORE_STATE_ABBREVIATION, OFFSHORE_STATE_ABBREVIATION),
         STATE = if_else(grepl("OCS", OFF_ACCIDENT_ORIGIN),
                         "OCS",
                         STATE) 
        )%>%
  mutate( cleanLoc = pmap(list(ILOC, LOCATION_LATITUDE, LOCATION_LONGITUDE,
                               OPERATOR_ID, REPORT_NUMBER, STATE,
                               OFF_ACCIDENT_ORIGIN, SYS), 
                          locCleaner) %>% unlist()) %>%
  left_join(miles, by = c("OPERATOR_ID", "SYS", "STATE","IYEAR"))%>%
  mutate(mileage = replace_na(mileage, 0))%>%
  ## add safety data
  #many to many left join
  left_join(filter(ops, sub.sys == "HL"), 
            by = c("OPERATOR_ID" = "sub.id"),
            suffix = c("", ".o"))%>%
  #determine valid records by checking date of incident by dates in safety rec
  mutate(valid = between(MDY, start, end-days(1)) ,
         valid = replace_na(valid, T))%>%
  group_by(REPORT_NUMBER)%>%
  mutate(rep = n())%>%
  ungroup()%>%
  filter(valid  | rep == 1)%>%
  select(!c("rep", "valid")) %>%
  #repeat step to catch any overlapping dates, 
  #typically occurs when one primary is suprseded and one current
  group_by(REPORT_NUMBER)%>%
  mutate(rep2 = n())%>%
  ungroup()%>%
  filter(ifelse(rep2 > 1,pri.status != "Current", TRUE ) )
# 
# miles.op <- tibble()
# for(i in 1:nrow(miles)){
#   row <- miles[i,]
#   row$MDY <- year( mdy(paste0("01-01-", row$IYEAR)) ) # turn year into jan 1st date
#   if(row$OPERATOR_ID %in% ops$sub.id){
#     ops_match <- filter(ops, sub.id == row$OPERATOR_ID, 
#                         sub.sys == row$SYS, 
#                         start <= row$MDY,
#                         end > row$MDY)
#     row_match <- left_join(row, ops_match, by = c("OPERATOR_ID" = "sub.id"))
#   }
#   else {
#     row_match <- row
#   }
#   miles.op <- bind_rows(miles.op, row_match)
# }



#### ENFORCEMENT DATA ####
# enforcement <- read_tsv("https://primis.phmsa.dot.gov/enforcement-documents/PHMSA%20Pipeline%20Enforcement%20Raw%20Data.txt")
# 
# enforcement %>%
#   mutate(reg = str_split(Cited_Regulations, ",")) %>%
#   unnest(reg)%>%
#   mutate(reg = str_trim(reg))%>%
#   count(reg)%>% view


#### JOINS, BINDS, RATE ####

#columns for abridged incidents
short_cols <- c( "REPORT_NUMBER", "NAME","OPERATOR_ID",  #basic characteristics
                 "IYEAR","MDY","MoYr" ,"LOCAL_DATETIME" ,   #temporal char
                 "LOCATION_LATITUDE","LOCATION_LONGITUDE", "cleanLoc", 
                 "STATE", "ON_OFF_SHORE","INTER_INTRA",#location
                 "SYS","MSYS" ,"SIGNIFICANT", "SERIOUS","COMMODITY_RELEASED_TYPE",#inc summary
                 "UNINTENTIONAL_RELEASE", "INTENTIONAL_RELEASE", "TOTAL_RELEASE","UNITS", #releases
                 "FATALITY_IND","FATAL", "INJURY_IND","INJURE", #human impact
                 "EXPLODE_IND","IGNITE_IND" ,  "NUM_PUB_EVACUATED", "TOTAL_COST_CURRENT",#impact 2
                 "INSTALLATION_YEAR", "SYSTEM_PART_INVOLVED", "PIPE_DIAMETER", #inc char 
                 "CAUSE","CAUSE_DETAILS","MAP_CAUSE","MAP_SUBCAUSE", "NARRATIVE", #inc char 
                 "mileage", "pri.id","pri.name"
                 )  #joined char

#abridged all inc  
all.inc <- rbind(select(hl.full%>%  rename(NAME = NAME.x), all_of(short_cols)), 
                 select(gt.full %>%  rename(NAME = NAME.x), all_of(short_cols)), 
                 select(gd.full, all_of(short_cols)))


## get incident rate per operator (by base ID not prime/sub)
## QUESTION: What do I do about terminal / tank farm incident rates? 
# operatorRate <- miles %>%
#   group_by(OPERATOR_ID, SYS, IYEAR)%>%
#   summarize(miles = sum(mileage))%>%
#   full_join(hl.op %>%
#               count(OPERATOR_ID, IYEAR) %>%
#               mutate(SYS = "HL"),
#              by = c("OPERATOR_ID", "IYEAR","SYS")
#             )%>% 
#   full_join(gt.op %>%
#                count(OPERATOR_ID, IYEAR) %>%
#                mutate(SYS = "GT"),
#             by = c("OPERATOR_ID", "IYEAR","SYS")
#   )%>%
#   full_join(gd.op %>%
#                count(OPERATOR_ID, IYEAR) %>%
#                mutate(SYS = "GD"),
#             by = c("OPERATOR_ID", "IYEAR","SYS")
#   )%>%
#   mutate(inc = sum(n,n.x,n.y, na.rm=T),
#          inc = replace_na(inc, 0))%>%
#   select(!c(n,n.x,n.y))%>%
#    #filter(OPERATOR_ID == 32363)%>% view()
#   ungroup()%>%
#   group_by(OPERATOR_ID, SYS)%>%
#   summarize(n = n(),
#             inc = sum(inc, na.rm=T)/n,
#             miles = sum(miles, na.rm = T)/n,
#             ipm = inc/miles,
#             ipm2 = (inc/n)/(miles/n),
#             ipm = ifelse(is.nan(ipm) & inc == 0, 0, ipm),
#             ipm2 = ifelse(is.nan(ipm2) & inc == 0, 0, ipm2)) 


# all.inc <- left_join(all.inc, operatorRate, 
#                      by = c("OPERATOR_ID", "SYS"), suffix = c("",".or"))
# 
# gt.op <- left_join(gt.op, operatorRate, 
#                    by = c("OPERATOR_ID", "SYS"), suffix = c("",".or"))
# gd.op <- left_join(gd.op, operatorRate, 
#                    by = c("OPERATOR_ID", "SYS"), suffix = c("",".or"))
# hl.op <- left_join(hl.op, operatorRate, 
#                    by = c("OPERATOR_ID", "SYS"), suffix = c("",".or"))



length(unique(all.inc$NAME)) #747

length(unique(c(unique(gt.full$OPERATOR_STREET_ADDRESS), 
                unique(gd.full$OPERATOR_STREET_ADDRESS),
                unique(hl.full$OPERATOR_STREET_ADDRESS)))) #659

length(unique(c(unique(gt.full$OPERATOR_ID), 
                unique(gd.full$OPERATOR_ID),
                unique(hl.full$OPERATOR_ID)))) #660? why different? they moved?

#### WRITING EXPORTS ####
  
#csvs for each full table
write_csv(gt.full, "data/clean/gt_inc.csv")
write_csv(gd.full, "data/clean/gd_inc.csv")
write_csv(hl.full, "data/clean/hl_inc.csv")


#csv for abridged all incidents
write_csv(all.inc, "data/clean/all_inc.csv")

#csv for mileage numbers 
#write_csv(miles.op, "data/clean/sys_miles.csv")

#csv for op match 
write.csv(ops, "data/clean/operator_id_match.csv")
