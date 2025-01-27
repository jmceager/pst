library(tidyverse)
library(readxl)

## goal here isnt actually mileage isa it 

## goal is to take annual reports and turn that into useful tables 

## what do i use annual reports for? 

## mileage, inspection mileage, inter intrastate, etc 

#download temp file of annual reports 
#go sheet by sheet to find the relevant columns to waht im doing 
#sheet X - mileage, inter intra, etc


# set up temp file 
temp <- tempfile()

#unzip 
hl_url_2010 <- "https://www.phmsa.dot.gov/sites/phmsa.dot.gov/files/data_statistics/pipeline/annual_hazardous_liquid_2010_present.zip"

download.file("https://www.phmsa.dot.gov/sites/phmsa.dot.gov/files/data_statistics/pipeline/PHMSA_Pipeline_Safety_Flagged_Incidents.zip", destfile =  temp, method = "curl")


distData <- read_xlsx(unzip(temp, files = "gd2010toPresent.xlsx"), sheet = 2)
tranData <- read_xlsx(unzip(temp, files = "gtggungs2010toPresent.xlsx"), sheet = 2)
hzrdData <- read_xlsx(unzip(temp, files = "hl2010toPresent.xlsx"), sheet = 2)
lngData <- read_xlsx(unzip(temp, files = "lng2011toPresent.xlsx"), sheet = 2)