#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(scrollytell)
library(dplyr)
library(readr)
library(ggplot2)
library(ggspatial)
library(ggtext)
library(albersusa)
library(lubridate)
library(sf)
library(stringr)
library(viridis)
library(gganimate)
library(showtext)
library(biscale)
library(reactable)

source("theme.R")

epsg <- "ESRI:102003"

pstFont = "Montserrat"
font_add_google(pstFont)
showtext_auto()
showtext_opts(dpi=300) 


df_all_yr <- read_csv(paste0(datadir, "clean/all_inc.csv")) %>%
  mutate(yrwk = week(MDY),
         yrday = yday(MDY),
         SYS = factor(SYS, levels = c("HL", "GT", "GD", "GG", "UNGS"))) %>%
  filter(REPORT_NUMBER != 20220042)

df_all <- df_all_yr %>% filter(IYEAR < 2023, STATE != "PR")


df <- readRDS("air_df.rds") %>% filter(IYEAR == 2022, STATE != "PR",REPORT_NUMBER != 20220042) %>%
  mutate(SYS = factor(SYS,levels = c("HL", "GT", "GD", "GG", "UNGS")))
df_base <- df %>% st_drop_geometry()

df %>%
  count(STATE) %>%
  left_join(miles %>%
              filter(IYEAR == 2022) %>%
              group_by(STATE)%>%
              summarise(miles = sum(miles)),
            by = "STATE")%>%
  mutate(inc = n/(miles/1000))%>%
  arrange(inc) #%>% tibble::view()



sdf<- df %>% 
  group_by(IYEAR, SYS) %>% 
  summarize(n = n(), 
            cost = sum(TOTAL_COST_CURRENT),
            rel = sum(TOTAL_RELEASE),
            fat = sum(FATAL),
            inj = sum(INJURE),
            sig = sum(SIGNIFICANT == "YES"),
            ser = sum(SERIOUS == "YES")) %>%
  group_by(IYEAR)%>%
  mutate(cost = sum(cost),
         fat = sum(fat),
         inj = sum(inj),
         sig = sum(sig),
         ser = sum(ser),
         n_all = sum(n))


all_miles <- read_csv(paste0(datadir, "clean/sys_miles.csv")) 
hlinc <- read_csv(paste0(datadir, "clean/hl_inc.csv")) %>% filter(IYEAR == 2022) 
hltop <- hlinc %>% select(NAME, LOCAL_DATETIME, TOTAL_RELEASE, WATER_CONTAM_IND, IPE, WATER_NAME) %>%
  slice_max(order_by = TOTAL_RELEASE, prop = .1)


miles <- all_miles %>%
  distinct(OPERATOR_ID, IYEAR, STATE, SYS, .keep_all = T)%>%
  group_by(STATE, IYEAR, SYS) %>%
  summarise(miles = sum(mileage))


statedf<- df %>% 
  filter(IYEAR == 2022)%>%
  group_by(STATE, SYS) %>% 
  summarize(n = n(), 
            cost = sum(TOTAL_COST_CURRENT),
            rel = sum(TOTAL_RELEASE),
            fat = sum(FATAL),
            inj = sum(INJURE),
            sig = sum(SIGNIFICANT == "YES"),
            ser = sum(SERIOUS == "YES")) %>%
  mutate(cost = sum(cost),
         fat = sum(fat),
         inj = sum(inj),
         sig = sum(sig),
         ser = sum(ser),
         n_all = sum(n))%>%
  mutate(cp = cost / n_all,
         sp = sig / n_all,
         serp = ser/n_all,
         relp = rel / n) %>%
  left_join(filter(miles, IYEAR == 2022), by = "STATE") %>%
  mutate(permi = n_all/(miles/1000))


nat_ir <- nrow(df) / sum(all_miles[all_miles$IYEAR==2022, "mileage"]) 

first_day_of_month_wday <- function(dx) {
  day(dx) <- 1
  wday(dx)
}

us_sf_adj <- usa_sf("laea") %>% st_transform(epsg)
us_box <- st_bbox(us_sf_adj)


# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$style(
      #graphs p {
      # margin-bottom:42vh;
      # }
      HTML("
           #graphs .scrolly-section{
           margin-bottom:75vh;
           position:relative;
           top:-66vh;
           }
           ")
    )
  ),

    # Application title
    titlePanel("2022 Incident Review"),

    # Intro?
    fluidRow(
      column(1),
      column(4,
             HTML(
               "<h2> 2022 In Brief </h2>
               <p> Last year saw some considerably high-impact incidents, including
               <a href = https://www.kxii.com/2022/10/12/explosion-overnight-coal-co-natural-gas-plant/> a compressor station explosion </a> in coalgate, OK (MPLX LP, ownned by Marathon),
               <a href =https://www.ntsb.gov/investigations/Pages/PLD22FR002.aspx >Marathon spilling 164,000 gallons of crude oil </a> into a Mississippi River tributary, and 
               <a href =https://www.reuters.com/business/energy/keystone-pipeline-shut-after-oil-spill-into-kansas-creek-2022-12-08/> Keystone's largest spill to date</a>.
               2022 ultimately concluded with 496 PHMSA-reported incidents creating $738,055,082 of damage. Incidents released 3,744,954 gallons of hazardous liquids and 1,985,331 mscf of gas across the nation, killing 5 and hospitalizing 17.
               </p> <br>
               <h4> Daily Incident Tracker </h4>
               <p>
               The plot to the right shows the incidents per day throughout 2022 in each of the three pipeline systems most-often reported on by PHMSA: Gas Distribution (GD), Gas Transmission (GT), and Hazardous Liquids (HL). December 23rd and January 27th were the two most incident-afflicted days on record last year, with each date witnessing 6 distinct pipeline incidents across the 50 states and Puerto Rico. 
               July 8th, October 3rd, and November 4th followed close behind with a considerable 5 incidents each. On the other hand, 91 days saw 0 incidents across all systems, indicating the nation went about one in every four days without an incident. 
               The longest incident-free stretche was 4 days spanning from March 19th to March 22nd. All told, the U.S. averaged 1.4 pipeline incidents per day during 2022. 
               </p>")
             ),
      column(6,
             radioGroupButtons("calSys",
                          label = "System",
                          choices = c("All", "GD", "GT", "HL")),
             plotOutput("calPlot")
             ),
      column(1)
    ),
    br(),
    hr(),
    br(),
    fluidRow(
      column(1),
      column(6,
             HTML("<center>"),
             div(img(src="outfile.gif", align = "center", width = "100%")),
             HTML("</center>")
             ),
      column(4,
             "The map to the left shows the daily incidents across the span of 2022, 
             as they occured. Watch as the incidents roll through each month and follow 
             the same pattern as the calendar above. Each color in the scale corresponds 
             to the week of the year each incident occured."),
      column(1)
    ),
    ##### map scroll ####
    fluidRow(
      column(1),
      column(10,
             scrolly_container("mapS",
                               scrolly_graph(br(),
                                             br(),
                                             HTML('<center>'),
                                             div(plotOutput("mapPlot", width = "100%")),
                                             # conditionalPanel(condition = 'input.mapS != "intro"',
                                             #                  div(plotOutput("mapPlot", height = "66vh"))),
                                             #plotOutput(ifelse(,'mapTime', "mapPlot"), height = '560px'),
                                             HTML('</center>'),
                                             br(),
                                             br()),
                               scrolly_sections(
                                 scrolly_section(id = "intro",
                                                 h2("Incident Breakdown"),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 p("Now that we know when and where these incidents occured, let's breakdown the", strong("497") ,"reported incidents by their impact and characteristics..."),
                                                 br(),
                                                 br(),
                                                 br()
                                                 ),
                                 scrolly_section(id = "sys",
                                                 h3("By System:"),
                                                 p(strong(scales::percent(nrow(df[df$SYS == "HL",])/nrow(df))), 
                                                   " were on ", 
                                                          span(style = paste0("color:", honey,"; font-weight:bold;"),
                                                               "Hazardous Liquids"),
                                                          " lines"),
                                                 p(strong(scales::percent(nrow(df[df$SYS == "GT",])/nrow(df))),
                                                   " were on ", 
                                                   span(style = paste0("color:", darkBlue,"; font-weight:bold;"),
                                                        "Gas Transmission"),
                                                   " lines"),
                                                 p(strong(scales::percent(nrow(df[df$SYS == "GD",])/nrow(df))),
                                                   " were on ", 
                                                   span(style = paste0("color:", liteGreen,"; font-weight:bold;"),
                                                        "Gas Distribution"),
                                                   " lines"),
                                                 p(strong(scales::percent(nrow(df[df$SYS == "GG",])/nrow(df))),
                                                   " were on ", 
                                                   span(style = paste0("color:", lilac,"; font-weight:bold;"),
                                                        "Gas Gathering"),
                                                   " lines"),
                                                 p(strong(scales::percent(nrow(df[df$SYS == "UNGS",])/nrow(df))),
                                                   " were on ", 
                                                   span(style = paste0("color:", liteBlue,"; font-weight:bold;"),
                                                        "Underground Natural Gas Storage"),
                                                   " wells")
                                                 ),
                                 scrolly_section(id = "sig",
                                                 br(),
                                                 h3("By PHMSA Designation:"),
                                                 p(
                                                   strong(scales::percent(nrow(df[df$SIGNIFICANT == "YES",])/nrow(df))),
                                                   "were deemed Significant"
                                                 ),
                                                 p(
                                                   strong(scales::percent(nrow(df[df$SERIOUS == "YES",])/nrow(df))),
                                                   "were deemed Serious"
                                                 ),
                                                 br(),
                                                 br(),
                                                 p("Per ", 
                                                   a(href = "https://www.phmsa.dot.gov/data-and-statistics/pipeline/national-pipeline-performance-measures#:~:text=Natural%20Gas%20Plants-,Pipeline%20Incidents%3A%2020%2DYear%20Trends,-PHMSA%27s%20incident%20data",
                                                     "PHMSA"),
                                                   ", Significant incidents are those including any of the following conditions:"),
                                                 tags$ol(
                                                   tags$li("Fatality or injury requiring in-patient hospitalization"),
                                                   tags$li("$50,000 or more in total costs, measured in 1984 dollars"),
                                                   tags$li("Highly volatile liquid releases of 5 barrels or more or other liquid releases of 50 barrels or more"),
                                                   tags$li("Liquid releases resulting in an unintentional fire or explosion")
                                                 ),
                                                 br(),
                                                 p("Serious incidents result in a fatality or injury requiring in-patient hospitalization."),
                                                 br(),
                                                 p("Both designations exclude gas distribution incidents caused by a nearby fire or explosion that impacted the pipeline system."),
                                                 br()
                                                 ),
                                 scrolly_section(id = "rel",
                                                 br(),
                                                 br(),
                                                 h3("By Spill or Release Size:"),
                                                 br(),
                                                 radioGroupButtons("relSys", label = "Filter by System:", choices = c("All", "HL", "GT", "GD"), selected = "All"),
                                                 p(em("Filter by system to visualize volumes on the map")),
                                                 br(),
                                                 htmlOutput("relText"),
                                                 br(),
                                                 conditionalPanel(condition = 'input.relSys == "All"', p("The points on the map are scaled on a system-wide basis from 0 to 10. The incident with the largest release in each system receives a 10, the smallest a 0, and all other incidents are scaled in relation to those values. When the map filters by system, the points rescale to show system-specific volumes (mscf or gallons). ")),
                                                 ## TODO: add col legend
                                                 ## TODO: maybe style buttons by system?
                                                 br()),
                                 scrolly_section(id = "cost",
                                                 h3("By Cost of Damage:"),
                                                 br(),
                                                 p(
                                                   "Incidents caused", strong(scales::dollar(sum(df$TOTAL_COST_CURRENT))),
                                                   "of damage across the U.S. during the course of 2022. "
                                                 ),
                                                 br(),
                                                 br(),
                                                 p("The highest-cost incident occured on December 7, 2022, in Washington County, Kansas. Over half a million gallons of crude oil spilled on a TransCanada line, causing over $480 million in damages. "),
                                                 br(),
                                                 h4("By System:"),
                                                 p(span(style = paste0("color:", honey,"; font-weight:bold;"),
                                                        "Hazardous Liquids"),
                                                   " incidents caused ",
                                                   strong(scales::dollar(sum(df_base[df_base$SYS == "HL","TOTAL_COST_CURRENT"]))), 
                                                    " of damage"),
                                                 p(span(style = paste0("color:", darkBlue,"; font-weight:bold;"),
                                                        "Gas Transmission"),
                                                   " incidents caused ",
                                                   strong(scales::dollar(sum(df_base[df_base$SYS == "GT","TOTAL_COST_CURRENT"]))), 
                                                   " of damage"),
                                                 p(span(style = paste0("color:", liteGreen,"; font-weight:bold;"),
                                                     "Gas Distribution"),
                                                   " incidents caused ",
                                                   strong(scales::dollar(sum(df_base[df_base$SYS == "GD","TOTAL_COST_CURRENT"]))), 
                                                   " of damage"),
                                                 p(span(style = paste0("color:", lilac,"; font-weight:bold;"),
                                                       "Gas Gathering"),
                                                   " incidents caused ",
                                                   strong(scales::dollar(sum(df_base[df_base$SYS == "GG","TOTAL_COST_CURRENT"]))), 
                                                   " of damage"),
                                                 p(span(style = paste0("color:", liteBlue,"; font-weight:bold;"),
                                                        "Underground Natural Gas Storage"),
                                                   " well incidents caused ",
                                                   strong(scales::dollar(sum(df_base[df_base$SYS == "UNGS","TOTAL_COST_CURRENT"]))), 
                                                   " of damage"),
                                                 br()
                                                 ),
                                 scrolly_section(id = "human",
                                                 br(),
                                                 h3("By Direct Human Impact:"),
                                                 p(
                                                   (strong(nrow(df[df$FATALITY_IND == "YES" |df$INJURY_IND == "YES" ,]) )) ,
                                                   " incidents caused a hospitalization or injury, accounting for ",
                                                   scales::percent(nrow(df[df$FATALITY_IND == "YES" |df$INJURY_IND == "YES" ,])/nrow(df)),
                                                   " of incidents."
                                                 ),
                                                 br(),
                                                 p(
                                                   "These incidents resulted in ",
                                                   strong(sum(df$FATAL))," deaths and ", 
                                                   strong(sum(df$INJURE))," injuries ending in hospitaliztion"
                                                 ),
                                                 br(),
                                                 p("The most devastating incident was a gas distribution explosion in Brooklyn, NY: killing 2 and hospitalizing 1, per PHMSA's records. "),
                                                 br()
                                 ),
                                 scrolly_section(id = "state_inc",
                                                 h3("By State:"),
                                                 br(),
                                                 p(
                                                   "At least one reportable incident occured in 43 states, Puerto Rico, and on the outer continental shelf outside of state jurisdictions. 12 of these 45 geographies had more than 10 incidents last year; 4 had at least 20. Texas far and away had the most incidents with 160: 133 of those on liquids lines, 11 on gas transmission lines, 9 on gas gathering lines, and 7 on gas distribution lines."
                                                 ),
                                                 br(),
                                                 h5("Top 5 States by Incident Count in 2022:"),
                                                 reactableOutput("top5", width = "100%"),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 br()
                                 ),
                                 scrolly_section(id = "state_rate",
                                                 h3("By State Incident Rate:"),
                                                 br(),
                                                 p(
                                                   "A few states in the top 5 for incidents counts remain in the top 5 for rates. The national rate in 2022 was about 0.17 incidents per 1,000 miles. Just 13 states saw an incident rate greater than the national rate last year."
                                                 ),
                                                 br(),
                                                 h5("Top 5 States by Incident Rate in 2022:"),
                                                 p(em("Rate given as incidents per 1,000 miles")),
                                                 reactableOutput("top5rate", width = "100%"),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 br()
                                 ) # scroll section
                                 ) # scroll  group
             ) # scroll containter
             ), # column
      column(1)
    ), #scrolly ends
    #some plots for operators 
    fluidRow(
      column(1),
      column(6,
             div(
               HTML("<center>"),
               plotOutput("yearPlot", width = "100%"),
               HTML("</center>")
             )),
      column(4,
             h4("Looking Back"),
             br(),
             p("Taking a look back at how each year's incidents occured since 2010 
               suggests that 2022 was a bit milder than most. For the most part, 
               this monthly breakdown shows fewer incidents per month in 2022 compared 
               to the previous 12 years. We can dig in a little further and determine 
               if last year was", em(" really " ), "an improvement in more than just raw 
               incident counts."),
             br()),
      column(1)
    ),
  hr(),
    #### plot scroll ####
    fluidRow(
      column(1),
      column(10,
             class = "scroll2",
             scrolly_container(
               "graphs",
               scrolly_graph(
                 br(),
                 br(),
                 HTML('<center>'),
                 conditionalPanel(condition = 'input.graphs != "intro"',
                                  div(plotOutput("graphPlot", 
                                                # height = "66vh",
                                               width = "100%"))),
                 #plotOutput(ifelse(,'mapTime', "mapPlot"), height = '560px'),
                 HTML('</center>'),
                 br(),
                 br(),
                 hr()
               ),
               scrolly_sections(
                 scrolly_section(id = "intro2",
                                 div(style = "height:10vh;")
                                 
                 ),
                 scrolly_section(id = "cost",
                                 h2("A Year in..."),
                                 br(),
                                 br(),
                                 h3("Cost of Damage:"),
                                 br(),
                                 br(),
                                 p("2022 saw a sharp increase in total cost of 
                                   damage caused by pipeline incidents, compared to 2021. 
                                   That said, a couple of years stand out against the rest: 
                                   2010 and 2018. "),
                                 br(),
                                 br(),
                                 
                 ),
                 scrolly_section(id = "costContext",
                                 h4("Adding Context:"),
                                 br(),
                                 br(),
                                 p("In each of these years, one incident made up at least half 
                                   of the total cost of damage from incidents. The plot on the right 
                                   highlights these ",
                                   span(style = paste0("color:", honey,"; "),
                                        "high-cost"),
                                   " incidents in ",
                                   span(style = paste0("color:", honey,"; "),
                                        "yellow."),
                                   "2010 was marked by the $1 Billion in damage 
                                   from Enbridge's Kalamazoo oil spill; while the 
                                   2018 spike was down to the $1.7 Billion 
                                   in damage caused by the Merrimack Valley gas 
                                   explosions on a Columbia Gas distribution line. "),
                                 br(),
                                 br(),
                                 br()
                                 
                 ),
                 scrolly_section(id = "release",
                                 h3("Total Release Size:"),
                                 br(),
                                 br(),
                                 p("Release sizes appear to be declining over the past decade, with a 
                                   significant drop in Gas Tranmission releases over 12 years. Hazardous Liquids have not 
                                   seen a significant change in release sizes. Although it looks like liquids releases are down in 2022 
                                   compared to years passed, previous drops have frequently been followed by spikes in subsequent years. 
                                  This pattern of volatility has held over the 12-year period analyzed, as exemplified in the adjacent plot."),
                                 br(),
                                 br()
                                 
                 ),
                 scrolly_section(id = "releaseInc",
                                 br(),
                                 br(),
                                 p("This trend of Gas Transmission releases declining and Liquids releases remaining relatively unchanged & volatile year-by-year appears to hold for both:"),
                                 br(),
                                 br(),
                                 h4("Release Size per Incident"),                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 p("and..."),
                                 br()
                                 
                 ),
                 scrolly_section(id = "releaseMi",
                                 h4("Release Size by Mileage:"),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br()
                                 
                 ),
                 scrolly_section(id = "releaseTop",
                                 br(),
                                 h5("The top 10% of releases in 2022"),
                                 p("accounted for more than half 
                                  of the total releases among the 3 most-reported systems in PHMSA's data.
                                  This contrast is most distinct among liquids incidents, where the top 10% 
                                   of incident-caused releases are responsible for over 90% of the total 2022 
                                   releases on liquids lines. In other words, 29 of the 293 liquids incidents in 2022 
                                   were responsible for releasing 3,455,886 of the 3,738,990 gallons released by liquids 
                                   lines last year. Of these 29 spills, 10 were deemed to have contaminated water and 
                                   17 impacted people or the environment, per",
                                   a(href = "https://www.phmsa.dot.gov/data-and-statistics/pipeline/national-pipeline-performance-measures#:~:text=Accidents%20Impacting%20People%20or%20the%20Environment%20%2D%20rate%20per%20mile%20and%20volume%20spilled%20per%20barrel%2Dmile%20transported.%20Displayed%20for%20all%20causes%2C%20Integrity%20Inspection%20target%2C%20and%20Operations%20and%20Maintenance%20target.",
                                     " PHMSA's defintion. ")
                                   ),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 hr()
                                
                                 
                 ),
                 scrolly_section(id = "costTop", ## worth considering changing some of these plots to show incidents in HCAs vs not over time 
                                 
                                 br(),
                                  br(),
                                 HTML("<p>The top 10% of incidents by cost also tend to far outweight the bottom 90%.
                                   This trend doesn't quite hold among gas distribution incidents, but big liquids 
                                   and gas transmission incidents are exceptionally costly in damage caused. The top
                                   10% of liquids incidents alone accounts for more than two-thirds of <em>all</em> pipeline incident 
                                   damage cost for the year. The adjacent plot presents the percent of system-wide damage cost 
                                   between the top 10% and bottom 90% of incidents, with labels detailing the rounded dollar value.</p>"),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br()
                                 
                 ),
                 #### operators sections ####
                 scrolly_section(id = "operatorScatter1", # no inc
                                 h2("What about the operators?"),
                                 br(),
                                 br(),
                                 p("The vast majority of operators reporting mileage to PHMSA had no incident in 2022. Of 2,837 operators 
                                   with reported mileage data, 2,656 had 0 incidents. Of these 2,656, 306 had 0 reported miles, but were included
                                   in PHMSA's reports. These no-incident operators accounted for about 1.37 million miles of gas transmission, gas 
                                   distribution, and hazardous liquids lines, or roughly 48% of those pipeline systems by mileage."),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br()
                 ),
                 scrolly_section(id = "operatorScatter2", # big inc 
                                 h3("High-Cost Incidents:"),
                                 br(),
                                 br(),
                                 p("45 Operators are responsible for the 48 incidents in the top 10% of damage cost in 2022. 
                                   These 48 high-cost incidents are responsible for $662,850,000 in damage across the U.S., accounting 
                                   for over 92% of the total damage done in the country during 2022. "),
                                 br(),
                                 br(),
                                 p("The table below examines the 3 operators with more than one high-cost incident. Expand each row 
                                   to learn more about these incidents' operators, locations, and causes."),
                                 br(),
                                 br(),
                                 reactableOutput("two_ten", width = "100%"),
                                 br(),
                                 br(),
                                 br()
                                 
                 ),
                 scrolly_section(id = "operatorScatter3", # frequent inc? 
                                 h3("Above Average:"),
                                 br(),
                                 br(),
                                 p("The dashed link pine shows the annual incident rate for 2022. The 163 operators above that
                                   line are responsible for 434 incidents, 5 fatalities, 14 hospitalizaitons, and $711,308,420
                                   in damage cost across the country. "),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br()
                                 
                 )
                 
                 
                 
                 
               )
             )
             ),
      column(1)
    ),
  hr(),
    fluidRow(
      column(1),
      column(10,
             p("Take a look at all of the incidents from 2022, summarized briefly in the table below.
               Group incidents by operator or state, search for specific places or operators, and learn 
               more about last year's incidents. "),
             radioGroupButtons("table_type",
                               label = "Grouping",
                               choices =c("None", "State", "Operator")),
             reactableOutput("all_inc", width = "100%"),
             br(),
             br(),
             br()),
      column(1)
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
    
  
  #### calendar plot ####
  calData <- reactive({
    df %>%
      sf::st_drop_geometry()%>%
      filter(IYEAR == 2022)%>%
      {if (input$calSys == "All") filter(.)
        else filter(., SYS == input$calSys)}%>%
      mutate(day = lubridate::day(MDY),
             month = lubridate::month(MDY, label = T, abbr = T),
             wday = lubridate::wday(MDY))%>% 
      count(wday, month,day)%>%
      tidyr::complete(day = 1:31, month, fill = list(n=0)) %>%
      filter(!(month == "Feb" & day > 28),
             !(month %in% c("Apr", "Jun", "Sep", "Nov") & day > 30))%>%
      mutate(dateNew = mdy(paste0(month, "/", day, "/", "2023")),
             wday = lubridate::wday(dateNew,week_start = 1),
             wk = ceiling((day(dateNew) + first_day_of_month_wday(dateNew) - 2) / 7)) 
  })
  
  output$calPlot <- renderPlot({
    req(input$calSys)
    cBreaks <- if_else(input$calSys == "All",
                      list(seq(0,6,2)),
                      list(seq(0,4,1)))
    
   calData() %>%
    ggplot(aes(x = wday, y = wk*-1, fill = n))+
      geom_tile()+
      scale_fill_gradient(name = "Incidents",
                           breaks = cBreaks[[1]],
                           low = "#fffff0",
                           high = midBlue)+
      facet_wrap(~month)+
      ggthemes::theme_tufte(  )+
      theme(
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        text = element_text(family = pstFont),
        legend.position = "bottom",
        legend.direction = "horizontal"
      )+
      labs(title = "2022 In Review: Daily Incident Tracking",
           x = NULL,
           y = NULL)
  })
  
  #### reactive text ####
  output$relText <- renderUI({
    reldf <- df %>%
      {if(input$relSys == "All") filter(.)
        else if(input$relSys == "HL") filter(., SYS == "HL")
        else if(input$relSys == "GT") filter(., SYS == "GT")
        else if(input$relSys == "GD") filter(., SYS == "GD")
        else filter(.)}%>%
      group_by(MSYS, COMMODITY_RELEASED_TYPE, UNITS)%>%
      summarize(rel = scales::comma(sum(TOTAL_RELEASE)))%>%
      mutate(com = str_to_title(COMMODITY_RELEASED_TYPE),
             com = str_replace(com, "Hvl", "HVLs"),
             com = str_replace(com, "Co2", "CO2"),
             com = str_remove(com, " Or Other Flammable Or Toxic Fluid Which Is A Gas At Ambient Conditions"),
             com = str_remove(com, " Which Is A Liquid At Ambient Conditions"))
    
    if(input$relSys == "All"){
      hRel <- reldf %>% filter(MSYS == "HL")
      gRel <- reldf %>% filter(MSYS == "Gas")
      
          ##TODO gotta fix this code: making loops get the relevant stuff 
      hList <- NULL
      for(i in 1:nrow(hRel)){
        newRel <- tags$li(paste0(hRel[[i,"rel"]], " gallons of ",hRel[[i, "com"]], " "))
        hList <-paste0(hList, newRel)
      }
    
      gList <- NULL
      for(i in 1:nrow(gRel)){
        newRel <- tags$li(paste0(gRel[[i,"rel"]], " MSCF of ",gRel[[i, "com"]]))
        gList <-paste0(gList, newRel)
      }
      
      pls<-tagList(h4("Liquids Incidents Released:"),
             HTML("<ul>",hList,"</ul>"),
             br(),
             h4("Gas Incidents Released:"),
             HTML("<ul>",gList,"</ul>"),
             br())
      
      return(
        pls
      )
      
    }
    else{
      relList <- NULL
        for(i in 1:nrow(reldf)){
          newRel <- tags$li(paste0(reldf[[i,"rel"]]," ", reldf[[i,"UNITS"]], " of ",reldf[[i, "com"]]))
          relList <- paste0(relList, newRel)
        }
      pls<- tagList(h4(paste0(input$relSys, " Incidents Released:")),
                HTML("<ul>",relList,"</ul>"),
                   br())
      
        
        return(
          
          pls
        )
    }
  })
  
  


  #### map render ####
  #sys, sig, human, rel, ccost

  #regular map 
  output$mapPlot <- renderPlot({
    req(input$mapS)
    base <- ggplot()+
      ggspatial::layer_spatial(us_sf_adj, fill = "gray75", colour = "white")+
      coord_sf(xlim = c(us_box$xmin[[1]], us_box$xmax[[1]]),
               ylim = c(us_box$ymin[[1]], us_box$ymax[[1]]))+
      theme_pst_map()+
      theme(legend.position = c(.5,.01))+
      labs(tag = "PST 2023",
           caption = "PHMSA 2023 Incident Flagged Files")
    
    if(input$mapS == "intro"){
      base + 
        layer_spatial(df, size = 2, col = "black")
    }
    
    else if(input$mapS == "sys"){
      base +
        layer_spatial(df, aes(colour = SYS),
                      alpha= .65, size = 2)+
        scale_color_manual(values = c(honey, darkBlue, liteGreen, lilac, liteBlue))
    }
    else if(input$mapS == "sig"){
      mapdf <- df %>% mutate(sig = if_else(SERIOUS == "YES", "Serious", 
                                           if_else(SIGNIFICANT == "YES", "Significant", 
                                                   "None")),
                             sig = factor(sig, levels = c("None", "Significant", "Serious")))
      
      base +
        layer_spatial(mapdf, aes(colour = sig, size = sig),
                      alpha= .65, size = 2)+
        scale_color_manual(values = c("#fff0d2", honey, crimson),
                           name = "Incident Designation")+
        scale_size_manual(values = c(1.5,3,4.5),
                          name = "Incident Designation")
    }
    else if(input$mapS == "human"){
      mapdf <- df %>%
        mutate(injure = cut(INJURE, breaks = c(0,.9,3,5),
                            labels = c(0,3,5), include.lowest = T),
               fatal = cut(FATAL, breaks = c(0,.9,3,5), 
                           labels = c(0,3,5), include.lowest = T),
               human = if_else(INJURY_IND == "YES", T, F) | if_else(FATALITY_IND == "YES", T, F)) %>% 
        bi_class( x = injure, y = fatal, style = "jenks", dim = 3, keep_factors = T, dig_lab = 3)
      pal = "DkViolet2"
      
      legend <- bi_legend(pal = pal,
                          dim = 3,
                          flip_axes = TRUE, 
                          xlab = "Injuries",
                          ylab = "Deaths",                          
                          size = 4)
      
      map <-base + 
        layer_spatial(mapdf, aes(colour = bi_class, alpha = human, size = human),
                        show.legend = F)+
        bi_scale_color(pal = pal,flip_axes = TRUE,
                           name = "Incident Designation")+
        scale_alpha_manual(values = c(.25,.8))+
        scale_size_manual(values = c(1.5,5.5))+
        theme_pst_map()
      
      cowplot::ggdraw() +
        cowplot::draw_plot(map, 0, .005, 1, 1) +
        cowplot::draw_plot(legend, 0.55, .05, 0.2, 0.2)
    }
    else if(input$mapS == "rel"){
      req(input$relSys)
      mapdf <- df %>%
        {if(input$relSys == "GD") filter(.,SYS == "GD")
          else if(input$relSys == "HL") filter(., SYS == "HL")
          else if(input$relSys == "GT") filter(., SYS == "GT")
          else  group_by(.,SYS)%>%  
            mutate(zRel = scales::rescale_max(TOTAL_RELEASE, to = c(0,10)))%>% 
            ungroup()
          } 
      
      if(input$relSys == "All"){
        base + 
          layer_spatial(mapdf, aes(size = zRel, col = SYS),
                        alpha= .65)+
          scale_size_area(name= "Release Size (Scaled)",
                          max_size = 10,
                          breaks = c(0,5,10),
                          labels = c("Smallest", "Average", "Largest"))+
          scale_color_manual(values = pstSys, name = "System")+
          theme(legend.box = "vertical")+
          guides(color = "none")
      }
      
      else{
        #color and breaks
        if(input$relSys == "GD"){
          rBreaks = c(100,1000,10000) 
          rCol = liteGreen
          unit = "MMSCF"
        }
        else if(input$relSys == "GT"){
          rBreaks = c(1000,10000,100000) 
          rCol = darkBlue
          unit = "MMSCF"
        }
        else{
          rBreaks = c(5000,100000,500000)
          rCol = honey
          unit = "K Gal."
        }
        #map 
        base + 
          layer_spatial(mapdf, aes(size = TOTAL_RELEASE),
                        alpha= .65, col = rCol)+
          scale_size_area(breaks = rBreaks, 
                          labels = scales::comma_format( scale = .001),
                          name= paste0("Release Size (", unit, ")"),
                          max_size = 10)
      }
      
      
    }
    
    else if(input$mapS == "cost"){
      mapdf <- df %>% filter(STATE != "PR")
      
      base +
        layer_spatial(mapdf, aes(size = TOTAL_COST_CURRENT, col = SYS),
                      alpha= .65)+
        scale_size_area(labels = scales::label_dollar(scale = .000001, suffix = "M"),
                        breaks = c(5e6,5e7, 5e8),
                        limits = c(0,5e8),
                        name= paste0("Damage Cost (2022, USD)"),
                        max_size = 13)+
        scale_color_manual(values = pstSys, name = "System")+
        guides(color = "none")
    }
    else if(input$mapS == "state_inc"){
      mapdf <- df %>% 
        st_drop_geometry() %>%
        count(STATE) %>%
        left_join(us_sf_adj, by = c("STATE" = "iso_3166_2"))
      
      base + 
        layer_spatial(mapdf, aes(fill = n), col = "#fffff0")+
        scale_fill_gradient(name = "Incidents", breaks = c(1,50,100,150),
                            low = "#fffff0", high = crimson)
      
    }
    else if(input$mapS == "state_rate"){
      mapdf <- df %>% 
        st_drop_geometry() %>%
        count(STATE) %>%
        left_join(filter(miles, IYEAR == 2022) %>% group_by(STATE) %>% summarize(miles = sum(miles, na.rm = T)),
                  by = "STATE") %>%
        mutate(rate = n / (miles/1000))%>%
        left_join(us_sf_adj, by = c("STATE" = "iso_3166_2"))
      
      base + 
        layer_spatial(mapdf, aes(fill = rate), col = "#fffff0")+
        scale_fill_gradient(name = "Incidents per 1K Mi.", breaks = c(.1,.45,.8),
                            low = "#fffff0", high = crimson)
    }
    else{
      base
    }
    
  })
  
  #### map table ####
  # plot top 5 incident states
  output$top5rate <- renderReactable({
    req(input$mapS)
    df %>%
      st_drop_geometry() %>% 
      count(STATE) %>% 
      left_join(filter(miles, IYEAR == 2022)%>% group_by(STATE) %>% summarize(miles = sum(miles, na.rm = T)),
                by = "STATE") %>%
      mutate(rate = n / (miles/1000),
             STATE = state.name[match(STATE, state.abb)])%>%
      slice_max(order_by = rate, n = 5)%>%
      reactable(
        columns = list(
          STATE = colDef(name = "State", minWidth = 75),
          miles = colDef(name = "Mileage", minWidth = 60, format = colFormat(digits = 0,separators = T)),
          rate = colDef(name = "Rate", minWidth = 60, format = colFormat(digits = 3)),
          n = colDef(name = "Total", minWidth = 60)
        ),
        columnGroups = list(
          colGroup(name = "Incidents", columns = c("rate", "n"))
        ),
        highlight = TRUE,
        compact = T,
        theme = reactableTheme(
          style = list(fontFamily = "Source Sans Pro, Helvetica, Arial, sans-serif")
        )
      )
  })
  
  
  output$top5 <- renderReactable({
    req(input$mapS)
   df %>% 
      st_drop_geometry() %>% 
      count(STATE, SYS) %>% 
      group_by(STATE) %>% 
      summarize(allSys = sum(n), 
                HL = sum(n[SYS == "HL"], na.rm = T),
                GT = sum(n[SYS == "GT"], na.rm = T),
                GD = sum(n[SYS == "GD"], na.rm = T),
                GG = sum(n[SYS == "GG"], na.rm = T),
                UNGS = sum(n[SYS == "UNGS"], na.rm = T)
      )%>% 
    slice_max(order_by = allSys, n = 5) %>%
      mutate(STATE = state.name[match(STATE, state.abb)]) %>%
      reactable(
        columns = list(
          STATE = colDef(name = "State", minWidth = 75),
          allSys = colDef(name = "Total", minWidth = 60),
          HL = colDef(minWidth  = 33, headerStyle = list(color = honey)),
          GD =colDef(minWidth  = 30, headerStyle = list(color = darkBlue)),
          GT = colDef(minWidth  = 30, headerStyle = list(color = liteGreen)),
          GG = colDef(minWidth  = 30, headerStyle = list(color = lilac)),
          UNGS = colDef(minWidth = 50, headerStyle = list(color = liteBlue))
        ),
        columnGroups = list(
          colGroup(name = "Incidents", columns = c("allSys", "HL","GT", "GD", "GG", "UNGS"))
        ),
        highlight = TRUE,
        compact = T,
        theme = reactableTheme(
          style = list(fontFamily = "Source Sans Pro, Helvetica, Arial, sans-serif")
        )
      )
  })
  
  
  output$two_ten <- renderReactable({
    inc_df<- df %>%
      st_drop_geometry()%>%
      filter(IYEAR == 2022, SYS %in% c("HL", "GD", "GT"))%>%
      group_by(SYS) %>%
      mutate(bin = quantile(TOTAL_COST_CURRENT, probs = c(.9))[[1]],
             top10 = ifelse(TOTAL_COST_CURRENT>=bin, T, F))%>%
      ungroup()%>%
      filter(top10)%>%
      group_by(OPERATOR_ID)%>%
      mutate(n = n())%>%
      ungroup()%>%
      filter(n >1)%>%
      mutate(Date = stamp("Jan 31", orders = "md")(MDY),
             Location = cleanLoc,
             COMMODITY_RELEASED_TYPE = ifelse(grepl("NON-HVL", COMMODITY_RELEASED_TYPE),
                                              "Refined and/or Petrol. Product (Non-HVL)", 
                                              COMMODITY_RELEASED_TYPE),
             Release = paste0(scales::comma(TOTAL_RELEASE)," " ,UNITS, " of ", COMMODITY_RELEASED_TYPE),
             Cost = scales::dollar(TOTAL_COST_CURRENT, scale = .000001, suffix = "M"),
             Cause = str_to_sentence(CAUSE))%>%
      select(NAME, Date, Location, Release, Cost, Cause)
      
    
    t_df <- df %>%
      st_drop_geometry()%>%
      filter(IYEAR == 2022, SYS %in% c("HL", "GD", "GT"))%>%
      group_by(SYS) %>%
      mutate(bin = quantile(TOTAL_COST_CURRENT, probs = c(.9))[[1]],
             top10 = ifelse(TOTAL_COST_CURRENT>=bin, T, F))%>%
      ungroup()%>%
      filter(top10)%>%
      group_by(OPERATOR_ID) %>%
      summarize(
        inc = n(),
        cost = sum(TOTAL_COST_CURRENT, na.rm = T),
        top10 = sum(top10 == T, na.rm = T) 
      )%>% #filter(top10 > 0) %>% summarize(n = n(), inc = sum(inc), cost = sum(cost)) %>%tibble::view()
      right_join(
        all_miles %>%
          filter(IYEAR == 2022)%>%
          distinct(OPERATOR_ID,NAME, pri.id, STATE, IYEAR, SYS,.keep_all = T)%>%
          group_by(OPERATOR_ID,NAME, pri.id, pri.name)%>%
          summarise(miles = sum(mileage, na.rm = T))
      )%>%
      tidyr::replace_na(list(inc = 0, cost = 0, top10 = 0))%>%
      distinct(OPERATOR_ID, .keep_all = T)%>%
      filter(top10 >1)%>%
      select(NAME, pri.name, cost, miles)%>%
      mutate(cost = scales::dollar(cost, scale = .000001, suffix = "M"))
    
      reactable(
        t_df,
        columns = list(
          NAME = colDef(name = "Operator", minWidth = 70),
          pri.name = colDef(name = "Primary", minWidth = 55),
          cost = colDef(name = "Damage Cost", minWidth  = 40),
          miles =colDef(name = "2022 Miles",minWidth  = 30, format = colFormat(separators = T, digits = 0))
        ),
        details = function(index) {
          justinc <- filter(inc_df, NAME == t_df$NAME[index]) %>% select(-NAME)
          tbl <- reactable(justinc, outlined = TRUE, highlight = TRUE, fullWidth = T)
          htmltools::div(style = list(margin = "12px 45px"), tbl)
        },
        onClick = "expand",
        rowStyle = list(cursor = "pointer"),
        highlight = TRUE,
        compact = T,
        theme = reactableTheme(
          style = list(fontFamily = "Source Sans Pro, Helvetica, Arial, sans-serif")
        )
      )
  })
  
  
  #### plots ####
  #plot incidents by month per year 
  output$yearPlot <- renderPlot({
    df_all_yr %>%
      filter(IYEAR < 2023)%>%
    count(month(MDY, label = T), IYEAR)%>%
    rename(month = 1)%>%
    mutate(yr = if_else(IYEAR == 2022, 2022, 2010))%>%
    ggplot(aes(x = month, y = n, col = as.factor(yr), group = IYEAR))+
    geom_line(aes(linewidth = as.factor(yr),alpha = as.factor(yr)))+
    annotate("label", x = 2, y = 52, label = "2022", 
             col = liteGreen)+
    scale_color_manual(values = c("gray55", liteGreen))+
    scale_linewidth_manual(values = c(0.5,1.2))+
    scale_alpha_manual(values = c(0.6,0.99))+
    guides(colour = "none", alpha = "none", linewidth = "none")+
    theme_pst()+
    labs(title = "Monthly Incident Counts by Year",
         subtitle = "Highlighting 2022",
         y = "Incidents",
         x = NULL,
         tag = "PST 2023", caption = "PHMSA 2023 Incident Flagged Files")
    })
  
  #incidents by mile
  # df_all %>%
  #   count(IYEAR)%>%
  #   left_join
  #   rename(month = 1)%>%
  #   mutate(yr = if_else(IYEAR == 2022, 2022, 2010))%>%
  #   ggplot(aes(x = month, y = n, col = as.factor(yr), group = IYEAR))+
  #   geom_line(aes(linewidth = as.factor(yr),alpha = as.factor(yr)))+
  #   scale_color_manual(values = c("gray55", liteGreen))+
  #   scale_linewidth_manual(values = c(0.5,1.2))+
  #   scale_alpha_manual(values = c(0.6,0.99))+
  #   theme_pst()
  
  
  
  #total cost context
  output$graphPlot <- renderPlot({
    req(input$graphs)
    if(input$graphs == "cost"){
      #total cost per year
      df_all %>%
        group_by( IYEAR)%>%
        summarize(TOTAL_COST_CURRENT = sum(TOTAL_COST_CURRENT, na.rm = T))%>%
        mutate(yr = if_else(IYEAR == 2022, 2022, 2010))%>%
        ggplot(aes(x = IYEAR, y = TOTAL_COST_CURRENT, fill = as.factor(yr), col = as.factor(yr)))+
        geom_bar(stat = "identity")+
        scale_fill_manual(values = c("gray55", liteGreen))+
        scale_color_manual(values = c("gray55", liteGreen))+
        scale_y_continuous(labels = scales::label_dollar(scale = .000000001, suffix = "B"), 
                           name = "Cost of Damage (2022 USD)")+
        scale_x_continuous(breaks = seq(2010,2022,2), name = NULL)+
        guides(color = "none")+
        theme_pst()+
        guides(fill = "none")
    }
    else if(input$graphs == "costContext"){
      df_sum <- df_all %>%
        filter(IYEAR %in% c(2010,2018))%>%
        group_by(IYEAR)%>%
        summarize(cost = sum(TOTAL_COST_CURRENT))%>%
        st_drop_geometry()
      
      df_lab <- df_all %>%
        filter(TOTAL_COST_CURRENT > 1e9) %>%
        left_join(df_sum, by = "IYEAR")%>%
        mutate(label = paste0( "<b>OPERATOR:</b> ", word(NAME,1,2),  " <br> ",
                               "<b>TOTAL COST:</b>", scales::dollar(TOTAL_COST_CURRENT), "<br>",
                               "<b>PERCENT OF ANNUAL COST:</b>", scales::percent(TOTAL_COST_CURRENT/(cost))),
               labx = IYEAR,
               laby = TOTAL_COST_CURRENT)%>%
        select(label, labx, laby, IYEAR, cost) %>%
        st_drop_geometry()
      
      #plot of total cost context
      df_all %>%
        mutate(huge = TOTAL_COST_CURRENT > 1e9)%>%
        group_by( IYEAR, huge)%>%
        summarize(TOTAL_COST_CURRENT = sum(TOTAL_COST_CURRENT, na.rm = T))%>%
        mutate(yr = if_else(IYEAR == 2022, 2022, 2010),
               huge = if_else(IYEAR == 2022, 2, ifelse(huge,1,0 )))%>%
        ggplot(aes(x = IYEAR, y = TOTAL_COST_CURRENT, fill = as.factor(yr), col = as.factor(-(huge))))+
        geom_bar(stat = "identity", linewidth = 1)+
        annotate(geom = "richtext", label = df_lab[1, "label"],
                 x = 2013, y = .9e9, size = 2)+
        annotate(geom = "richtext", label = df_lab[2, "label"],
                 x = 2015, y = 1.9e9, size = 2)+
        scale_fill_manual(values = c("gray55", liteGreen))+
        scale_y_continuous(labels = scales::label_dollar(scale = .000000001, suffix = "B"),
                           name = "Cost of Damage (2022 USD)")+
        scale_color_manual(values = c(liteGreen, honey, "gray55"))+
        scale_x_continuous(breaks = seq(2010,2022,2), name = NULL)+
        guides(fill = "none", colour = "none")+
        theme_pst()
    }
    else if(input$graphs == "release"){
      df_all %>%
        filter(SYS %in% c("HL", "GT"))%>%
        group_by( IYEAR, UNITS, SYS)%>%
        summarize(TOTAL_RELEASE = sum(TOTAL_RELEASE, na.rm = T))%>%
        mutate(yr = if_else(IYEAR == 2022, 2022, 2010),
               UNITS = ifelse(SYS == "GT", "Gas Trans.\n(MMSCF)", "Haz. Liquids\n(K Gal.)"),
               SYS = ifelse(SYS == "GT", "Gas Trans.\n(All Systems)", "Hazardous \nLiquids"))%>%
        ggplot(aes(x = IYEAR, y = TOTAL_RELEASE, col = SYS, fill = SYS))+
        geom_smooth(method = lm, linewidth = .5, linetype = "dashed")+
        geom_line(linewidth = 1)+
        scale_colour_manual(values = c( darkBlue, honey))+
        scale_fill_manual(values = c( darkBlue, honey))+
        scale_x_continuous(breaks = seq(2010,2022,4), name = NULL)+
        scale_y_continuous(labels = scales::label_comma(scale = .001),
                           name = "Total Annual Releases")+
        facet_wrap(~UNITS, scales = "free_y")+
        theme_pst()+
        theme(strip.background = element_rect(fill = "gray95"),
              strip.text.x = element_text(colour = "gray15"),
              legend.position = "none")
    }
    else if(input$graphs == "releaseMi"){
      df_all %>%
        filter(SYS %in% c("HL", "GT"))%>%
        group_by( IYEAR, UNITS, SYS)%>%
        summarize(TOTAL_RELEASE = sum(TOTAL_RELEASE, na.rm = T))%>%
        left_join(miles %>% group_by(IYEAR, SYS) %>% summarize(miles = sum(miles, na.rm = T)),
                  by =c("IYEAR", "SYS"))%>%
        mutate(yr = if_else(IYEAR == 2022, 2022, 2010),
               UNITS = ifelse(SYS == "GT", "Gas Trans.\n(MMSCF)", "Haz. Liquids\n(K Gal.)"),
               SYS = ifelse(SYS == "GT", "Gas Trans.\n(All Systems)", "Hazardous \nLiquids"),
               relMile = TOTAL_RELEASE / (miles))%>%
        ggplot(aes(x = IYEAR, y = relMile, col = SYS, fill = SYS))+
        geom_smooth(method = lm, linewidth = .5, linetype = "dashed")+
        geom_line(linewidth = 1)+
        scale_colour_manual(values = c( darkBlue, honey))+
        scale_fill_manual(values = c( darkBlue, honey))+
        scale_x_continuous(breaks = seq(2010,2022,4), name = NULL)+
        scale_y_continuous(labels = scales::label_comma(.001),
                           name = "Total Releases per 1,000 Miles")+
        facet_wrap(~UNITS, scales = "free_y")+
        theme_pst()+
        theme(strip.background = element_rect(fill = "gray95"),
              strip.text.x = element_text(colour = "gray15"),
              legend.position = "none")
    }
    else if(input$graphs == "releaseInc"){
      df_all %>%
        filter(SYS %in% c("HL", "GT"))%>%
        group_by( IYEAR, UNITS, SYS)%>%
        summarize(TOTAL_RELEASE = sum(TOTAL_RELEASE, na.rm = T),
                  n= n())%>%
        mutate(yr = if_else(IYEAR == 2022, 2022, 2010),
               UNITS = ifelse(SYS == "GT", "Gas Trans.\n(MMSCF)", "Haz. Liquids\n(K Gal.)"),
               SYS = ifelse(SYS == "GT", "Gas Trans.\n(All Systems)", "Hazardous \nLiquids"),
               relInc = TOTAL_RELEASE / n)%>%
        ggplot(aes(x = IYEAR, y = relInc, col = SYS, fill = SYS))+
        geom_smooth(method = lm, linewidth = .5, linetype = "dashed")+
        geom_line(linewidth = 1)+
        scale_colour_manual(values = c( darkBlue, honey))+
        scale_fill_manual(values = c( darkBlue, honey))+
        scale_x_continuous(breaks = seq(2010,2022,4), name = NULL)+
        scale_y_continuous(labels = scales::label_comma(),
                           name = "Average Release per Incident")+
        facet_wrap(~UNITS, scales = "free_y")+
        theme_pst()+
        theme(strip.background = element_rect(fill = "gray95"),
              strip.text.x = element_text(colour = "gray15"),
              legend.position = "none")
    }
    
    else if(input$graphs == "releaseTop"){
      df_all %>%
        st_drop_geometry()%>%
        filter(IYEAR == 2022, SYS %in% c("HL", "GD", "GT"))%>%
        group_by(SYS) %>%
        mutate(bin = quantile(TOTAL_RELEASE, probs = c(.9))[[1]],
               top10 = ifelse(TOTAL_RELEASE>=bin, T, F))%>%
        ungroup()%>%
        group_by(SYS, top10)%>%
        summarize(rel = sum(TOTAL_RELEASE, na.rm = T))%>%
        group_by(SYS)%>%
        mutate(allRel = sum(rel, na.rm = T))%>%
        ungroup()%>%
        mutate(rel = rel / allRel,
               top10 = ifelse(top10, "Top", "Not"))%>%
        #tidyr::pivot_wider(id_cols = SYS, values_from = rel, names_from = top10)%>%
        ggplot(aes(x = reorder(SYS, -allRel), y = rel, fill = top10))+
        geom_bar(stat = "identity", position = "dodge", width = .65)+
        # geom_linerange(aes(ymin = Not, ymax = Top), col = "gray33")+
        # geom_point(aes(y = Not, col = "<90"), size = 2.7)+
        # geom_point(aes(y = Top, col = "≥90"), size = 2.7)+
        scale_fill_manual(values = c( midBlue, crimson), name = NULL,
                          labels = c("Bottom 90%", "Top 10%"))+
        scale_y_continuous(labels = scales::label_percent(),
                           name = "2022 Release Percentage")+
        theme_pst()+
        theme(strip.background = element_rect(fill = "gray95"),
              strip.text.x = element_text(colour = "gray15"),
              legend.position = c(.8,.8))+
        labs(x = "System")
    }
    else if(input$graphs == "costTop"){
      df_all %>%
        st_drop_geometry()%>%
        filter(IYEAR == 2022, SYS %in% c("HL", "GD", "GT"))%>%
        group_by(SYS) %>%
        mutate(bin = quantile(TOTAL_COST_CURRENT, probs = c(.9))[[1]],
               top10 = ifelse(TOTAL_COST_CURRENT>=bin, T, F))%>%
        ungroup()%>%
        group_by(SYS, top10)%>%
        summarize(cost = sum(TOTAL_COST_CURRENT, na.rm = T))%>%
        group_by(SYS)%>%
        mutate(allCost = sum(cost, na.rm = T))%>%
        ungroup()%>%
        mutate(top10 = ifelse(top10, "Top", "Not"),
               per = cost/allCost,
               cost_lab = scales::dollar(cost, scale = .000001, suffix = "M", accuracy = .1))%>%
        ggplot(aes(x = reorder(SYS, -allCost), y = per, fill = top10))+
        geom_bar(stat = "identity", position = "dodge", width = .65)+
        geom_text(aes(label = cost_lab, y = per + .05), position = position_dodge(width = .65), size = 2)+
        scale_fill_manual(values = c( midBlue, crimson), name = NULL,
                          labels = c("Bottom 90%", "Top 10%"))+
        scale_y_continuous(labels = scales::label_percent(),
                           name = "2022 Damage Cost")+
        theme_pst()+
        theme(strip.background = element_rect(fill = "gray95"),
              strip.text.x = element_text(colour = "gray15"),
              legend.position = c(.8,.8))+
        labs(x = "System")
    }
    #### operator graph ####
    else if(input$graphs == "operatorScatter1"){ # no inc
      df %>%
        st_drop_geometry()%>%
        filter(IYEAR == 2022, SYS %in% c("HL", "GD", "GT"))%>%
        group_by(SYS) %>%
        mutate(bin = quantile(TOTAL_COST_CURRENT, probs = c(.9))[[1]],
               top10 = ifelse(TOTAL_COST_CURRENT>=bin, T, F))%>%
        ungroup()%>%
        group_by(OPERATOR_ID) %>%
        summarize(
          inc = n(),
          cost = sum(TOTAL_COST_CURRENT, na.rm = T),
          top10 = sum(top10 == T, na.rm = T) 
        )%>% #filter(top10 > 0) %>% summarize(n = n(), inc = sum(inc), cost = sum(cost)) %>%tibble::view()
        right_join(
          all_miles %>%
            filter(IYEAR == 2022)%>%
            distinct(OPERATOR_ID,NAME, pri.id, STATE, IYEAR, SYS,.keep_all = T)%>%
            group_by(OPERATOR_ID,NAME, pri.id, pri.name)%>%
            summarise(miles = sum(mileage, na.rm = T)),
          by = "OPERATOR_ID"
        )%>% 
        tidyr::replace_na(list(inc = 0, cost = 0, top10 = 0))%>%
        mutate(none = ifelse(inc == 0, T, F))%>%
        distinct(OPERATOR_ID, .keep_all = T)%>%
        # mutate(any = inc>0)%>%
        # group_by(any)%>%
        # summarise(miles = sum(miles),
        #           n = n())
        ggplot(aes(x = miles, y = inc, size = cost, col = factor(none)))+
        geom_point(alpha = .7)+
        scale_y_continuous()+
        scale_x_continuous(labels = scales::label_comma(scale = .001, suffix = "K"))+
        scale_size_continuous(range = c(1,12), breaks = c( 1e7, 1e8,  5e8), limits = c(0,5e8),
                              labels = scales::label_dollar(scale = .000001, suffix = "M"))+
        # scale_colour_binned(low = "#93BBCC", high = darkBlue, breaks = c(0.5,1.5, 2),
        #                     labels = c("0", "1", "2"))+
        scale_color_manual( values = c("gray85",liteGreen))+
        guides(colour = "none")+
        theme_pst()+
        theme(strip.background = element_rect(fill = "gray95"),
              strip.text.x = element_text(colour = "gray15"),
              legend.position = c(.8,.65))+
        labs(x = "Miles", y = "Incidents", size = "Damage Cost", color = "90th Percentile Incidents")
    }
    else if(input$graphs == "operatorScatter2"){ #top 10 inc 
      df %>%
        st_drop_geometry()%>%
        filter(IYEAR == 2022, SYS %in% c("HL", "GD", "GT"))%>%
        group_by(SYS) %>%
        mutate(bin = quantile(TOTAL_COST_CURRENT, probs = c(.9))[[1]],
               top10 = ifelse(TOTAL_COST_CURRENT>=bin, T, F))%>%
        ungroup()%>%
        group_by(OPERATOR_ID) %>%
        summarize(
          inc = n(),
          cost = sum(TOTAL_COST_CURRENT, na.rm = T),
          top10 = sum(top10 == T, na.rm = T) 
        )%>% #filter(top10 > 0) %>% summarize(n = n(), inc = sum(inc), cost = sum(cost)) %>%tibble::view()
        right_join(
          all_miles %>%
            filter(IYEAR == 2022)%>%
            distinct(OPERATOR_ID,NAME, pri.id, STATE, IYEAR, SYS,.keep_all = T)%>%
            group_by(OPERATOR_ID,NAME, pri.id, pri.name)%>%
            summarise(miles = sum(mileage, na.rm = T))
        )%>%
        distinct(OPERATOR_ID, .keep_all = T)%>%
        tidyr::replace_na(list(inc = 0, cost = 0, top10 = 0))%>%
        ggplot(aes(x = miles, y = inc, size = cost, col = top10))+
        geom_point(alpha = .7)+
        scale_y_continuous()+
        scale_x_continuous(labels = scales::label_comma(scale = .001, suffix = "K"))+
        scale_size_continuous(range = c(1,12), breaks = c( 1e7, 1e8,  5e8), limits = c(0,5e8),
                              labels = scales::label_dollar(scale = .000001, suffix = "M"))+
        # scale_colour_binned(low = "#93BBCC", high = darkBlue, breaks = c(0.5,1.5, 2),
        #                     labels = c("0", "1", "2"))+
        #scale_color_manual( values = c("gray85","#93BBCC", darkBlue))+
        scale_color_steps(low = "gray85", high = crimson, guide = "legend", breaks = c(.1,1, 2), 
                          labels = c(0,1,2))+
        theme_pst()+
       theme(strip.background = element_rect(fill = "gray95"),
              strip.text.x = element_text(colour = "gray15"),
              legend.position = c(.8,.65))+
        labs(x = "Miles", y = "Incidents", size = "Damage Cost", color = "90th Percentile Incidents")
    }
    else if(input$graphs == "operatorScatter3"){
      
      
      df %>%
        st_drop_geometry()%>%
        filter(IYEAR == 2022, SYS %in% c("HL", "GD", "GT"))%>%
        group_by(SYS) %>%
        mutate(bin = quantile(TOTAL_COST_CURRENT, probs = c(.9))[[1]],
               top10 = ifelse(TOTAL_COST_CURRENT>=bin, T, F))%>%
        ungroup()%>%
        group_by(OPERATOR_ID) %>%
        summarize(
          inc = n(),
          cost = sum(TOTAL_COST_CURRENT, na.rm = T),
          top10 = sum(top10 == T, na.rm = T) ,
          fatal = sum(FATAL, na.rm = T),
          injure = sum(INJURE, na.rm = T)
        )%>%
        right_join(
          all_miles %>%
            filter(IYEAR == 2022)%>%
            distinct(OPERATOR_ID,NAME, pri.id, STATE, IYEAR, SYS,.keep_all = T)%>%
            group_by(OPERATOR_ID,NAME, pri.id, pri.name)%>%
            summarise(miles = sum(mileage, na.rm = T))
        )%>%
        tidyr::replace_na(list(inc = 0, cost = 0))%>%
        group_by(OPERATOR_ID, miles)%>%
        mutate(ir = sum(inc,na.rm = T)/miles,
               ir = ifelse(sum(inc,na.rm = T) == 0, 0, ir),
               ir = ifelse(ir == Inf, 0, ir))%>%
        ungroup()%>%
        distinct(OPERATOR_ID, .keep_all = T)%>%
        mutate(hi_ir = if_else(ir> nat_ir, T, F),
               hi_ir = ifelse(is.na(hi_ir), F, hi_ir),
               mi_ir = miles * nat_ir,
               mid_id = median(ir),
               mult_ir = (ir-nat_ir)/nat_ir,
               i_d = inc - (miles*nat_ir),
               mir = ir/nat_ir)%>% # tibble:: view()
        #pull(hi_ir)%>% table() # tibble:: view()
        #filter(hi_ir == F & inc > 4)%>% #tibble:: view()
        #filter(ir < .5)%>%
        ggplot(aes(x = miles, y = inc, size = cost, col = i_d))+
        geom_abline(intercept = .01, slope = nat_ir,
                    linetype = "dashed", alpha = .6, col = lilac)+
        geom_point(alpha = .7)+
        scale_y_continuous()+
        scale_x_continuous(labels = scales::label_comma(scale = .001, suffix = "K"))+
        scale_size_continuous(range = c(1,12), breaks = c( 1e7, 1e8,  5e8), limits = c(0,5e8),
                              labels = scales::label_dollar(scale = .000001, suffix = "M"))+
        # scale_colour_binned(low = "#93BBCC", high = darkBlue, breaks = c(0.5,1.5, 2),
        #                     labels = c("0", "1", "2"))+
        # #scale_color_manual( values = c("gray85","#93BBCC", darkBlue))+
        # scale_color_steps(low = "gray85", high = darkBlue, guide = "legend", breaks = c(.5,1, 2), 
        #                   labels = c(0,1,2))+
       # scale_color_manual(values = c(darkBlue, crimson), labels = c("Below Avg.", "Above Avg."))+
        scale_color_gradient2(low = darkBlue, mid = "#ffffd0", high = crimson, midpoint = 0, limits = c(-6,22))+
        theme_pst()+
        theme(strip.background = element_rect(fill = "gray95"),
              strip.text.x = element_text(colour = "gray15"),
              legend.position = c(.8,.65))+
        labs(x = "Miles", y = "Incidents", size = "Damage Cost", color = "Incident Rate Compared to Average\n(X times the Average Rate)")
    }
    
    else if(input$graphs == "operatorScatter3b"){
      
      
      df %>%
        st_drop_geometry()%>%
        filter(IYEAR == 2022, SYS %in% c("HL", "GD", "GT"))%>%
        group_by(SYS) %>%
        mutate(bin = quantile(TOTAL_COST_CURRENT, probs = c(.9))[[1]],
               top10 = ifelse(TOTAL_COST_CURRENT>=bin, T, F))%>%
        ungroup()%>%
        group_by(OPERATOR_ID) %>%
        summarize(
          inc = n(),
          cost = sum(TOTAL_COST_CURRENT, na.rm = T),
          top10 = sum(top10 == T, na.rm = T) ,
          fatal = sum(FATAL, na.rm = T),
          injure = sum(INJURE, na.rm = T)
        )%>%
        right_join(
          all_miles %>%
            filter(IYEAR == 2022)%>%
            distinct(OPERATOR_ID,NAME, pri.id, STATE, IYEAR, SYS,.keep_all = T)%>%
            group_by(OPERATOR_ID,NAME, pri.id, pri.name)%>%
            summarise(miles = sum(mileage, na.rm = T))
        )%>%
        tidyr::replace_na(list(inc = 0, cost = 0))%>%
        group_by(OPERATOR_ID, miles)%>%
        mutate(ir = sum(inc,na.rm = T)/miles,
               ir = ifelse(sum(inc,na.rm = T) == 0, 0, ir),
               ir = ifelse(ir == Inf, 0, ir))%>%
        ungroup()%>%
        distinct(OPERATOR_ID, .keep_all = T)%>%
        mutate(hi_ir = if_else(ir> nat_ir, T, F),
               hi_ir = ifelse(is.na(hi_ir), F, hi_ir),
               mi_ir = miles * nat_ir,
               mid_id = median(ir),
               mult_ir = (ir-nat_ir)/nat_ir,
               i_d = inc - (miles*nat_ir),
               mir = ir/nat_ir)%>% # tibble:: view() 
        mutate(col = if_else(inc > 9, T, F))%>%
      #pull(hi_ir)%>% table() # tibble:: view()
      #filter(hi_ir == F & inc > 4)%>% #tibble:: view()
      #filter(ir < .5)%>%
      ggplot(aes(x = miles, y = inc, size = cost, col = col))+
        geom_abline(intercept = .01, slope = nat_ir,
                    linetype = "dashed", alpha = .6, col = lilac)+
        geom_point(alpha = .7)+
        scale_y_continuous()+
        scale_x_continuous(labels = scales::label_comma(scale = .001, suffix = "K"))+
        scale_size_continuous(range = c(1,9), breaks = c( 1e7, 1e8,  5e8), limits = c(0,5e8),
                              labels = scales::label_dollar(scale = .000001, suffix = "M"))+
        scale_color_manual( values = c("gray85",crimson))+
        theme_pst()+
        theme(strip.background = element_rect(fill = "gray95"),
              strip.text.x = element_text(colour = "gray15"),
              legend.position = c(.8,.65))+
        labs(x = "Miles", y = "Incidents", size = "Damage Cost", color = "Incident Rate Compared to Average\n(X times the Average Rate)")
    }
    
    
    else if(input$graphs == "operatorScatter4"){
      
      
      pdf <- df %>%
        st_drop_geometry()%>%
        filter(IYEAR == 2022, SYS %in% c("HL", "GD", "GT"))%>%
        group_by(SYS) %>%
        mutate(bin = quantile(TOTAL_COST_CURRENT, probs = c(.9))[[1]],
               top10 = ifelse(TOTAL_COST_CURRENT>=bin, T, F))%>%
        ungroup()%>%
        group_by(OPERATOR_ID) %>%
        summarize(
          inc = n(),
          cost = sum(TOTAL_COST_CURRENT, na.rm = T),
          top10 = sum(top10 == T, na.rm = T) 
        )%>%
        right_join(
          all_miles %>%
            filter(IYEAR == 2022)%>%
            distinct(OPERATOR_ID,NAME, pri.id, STATE, IYEAR, SYS,.keep_all = T)%>%
            group_by(OPERATOR_ID,NAME, pri.id, pri.name)%>%
            summarise(miles = sum(mileage, na.rm = T))
        )%>%
        tidyr::replace_na(list(inc = 0, cost = 0))%>%
        filter(inc > 0)%>%
        group_by(OPERATOR_ID, miles)%>%
        mutate(ir = n()/miles,
               ir = ifelse(n() == 0, 0, ir),
               ir = ifelse(ir == Inf, 0, ir))%>%
        ungroup()%>%
        mutate(hi_ir = if_else(inc > miles * nat_ir, T, F),
               hi_ir = ifelse(is.na(hi_ir), F, hi_ir),
               mi_ir = miles * nat_ir,
               mid_id = median(ir),
               mult_ir = (ir-nat_ir)/nat_ir,
               i_d = inc - (miles*nat_ir)) %>% 
        filter(inc > 0)
        #filter(hi_ir == F & inc > 4)%>% #tibble:: view()
        #filter(ir < .5)%>%
        pdf %>%
        ggplot(aes(x = ir, y = cost/inc, col = inc))+
          geom_hline(yintercept = median(pdf$cost))+
          geom_vline(xintercept = nat_ir)+
        geom_point(alpha = .7)+
        scale_y_continuous(trans = "pseudo_log")+
       scale_x_continuous(labels = scales::label_comma(), trans = "log2")+
        scale_size_continuous(range = c(1,9), breaks = c( 1e7, 1e8,  5e8), limits = c(0,5e8),
                              labels = scales::label_dollar(scale = .000001, suffix = "M"))+
        # scale_colour_binned(low = "#93BBCC", high = darkBlue, breaks = c(0.5,1.5, 2),
        #                     labels = c("0", "1", "2"))+
        # #scale_color_manual( values = c("gray85","#93BBCC", darkBlue))+
        # scale_color_steps(low = "gray85", high = darkBlue, guide = "legend", breaks = c(.5,1, 2), 
        #                   labels = c(0,1,2))+
        # scale_color_manual(values = c(darkBlue, crimson), labels = c("Below Avg.", "Above Avg."))+
       # scale_color_gradient2(low = darkBlue, mid = "#ffffd0", high = crimson, limits = c(-5,22))
      theme_pst()+
        theme(strip.background = element_rect(fill = "gray95"),
              strip.text.x = element_text(colour = "gray15"),
              legend.position = c(.8,.8))+
        labs(x = "Incident Rate", y = "Cost", size = "Damage Cost", color = "Incident Rate")
    }
    
    else{}
    
  })
  
  
  df_all %>%
    group_by(IYEAR) %>%
    summarize(Incidents = n(),
              )

  # total release
  
  
  
  df %>%
    group_by(NAME)%>%
    summarise(n = n(),
              cost = sum(TOTAL_COST_CURRENT),
              mileage = list(unique(mileage))) #%>% tibble::view()
    
    
    #end on IM slide? 
  
  #### all inc ####
  output$all_inc <- renderReactable({
    req(input$table_type)
    df_22 <- df %>%
      st_drop_geometry()%>%
      filter(IYEAR == 2022)%>%
      mutate(Date = MDY,
             Location = cleanLoc,
             COMMODITY_RELEASED_TYPE = ifelse(grepl("NON-HVL", COMMODITY_RELEASED_TYPE),
                                              "Refined and/or Petrol. Product (Non-HVL)", 
                                              COMMODITY_RELEASED_TYPE),
             Release = paste0(scales::comma(TOTAL_RELEASE, accuracy = 1)," " ,UNITS, " of ", COMMODITY_RELEASED_TYPE),
             Cause = str_to_sentence(CAUSE))%>%
      rename(Fatalities = FATAL, 
             Injuries = INJURE,
             Operator = NAME,
             Cost = TOTAL_COST_CURRENT,
             Primary = pri.name,
             State = STATE,
             System = SYS)%>%
      select(Operator, Primary, Date, System, State, Location, Release, 
             Cost, Cause, Fatalities, Injuries)
    
    if(input$table_type == "Operator"){
      #data
      a_df <- df %>%
        st_drop_geometry()%>%
        group_by(NAME, pri.name) %>%
        summarize(
          Incidents = n(),
          gt_inc = sum(SYS == "GT"),
          gd_inc = sum(SYS == "GD"),
          gg_inc = sum(SYS == "GG"),
          ungs_inc = sum(SYS == "UNGS"),
          hl_inc = sum(SYS == "HL"),
          Sys = list(unique(SYS)),
          States = list(unique(STATE)),
          tot_cost = sum(TOTAL_COST_CURRENT, na.rm = T),
          tot_fat = sum(FATAL, na.rm = T),
          tot_inj = sum(INJURE, na.rm = T)
        ) %>%  #filter(top10 > 0) %>% summarize(n = n(), inc = sum(inc), cost = sum(cost)) %>%tibble::view()
        left_join(
          all_miles %>%
            filter(IYEAR == 2022)%>%
            distinct(OPERATOR_ID,NAME, pri.id, STATE,SYS,.keep_all = T)%>%
            group_by(OPERATOR_ID,NAME, pri.id, pri.name)%>%
            summarise(miles = sum(mileage, na.rm = T),
                      Sys = list(unique(SYS))),
          by = c("NAME" = "NAME", "pri.name" = 'pri.name')
        )%>%
        tidyr::replace_na(list(Incidents = 0, tot_cost = 0, tot_inj = 0, tot_fat = 0)) %>%
       mutate(ipm = Incidents /( miles/1000)) %>%
        distinct(OPERATOR_ID, .keep_all = T)
      #table
      reactable(
        a_df %>% select(!ends_with("_inc") & !c("OPERATOR_ID", "pri.id", "Sys.y")) ,
        defaultSorted = list(Incidents = "desc"),
        columns = list(
          NAME = colDef(name = "Operator", minWidth = 75),
          pri.name = colDef(name = "Primary", minWidth = 60),
          Sys.x = colDef(name = "System", minWidth = 25,
                         html = T,
                         cell = function(value, index){
                           l = length(value)
                           weight = "bold"
                           vals <- HTML("")
                           for(i in 1:l){
                             ival = value[i]
                             color <- case_when(
                               ival == "GT" ~ darkBlue,
                               ival == "GD" ~ liteGreen,
                               ival == "GG" ~ lilac,
                               ival == "HL" ~ honey,
                               ival == "UNGS" ~ liteBlue,
                               .default = "gray35"
                             )

                             val <- HTML("<span style = \"font-weight:bold; color:", color, ";\">",
                                            ival, "</span>")
                             vals <- HTML(vals,ifelse(i >1, "&", ""), val)
                             
                           }
                           vals

                         }
                         
                           # inline style color and system 
                           #this shouolndt be a style it should be a cell???
                           # style split at commas? Idk how exactly
                         ),
          States = colDef(name = "States", minWidth = 30),
          tot_cost = colDef(name = "Cost", minWidth = 30,
                            cell = function(value) if_else(value > 999999,
                                                           scales::dollar(value, scale = .000001, accuracy = .1, suffix = "M"),
                                                           if_else(value > 999, 
                                                                   scales::dollar(value, scale = .001, accuracy = .1, suffix = "K"),
                                                                   scales::dollar(value)) 
                            ),
                            style = function(value) {
                              if (value >= 1e6) {
                                color <- crimson
                                weight = "bold"
                              } else if (value >= 1e3){
                                color <- crimson
                                weight = "normal"
                              } else {
                                color <- "black"
                                weight = "normal"
                              }
                              list(color = color, fontWeight = weight)
                            }),
          tot_fat = colDef(name = "Fatalities", minWidth = 30,
                           style = function(value) {
                             if (value >= 2) {
                               color <- crimson
                               weight = "bold"
                             } else if (value >= 1){
                               color <- crimson
                               weight = "normal"
                             } else {
                               color <- "black"
                               weight = "normal"
                             }
                             list(color = color, fontWeight = weight)
                           }),
          tot_inj = colDef(name = "Injuries", minWidth = 30,
                           style = function(value) {
                             if (value >= 2) {
                               color <- crimson
                               weight = "bold"
                             } else if (value >= 1){
                               color <- crimson
                               weight = "normal"
                             } else {
                               color <- "black"
                               weight = "normal"
                             }
                             list(color = color, fontWeight = weight)
                           }),
          miles =colDef(name = "2022 Miles",minWidth  = 30, format = colFormat(separators = T, digits = 0)),
          ipm = colDef(name = "Incident Rate", format= colFormat(digits = 3) )
        ),
        details = function(index) {
          justinc <- filter(df_22, Operator == a_df$NAME[index]) %>% select(Date, System, Location,Cause, Release, Cost, Fatalities, Injuries) # update with if elses to address ot
          tbl <- reactable(justinc, outlined = TRUE, highlight = TRUE, fullWidth = T,
                           columns = list(
                             Cost = colDef(cell = function(value) {if_else(value > 999999,
                                                                           scales::dollar(value, scale = .000001, accuracy = .1, suffix = "M"),
                                                                           if_else(value > 999, 
                                                                                   scales::dollar(value, scale = .001, accuracy = .1, suffix = "K"),
                                                                                   scales::dollar(value)) 
                                           )},
                                           style = function(value) {
                                             if (value >= 1e6) {
                                               color <- crimson
                                               weight = "bold"
                                             } else if (value >= 1e3){
                                               color <- crimson
                                               weight = "normal"
                                             } else {
                                               color <- "black"
                                               weight = "normal"
                                             }
                                             list(color = color, fontWeight = weight)
                                           }
                             ),
                             System = colDef( minWidth = 30, style = function(value){
                               color <- case_when(
                                 value == "GT" ~ darkBlue,
                                 value == "GD" ~ liteGreen,
                                 value == "GG" ~ lilac,
                                 value == "HL" ~ honey,
                                 value == "UNGS" ~ liteBlue,
                                 .default = "gray35")
                               
                                 list(color = color)
                             }),
                             Date = colDef(minWidth = 40, cell = function(value) {stamp("Jan 31", "md")(value)}),
                             Fatalities = colDef(minWidth = 45,
                                                 style = function(value) {
                                                   if (value > 0) {
                                                     color <- crimson
                                                     weight = "bold"
                                                   } else {
                                                     color <- "black"
                                                     weight = "normal"
                                                   }
                                                   list(color = color, fontWeight = weight)
                                                 }),
                             Injuries = colDef( minWidth  = 45,
                                                style = function(value) {
                                                  if (value > 0) {
                                                    color <- crimson
                                                    weight = "bold"
                                                  } else {
                                                    color <- "black"
                                                    weight = "normal"
                                                  }
                                                  list(color = color, fontWeight = weight)
                                                })
                           ))
          htmltools::div(style = list(margin = "9px 30px"), tbl)
        },
        onClick = "expand",
        rowStyle = list(cursor = "pointer"),
        highlight = TRUE,
        compact = T,
        searchable = T,
        filterable = T,
        showPageSizeOptions = T,
        theme = reactableTheme(
          style = list(fontFamily = "Source Sans Pro, Helvetica, Arial, sans-serif")
        )
      )
    }
    else if(input$table_type == "State"){
      #data
      a_df <- df %>%
        st_drop_geometry()%>%
        group_by(STATE) %>%
        summarize(
          Incidents = n(),
          gt_inc = sum(SYS == "GT"),
          gd_inc = sum(SYS == "GD"),
          gg_inc = sum(SYS == "GG"),
          ungs_inc = sum(SYS == "UNGS"),
          hl_inc = sum(SYS == "HL"),
          Sys = list(unique(SYS)),
          tot_cost = sum(TOTAL_COST_CURRENT, na.rm = T),
          tot_fat = sum(FATAL, na.rm = T),
          tot_inj = sum(INJURE, na.rm = T)
        ) %>%  #filter(top10 > 0) %>% summarize(n = n(), inc = sum(inc), cost = sum(cost)) %>%tibble::view()
        left_join(
          all_miles %>%
            filter(IYEAR == 2022)%>%
            distinct(OPERATOR_ID,NAME, pri.id, STATE,SYS,.keep_all = T)%>%
            group_by(STATE)%>%
            summarise(miles = sum(mileage, na.rm = T),
                      Sys = list(unique(SYS)),
                      Operators = length(unique(OPERATOR_ID)), # number of unique ops 
            ),
          by = "STATE",
          suffix = c("", ".m")
        )%>%
        tidyr::replace_na(list(Incidents = 0, tot_cost = 0, tot_inj = 0, tot_fat = 0)) %>%
        mutate(ipm = Incidents /( miles/1000)) %>%
        distinct(STATE, .keep_all = T) %>%
        arrange(-Incidents) %>%
        filter(STATE != "OCS")
      #table
      reactable(
        a_df  %>% select(!ends_with("_inc") & !c("Sys")) %>% select(STATE, Incidents, Sys.m, tot_cost, tot_fat, tot_inj, Operators, miles, ipm) ,
        defaultSorted = list(Incidents = "desc"),
        columns = list(
          Operators = colDef(name = "Unique Operators", minWidth = 40),
          Sys.m = colDef(name = "System", minWidth = 40,
                         html = T,
                         cell = function(value, index){
                           l = length(value)
                           weight = "bold"
                           vals <- HTML("")
                           for(i in 1:l){
                             ival = value[i]
                             color <- case_when(
                               ival == "GT" ~ darkBlue,
                               ival == "GD" ~ liteGreen,
                               ival == "GG" ~ lilac,
                               ival == "HL" ~ honey,
                               ival == "UNGS" ~ liteBlue,
                               .default = "gray35"
                             )
                             
                             val <- HTML("<span style = \"font-weight:bold; color:", color, ";\">",
                                         a_df[[index, paste0(str_to_lower(ival), "_inc")]], " ",
                                         ival, "</span>")
                             vals <- HTML(vals,ifelse(i >1, ";", ""), val)
                             
                           }
                           vals
                           
                         }
                         
                         # inline style color and system 
                         #this shouolndt be a style it should be a cell???
                         # style split at commas? Idk how exactly
          ),
          STATE = colDef(name = "State", minWidth = 50,
                         cell = function(value){state.name[which(state.abb == value)] }),
          tot_cost = colDef(name = "Cost", minWidth = 30,
                            cell = function(value) if_else(value > 999999,
                                                           scales::dollar(value, scale = .000001, accuracy = .1, suffix = "M"),
                                                           if_else(value > 999, 
                                                                   scales::dollar(value, scale = .001, accuracy = .1, suffix = "K"),
                                                                   scales::dollar(value)) 
                            ),
                            style = function(value) {
                              if (value >= 1e6) {
                                color <- crimson
                                weight = "bold"
                              } else if (value >= 1e3){
                                color <- crimson
                                weight = "normal"
                              } else {
                                color <- "black"
                                weight = "normal"
                              }
                              list(color = color, fontWeight = weight)
                            }),
          tot_fat = colDef(name = "Fatalities", minWidth = 30,
                           style = function(value) {
                             if (value >= 2) {
                               color <- crimson
                               weight = "bold"
                             } else if (value >= 1){
                               color <- crimson
                               weight = "normal"
                             } else {
                               color <- "black"
                               weight = "normal"
                             }
                             list(color = color, fontWeight = weight)
                           }),
          tot_inj = colDef(name = "Injuries", minWidth = 30,
                           style = function(value) {
                             if (value >= 2) {
                               color <- crimson
                               weight = "bold"
                             } else if (value >= 1){
                               color <- crimson
                               weight = "normal"
                             } else {
                               color <- "black"
                               weight = "normal"
                             }
                             list(color = color, fontWeight = weight)
                           }),
          miles =colDef(name = "2022 Miles",minWidth  = 30, format = colFormat(separators = T, digits = 0)),
          ipm = colDef(name = "Incident Rate", format= colFormat(digits = 3) )
        ),
        details = function(index) {
          justinc <- filter(df_22, State == a_df$STATE[index]) %>% select(Date, Operator, System, Location,Cause, Release, Cost, Fatalities, Injuries) # update with if elses to address ot
          tbl <- reactable(justinc, outlined = TRUE, highlight = TRUE, fullWidth = T,
                           columns = list(
                             Cost = colDef(minWidth = 40, cell = function(value) {if_else(value > 999999,
                                                                           scales::dollar(value, scale = .000001, accuracy = .1, suffix = "M"),
                                                                           if_else(value > 999, 
                                                                                   scales::dollar(value, scale = .001, accuracy = .1, suffix = "K"),
                                                                                   scales::dollar(value)) 
                             )},
                             style = function(value) {
                               if (value >= 1e6) {
                                 color <- crimson
                                 weight = "bold"
                               } else if (value >= 1e3){
                                 color <- crimson
                                 weight = "normal"
                               } else {
                                 color <- "black"
                                 weight = "normal"
                               }
                               list(color = color, fontWeight = weight)
                             }
                             ),
                             Operator = colDef(minWidth = 100),
                             System = colDef(minWidth = 30, style = function(value){
                               color <- case_when(
                                 value == "GT" ~ darkBlue,
                                 value == "GD" ~ liteGreen,
                                 value == "GG" ~ lilac,
                                 value == "HL" ~ honey,
                                 value == "UNGS" ~ liteBlue,
                                 .default = "gray35")
                               
                               list(color = color)
                             }),
                             Date = colDef(minWidth = 30, cell = function(value) {stamp("Jan 31", "md")(value)}),
                             Fatalities = colDef(minWidth = 45,
                                                 style = function(value) {
                                                   if (value > 0) {
                                                     color <- crimson
                                                     weight = "bold"
                                                   } else {
                                                     color <- "black"
                                                     weight = "normal"
                                                   }
                                                   list(color = color, fontWeight = weight)
                                                 }),
                             Injuries = colDef( minWidth  = 45,
                                                style = function(value) {
                                                  if (value > 0) {
                                                    color <- crimson
                                                    weight = "bold"
                                                  } else {
                                                    color <- "black"
                                                    weight = "normal"
                                                  }
                                                  list(color = color, fontWeight = weight)
                                                })
                           ))
          htmltools::div(style = list(margin = "9px 30px"), tbl)
        },
        onClick = "expand",
        rowStyle = list(cursor = "pointer"),
        highlight = TRUE,
        compact = T,
        searchable = T,
        filterable = T,
        showPageSizeOptions = T,
        theme = reactableTheme(
          style = list(fontFamily = "Source Sans Pro, Helvetica, Arial, sans-serif")
        )
      )
    }
    else{
      #data 
      reactable(
        df_22 %>% 
          arrange(Date)%>%
          #mutate(Date = stamp("Jan 31", orders = "md")(Date))%>%
          select(-State),
        columns = list(
          Operator = colDef(name = "Operator", minWidth = 75),
          Primary = colDef(name = "Primary", minWidth = 60),
          Date = colDef(minWidth = 50, cell = function(value) {stamp("Jan 31", "md")(value)}),
          Location = colDef(minWidth = 50),
          System = colDef(minWidth = 20, 
                          style = function(value){
                            weight = "bold"
                            if(value == "GT"){
                              color = darkBlue
                            } else if(value == "GD"){
                              color = liteGreen
                            } else if(value == "GG"){
                              color = lilac
                            } else if(value == "HL"){
                              color = honey
                            } else if(value == "UNGS"){
                              color = liteBlue
                            } else{
                              color = "gray35"
                              weight = "normal"
                            }
                            list(color = color, fontWeight = weight)
                          }),
          Release = colDef(minWidth = 33),
          Cost = colDef(minWidth = 33, 
                        cell = function(value) {if_else(value > 999999,
                                                      scales::dollar(value, scale = .000001, accuracy = .1, suffix = "M"),
                                                      if_else(value > 999, 
                                                              scales::dollar(value, scale = .001, accuracy = .1, suffix = "K"),
                                                              scales::dollar(value)) 
                                                      )},
                        style = function(value) {
                          if (value >= 1e6) {
                            color <- crimson
                            weight = "bold"
                          } else if (value >= 1e3){
                            color <- crimson
                            weight = "normal"
                          } else {
                            color <- "black"
                            weight = "normal"
                          }
                          list(color = color, fontWeight = weight)
                        }
                        ),
          Cause = colDef(minWidth = 33),
          Fatalities = colDef(minWidth = 33,
                              style = function(value) {
                                if (value > 0) {
                                  color <- crimson
                                  weight = "bold"
                                } else {
                                  color <- "black"
                                  weight = "normal"
                                }
                                list(color = color, fontWeight = weight)
                              }),
          Injuries = colDef( minWidth  = 33,
                             style = function(value) {
                               if (value > 0) {
                                 color <- crimson
                                 weight = "bold"
                               } else {
                                 color <- "black"
                                 weight = "normal"
                               }
                               list(color = color, fontWeight = weight)
                             })
        ),
        highlight = TRUE,
        compact = T,
        searchable = T,
        filterable = T,
        showPageSizeOptions = T,
        theme = reactableTheme(
          style = list(fontFamily = "Source Sans Pro, Helvetica, Arial, sans-serif")
        )
      )
    }
  })
  
  
  #plot incident rate by year?
  output$irPlot <- renderPlot({
    
  })
  
  #### scrolly stuff ####
  output$mapS <- renderScrollytell({scrollytell()}) 
  output$graphs <- renderScrollytell({scrollytell()}) 
  observe({cat("section:", input$scr, "\n")})

    
  }

# Run the application 
shinyApp(ui = ui, server = server)
