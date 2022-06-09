#this is a shiny app 
#it is not actually shiny, thats just the name of the app
library(shiny)
library(shinyWidgets)
library(reactable)
library(tidyverse)
library(shinydashboard)
library(lubridate)
library(scales)
library(leaflet)
library(leaflet.providers)
library(htmltools)
library(htmlwidgets)
library(plotly)
library(waiter)

#moved helper functions to separate script
source("helpfun.R")

fargs <- formals(icon)
fargs$verify_fa <- FALSE
formals(icon) <- fargs

# two months ago day one 
newDate <- Sys.Date() - months(3) - (day(Sys.Date())-1)

## use dashboard pieces for ui 
#### header  ####
header <- dashboardHeader(
  title = tags$a(href = "http://pstrust.org",
                 tags$img(src = "PST_logo_white.png", height='85%')
  ),
  #dropdownMenu("sourceMenu"),
  #dropdownMenu("shareMenu"),
  #dropdownMenuOutput("helpMenu")
  tags$li(class = "dropdown",
          id="info-down",
          dropMenu(
            dropdownButton("Info", 
                           status = 'success', 
                           icon = icon('info'), 
                           circle = T),
            h5(strong('Information')),
            hr(),
            textOutput("infoText"),
            placement = "bottom",
            arrow = TRUE,
            theme = "material")
  ),
  tags$li(class = "dropdown",
          id = "info-down",
          dropMenu(
            dropdownButton("Sources", 
                           status = 'success', 
                           icon = icon('database'), 
                           circle = T),
            h5(strong('Sources')),
            hr(),
            tags$p("All data is sourced from the U.S. Department of Transportation's Pipeline 
                   and Hazardous Materials Safety Administration data. This includes their 
                   incident flagged files and their pipeline mileage data. Both can be accessed 
                   through their website at the link below."),
            br(),
            tags$a("PHMSA Data Overview", href = "https://www.phmsa.dot.gov/data-and-statistics/pipeline/data-and-statistics-overview", target="_blank", class = "source-link"),
            placement = "bottom",
            arrow = T,
            theme = "material")
  ),
  tags$li(class = "dropdown",
          dropMenu(
            dropdownButton("Share", 
                           status = 'success', 
                           icon = icon('share-alt'), 
                           circle = T),
            h5(strong('Sharing')),
            hr(),
            tags$a(icon("facebook"), href = "https://www.facebook.com/sharer/sharer.php?u=https%3A//jamespst.shinyapps.io/testing/", class = "share-icon"),
            br(),
            tags$a(icon("twitter"), href = "https://twitter.com/intent/tweet?text=Check%20this%20out%3A%20https%3A//jamespst.shinyapps.io/testing/", class = "share-icon"),
            br(),
            tags$a(icon("linkedin"), href = "https://www.linkedin.com/shareArticle?mini=true&url=https%3A//jamespst.shinyapps.io/testing/&title=&summary=&source=", class = "share-icon"),
            br(),
            tags$a(icon("envelope"), href = "mailto:?body=https%3A//jamespst.shinyapps.io/testing/", class = "share-icon"),
            placement = "bottom",
            arrow = T,
            theme = "material")
  )
)

#### sidebar ####
sidebar <- dashboardSidebar(
  sidebarMenu(id = "tabs",
              menuItem("This Month", tabName="now", icon = icon("newspaper"),selected = TRUE),
              menuItem("Incident Map", tabName = "leafs", icon = icon("map")),
              menuItem("Repeat Offenders", tabName = "repeat", icon = icon("industry")),
              menuItem("Timeline",  tabName = "timeline", icon = icon("timeline")),
              menuItem("Perp Table", tabName = "hist", icon = icon("table"))
              
  ),
  
  column(10,
         hr(),
         dateInput2("thisMonth", 
                    "Month/Year", 
                    startview = "year", 
                    minview = "months", 
                    maxview = "decades",
                    value = newDate,
                    format = "mm/yyyy",
                    min = min(incs$MoYr)
         ),
         radioButtons("system", "Pipeline System:",
                      c("All" = "all",
                        "Gas Transmission" = "GT",
                        "Gas Distribution" = "GD",
                        "Hazardous Liquid" = "HL"),
                      selected = "all"),
         radioButtons("weight", "Determinant:",
                      c("Release Size" = "TOTAL_RELEASE",
                        "Cost of Damage" = "TOTAL_COST_CURRENT",
                        "Deaths" = "FATAL",
                        "Deaths + Injuries" = "humans"),
                      selected = "TOTAL_RELEASE")
  )
  
)

#### body ####
body <- dashboardBody(
  tags$head(tags$title("Perp Walk"),
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
            tags$script('
                        var width = 0;
                        $(document).on("shiny:connected", function(e) {
                          width = window.innerWidth;
                          Shiny.onInputChange("width", width);
                        });
                        $(window).resize(function(e) {
                          width = window.innerWidth;
                          Shiny.onInputChange("width", width);
                        });
                        ')),
  tabItems(
    #### history table ####
    tabItem(
      tabName = "hist",
      fluidPage(
        # Show a plot of the generated distribution
        box(
          reactableOutput("histable", height = "85vh"),
          width = 12
        )
      )
    ),
    #idk yet
    tabItem(
      tabName = "repeat",
      fluidPage(
        box(
          reactableOutput("repeatPerps", height = "85vh"),
          width = 12
        )
      )
    ),
    #map
    tabItem(
      tabName = "leafs",
      fluidPage(
        leafletOutput("incMap", height = "89vh")
      )
    ),
    #### value boxes? landing page?  ####
    tabItem(
      tabName = "now",
      useWaiter(), 
      waiterShowOnLoad(html = spin_ripple()),
      fluidRow(
        conditionalPanel(
          condition = "input.system == 'all' || input.system == 'GD'",
          box(
            title = "Gas Distribution",
            width = 12,
            column(6,
                   valueBoxOutput("gdNew", width = 12),          
                   valueBoxOutput("gdFire", width = 3),
                   valueBoxOutput("gdExplode", width = 3),
                   valueBoxOutput("gdInjure", width = 3),
                   valueBoxOutput("gdFatal", width = 3)),
            column(6,
                   valueBoxOutput("gdCost", width = 12),
                   valueBoxOutput("gdSpill", width = 12)
            )
          )
        )
      ),
      fluidRow(
        conditionalPanel(
          condition = "input.system == 'all' || input.system == 'GT'",
          box(
            title = "Gas Transmission",
            width = 12,
            column(6,
                   valueBoxOutput("gtNew", width = 12),          
                   valueBoxOutput("gtFire", width = 3),
                   valueBoxOutput("gtExplode", width = 3),
                   valueBoxOutput("gtInjure", width = 3),
                   valueBoxOutput("gtFatal", width = 3)),
            column(6,
                   valueBoxOutput("gtCost", width = 12),
                   valueBoxOutput("gtSpill", width = 12)
            )
          )
        )
      ),
      fluidRow(
        conditionalPanel(
          condition = "input.system == 'all' || input.system == 'HL'",
          box(
            title = "Hazardous Liquids",
            width = 12,
            column(6,
                   valueBoxOutput("hlNew", width = 12),          
                   valueBoxOutput("hlFire", width = 3),
                   valueBoxOutput("hlExplode", width = 3),
                   valueBoxOutput("hlInjure", width = 3),
                   valueBoxOutput("hlFatal", width = 3)),
            column(6,
                   valueBoxOutput("hlCost", width = 12),
                   valueBoxOutput("hlSpill", width = 12)
            )
          )
        )
      )
    ),
    #### timeline plot? hm ####
    tabItem(
      tabName = "timeline",
      fluidRow(
        box(width = 12,
            useWaitress(color = "#61A893"),
            br(),
            dropdown(
              tags$h4("Controls"),
              h6("Time Period"),
              switchInput(
                label = "<i class=\"fa-solid fa-calendar-days\"></i>",
                inputId = "periodSwitch",
                value = F,
                onLabel = "Year",
                offLabel = "Month",
                onStatus = "info",
                offStatus = "primary"
              ),
              h6("Point Size"),
              radioGroupButtons(
                 inputId = "sizeButton",
                 choiceNames = c("None",
                                 "Cost",
                                 "Deaths",
                                 "Injured",
                                 "Operator Mileage",
                                 "Evacuations"),
                 choiceValues = c("",
                                  "TOTAL_COST_CURRENT",
                                  "FATAL",
                                  "INJURE",
                                  "mileage",
                                  "NUM_PUB_EVACUATED"),
                 status = "primary",
                 direction = "vertical"
               ),
              h6("Log Y-Axis"),
              materialSwitch(
                 inputId = "logY",
                 value = FALSE,
                 status = "primary"
               ),
              style = "jelly", icon = icon("gear"),
              status = "primary", width = "300px",
              animate = animateOptions(
                enter = animations$fading_entrances$fadeInLeftBig,
                exit = animations$fading_exits$fadeOutRightBig
              )
            ),
            br(),
            uiOutput("timePlot"),
            br(),
            conditionalPanel(
              condition = "input.system == 'all' && input.weight == 'TOTAL_RELEASE'",
              uiOutput("hlTimePlot")
            ) #cond pan
       ) #box 
      ) # row
    ) #tab item
  ) # tab itemS
  
)




#### ui ####
shinyUI( 
  dashboardPage(
    header,
    sidebar,
    body
  )
)