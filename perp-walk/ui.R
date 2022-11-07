#worst incidents of month dashboard server side
# this is a shiny app that isnt very shiny at all

# TODO: Fix waiter auto load
# TODO: add overlay for educational purposes - intro.js
# TODO: some locations in allinc not quite figuring their stuff out
# TODO: reformat and style code to be consistent with standards 

# load data just for date 
recentInc <- incs %>%
  filter(MDY == max(MDY))

# two months ago day one 
newDate <- if_else(day(recentInc$MDY[1]) > 25, 
                   recentInc$MDY[1] - day(recentInc$MDY[1]) + days(1),
                   recentInc$MDY[1] - months(1) - day(recentInc$MDY[1]) + days(1))

## use dashboard pieces for ui 
#### header  ####
header <- dashboardHeader(
  title = tags$a(href = "http://pstrust.org",
                 tags$img(src = "PST_logo_white.png", 
                          height='85%',
                          alt = "Pipeline Safety Trust")
  ),
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
              br(),
              actionButton("help", "Tutorial Walkthrough"),
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
              tags$a(icon("facebook"), href = "https://www.facebook.com/sharer/sharer.php?u=https%3A//jamespst.shinyapps.io/worst-month/", class = "share-icon"),
              br(),
              tags$a(icon("twitter"), href = "https://twitter.com/intent/tweet?text=Check%20this%20out%3A%20https%3A//jamespst.shinyapps.io/worst-month/", class = "share-icon"),
              br(),
              tags$a(icon("linkedin"), href = "https://www.linkedin.com/shareArticle?mini=true&url=https%3A//jamespst.shinyapps.io/worst-month/&title=&summary=&source=", class = "share-icon"),
              br(),
              tags$a(icon("envelope"), href = "mailto:?body=https%3A//jamespst.shinyapps.io/worst-month/", class = "share-icon"),
              placement = "bottom",
              arrow = T,
              theme = "material")
    )
)

#### sidebar ####
sidebar <- dashboardSidebar(
  sidebarMenu(id = "tabs",
              menuItem("The Latest", tabName="now", icon = icon("newspaper"),selected = TRUE),
              menuItem("Repeat Offenders", tabName = "repeat", icon = icon("redo")),
              menuItem("Monthly Map", tabName = "leafs", icon = icon("map")),
              menuItem("Incident Plots",  tabName = "timeline", icon = icon("chart-column")),
              menuItem("Full Table", tabName = "hist", icon = icon("table"))            
  ),
  
  column(10,
         hr(),
         div(
           id = "controls",
           dateInput2("thisMonth", 
                      "Month/Year", 
                      startview = "year", 
                      minview = "months", 
                      maxview = "decades",
                      value = newDate,
                      max = newDate,
                      min = min(incs$MDY),
                      format = "mm/yyyy"
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
  
)

#### body ####
body <- dashboardBody(
  tags$head(tags$title("Worst Incidents of the Month"),
            introjsUI(),
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
            tags$link(rel = "icon", href = "www/favicon.ico"),
            tags$head(includeHTML(("google-analytics.html"))),
            tags$script('
                        var width = 0;
                        $(document).on("shiny:connected", function(e) {
                          width = window.innerWidth;
                          Shiny.setInputValue("width", width);
                        });
                        $(window).resize(function(e) {
                          width = window.innerWidth;
                          Shiny.setInputValue("width", width);
                        });
                        ')),
  useShinyalert(),
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
    #### maptab ####
    tabItem(
      tabName = "leafs",
      fluidPage(
        box(
          height = "87vh",
          width = 12,
          leafletOutput("incMap", height = "84vh")
        )
      )
    ),
    #### value boxes? landing page?  ####
    tabItem(
      tabName = "now",
      useWaiter(), 
      waiterPreloader(html = spin_ripple()),
      fluidRow(
        conditionalPanel(
          condition = "input.system == 'all' || input.system == 'GD'",
          box(
            title = "Gas Distribution",
            id = "GDBox",
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
            id = "GTBox",
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
            id = "HLBox",
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
    #### timeline plot ####
    tabItem(
      tabName = "timeline",
      fluidRow(
        box(width = 12,
            id = "tlBox",
            br(),
            column(
                id = "buttonCol",
                2,
                h4("Options"),
                h6("Period"),
                switchInput(
                  label = "<i class=\"fa-solid fa-calendar-days\"></i>",
                  inputId = "periodSwitch",
                  value = F,
                  onLabel = "Year",
                  offLabel = "Month",
                  onStatus = "info",
                  offStatus = "primary"
                  ),
                h6("Size"),
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
               #bring up switch for going between HL and NG release plots
               conditionalPanel(
                 id = "conditionButton",
                 condition = "input.system == 'all' && input.weight == 'TOTAL_RELEASE'",
                 h6("System"),
                 switchInput(
                   label = "<i class=\"fa-solid fa-industry\"></i>",
                   inputId = "relSys",
                   value = F,
                   onLabel = "HL",
                   offLabel = "Gas",
                   onStatus = "warning",
                   offStatus = "primary")
                 )
            ), # close col
        column(
          width = 10,
            div(
              style = "position:relative",
              conditionalPanel(
                condition = "input.system != 'all' || input.weight != 'TOTAL_RELEASE' || input.relSys == false ",
                plotOutput("timePlot",width = "100%", 
                           hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")),
                uiOutput("hover_info")
              ), #cond pan
              
              # HL Release Plot
              conditionalPanel(
                condition = "input.system == 'all' && input.weight == 'TOTAL_RELEASE' && input.relSys == true",
                plotOutput("hlTimePlot", width = "100%", 
                           hover = hoverOpts("hl_hover", delay = 100, delayType = "debounce")),
                uiOutput("hl_info")
              ) #cond pan
            ) # div
        ) # fl row 2
            
       ) #box 
      ) # fl row 1
    ) #tab item
  ) # tab itemS
)




#### ui ####
shinyUI( 
  dashboardPage(
    title = "Pipeline Safety Trust",
    header,
    sidebar,
    body
  )
)