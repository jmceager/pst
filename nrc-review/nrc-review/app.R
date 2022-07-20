library(shiny)
library(tidyverse)
library(leaflet)
library(leaflet.providers)
library(reactable)

source("nrc_geocode.R")

# get cols
cols <- c("SEQNOS", "LOC_FULL", "RESPONSIBLE_COMPANY","INC_DATE", "INCIDENT_DATE_TIME",
          "INCIDENT_CAUSE","NAME_OF_MATERIAL", "AMOUNT_OF_MATERIAL","UNIT_OF_MEASURE",
          "MEDIUM_DESC","ADDITIONAL_MEDIUM_INFO","ANY_DAMAGES","DAMAGE_AMOUNT",
          "FIRE_INVOLVED","ANY_INJURIES","NUMBER_INJURED","NUMBER_HOSPITALIZED",
          "ANY_FATALITIES","NUMBER_FATALITIES","NUMBER_EVACUATED",
          "lat","lon", "SYS", "size")
# real time data 
#df <- nrcGeo("https://nrc.uscg.mil/FOIAFiles/Current.xlsx") %>%
#  select(all_of(cols))
#write to csv for testing 
#write.csv(df, file = "testing.csv")
# reading testing data
df <- read_csv("testing.csv") 
df <- df[,2:length(df)] %>%
  mutate(INCIDENT_DATE_TIME = format(INCIDENT_DATE_TIME, format = "%H:%M:%S"))

ds <- stamp("8 March, 2022")


# Define UI 
ui <- fluidPage(

    # Application title
    titlePanel("NRC Pipeline Calls"),
    
    #header
    tags$style("@import url(https://use.fontawesome.com/releases/v6.0.0/css/all.css);"),

    # fluid row layout
    fluidRow(
        column(4, 
               h5(paste0("Last Updated:\n",ds(max(df$INC_DATE)))),
               tags$a(href = "https://nrc.uscg.mil/", "NRC FOIA Spreadsheet"),
               hr(),
               helpText("Filter pipeline-specific calls to the NRC by specific 
                         dates or weeks. Selecting points in the map will select 
                         rows in the table below. To clear your map selection, click
                         on your selected circle again. You can also sort and filter 
                         the table using the headers and search boxes in each column."),
               radioButtons("time", 
                            h3("Time Period"),
                            choices = c("Dates" = 1,
                                        "Weeks" = 2),
                            selected = 1),
               hr(),
               conditionalPanel(condition = "input.time == 1",
                                dateRangeInput("date",
                                               h5("Date Range (dd-mm-yyyy)"),
                                               start = max(df$INC_DATE) - weeks(2),
                                               end  = max(df$INC_DATE),
                                               max = max(df$INC_DATE),
                                               min = min(df$INC_DATE),
                                               format = "dd-mm-yyyy")),
               conditionalPanel(condition = "input.time == 2",
                                numericInput(inputId = "week",
                                             label = h5("# of Weeks"),
                                             value = 2)),
               hr()
               ), #close column

        # main info
        column(8,
               leafletOutput("map")
               ) #close col
        ), # close row 
    #fluidrow for table
    fluidRow(
      column(12,
             reactableOutput("table"))
    ) # close row
) # close page 

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  clickInc <- reactiveVal()
  
  rdf <- reactive({
    if(is.null(clickInc())){
      if(input$time == 1){
        rdf <- df %>% 
          dplyr::filter(between(INC_DATE, input$date[1], input$date[2])) %>%
          mutate(selected = "N")
      }
      else{
        rdf <- df %>% 
          dplyr::filter(between(INC_DATE, max(INC_DATE) - weeks(input$week), max(INC_DATE))) %>%
          mutate(selected = "N")
      }
    }
    else{
      if(input$time == 1){
        rdf <- df %>% 
          dplyr::filter(between(INC_DATE, input$date[1], input$date[2])) %>%
          mutate(selected = if_else(SEQNOS == clickInc(), "Y", "N"))
      }
      else{
        rdf <- df %>% 
          dplyr::filter(between(INC_DATE, max(INC_DATE) - weeks(input$week), max(INC_DATE))) %>%
          mutate(selected = if_else(SEQNOS == clickInc(), "Y", "N"))
      }
    }
    rdf %>%
      mutate(size = replace_na(size, 8))
    
  })
  
  #### leaflet ####
  output$map <- renderLeaflet({
    leaflet()%>%
      addProviderTiles(providers$OpenStreetMap.HOT)%>%
      fitBounds(-124.39, 25.82, -66.54, 49.38)%>%
      addEasyButton(easyButton(
        icon="fa-solid fa-globe", title="Zoom to Extent",
        onClick=JS("function(btn, map){ map.fitBounds([[25.82, -124.39],[49.38, -66.94]]);}")))
  }) # close leaflet
  
  outputOptions(output, "map", suspendWhenHidden = FALSE)
  
  # leaf proxy / observe filters & clicks
  observe({
    #create palette
    selPal <- colorFactor(palette = c("#003E59", "#61A893"), rdf()$selected)
    #update map 
    leafletProxy("map", data = rdf()) %>%
      removeControl("legend")%>%
      addCircleMarkers(lat = ~lat, lng = ~lon, layerId = ~SEQNOS,
                       weight = 2, fillOpacity = .6, 
                       color = "#003E59",
                       fillColor = ~selPal(selected),
                       radius = ~size
                       )%>%
      addLegendCustom(legName = "legend", title = "Release Quartile", position = "bottomright")
  })
  
  #check for clicked point on map
  observeEvent(input$map_marker_click, {
    # Capture the info of the clicked marker
    if(!is.null(clickInc()) && clickInc() == input$map_marker_click$id){
      clickInc(NULL)     # Reset filter
    }
    else{
      clickInc(input$map_marker_click$id)
    }
  })
  
#### reactable ####
  output$table <- renderReactable({
    if(is.null(clickInc())){
      tdf <- rdf() %>%
        mutate(details = NA)
    }
    else{
      tdf <- rdf() %>% 
        dplyr::filter(SEQNOS == clickInc()) %>%
        mutate(details = NA)
    }
    reactable(tdf,
              #options
              filterable = TRUE,
              striped = TRUE,
              highlight = TRUE,
              sortable = TRUE,
              showPageSizeOptions = TRUE,
              showSortable = TRUE,
              #theme stuff
              defaultColDef = colDef(
                align = "center",
                minWidth = 100
              ),
              defaultSorted = list(INC_DATE = "desc"),
              minRows = 10,
              defaultPageSize = 20,
              pageSizeOptions = c(10,20,30),
              #column definitions
              columns = list(
                INC_DATE = colDef(format = colFormat(date = TRUE),
                             name = "Date"),
                LOC_FULL = colDef(name = "Place", width = 110),
                RESPONSIBLE_COMPANY = colDef(name = "Operator"),
                SEQNOS = colDef(show = F),
                INCIDENT_DATE_TIME = colDef(name = "Time", show = F),
                NAME_OF_MATERIAL = colDef(name = "Material"),
                MEDIUM_DESC = colDef(name = "Release Medium"),
                ADDITIONAL_MEDIUM_INFO = colDef(name = "Medium Info", show = F),
                ANY_DAMAGES = colDef(name = "Damage?", show = F),
                DAMAGE_AMOUNT = colDef(name = "Damage Cost", format = colFormat(currency = "USD"), show = F),
                AMOUNT_OF_MATERIAL = colDef(name = "Amount Released",
                                       format = colFormat(separators = TRUE),
                                       minWidth = 140),
                INCIDENT_CAUSE = colDef(name = "Cause"),
                UNIT_OF_MEASURE = colDef(name = "Release Unit", minWidth = 110),
                FIRE_INVOLVED = colDef(name = "Fire?"),
                ANY_INJURIES = colDef(name = "Injuries?"),
                NUMBER_INJURED = colDef(name = "Injured", show = F),
                NUMBER_HOSPITALIZED = colDef(name = "Hospitalized", show = F),
                ANY_FATALITIES = colDef(name = "Fatalities?"),
                NUMBER_FATALITIES = colDef(name = "Fatalities", show = F),
                NUMBER_EVACUATED = colDef(name = "Evacuations", show = F),
                lat = colDef(show = F),
                lon = colDef(show = F),
                SYS = colDef(show = F),
                size = colDef(show = F),
                selected = colDef(show = F),
                details = colDef(name = "Details", sortable = FALSE, 
                                 cell = function() htmltools::tags$button("Show details"))
                ),
              onClick = JS("function(rowInfo, column) {
                            // Only handle click events on the 'details' column
                            if (column.id !== 'details') {
                              return
                            }
                        
                            // Display an alert dialog with details for the row
                            window.alert('Details for row ' + rowInfo.index + ':\\n' + JSON.stringify(rowInfo.values, null, 2))
                        
                            // Send the click event to Shiny, which will be available in input$show_details
                            // Note that the row index starts at 0 in JavaScript, so we add 1
                            if (window.Shiny) {
                              Shiny.setInputValue('show_details', { index: rowInfo.index + 1 }, { priority: 'event' })
                            }
                          }")
              )
  })
  
}# close server

# Run the application 
shinyApp(ui = ui, server = server)
