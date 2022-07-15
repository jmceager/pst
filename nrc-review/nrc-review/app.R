#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(tidyverse)
library(leaflet)
library(leaflet.providers)
library(reactable)

source("nrc_geocode.R")

df <- nrcGeo("https://nrc.uscg.mil/FOIAFiles/Current.xlsx")
ds <- stamp("8 March, 2022")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NRC Data Review"),

    # Sidebar with a slider input for number of bins 
    fluidRow(
        column(4, 
               radioButtons("time", 
                            h3("Time Filtering"),
                            choices = c("Dates" = 1,
                                        "Weeks" = 2),
                            selected = 1),
               hr(),
               conditionalPanel(condition = "input.time == 1",
                                dateRangeInput("date",
                                               h5("Date Range"),
                                               start = max(df$INC_DATE) - weeks(2),
                                               end  = max(df$INC_DATE),
                                               max = max(df$INC_DATE),
                                               min = min(df$INC_DATE),
                                               format = "dd-mm-yyyy")),
               conditionalPanel(condition = "input.time == 2",
                                numericInput(inputId = "week",
                                             label = h5("# of Weeks"),
                                             value = 2))
                 #close wellpanel
               ), #close column

        # main info
        column(8,
               h4(paste0("FOIA Spreadsheet Last Updated: ",ds(max(df$INC_DATE)))),
               hr(),
               leafletOutput("map"),
               hr(),
               reactableOutput("table")
               ) #close col
        ) # close row 
) # close page 

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  rdf <- reactive({
    if(input$time == 1){
      df %>% 
        dplyr::filter(between(INC_DATE, input$date[1], input$date[2]))
    }
    else{
      df %>% 
        dplyr::filter(between(INC_DATE, max(INC_DATE) - weeks(input$week), max(INC_DATE)))
    }
  })

  #leaf base map
  output$map <- renderLeaflet({
    leaflet()%>%
      addProviderTiles(providers$OpenStreetMap.HOT)%>%
      fitBounds(-124.39, 25.82, -66.94, 49.38)%>%
      addEasyButton(easyButton(
        icon="globe", title="Zoom to Extent",
        onClick=JS("function(btn, map){ map.fitBounds([[25.82, -124.39],[49.38, -66.94]]);}")))
  }) # close leaflet
  
  outputOptions(output, "map", suspendWhenHidden = FALSE)
  # leaf proxy / observe filters 
  observe({
    leafletProxy("map", data = rdf()) %>%
     # removeControl("legend")%>%
      addCircleMarkers( #weight = 1, fillOpacity = 0.6,
                       lat = ~lat, lng = ~lon#,
                       # popup = paste0("<b>Operator:</b>",
                       #                df$RESPONSIBLE_COMPANY,
                       #                "<br>",
                       #                "<b>Date:</b>",
                       #                ds(df$INC_DATE),
                       #                "<br>",
                       #                "<b>Place:</b> ", 
                       #                df$LOC_FULL, 
                       #                "<br>",
                       #                "<b>Release:</b> ",
                       #                comma(round(df$AMOUNT_OF_MATERIAL, digits = 0)), " ",
                       #                mapData()$UNIT_OF_MEASURE,
                       #                "<br>",
                       #                "<b>Material:</b> $",
                       #                df$NAME_OF_MATERIAL,
                       #                "<br>",
                       #                "<b>Medium:</b> ",
                       #                df$MEDIUM_DESC,
                       #                "<br>",
                       #                "<b>Cause:</b> ",
                       #                df$INCIDENT_CAUSE
                       # )
      )
    
  })

  output$table <- renderReactable({
    reactable(rdf())
  })
  
}# close server

# Run the application 
shinyApp(ui = ui, server = server)
