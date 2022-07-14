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

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NRC Data Review"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons("time", h3("Time Filtering"),
                         choices = c("Dates" = 1,
                                    "Weeks" = 2),
                         selected = 1),
            conditionalPanel(condition = "input.time == 1",
                             dateRangeInput("dates",
                                            h5("Date Range"))),
            conditionalPanel(condition = "input.time == 2",
                             numericInput(inputId = "weeks",
                                          label = h5("# of Weeks"),
                                          value = 2))
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
          leafletOutput("map")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$map <- renderLeaflet({
    leaflet()%>%
      addProviderTiles(providers$OpenStreetMap.HOT)%>%
      fitBounds(-124.39, 25.82, -66.94, 49.38)%>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Extent",
        onClick=JS("function(btn, map){ map.fitBounds([[25.82, -124.39],[49.38, -66.94]]);}")))
  }) # close leaflet

  
}# close server

# Run the application 
shinyApp(ui = ui, server = server)
