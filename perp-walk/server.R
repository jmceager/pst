#Perp Walk (name in progress) dashboard
#    http://shiny.rstudio.com/

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
library(waiter)
library(showtext)
library(thematic)
#> Loading required package: sysfonts
#> Loading required package: showtextdb
dir.create('~/.fonts')
file.copy("www/Montserrat.ttf", "~/.fonts")
system('fc-cache -f ~/.fonts')
pstFont = "Montserrat"
#moved helper functions to separate script
source("helpfun.R")

#### themes ####

#set ggplot theme to function 
ggplot2::theme_set(theme_minimal())
#thematic_shiny(font = "Montserrat")
sysCol <- c("GD Perp" = "#6a3d9a",
            "GD" = "#cab2d6",
            "GT Perp" = "#1f78b4",
            "GT" = "#a6cee3",
            "HL Perp" = "#ff7f00",
            "HL" = "#fdbf6f")

fargs <- formals(icon)
fargs$verify_fa <- FALSE
formals(icon) <- fargs
  

#### data load ####
#loading data
incs <- read_csv("https://raw.githubusercontent.com/jmceager/pst/main/phmsa-clean/data/clean/all_inc.csv") %>%
  mutate(COMMODITY_RELEASED_TYPE = if_else(
    COMMODITY_RELEASED_TYPE == "REFINED AND/OR PETROLEUM PRODUCT (NON-HVL) WHICH IS A LIQUID AT AMBIENT CONDITIONS",
    "NON-HVL REFINED AND/OR PETROL PRODUCT",
    if_else(COMMODITY_RELEASED_TYPE == "HVL OR OTHER FLAMMABLE OR TOXIC FLUID WHICH IS A GAS AT AMBIENT CONDITIONS",
            "HVL OR FLAMMABLE OR TOXIC FLUID",
            if_else(COMMODITY_RELEASED_TYPE == "BIOFUEL / ALTERNATIVE FUEL(INCLUDING ETHANOL BLENDS)",
                    "BIOFUEL / ALT FUEL", COMMODITY_RELEASED_TYPE)))
  )%>%
  dplyr::filter(!grepl("GG",SYS), !grepl("UNGS",SYS)) %>% # dont need gathering or ungs
  mutate(daytxt = as.character(MDY, format = "%b %d, %Y"), 
         NUM_PUB_EVACUATED = replace_na(NUM_PUB_EVACUATED, 0),
         humans = FATAL + INJURE,
         IMONTH = month(MDY)) 
  
tab_cols <- c("NAME", "MDY","cleanLoc",  "FATAL","INJURE",
              "NUM_PUB_EVACUATED","IGNITE_IND","EXPLODE_IND",
              "COMMODITY_RELEASED_TYPE", "STATE",
              "TOTAL_RELEASE","TOTAL_COST_CURRENT","CAUSE", "NARRATIVE")

all_cols <- c("NAME", "MDY", "cleanLoc", "SYS","FATAL","INJURE",
              "NUM_PUB_EVACUATED","IGNITE_IND","EXPLODE_IND",
              "TOTAL_RELEASE", "UNITS","COMMODITY_RELEASED_TYPE", 
              "TOTAL_COST_CURRENT","CAUSE", "NARRATIVE", "STATE")


#### server start #####
# Define server logic required to draw a histogram
shinyServer( function(input, output, session) {
  
  #### header stuff ####
  helpMessage <- reactive({
    if(input$tabs == "now"){
       "This tab presents the 3 perpetrators in the selected month 
        based on PHMSA data. Perpetrators are defined by their system 
        and the determinant selected in the sidebar. Traditionally, PST has 
        used the size of release or spill to select a \"Perp of the Month.\" 
        Now, you can check to see which operators are the worst in a given month 
        based on the cost of their damage, number of deaths, deaths plus injuries, 
        or release size. Your selections will carry over to other tabs, where you 
        can compare incidents and operators through other criteria."
    }
    else if(input$tabs == "leafs"){
      "This tab presents a map of all incidents in the selected month. Determinants
      and systems can still be selected and changed in the sidebar. Click on points 
      for a popup with more information about that given incident, including the 
      operator name, and other impact statistics."
    }
    else if(input$tabs == "repeat"){
      "This tab presents a table of repeat perpetrators given the 
      system and determinant selected. Grouped rows present aggregated values for each   
      Operators' \"Perp\" incidents. In the case of columns like Release Size or Cost 
      of Damage, these are sums; but in columns like State or System, these are ranges of 
      values for each operator. Clicking on a grouped row will expand so you can see 
      each  \"Perp\" incident and its unique details."
    }
    else if(input$tabs == "timeline"){
      "This tab offers a few unique options to examine incidents. You can select 
      either annual or monthly periods, and choose a variable for the size of the points 
      using the drop down menu with the gear icon. To offer another option and combat 
      occasional bunching near the x-axis for certain data, you can also apply a 
      logarithm transformation to the y-axis. The results of this transformation can be 
      hugely educational but it's important to note how the axis labels and breaks change. 
      Finally, when looking at release size as a determinant, the Hazardous Liquids data 
      is split into a second plot, since HL releases are measured in different units."
    }
    else if(input$tabs == "hist"){
      "This tab presents every \"Perp\" incident over the whole period of the data 
      (2010-Present), based on the selected system and determinant in the sidebar. 
      Each column is sortable and searchable, and the number of rows can be changed 
      at the bottom to accomodate different window sizes."
    }
    else{
        "Under Construction..."
    }
  })
  
  output$infoText <- renderText(
    helpMessage()
  )
  
  #### create a reactive df for all incidents ####
  incidentReact <- reactive({
    if(input$system == "all"){
      incs %>%
        group_by(MoYr, SYS)%>%
        slice(which.max(.data[[input$weight]]))%>%
        dplyr::filter(.data[[input$weight]]>0)%>%
        select(all_of(all_cols))
    }
    else{
      incs %>%
        group_by(MoYr, SYS)%>%
        dplyr::filter(grepl(input$system, SYS))%>%
        slice(which.max(.data[[input$weight]]))%>%
        dplyr::filter(.data[[input$weight]]>0)%>%
        select(all_of(tab_cols))
    }
    
  })
  
  
  #### reactive pretty weight and size text ####
  prettyweight <- reactive({
    if_else(
      input$weight == "TOTAL_RELEASE",
      true = "Commodity Releases",
      false = if_else(
        input$weight == "TOTAL_COST_CURRENT",
        "Property Damage Costs",
        if_else(
          input$weight == "FATAL",
          "Fatalities",
          if_else(
            input$weight == "humans",
            "Fatalities or Injuries",
            "N/A"
          )
        )
      )
    )
  })
  
  prettysize <- reactive({
    if_else(
      input$sizeButton == "TOTAL_RELEASE",
      true = "Commodity Releases",
      false = if_else(
        input$sizeButton == "TOTAL_COST_CURRENT",
        "Property Damage Costs",
        if_else(
          input$sizeButton == "FATAL",
          "Fatalities",
          if_else(
            input$sizeButton == "INJURE",
            "Injuries",
            if_else(
              input$sizeButton == "mileage",
              "Operator Miles",
              if_else(
                input$sizeButton == "NUM_PUB_EVACUATED",
                "Evacuations",
                "Null"
              )
            )
          )
        )
      )
    )
  })

  
  #### Reactable table: ####
  #if either, then just those, if both then how to deal?
  output$histable <- renderReactable({
    #all table
    if(input$system == "all"){
      reactable(incidentReact(),
                #options
                searchable = TRUE,
                striped = TRUE,
                highlight = TRUE,
                sortable = TRUE,
                showPageSizeOptions = TRUE,
                showSortable = TRUE,
                filterable=TRUE,
                #theme stuff
                defaultColDef = colDef(
                  align = "center"
                ),
                defaultSorted = list(MDY = "desc"),
                minRows = 10,
                defaultPageSize = 20,
                pageSizeOptions = c(10,20,30),
                #column definitions
                columns = list(
                  MDY = colDef(format = colFormat(date = TRUE),
                               name = "Date"),
                  cleanLoc = colDef(name = "Place", width = 110),
                  MSYS = colDef(show = F),
                  MoYr = colDef(show = F),
                  STATE = colDef(show = F),
                  SYS = colDef(name = "System"),
                  NAME = colDef(name = "Operator", align = "left", minWidth = 180),
                  FATAL = colDef(name = "Deaths", width = 100),
                  INJURE = colDef(name = "Injuries", width = 100),
                  IGNITE_IND = colDef(name = "Ignited?", width = 100),
                  EXPLODE_IND = colDef(name = "Exploded?",width = 100),
                  NUM_PUB_EVACUATED = colDef(name = "People Evacuated", width = 100),
                  TOTAL_COST_CURRENT = colDef(name = "Cost of Damage", minWidth = 100,
                                              format = colFormat(currency = "USD",
                                                                 separators = TRUE)),
                  TOTAL_RELEASE = colDef(name = "Amount Released",
                                         format = colFormat(separators = TRUE),
                                         minWidth = 140),
                  COMMODITY_RELEASED_TYPE = colDef(name = "Commodity Released", width = 140),
                  CAUSE = colDef(name = "Cause", width = 140),
                  UNIT = colDef(name = "Release Unit", width = 100),
                  NARRATIVE = colDef( show = FALSE, html = TRUE)),
                theme = reactableTheme(
                  searchInputStyle = list(width = "100%"),
                  color = "hsl(233, 9%, 87%)",
                  backgroundColor = "hsl(233, 9%, 19%)",
                  borderColor = "hsl(233, 9%, 22%)",
                  stripedColor = "hsl(233, 12%, 22%)",
                  highlightColor = "hsl(198, 12%, 17%)",
                  inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
                  selectStyle = list(backgroundColor = "hsl(233, 12%, 28%)"),
                  pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
                  pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
                )
      )
      
    } 
    #singular table
    else{
      reactable(incidentReact(),
                #options
                searchable = TRUE,
                striped = TRUE,
                highlight = TRUE,
                sortable = TRUE,
                showPageSizeOptions = TRUE,
                showSortable = TRUE,
                filterable=TRUE,
                defaultColDef = colDef(
                  align = "center"
                ),
                defaultSorted = list(MDY = "desc"),
                minRows = 10,
                defaultPageSize = 20,
                pageSizeOptions = c(10,20,30),
                #column definitions
                columns = list(
                  MDY = colDef(format = colFormat(date = TRUE),
                               name = "Date"),
                  cleanLoc = colDef(name = "Place", width = 110),
                  MSYS = colDef(show = F),
                  MoYr = colDef(show = F),
                  STATE = colDef(show = F),
                  SYS = colDef(show = FALSE),
                  NAME = colDef(name = "Operator", align = "left", minWidth = 180),
                  FATAL = colDef(name = "Deaths", width = 100),
                  INJURE = colDef(name = "Injuries", width = 100),
                  IGNITE_IND = colDef(name = "Ignited?", width = 100),
                  EXPLODE_IND = colDef(name = "Exploded?",width = 100),
                  NUM_PUB_EVACUATED = colDef(name = "People Evacuated", width = 100),
                  TOTAL_COST_CURRENT = colDef(name = "Cost of Damage", minWidth = 100,
                                              format = colFormat(currency = "USD",
                                                                 separators = TRUE)),
                  TOTAL_RELEASE = colDef(name = "Amount Released",
                                         format = colFormat(separators = TRUE),
                                         minWidth = 140),
                  COMMODITY_RELEASED_TYPE = colDef(name = "Commodity Released", width = 140),
                  CAUSE = colDef(name = "Cause", width = 140),
                  NARRATIVE = colDef( show = FALSE, html = TRUE)),
                theme = reactableTheme(
                  searchInputStyle = list(width = "100%"),
                  color = "hsl(233, 9%, 87%)",
                  backgroundColor = "hsl(233, 9%, 19%)",
                  borderColor = "hsl(233, 9%, 22%)",
                  stripedColor = "hsl(233, 12%, 22%)",
                  highlightColor = "hsl(162, 12%, 24%)",
                  inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
                  selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
                  pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
                  pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
                )
           )
    }
  })
  
  ## need to update reactable styling and select cols and all that 
  
  
  #### Reactable frequent offenders####
  #count number of instances per subsidiary operator 
  output$repeatPerps <- renderReactable(
    if(input$system == "all"){
      #setup reactable data
      iR <- incidentReact()%>%
        ungroup()%>%
        mutate(IGIN = if_else(IGNITE_IND == "YES", 1, 0),
               EXIN = if_else(EXPLODE_IND == "YES", 1, 0))%>%
        group_by(NAME)%>%
        mutate(inc = n(),
               IGIN = sum(IGIN)/inc,
               EXIN = sum(EXIN)/inc,
               FS = sum(FATAL),
               IS = sum(INJURE),
               ES = sum(NUM_PUB_EVACUATED))%>%
        filter(inc >= 2)%>%
        ungroup()%>%
        select(!NARRATIVE)%>%
        mutate(STATE = replace_na(STATE, "Offshore"),
               igCol = colorScale(IGIN, pal = "YlOrRd", scale = "P"),
               exCol = colorScale(EXIN, pal = "YlOrRd", scale = "P"),
               fCol = colorScale(FS, pal = "OrRd", scale = "N"),
               iCol = colorScale(IS, pal = "OrRd", scale = "N"),
               eCol = colorScale(ES, pal = "Oranges", scale = "N", begin = .4))
      
      #get top 10 names including ties
      iList <- iR %>%
        group_by(NAME)%>%
        slice_max(inc, with_ties = F)%>%
        ungroup()%>%
        slice_max(n = 10, order_by = inc, with_ties = F)%>%
        select(NAME)
      
      #filter
      iR%>%
        filter(NAME %in% iList$NAME)%>%
        ## reactable starts here
        reactable(
          groupBy = "NAME",
          striped = TRUE,
          highlight = TRUE,
          sortable = TRUE,
          showPageSizeOptions = TRUE,
          showSortable = TRUE,
          defaultColDef = colDef(
            align = "center"
          ),
      #    defaultSorted = list(inc = "desc"),
          columns = list(
            NAME = colDef(width = 150, 
                          align = "left",
                          name = "Operator"),
            inc = colDef(name = "Perp Count", 
                         defaultSortOrder = "desc",
                         show = F),
            STATE = colDef(aggregate = "unique",
                           name = "States"),
            SYS = colDef(aggregate = "unique",
                            name = "System"),
            FATAL = colDef(name = "Deaths",
                           html = T,
                           align = "center",
                           aggregate = JS(
                             "function(values, rows){
                             let fatal = 0 
                             rows.forEach(function(row){
                              fatal += row['FATAL']
                              })
                             const preCol = \"%23\"
                             const circleColor = rows[0]['fCol']
                             const gradCircle = (
                               '<svg width=45 height=45 focusable=false>' +
                                '<circle cx=22.5 cy=22.5 r=18 fill='+ circleColor + ' stroke-width=2 stroke=rgba(170,170,170,0.03)></circle>' +
                               '</svg>'
                             )
                               
                             const label = '<div style=\"position: absolute; top: 50%; left: 50%; ' +
                                   'color:black; font-size:1.1vw; font-weight:600;'+
                                   'transform: translate(-50%, -50%); \">' + fatal + '</div>'
                               
                             return '<div style=\"display: inline-flex; position: relative\">' + gradCircle + label + '</div>'
                             }"
                           )),
            INJURE = colDef(name = "Injuries",
                            html = T,
                            align = "center",
                            aggregate = JS(
                              "function(values, rows){
                               let injure = 0 
                               rows.forEach(function(row){
                                injure += row['INJURE']
                                })
                               const preCol = \"%23\"
                               const circleColor = rows[0]['iCol']
                               const gradCircle = (
                                 '<svg width=45 height=45 focusable=false>' +
                                  '<circle cx=22.5 cy=22.5 r=18 fill='+ circleColor + ' stroke-width=2 stroke=rgba(170,170,170,0.03)></circle>' +
                                 '</svg>'
                               )
                                 
                               const label = '<div style=\"position: absolute; top: 50%; left: 50%; ' +
                                     'color:black; font-size:1.1vw;font-weight:600; '+
                                     'transform: translate(-50%, -50%); \">' + injure + '</div>'
                                 
                               return '<div style=\"display: inline-flex; position: relative\">' + gradCircle + label + '</div>'
                               }"
                            )),
            NUM_PUB_EVACUATED = colDef(name = "Public Evacuated",
                                       html = T,
                                       align = "center",
                                       aggregate = JS(
                                         "function(values, rows){
                                         let evac = 0 
                                         rows.forEach(function(row){
                                          evac += row['NUM_PUB_EVACUATED']
                                          })
                                         const preCol = \"%23\"
                                         const circleColor = rows[0]['eCol']
                                         const gradCircle = (
                                           '<svg width=45 height=45 focusable=false>' +
                                            '<circle cx=22.5 cy=22.5 r=18 fill='+ circleColor + ' stroke-width=2 stroke=rgba(170,170,170,0.03)></circle>' +
                                           '</svg>'
                                         )
                                           
                                         const label = '<div style=\"position: absolute; top: 50%; left: 50%; ' +
                                               'color:black; font-size:1.1vw; font-weight:600;'+
                                               'transform: translate(-50%, -50%); \">' + evac + '</div>'
                                           
                                         return '<div style=\"display: inline-flex; position: relative\">' + gradCircle + label + '</div>'
                                         }"
                                       )),
            TOTAL_RELEASE = colDef(aggregate = JS(
              "function(values,rows){
              let mscfRel = 0
              let galRel = 0
              rows.forEach(function(row){
                if(row['SYS'] == 'HL'){
                galRel += row['TOTAL_RELEASE']
                }
                else {
                mscfRel += row['TOTAL_RELEASE']
                }
              })
              if(galRel > 0 && mscfRel > 0){
                galRel = galRel.toString().split('.')
                galRel = galRel[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',')
                mscfRel = mscfRel.toString().split('.')
                mscfRel = mscfRel[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',')
                return [galRel, ' Gal; ', mscfRel, ' mscf']
              }
              else if(galRel >0 && mscfRel == 0){
                galRel = galRel.toString().split('.')
                galRel = galRel[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',')
                return [galRel, ' Gal']
              }
              else{
                mscfRel = mscfRel.toString().split('.')
                mscfRel = mscfRel[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',')
                return [mscfRel, ' mscf']
              }
              }"
            ),
                                   name = "Release Size",
                                   format = colFormat(digits = 0,
                                                      separators = T)),
            IGNITE_IND = colDef(name = "Fire",
                                html = T,
                                align = "center",
                                aggregate = JS(
                                "function(values,rows){
                                 let totalFire = 0
                                 let nrow = 0
                                 rows.forEach(function(row) {
                                   if(row['IGNITE_IND'] == 'YES'){
                                   totalFire += 1
                                   }
                                   nrow += 1
                                 })
                                 let perFire = Math.round(rows[0]['IGIN'] * 100 )
                                 
                                 const sliceColor = rows[0]['igCol']
                                 const sliceLength = 2 * Math.PI * 24
                                 const sliceOffset = sliceLength * (1 - perFire / 100)
                                 const donutChart = (
                                   '<svg width=50 height=50 style=\"transform: rotate(-90deg)\" focusable=false>' +
                                     '<circle cx=25 cy=25 r=21 fill=none stroke-width=4 stroke=rgba(0,0,0,0.1)></circle>' +
                                     '<circle cx=25 cy=25 r=21 fill=none stroke-width=4 stroke=' + sliceColor +
                                     ' stroke-dasharray=' + sliceLength + ' stroke-dashoffset=' + sliceOffset + '></circle>' +
                                   '</svg>'
                                 )
                                 const label = '<div style=\"position: absolute; top: 50%; left: 50%; ' +
                                   'font-weight:600;transform: translate(-50%, -50%)\">' + (perFire) + '%' + '</div>'
                                 return '<div style=\"display: inline-flex; position: relative\">' + donutChart + label + '</div>'
                                 }"
                                )),
            EXPLODE_IND = colDef(name = "Explosion",
                                 html = T,
                                 align = "center",
                                 aggregate = JS(
                                   "function(values,rows){
                                    let totalExp = 0
                                    rows.forEach(function(row) {
                                      if(row['EXPLODE_IND'] == 'YES'){
                                      totalExp += 1
                                      }
                                    })
                                    let perExp = Math.round( rows[0]['EXIN'] * 100)
                                 
                                    const sliceColor = rows[0]['exCol']
                                    const sliceLength = 2 * Math.PI * 24
                                    const sliceOffset = sliceLength * (1- perExp /100)
                                    const donutChart = (
                                      '<svg width=50 height=50 style=\"transform: rotate(-90deg)\" focusable=false>' +
                                        '<circle cx=25 cy=25 r=21 fill=none stroke-width=4 stroke=rgba(0,0,0,0.1)></circle>' +
                                        '<circle cx=25 cy=25 r=21 fill=none stroke-width=4 stroke=' + sliceColor +
                                        ' stroke-dasharray=' + sliceLength + ' stroke-dashoffset=' + sliceOffset + '></circle>' +
                                      '</svg>'
                                    )
                                    const label = '<div style=\"position: absolute; top: 50%; left: 50%; ' +
                                      'font-weight:600;transform: translate(-50%, -50%)\">' + (perExp) + '%' + '</div>'
                                    return '<div style=\"display: inline-flex; position: relative\">' + donutChart + label + '</div>'
                                    }"  
                                   )),
            MoYr = colDef(show = F),
            UNITS = colDef(name = "Units", show = F),
            TOTAL_COST_CURRENT = colDef(name = "Cost of Damage", 
                                        minWidth = 100,
                                        aggregate = JS(
                                          "function(values, rows){ 
                                          function numFormatter(num) {
                                            if(num > 999 && num < 1000000){
                                                return (num/1000).toFixed(1) + 'K'; // convert to K for number from > 1000 < 1 million 
                                            }else if(num > 1000000){
                                                return (num/1000000).toFixed(1) + 'M'; // convert to M for number from > 1 million 
                                            }else if(num < 900){
                                                return num; // if value < 1000, nothing to do
                                            }
                                          }
                                          let dCost = 0
                                          rows.forEach(function(row) {
                                            dCost += row['TOTAL_COST_CURRENT']
                                          })
                                          let finalCost = '$' + numFormatter(dCost) 
                                          return finalCost
                                          }"
                                        ),
                                        format = colFormat(currency = "USD",
                                                           separators = TRUE,
                                                           digits = 0)),
            COMMODITY_RELEASED_TYPE = colDef(name = "Material Released",
                                             aggregate = JS(
                                               "function(values,rows){
                                               var items = []
                                               rows.forEach(function(row){
                                                items.push(row['COMMODITY_RELEASED_TYPE'])
                                               })
                                               let uniqueItems = [...new Set(items)]
                                               if(uniqueItems.length > 1){
                                                return 'Various'
                                               }
                                               else{
                                                return uniqueItems
                                               }
                                               }"
                                             )),
            CAUSE = colDef(name = "Cause"),
            cleanLoc = colDef(name = "Place"),
            MDY = colDef(name = "Date"),
            IGIN = colDef(show = F),
            EXIN = colDef(show = F),
            FS = colDef(show = F),
            IS = colDef(show = F),
            ES = colDef(show = F),
            igCol = colDef(show = F),
            exCol = colDef(show = F),
            eCol = colDef(show = F),
            iCol = colDef(show = F),
            fCol = colDef(show = F)
          ),
          theme = reactableTheme(
            searchInputStyle = list(width = "100%"),
            color = "hsl(233, 9%, 87%)",
            backgroundColor = "hsl(233, 9%, 19%)",
            borderColor = "hsl(233, 9%, 22%)",
            stripedColor = "hsl(233, 12%, 22%)",
            highlightColor = "hsl(162, 12%, 24%)",
            inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
            selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
            pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
            pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
          )
        )
    }
    else{
      #setup reactable data
      iR <- incidentReact()%>%
        ungroup()%>%
        group_by(NAME)%>%
        mutate(inc = n())%>%
        filter(inc >= 2)%>%
        ungroup()%>%
        select(!NARRATIVE)%>%
        mutate(STATE = replace_na(STATE, "Offshore"))
      
      #get top 10 names including ties
      iList <- iR %>%
        group_by(NAME)%>%
        slice_max(inc, with_ties = F)%>%
        ungroup()%>%
        slice_max(n = 10, order_by = inc, with_ties = F)%>%
        select(NAME)
      
      #filter
      iR%>%
        filter(NAME %in% iList$NAME)%>%
        reactable(
          groupBy = "NAME",
          striped = TRUE,
          highlight = TRUE,
          sortable = TRUE,
          showPageSizeOptions = TRUE,
          showSortable = TRUE,
          defaultColDef = colDef(
            align = "center"
          ),
          #    defaultSorted = list(inc = "desc"),
          columns = list(
            NAME = colDef(width = 150, 
                          align = "left",
                          name = "Operator"),
            inc = colDef(name = "Perp Count", show = F),
            STATE = colDef(aggregate = "unique",
                           name = "States"),
            SYS = colDef(aggregate = "unique",
                            name = "System"),
            FATAL = colDef(aggregate = "sum",
                           name = "Deaths"),
            INJURE = colDef(aggregate = "sum",
                            name = "Injuries"),
            NUM_PUB_EVACUATED = colDef(aggregate = "sum",
                                       name = "Public Evacuated"),
            TOTAL_RELEASE = colDef(aggregate = JS(
              "function(values,rows){
              let mscfRel = 0
              let galRel = 0
              rows.forEach(function(row){
                if(row['SYS'] == 'HL'){
                galRel += row['TOTAL_RELEASE']
                }
                else {
                mscfRel += row['TOTAL_RELEASE']
                }
              })
              if(galRel > 0 && mscfRel > 0){
                galRel = galRel.toString().split('.')
                galRel = galRel[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',')
                mscfRel = mscfRel.toString().split('.')
                mscfRel = mscfRel[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',')
                return [galRel, ' Gal; ', mscfRel, ' mscf']
              }
              else if(galRel >0 && mscfRel == 0){
                galRel = galRel.toString().split('.')
                galRel = galRel[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',')
                return [galRel, ' Gal']
              }
              else{
                mscfRel = mscfRel.toString().split('.')
                mscfRel = mscfRel[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',')
                return [mscfRel, ' mscf']
              }
              }"
            ),
            name = "Release Size",
            format = colFormat(digits = 0,
                               separators = T)),
            IGNITE_IND = colDef(name = "Fire",
                                aggregate = JS(
                                  "function(values,rows){
                                 let totalFire = 0
                                 rows.forEach(function(row) {
                                   if(row['IGNITE_IND'] == 'YES'){
                                   totalFire += 1
                                   }
                                 })
                                 return totalFire
                                 }"
                                )),
            EXPLODE_IND = colDef(name = "Explosion",
                                 aggregate = JS(
                                   "function(values,rows){
                                    let totalFire = 0
                                    rows.forEach(function(row) {
                                      if(row['IGNITE_IND'] == 'YES'){
                                      totalFire += 1
                                      }
                                    })
                                    return totalFire
                                    }"  
                                 )),
            MoYr = colDef(show = F),
            UNITS = colDef(name = "Units", show = F),
            TOTAL_COST_CURRENT = colDef(name = "Cost of Damage", 
                                        aggregate = "sum",
                                        minWidth = 100,
                                        format = colFormat(currency = "USD",
                                                           separators = TRUE)),
            COMMODITY_RELEASED_TYPE = colDef(name = "Material Released",
                                             aggregate = JS(
                                               "function(values,rows){
                                               var items = []
                                               rows.forEach(function(row){
                                                items.push(row['COMMODITY_RELEASED_TYPE'])
                                               })
                                               let uniqueItems = [...new Set(items)]
                                               if(uniqueItems.length > 1){
                                                return 'Various'
                                               }
                                               else{
                                                return uniqueItems
                                               }
                                               }"
                                             )),
            CAUSE = colDef(name = "Cause"),
            cleanLoc = colDef(name = "Place"),
            MDY = colDef(name = "Date")
          ),
          theme = reactableTheme(
            searchInputStyle = list(width = "100%"),
            color = "hsl(233, 9%, 87%)",
            backgroundColor = "hsl(233, 9%, 19%)",
            borderColor = "hsl(233, 9%, 22%)",
            stripedColor = "hsl(233, 12%, 22%)",
            highlightColor = "hsl(162, 12%, 24%)",
            inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
            selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
            pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
            pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
          )
        )
    }
  )
  
  
  
  #### data for value boxes / this month ####
  # building the data for each box
  #GD
  recentGD <-  reactive({
    numb <- incs %>% 
      dplyr::filter(grepl("GD", SYS), MoYr == input$thisMonth) %>%
      slice_max(.data[[input$weight]])
    if(nrow(numb) == 1) {
      incs %>% 
        dplyr::filter(grepl("GD", SYS), MoYr == input$thisMonth) %>%
        slice_max(.data[[input$weight]])
    }
    else {
      incs %>% 
        dplyr::filter(grepl("GD", SYS), MoYr == input$thisMonth) %>%
        slice(1)%>%
        mutate(NAME = "N/A",
               INJURE = 0,
               FATAL = 0,
               TOTAL_COST_CURRENT = 0,
               TOTAL_RELEASE = 0,
               EXPLODE_IND = "NO",
               IGNITE_IND = "NO")
    }
  })
  
  #GT
  recentGT <-  reactive({
    numb <- incs %>% 
      dplyr::filter(grepl("GT", SYS), MoYr == input$thisMonth) %>%
      slice_max(.data[[input$weight]])
    if(nrow(numb) == 1) {
      incs %>% 
        dplyr::filter(grepl("GT", SYS), MoYr == input$thisMonth) %>%
        slice_max(.data[[input$weight]])
    }
    else {
      incs %>% 
        dplyr::filter(grepl("GT", SYS), MoYr == input$thisMonth) %>%
        slice(1)%>%
        mutate(NAME = "N/A",
               INJURE = 0,
               FATAL = 0,
               TOTAL_COST_CURRENT = 0,
               TOTAL_RELEASE = 0,
               EXPLODE_IND = "NO",
               IGNITE_IND = "NO")
    }
  })
  
  #HL boxes
  recentHL <-  reactive({
    numb <- incs %>% 
      dplyr::filter(MSYS == "HL", MoYr == input$thisMonth) %>%
      slice_max(.data[[input$weight]])
    if(nrow(numb) == 1) {
      incs %>% 
        dplyr::filter(MSYS == "HL", MoYr == input$thisMonth) %>%
        slice_max(.data[[input$weight]])
    }
    else {
      incs %>% 
        dplyr::filter(MSYS == "HL", MoYr == input$thisMonth) %>%
        slice(1)%>%
        mutate(NAME = "N/A",
               INJURE = 0,
               FATAL = 0,
               TOTAL_COST_CURRENT = 0,
               TOTAL_RELEASE = 0,
               EXPLODE_IND = "NO",
               IGNITE_IND = "NO")
    }
  })
  
  #### column for gas trans. incidents ####
  #title boxes
  output$gtNew <- renderValueBox({
    valueBox(
      value = tags$p(recentGT()$NAME,  
                     class = "value-name"#,
                     #style = "font-size: 3vw; white-space: pre-line;"
                     ),
      subtitle = HTML(
        if_else( recentGT()$NAME != "N/A",
                 paste0(format(recentGT()$MDY, format="%B %d, %Y"),
                        if_else(grepl("Outer Continental Shelf",recentGT()$cleanLoc),
                                " on the ",
                                " in "),
                        recentGT()$cleanLoc
                 ),
                 paste0("No Incidents with <em>", prettyweight(), "</em> this month")
        )
      ), 
      icon = icon("fire-flame-simple"),
      color = "olive"
    )
  }
  )
  
  
  
  output$gtFire <- renderValueBox({
    valueBox(
      value = tags$p(str_to_sentence( recentGT()$IGNITE_IND ),
                     style = "white-space:pre-wrap; font-size: 2vw;"),
      subtitle = HTML("Fire"),
      icon = tags$i(class = "fa-solid fa-fire", style = "font-size: 3vw"),
      color = if_else(recentGT()$IGNITE_IND == "YES", "red","yellow")
    )
  })
  
  output$gtExplode <- renderValueBox({
    valueBox(
      value = tags$p(str_to_sentence( recentGT()$EXPLODE_IND ),
                     style = "white-space:pre-wrap; font-size: 2vw;") ,
      subtitle = HTML("Explosion"),
      icon = tags$i(class = "fa-solid fa-bomb", style = "font-size: 3vw"),
      color = if_else(recentGT()$EXPLODE_IND == "YES", "red","yellow")
    )
  })
  
  output$gtFatal <- renderValueBox({
    valueBox(
      value = tags$p(
        if_else(recentGT()$FATAL > 0,
                as.character(recentGT()$FATAL),
                "No"),
        style = "white-space:pre-wrap; font-size: 2vw;"
      )
      ,
      subtitle = HTML("Deaths"),
      icon = tags$i(class = "fa-solid fa-skull-crossbones", style = "font-size: 3vw"),
      color = if_else(recentGT()$FATAL >0, "red","yellow")
    )
  })
  
  output$gtInjure <- renderValueBox({
    valueBox(
      value = tags$p(
        if_else(recentGT()$INJURE > 0,
                as.character(recentGT()$INJURE),
                "No"),
        style = "white-space:pre-wrap; font-size: 2vw;"
      )
      ,
      subtitle = HTML("Injuries"),
      icon = tags$i(class = "fa-solid fa-crutch", style = "font-size: 3vw"),
      color = if_else(recentGT()$INJURE >0, "red","yellow")
    )
  })
  
  output$gtSpill <- renderValueBox({
    valueBox(
      value = tags$p(
        if_else(recentGT()$TOTAL_RELEASE > 0,
                comma(recentGT()$TOTAL_RELEASE),
                "No"),
        style = "white-space:pre-wrap; font-size: 2vw;"
      )
      ,
      subtitle = HTML( paste0("mscf of <em>", 
                              str_to_lower(recentGT()$COMMODITY_RELEASED_TYPE), 
                              "</em> released")),
      icon = tags$i(class = "fa-solid fa-smog", style = "font-size: 3vw"),
      color = "olive"
    )
  })
  
  output$gtCost <- renderValueBox({
    valueBox(
      value = tags$p(
        if_else(recentGT()$TOTAL_COST_CURRENT > 0,
                paste0("$",comma(recentGT()$TOTAL_COST_CURRENT)),
                "$0"),
        style = "white-space:pre-wrap; font-size: 2vw;"
      )
      ,
      subtitle = HTML( paste0("in Property Damage <br> (2022 dollars)")),
      icon = tags$i(class = "fa-solid fa-dollar-sign", style = "font-size: 3vw"),
      color = "olive"
    )
  })
  
  
  
  #### column for gas dist. incidents ####
  #title boxes
  output$gdNew <- renderValueBox({
    valueBox(
      value = tags$p(recentGD()$NAME, 
                     class = "value-name",
                     style = "font-size: 3vw; white-space: pre-line;"),
      subtitle = HTML(
        if_else( recentGD()$NAME != "N/A",
                 paste0(format(recentGD()$MDY, format="%B %d, %Y"),
                        " in ",
                        recentGD()$cleanLoc
                 ),
                 paste0("No Incidents with <em>", prettyweight(), "</em> this month")
        )
      ), 
      icon = icon("fire-flame-simple"),
      color = "fuchsia"
    )
  }
  )
  
  
  
  output$gdFire <- renderValueBox({
    valueBox(
      value = tags$p(str_to_sentence( recentGD()$IGNITE_IND ),
                     style = "white-space:pre-wrap; font-size: 2vw;"),
      subtitle = HTML("Fire"),
      icon = tags$i(class = "fa-solid fa-fire", style = "font-size: 3vw"),
      color = if_else(recentGD()$IGNITE_IND == "YES", "red","yellow")
    )
  })
  
  output$gdExplode <- renderValueBox({
    valueBox(
      value = tags$p(str_to_sentence( recentGD()$EXPLODE_IND ),
                     style = "white-space:pre-wrap; font-size: 2vw;") ,
      subtitle = HTML("Explosion"),
      icon = tags$i(class = "fa-solid fa-bomb", style = "font-size: 3vw"),
      color = if_else(recentGD()$EXPLODE_IND == "YES", "red","yellow")
    )
  })
  
  output$gdFatal <- renderValueBox({
    valueBox(
      value = tags$p(
        if_else(recentGD()$FATAL > 0,
                as.character(recentGD()$FATAL),
                "No"),
        style = "white-space:pre-wrap; font-size: 2vw;"
      )
      ,
      subtitle = HTML("Deaths"),
      icon = tags$i(class = "fa-solid fa-skull-crossbones", style = "font-size: 3vw"),
      color = if_else(recentGD()$FATAL >0, "red","yellow")
    )
  })
  
  output$gdInjure <- renderValueBox({
    valueBox(
      value = tags$p(
        if_else(recentGD()$INJURE > 0,
                as.character(recentGD()$INJURE),
                "No"),
        style = "white-space:pre-wrap; font-size: 2vw;"
      )
      ,
      subtitle = HTML("Injuries"),
      icon = tags$i(class = "fa-solid fa-crutch", style = "font-size: 3vw"),
      color = if_else(recentGD()$INJURE >0, "red","yellow")
    )
  })
  
  output$gdSpill <- renderValueBox({
    valueBox(
      value = tags$p(
        if_else(recentGD()$TOTAL_RELEASE > 0,
                comma(recentGD()$TOTAL_RELEASE),
                "No"),
        style = "white-space:pre-wrap; font-size: 2vw;"
      )
      ,
      subtitle = HTML( paste0("mscf of <em>", 
                              str_to_lower(recentGD()$COMMODITY_RELEASED_TYPE), 
                              "</em> released")),
      icon = tags$i(class = "fa-solid fa-smog", style = "font-size: 3vw"),
      color = "fuchsia"
    )
  })
  
  output$gdCost <- renderValueBox({
    valueBox(
      value = tags$p(
        if_else(recentGD()$TOTAL_COST_CURRENT > 0,
                paste0("$",comma(recentGD()$TOTAL_COST_CURRENT)),
                "$0"),
        style = "white-space:pre-wrap; font-size: 2vw;"
      )
      ,
      subtitle = HTML( paste0("in Property Damage <br> (2022 dollars)")),
      icon = tags$i(class = "fa-solid fa-dollar-sign", style = "font-size: 3vw"),
      color = "fuchsia"
    )
  })
  
 
  
  ####column for hl incidents####
  output$hlNew <- renderValueBox({
    valueBox(
      value = tags$p(recentHL()$NAME,  
                     class = "value-name",
                     style = "font-size: 3vw; white-space: pre-line;"),
      subtitle = HTML(
        if_else( recentHL()$NAME != "N/A",
                 paste0(format(recentHL()$MDY, format="%B %d, %Y"),
                        if_else(grepl("Outer Continental Shelf",recentHL()$cleanLoc),
                                " on the ",
                                " in "),
                        recentHL()$cleanLoc
                 ),
                 paste("No Incidents with <em>", prettyweight(), "</em> this month", sep = " ")
        )
      ), 
      icon = icon("triangle-exclamation"),
      color = "maroon"
    )
  }
  )
  
  
  
  output$hlFire <- renderValueBox({
    valueBox(
      value = tags$p(str_to_sentence( recentHL()$IGNITE_IND ),
                     style = "white-space:pre-wrap; font-size: 2vw;"),
      subtitle = HTML("Fire"),
      icon = tags$i(class = "fa-solid fa-fire", style = "font-size: 3vw"),
      color = if_else(recentHL()$IGNITE_IND == "YES", "red","yellow")
    )
  })
  
  output$hlExplode <- renderValueBox({
    valueBox(
      value = tags$p(str_to_sentence( recentHL()$EXPLODE_IND ),
                     style = "white-space:pre-wrap; font-size: 2vw;") ,
      subtitle = HTML("Explosion"),
      icon = tags$i(class = "fa-solid fa-bomb", style = "font-size: 3vw"),
      color = if_else(recentHL()$EXPLODE_IND == "YES", "red","yellow")
    )
  })
  
  output$hlFatal <- renderValueBox({
    valueBox(
      value = tags$p(
        if_else(recentHL()$FATAL > 0,
                as.character(recentHL()$FATAL),
                "No"),
        style = "white-space:pre-wrap; font-size: 2vw;"
      )
      ,
      subtitle = HTML("Deaths"),
      icon = tags$i(class = "fa-solid fa-skull-crossbones", style = "font-size: 3vw"),
      color = if_else(recentHL()$FATAL >0, "red","yellow")
    )
  })
  
  output$hlInjure <- renderValueBox({
    valueBox(
      value = tags$p(
        if_else(recentHL()$INJURE > 0,
                as.character(recentHL()$INJURE),
                "No"),
        style = "white-space:pre-wrap; font-size: 2vw;"
      )
      ,
      subtitle = HTML("Injuries"),
      icon = tags$i(class = "fa-solid fa-crutch", style = "font-size: 3vw"),
      color = if_else(recentHL()$INJURE >0, "red","yellow")
    )
  })
  
  output$hlSpill <- renderValueBox({
    valueBox(
      value = tags$p(
        if_else(recentHL()$TOTAL_RELEASE > 0,
                comma(recentHL()$TOTAL_RELEASE),
                "No"),
        style = "white-space:pre-wrap; font-size: 2vw;"
      )
      ,
      subtitle = HTML( paste0("Gallons of <em>", 
                              str_to_lower(recentHL()$COMMODITY_RELEASED_TYPE), 
                              "</em> released")),
      icon = tags$i(class = "fa-solid fa-fill-drip", style = "font-size: 3vw"),
      color = "maroon"
    )
  })
  
  output$hlCost <- renderValueBox({
    valueBox(
      value = tags$p(
        if_else(recentHL()$TOTAL_COST_CURRENT > 0,
                paste0("$",comma(recentHL()$TOTAL_COST_CURRENT)),
                "0"),
        style = "white-space:pre-wrap; font-size: 2vw;"
      )
      ,
      subtitle = HTML( paste0("in Property Damage <br> (2022 dollars)")),
      icon = tags$i(class = "fa-solid fa-dollar-sign", style = "font-size: 3vw"),
      color = "maroon"
    )
  })
  
#### map tab ####
  ## Map stuff 
  #create map df
  mapData <- reactive({
    if(input$system == "all"){
      if(input$weight == "TOTAL_RELEASE"){
        incs %>% 
          dplyr::filter(MoYr == input$thisMonth) %>%
          group_by(SYS) %>%
          mutate(Max = if_else(.data[[input$weight]] == max(.data[[input$weight]]), T, F),
                 color = if_else(grepl("GD", SYS), 
                                 if_else(Max == T,"#6a3d9a","#cab2d6"),
                                 if_else(grepl("GT", SYS),
                                         if_else(Max == T, "#1f78b4","#a6cee3"),
                                         if_else(Max ==T,"#ff7f00","#fdbf6f")
                                 )
                 )
          )%>% 
          ungroup()%>%
          group_by(MSYS)%>%
          mutate(size = rangeBrother(.data[[input$weight]]))
          
      }
      else{
        incs %>% 
          dplyr::filter(MoYr == input$thisMonth) %>%
          group_by(SYS) %>%
          mutate(Max = if_else(.data[[input$weight]] == max(.data[[input$weight]]) &
                                 max(.data[[input$weight]]) > 0, T, F),
                 color = if_else(grepl("GD", SYS), 
                                 if_else(Max == T,"#6a3d9a","#cab2d6"),
                                 if_else(grepl("GT", SYS),
                                         if_else(Max == T, "#1f78b4","#a6cee3"),
                                         if_else(Max ==T,"#ff7f00","#fdbf6f")
                                         )
                                 )
                 )%>%
          ungroup()%>%
          mutate(size = rangeBrother(.data[[input$weight]]))
      }
    }
    else {
     incs %>% 
        dplyr::filter(grepl(input$system, SYS), MoYr == input$thisMonth) %>%
        mutate(Max = if_else(.data[[input$weight]] == max(.data[[input$weight]]), T, F),
               size = rangeBrother(.data[[input$weight]]),
               color = if_else(grepl("GD", SYS), 
                               if_else(Max == T,"#6a3d9a","#cab2d6"),
                               if_else(grepl("GT", SYS),
                                       if_else(Max == T, "#1f78b4","#a6cee3"),
                                       if_else(Max ==T,"#ff7f00","#fdbf6f")
                                        )
                                )
               )
    }
  })
  
  #draw basemap
  output$incMap <- renderLeaflet({
    leaflet()%>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      fitBounds(-124.39, 25.82, -66.94, 49.38)%>%
      addEasyButton(easyButton(
        icon="fa-regular fa-map", title="Zoom to Extent",
        onClick=JS("function(btn, map){ map.fitBounds([[25.82, -124.39],[49.38, -66.94]]);}")))
  })
  
  #this fixes issue of leaflet not loading whne its not the main tab
  outputOptions(output, "incMap", suspendWhenHidden = FALSE)
  
  #draw markers on top that react 
  # note: redo size in the map df so that it's based on quantiles maybe?
  #should size be unique to each month or not? 
  observe({
    req(input$tabs == "leafs")
    leafletProxy("incMap", data = mapData()) %>%
      clearMarkers() %>%
      removeControl("colorLegend")%>%
      addCircleMarkers(radius = ~size, weight = 1, fillOpacity = 0.6,
                       fillColor = ~color,
                       color = ~color,
                       lat = ~LOCATION_LATITUDE, lng = ~LOCATION_LONGITUDE,
                       popup = paste0("<b>Operator:</b>",
                                      mapData()$NAME,
                                      "<br>",
                                      "<b>Date:</b>",
                                      mapData()$MDY,
                                      "<br>",
                                      "<b>System:</b>",
                                      mapData()$SYS,
                                      "<br>",
                                      "<b>Place:</b> ", 
                                      mapData()$cleanLoc , 
                                      "<br>",
                                      "<b>Release:</b> ",
                                      comma(round(mapData()$TOTAL_RELEASE, digits = 0)), " ",
                                      mapData()$UNITS,
                                      "<br>",
                                      "<b>Cost of Damage:</b> $",
                                      comma(mapData()$TOTAL_COST_CURRENT),
                                      "<br>",
                                      "<b>Fatalities:</b> ",
                                      mapData()$FATAL,
                                      "<br>",
                                      "<b>Injuries:</b> ",
                                      mapData()$INJURE
                       )
      )%>%
      addLegend(
        layerId = "colorLegend",
        colors = c("#6a3d9a","#cab2d6","#1f78b4","#a6cee3","#ff7f00","#fdbf6f"),
        labels = c("GD Perp","Other GD","GT Perp","Other GT","HL Perp","Other HL"),
        position = c("topleft")
      )
    
  })
  
  observe({
    req(input$tabs == "leafs")
    leafProxy <- leafletProxy("incMap", data = mapData())
    if (input$system == "all" & input$weight == "TOTAL_RELEASE"){
      gasWeight <- mapData() %>% 
        dplyr::filter(UNITS == "mscf")
      hazWeight <- mapData()%>%
        dplyr::filter(UNITS != "mscf")
      leafProxy %>%
        removeControl("sizeLeg")%>%
        removeControl("gasLeg")%>%
        removeControl("hazLeg")%>%
        addLegendCustom(weight = unlist(gasWeight[input$weight]),
                        units = "mscf",
                        sys = "Gas",
                        weightName = input$weight,
                        legName = "gasLeg")%>%
        addLegendCustom(weight = unlist(hazWeight[input$weight]),
                        units = "gal",
                        sys = "HL",
                        weightName = input$weight,
                        legName = "hazLeg")
    }
    else {
      units <- if_else(input$weight == "TOTAL_RELEASE" & input$system != "HL","mscf",
                       if_else(input$weight == "TOTAL_RELEASE" & input$system == "HL", "gal",
                               "")
                       )
      leafProxy %>%
        removeControl("gasLeg")%>%
        removeControl("hazLeg")%>%
        removeControl("sizeLeg")%>%
        addLegendCustom(weight = mapData()[[input$weight]], 
                        weightName = input$weight,
                        units = units,
                        sys = input$system,
                        legName = "sizeLeg")
    }
  })
  
#### Timeline Plots  ####
  plotData <- reactive({
    selMo =  if_else(rep(input$periodSwitch == F,12), rep(month(ymd(input$thisMonth)),12), seq(1,12,1) )
    selYr = year(ymd(input$thisMonth))
    if(input$system == "all"){
      if(input$weight == "TOTAL_RELEASE"){
        incs %>%
          mutate(booMo = if_else(IMONTH %in% selMo, T,F),
                 booYr = if_else(IYEAR == selYr, T,F)) %>%
          dplyr::filter(booMo == T & booYr == T) %>%
          group_by(SYS, MoYr) %>%
          mutate(Max = if_else(.data[[input$weight]] == max(.data[[input$weight]]), T, F))%>%
          mutate(SysMax = paste0(SYS, ifelse(Max, " Perp", "")),
                 None = 1)

      }
      else{
        incs %>%
          mutate(booMo = if_else(IMONTH %in% selMo, T,F),
                 booYr = if_else(IYEAR == selYr, T,F)) %>%
          dplyr::filter(booMo == T & booYr == T) %>%
          group_by(SYS, MoYr) %>%
          mutate(Max = if_else(.data[[input$weight]] == max(.data[[input$weight]]) &
                                 max(.data[[input$weight]]) > 0, T, F))%>%
          mutate(SysMax = paste0(SYS, ifelse(Max, " Perp", "")),
                 None = 1)
      }
    }
    else {
      incs %>%
        mutate(booMo = if_else(IMONTH %in% selMo, T,F),
               booYr = if_else(IYEAR == selYr, T,F)) %>%
        dplyr::filter(booMo == T & booYr == T) %>%
        dplyr::filter(grepl(input$system, SYS)) %>%
        group_by(MoYr)%>%
        mutate(Max = if_else(.data[[input$weight]] == max(.data[[input$weight]]), T, F))%>%
        mutate(SysMax = paste0(SYS, ifelse(Max, " Perp", "")),
               None = 1)
    }
  })

  waitress <- Waitress$new("#timePlot") # call the waitress
  
 #### create plots based on button behavior ####
  output$timePlot <- renderPlot({ 
    for(i in 1:10){
      waitress$inc(10) # increase by 10%
      Sys.sleep(.1)
    }
    on.exit(waitress$close())
    #for year
    if(input$system == "all" & input$weight == "TOTAL_RELEASE"){
      df <- plotData() %>%
        filter(!grepl("HL", SYS))
      sysCol <- sysCol[1:4] #sysCol is the color dictionairy 
    }
    else{
      df <- plotData()
      sys <- input$system
      if(input$system == "HL"){
        sysCol <- sysCol[5:6]
      }
      else if(input$system == "GD"){
        sysCol <- sysCol[1:2]
      }
      else if(input$system == "GT"){
        sysCol <- sysCol[3:4]
      }
    }
    ggplot(data = df, aes(x = MDY,
                          y = df[[input$weight]],
                          fill = SysMax,
                          color = SysMax,
                          size = df[[input$sizeButton]]))+
      geom_point(alpha = .7)+
      scale_fill_manual(values = sysCol, guide = "none")+
      scale_color_manual(values = sysCol)+
      scale_x_date(date_labels = if_else( input$periodSwitch, "%b", "%b %d"),
                   date_breaks = if_else(input$periodSwitch, "1 month", "7 days"),
                   limits = dayRange(input$thisMonth,if_else(input$periodSwitch, "y","m")),
                   name = ""
                   )+
      scale_size(name = str_wrap(prettysize(),15),
                 limits = c(0, max(df[[input$sizeButton]])))+
      scale_y_continuous(name = prettyweight(),
                         trans = if_else(input$logY == T, "pseudo_log","identity"),
                         breaks = yBreak(df[[input$weight]], input$logY,"b"),
                         labels = yBreak(df[[input$weight]], input$logY,"l"))+
      labs(title = "Perpetrators Among All Incidents",
           subtitle = paste(weightName(input$weight, input$system), "in", if_else(input$periodSwitch,
                                                                                  paste(year(ymd(input$thisMonth))),
                                                                                  paste(month(ymd(input$thisMonth),
                                                                                              label = T,
                                                                                              abbr = F),
                                                                                        year(ymd(input$thisMonth)))
               )),
           color = "System")+
      theme_pst(font = pstFont)
  })


  output$hlTimePlot <- renderPlot({
    # data 
    df <- filter(plotData(), grepl("HL", SYS))
    #the actual plot
    ggplot(data = df, aes(x = MDY,
                          y = df[[input$weight]],
                          fill = SysMax,
                          color = SysMax,
                          size = df[[input$sizeButton]]))+
      geom_point(alpha = .7)+
      scale_color_manual(values = sysCol[5:6])+
      scale_fill_manual(values = sysCol, guide = "none")+
      scale_x_date(date_labels = if_else(input$periodSwitch,
                                         "%b",
                                         "%b %d"),
                   date_breaks = if_else(input$periodSwitch, "1 month", "7 days"),
                   limits = dayRange(input$thisMonth, if_else(input$periodSwitch, "y","m")),
                   name = ""
                   )+
      scale_size(name = str_wrap(prettysize(),15),
                 limits = c(0, max(df[[input$sizeButton]])))+
      scale_y_continuous(name = prettyweight(),
                         trans = if_else(input$logY == T, "pseudo_log","identity"),
                         breaks = yBreak(filter(plotData(),grepl("HL", SYS))[[input$weight]],
                                         input$logY,"b"),
                         labels = yBreak(filter(plotData(),grepl("HL", SYS))[[input$weight]],
                                         input$logY,"l"))+
      labs(title = "Perpetrators Among All Incidents",
           subtitle = paste(weightName(input$weight, "HL"), "in", if_else(input$periodSwitch,
                                                                          paste(year(ymd(input$thisMonth))),
                                                                          paste(month(ymd(input$thisMonth),
                                                                                      label = T,
                                                                                      abbr = F),
                                                                                    year(ymd(input$thisMonth)))
                                                                          )),
           color = "System")+
      theme_pst(font = pstFont)
  })
  
  waiter_hide()
  
  #### GGPlot Tooltips ####
  output$hover_info <- renderUI({
    ## get relevant data 
    if(input$system == "all" & input$weight == "TOTAL_RELEASE"){
      df <- plotData() %>%
        filter(!grepl("HL", SYS)) 
    }
    else{
      df <- plotData()
    }
    
    if(input$logY){
      df <- df %>%
        mutate_at(vars(input$weight),pseudo_log_trans()$transform )
    }
    
    #get the location of the hover and get the relevant data point
    hover <- input$plot_hover
    point <- nearPoints(df, hover, threshold = 5, maxpoints = 1, addDist = TRUE,
                        yvar = input$weight)
    
    #no tooltip if cursor not near points 
    if (nrow(point) == 0) return(NULL)
    
    #get the coordinates for the cursor 
    left_px <- hover$coords_css$x
    top_px <- hover$coords_css$y
    
    # create style property for tooltip
    # transparent bg and z-index for placement
    pos <- paste0("left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    # tooltip  as wellPanel
    wellPanel(
      class = "ggtip",
      style = pos,
      p(HTML(paste0("<b> Operator: </b>", point$NAME, "<br/>",
                    "<b> Location: </b>", point$cleanLoc, "<br/>",
                    "<b> Date: </b>", point$daytxt, "<br/>",
                    "<b> Release: </b>", comma(point$TOTAL_RELEASE),"<br/>",
                    "<b> Cost: </b> $", comma(point$TOTAL_COST_CURRENT), "<br/>"
      )))
    )
  })
  
  output$hl_info <- renderUI({
    ## get relevant data 
    df <- filter(plotData(), grepl("HL", SYS))
    
    #get the location of the hover and get the relevant data point
    hover <- input$hl_hover
    point <- nearPoints(df, hover, threshold = 5, maxpoints = 1, addDist = TRUE,
                        yvar = input$weight)
    
    #no tooltip if cursor not near points 
    if (nrow(point) == 0) return(NULL)
    
    #get the coordinates for the cursor 
    left_px <- hover$coords_css$x
    top_px <- hover$coords_css$y
    
    # create style property for tooltip
    # transparent bg and z-index for placement
    pos <- paste0("left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    # tooltip  as wellPanel
    wellPanel(
      class = "ggtip",
      style = pos,
      p(HTML(paste0("<b> Operator: </b>", point$NAME, "<br/>",
                    "<b> Location: </b>", point$cleanLoc, "<br/>",
                    "<b> Date: </b>", point$daytxt, "<br/>",
                    "<b> Release: </b>", comma(point$TOTAL_RELEASE), " Gal. <br/>",
                    "<b> Cost: </b> $", comma(point$TOTAL_COST_CURRENT), "<br/>"
      )))
    )
  })
  
}
)