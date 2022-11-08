#worst incidents of month dashboard server side

dir.create('~/.fonts')
file.copy("www/Montserrat.ttf", "~/.fonts")
system('fc-cache -f ~/.fonts')
pstFont = "Montserrat"

#set ggplot theme to function 
ggplot2::theme_set(theme_minimal())


#### server start #####
# Define server logic required to draw a histogram
shinyServer( function(input, output, session) {
  
  #### waiter ####
  w <- Waiter$new(id = c("timePlot","hlTimePlot"))
  
  #### opening dialog ####
  shinyalert(title = "Welcome!", 
             text = tags$div(
               tags$p("This dashboard explores pipeline incident data, provided by the Pipeline and 
                      Hazardous Materials Safety Administration, to explore which operators are
                      responsible for the worst incidents every month. Historically, the 
                      Pipeline Safety Trust has used the size of the release to determine that 
                      label, but we now offer a few different metrics for you to examine 
                      what goes into labeling incidents as the \"worst\" every month. This dashboard 
                      also offers a few new ways to visualize how these incidents compare on a monthly basis, 
                      as well as detailing which operators are most frequently responsible for these incidents. "),
               tags$p("For more information, use the tutorial button located in the Information drop-down labeled: ", icon("info")), 
             ),  html = T, size = "m")
  
  #### intro js ####
  # initiate hints on startup with custom button and event
  hintjs(session, options = list("hintButtonLabel"="Hope this hint was helpful"),
         events = list("onhintclose"=I('alert("Wasn\'t that hint helpful")')))
  
  steps <- reactive(tibble(
    element = c(
      NA,
      ifelse(input$system == "all", "#GDBox", paste0("#",input$system,"Box")),
      ".sidebar-menu",
      "#controls",
      ".navbar-custom-menu",
      NA
    ),
    intro = c(
      #welcome
      "Welcome! This dashboard is designed to show you which incidents 
      were the worst in a given month & help shed light on who was responsible
      for them. It also offers you, the user, a chance to define what really 
      is the \"worst\" incident. Let's take a look at how it all works:",
      #value box box
      "Here is where you will find a high-level summary of the worst incident
      in each system this month. These boxes give a quick glance at which operator
      was responsible for the worst incident, and what kind of impact that incident had.",
      #sidebar
      "These buttons direct you to other tabs, where the info button can explain more 
      on those pages. Briefly: Repeat Offenders shows a table of the most frequently responsible operators, 
      the Monthly Map geographically displays the worst incidents alongside all other incidents in a month, 
      Incident Plots offer an opportunity to graph these incidents by a host of variables, and 
      the Full Table lists out the culprits and their incidents for the entire period of 
      time covered by this set of PHMSA data (2010 to Present)",
      #controls
      "These controls allow you to select the month that you are interested in, the 
      system that you are interested in, and how you would like to determine which 
      incidents are the \"worst\". These controls work across tabs, so you can switch 
      between all of them seamlessly. ",
      #navbar 
      "Finally, using these dropdown menus, you can access more information such as 
      the link to the source data a brief description of each tab, and links for 
      sharing this dashboard to social media and email.",
      #finally
      "Now it's time for you to explore the data!"
    ),
    position = c("auto", "auto","right", "right","left","auto")
  ))
  
  # start introjs when button is pressed with custom options and events
  observeEvent(input$help, {
               introjs(session, options = list("nextLabel"="Next",
                                               "prevLabel"="Previous",
                                               "skipLabel"="Exit",
                                               steps = steps()))
  })
    
  #### header stuff ####
  helpMessage <- reactive({
    if(input$tabs == "now"){
       "This tab presents the 3 worst incidents in the selected month 
        based on PHMSA data. The worst incidents are defined by their system 
        and the determinant selected in the sidebar. Your selections 
        will carry over to other tabs, where you can compare incidents 
        and operators through other criteria."
    }
    else if(input$tabs == "leafs"){
      "This tab presents a map of all incidents in the selected month. Determinants
      and systems can still be selected and changed in the sidebar. Click on points 
      for a popup with more information about that given incident, including the 
      operator name, and other impact statistics."
    }
    else if(input$tabs == "repeat"){
      "This tab presents a table of operators with multiple worst-of-the-month incidents given the 
      system and determinant selected. Grouped rows present aggregated values for each   
      Operators' \"Worst\" incidents. In the case of columns like Release Size or Cost 
      of Damage, these are sums; but in columns like State or System, these are ranges of 
      values for each operator. Clicking on a grouped row will expand so you can see 
      each  \"Worst\" incident and its unique details."
    }
    else if(input$tabs == "timeline"){
      "This tab offers a few unique options to examine incidents. You can select 
      either annual or monthly periods, and choose a variable for the size of the points 
      using the drop down menu with the gear icon. To offer another option and combat 
      occasional bunching near the x-axis for certain data, you can also apply a 
      logarithmic transformation to the y-axis. The results of this transformation can be 
      hugely educational but it's important to note how the axis labels and breaks change. 
      Finally, when looking at release size as a determinant, the Hazardous Liquids data 
      is split into a second plot, since HL releases are measured in different units."
    }
    else if(input$tabs == "hist"){
      "This tab presents every \"Worst\" incident over the whole period of the data 
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
  output$repeatPerps <- renderReactable({
    
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
        mutate(igCol = colorScale(IGIN, pal = "YlOrRd", scale = "P"),
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
      iR <- filter(iR, NAME %in% iList$NAME)
        ## reactable starts here
        reactable(
          data = iR,
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
            inc = colDef(name = "Worst Count", 
                         defaultSortOrder = "desc",
                         show = F),
            STATE = colDef(aggregate = "unique",
                           name = "States"),
            SYS = colDef(aggregate = JS(
              "function(values, rows){
              let systems = []
              rows.forEach(function(row){
                if (!systems.includes(row['SYS'])){
                  systems.push(row['SYS'])
                }
              })
              
              const hlCol = '#ff7f00'
              const gdCol = '#6a3d9a'
              const gtCol = '#1f78b4'
              
              systems.forEach(function(sys, i){
                if (sys === 'HL'){
                  systems[i] = '<span style = \"color:#ff7f00;  \">' + sys + '</span>'
                }
                else if (sys === 'GD'){
                  systems[i] = '<span style = \"color:#6a3d9a; \">' +  sys + '</span>'
                }
                else {
                  systems[i] = '<span style = \"color:#1f78b4; \">' +  sys + '</span>'
                }
              })
              
              let styledList = '<div style = \"font-weight:600; font-size: 1.1em; \">' +
                               systems.join('<span style = \"color:#ffffff; \">, </span>') + '</div>'
              return styledList
              }"
            ),
                         name = "System",
                         html = T,
                         style = function(value, index, name) {
                           if (value == "HL") {
                             list(fontWeight = 500, color = "#ff7f00")
                           }
                           else if (value == "GD") {
                             list(fontWeight = 500, color = "#6a3d9a")
                           }
                           else {
                             list(fontWeight = 500, color = "#1f78b4")
                           }
                         }),
            FATAL = colDef(name = "Deaths",
                           html = T,
                           align = "center",
                           cell = function(value, index){
                             valStyle = if_else(value > 0, 
                                                "font-weight:600;",
                                                "font-weight:300;")
                             div(style = valStyle,
                                 value)
                           },
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
                                   'color:black; font-size:1.1em; font-weight:600;'+
                                   'transform: translate(-50%, -50%); \">' + fatal + '</div>'
                               
                             return '<div style=\"display: inline-flex; position: relative\">' + gradCircle + label + '</div>'
                             }"
                           )),
            INJURE = colDef(name = "Injuries",
                            html = T,
                            align = "center",
                            cell = function(value, index){
                              valStyle = if_else(value > 0, 
                                                 "font-weight:600;",
                                                 "font-weight:300;")
                              div(style = valStyle,
                                  value)
                            },
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
                                     'color:black; font-size:1.1em;font-weight:600; '+
                                     'transform: translate(-50%, -50%); \">' + injure + '</div>'
                                 
                               return '<div style=\"display: inline-flex; position: relative\">' + gradCircle + label + '</div>'
                               }"
                            )),
            NUM_PUB_EVACUATED = colDef(name = "Public Evacuated",
                                       html = T,
                                       align = "center",
                                       cell = function(value, index){
                                         valStyle = if_else(value > 0, 
                                                            "font-weight:600;",
                                                            "font-weight:300;")
                                         div(style = valStyle,
                                             value)
                                       },
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
                                               'color:black; font-size:1.1em; font-weight:600;'+
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
                galRel = galRel.toLocaleString('en-US').split('.')[0]
                mscfRel = mscfRel.toLocaleString('en-US').split('.')[0]
                return [galRel + ' Gal; '+ mscfRel + ' mscf']
              }
              else if(galRel >0 && mscfRel == 0){
                galRel = galRel.toLocaleString('en-US').split('.')[0]
                return [galRel + ' Gal']
              }
              else{
                mscfRel = mscfRel.toLocaleString('en-US').split('.')[0]
                return [mscfRel + ' mscf']
              }
              }"
            ),
                                   name = "Release Size",
                                   cell = function(value, index){
                                     unit <- iR$UNITS[index]
                                     valForm <- comma(value, accuracy = 1)
                                     div(
                                       div(style = "font-weight:500; font-size: 1em;",
                                           valForm),
                                       div(style = "font-weight:300; font-size:.85em;",
                                           unit)
                                     )
                                     
                                   }),
            IGNITE_IND = colDef(name = "Fire",
                                html = T,
                                align = "center",
                                cell = function(value, index){
                                  div(style = if_else(value == "YES",
                                                      "font-weight:600;",
                                                      "font:weight:300;"),
                                      value)
                                },
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
                                 cell = function(value, index){
                                   div(style = if_else(value == "YES",
                                                       "font-weight:600;",
                                                       "font:weight:300;"),
                                       value)
                                 },
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
            MDY = colDef(name = "Date",
                         style = function(value, index, name){
                           formDate(value)
                         }),
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
        mutate(igCol = colorScale(IGIN, pal = "YlOrRd", scale = "P"),
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
      iR <- filter(iR, NAME %in% iList$NAME)
      ## reactable starts here
      reactable(
        data = iR,
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
          inc = colDef(name = "Worst Count", 
                       defaultSortOrder = "desc",
                       show = F),
          STATE = colDef(aggregate = "unique",
                         name = "States"),
          SYS = colDef(aggregate = JS(
            "function(values, rows){
              let systems = []
              rows.forEach(function(row){
                if (!systems.includes(row['SYS'])){
                  systems.push(row['SYS'])
                }
              })
              
              const hlCol = '#ff7f00'
              const gdCol = '#6a3d9a'
              const gtCol = '#1f78b4'
              
              systems.forEach(function(sys, i){
                if (sys === 'HL'){
                  systems[i] = '<span style = \"color:#ff7f00;  \">' + sys + '</span>'
                }
                else if (sys === 'GD'){
                  systems[i] = '<span style = \"color:#6a3d9a; \">' +  sys + '</span>'
                }
                else {
                  systems[i] = '<span style = \"color:#1f78b4; \">' +  sys + '</span>'
                }
              })
              
              let styledList = '<div style = \"font-weight:600; font-size: 1.1em; \">' +
                               systems.join('<span style = \"color:#ffffff; \">, </span>') + '</div>'
              return styledList
              }"
          ),
          name = "System",
          html = T,
          style = function(value, index, name) {
            if (value == "HL") {
              list(fontWeight = 500, color = "#ff7f00")
            }
            else if (value == "GD") {
              list(fontWeight = 500, color = "#6a3d9a")
            }
            else {
              list(fontWeight = 500, color = "#1f78b4")
            }
          }),
          FATAL = colDef(name = "Deaths",
                         html = T,
                         align = "center",
                         cell = function(value, index){
                           valStyle = if_else(value > 0, 
                                              "font-weight:600;",
                                              "font-weight:300;")
                           div(style = valStyle,
                               value)
                         },
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
                                   'color:black; font-size:1.1em; font-weight:600;'+
                                   'transform: translate(-50%, -50%); \">' + fatal + '</div>'
                               
                             return '<div style=\"display: inline-flex; position: relative\">' + gradCircle + label + '</div>'
                             }"
                         )),
          INJURE = colDef(name = "Injuries",
                          html = T,
                          align = "center",
                          cell = function(value, index){
                            valStyle = if_else(value > 0, 
                                               "font-weight:600;",
                                               "font-weight:300;")
                            div(style = valStyle,
                                value)
                          },
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
                                     'color:black; font-size:1.1em;font-weight:600; '+
                                     'transform: translate(-50%, -50%); \">' + injure + '</div>'
                                 
                               return '<div style=\"display: inline-flex; position: relative\">' + gradCircle + label + '</div>'
                               }"
                          )),
          NUM_PUB_EVACUATED = colDef(name = "Public Evacuated",
                                     html = T,
                                     align = "center",
                                     cell = function(value, index){
                                       valStyle = if_else(value > 0, 
                                                          "font-weight:600;",
                                                          "font-weight:300;")
                                       div(style = valStyle,
                                           value)
                                     },
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
                                               'color:black; font-size:1.1em; font-weight:600;'+
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
                galRel = galRel.toLocaleString('en-US').split('.')[0]
                mscfRel = mscfRel.toLocaleString('en-US').split('.')[0]
                return [galRel + ' Gal; '+ mscfRel + ' mscf']
              }
              else if(galRel >0 && mscfRel == 0){
                galRel = galRel.toLocaleString('en-US').split('.')[0]
                return [galRel + ' Gal']
              }
              else{
                mscfRel = mscfRel.toLocaleString('en-US').split('.')[0]
                return [mscfRel + ' mscf']
              }
              }"
          ),
          name = "Release Size",
          cell = function(value, index){
            unit <- iR$UNITS[index]
            valForm <- comma(value, accuracy = 1)
            div(
              div(style = "font-weight:500; font-size: 1em;",
                  valForm),
              div(style = "font-weight:300; font-size:.85em;",
                  unit)
            )
            
          }),
          IGNITE_IND = colDef(name = "Fire",
                              html = T,
                              align = "center",
                              cell = function(value, index){
                                div(style = if_else(value == "YES",
                                                    "font-weight:600;",
                                                    "font:weight:300;"),
                                    value)
                              },
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
                               cell = function(value, index){
                                 div(style = if_else(value == "YES",
                                                     "font-weight:600;",
                                                     "font:weight:300;"),
                                     value)
                               },
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
          MDY = colDef(name = "Date",
                       style = function(value, index, name){
                         formDate(value)
                       }),
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
  })
  
  
  
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
                     class = "value-name",
                     style = "font-size: 3vw; white-space: pre-line;"
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
      subtitle = HTML( paste0("thousand standard cubic feet of <em>", 
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
      subtitle = HTML( paste0("thousand standard cubic feet of <em>", 
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
                       popup = paste0("<b>Operator:</b> ",
                                      mapData()$NAME,
                                      "<br>",
                                      "<b>Date:</b> ",
                                      mapData()$MDY,
                                      "<br>",
                                      "<b>System:</b> ",
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
        colors = c("#cab2d6","#6a3d9a","#a6cee3","#1f78b4","#fdbf6f","#ff7f00"),
        labels = c("Gas Distribution","Worst G.D.","Gas Transmission","Worst G.T.","Hazardous Liquid","Worst H.L."),
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
          mutate(SysMax = paste0(SYS, ifelse(Max, " Worst", "")),
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
          mutate(SysMax = paste0(SYS, ifelse(Max, " Worst", "")),
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
        mutate(SysMax = paste0(SYS, ifelse(Max, " Worst", "")),
               None = 1)
    }
  })


 #### create plots based on button behavior ####
  output$timePlot <- renderPlot({ 
    w$show()
    on.exit({
      w$hide()
    })
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
                 limits = c(0, max(df[[input$sizeButton]])),
                 labels = ifelse(input$sizeButton == "TOTAL_COST_CURRENT",
                                  scales::dollar_format(scale = .001, suffix = " K"),
                                  scales::comma_format())  )+
      scale_y_continuous(name = prettyweight(),
                         trans = if_else(input$logY == T, "pseudo_log","identity"),
                         breaks = yBreak(df[[input$weight]], input$logY,"b"),
                         labels = yBreak(df[[input$weight]], input$logY,"l"))+
      labs(title = paste0(if_else(input$periodSwitch,
                                  paste(year(ymd(input$thisMonth))),
                                  paste(month(ymd(input$thisMonth),
                                              label = T,
                                              abbr = F),
                                        year(ymd(input$thisMonth)))), " Pipeline Incidents"),
           subtitle = paste0(if_else( input$weight == "TOTAL_RELEASE",
                                      if_else(input$relSys, 
                                              "Hazardous Liquid Incidents",
                                              "Gas Distribution and Transmission Incidents"),
                                      "All Systems' Incidents"), 
                             " by ", weightName(input$weight, input$system)),
           color = "System")+
      theme_pst(font = pstFont)
  })

##TODO: update tooltip so if its in bottom half it goes up, and left side goes right
  output$hlTimePlot <- renderPlot({
    w$show()
    on.exit({
      w$hide()
    })
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
                 limits = c(0, max(df[[input$sizeButton]])),
                 labels = ifelse(input$sizeButton == "TOTAL_COST_CURRENT",
                                 scales::dollar_format(scale = .001, suffix = " K"),
                                 scales::comma_format()))+
      scale_y_continuous(name = prettyweight(),
                         trans = if_else(input$logY == T, "pseudo_log","identity"),
                         breaks = yBreak(filter(plotData(),grepl("HL", SYS))[[input$weight]],
                                         input$logY,"b"),
                         labels = yBreak(filter(plotData(),grepl("HL", SYS))[[input$weight]],
                                         input$logY,"l"))+
      labs(title = paste0(if_else(input$periodSwitch,
                                  paste(year(ymd(input$thisMonth))),
                                  paste(month(ymd(input$thisMonth),
                                              label = T,
                                              abbr = F),
                                        year(ymd(input$thisMonth)))), " Pipeline Incidents"),
           subtitle = paste0(if_else( input$weight == "TOTAL_RELEASE",
                                      if_else(input$relSys, 
                                              "Hazardous Liquid Incidents",
                                              "Gas Distribution and Transmission Incidents"),
                                      "All Systems' Incidents"), 
                             " by ", weightName(input$weight, input$system)),
           color = "System")+
      theme_pst(font = pstFont)
  })
  

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