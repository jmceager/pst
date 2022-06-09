library(tidyverse)
library(htmltools)
library(htmlwidgets)
library(scales)
library(ggthemes)
library(showtext)
library(viridis)
#> Loading required package: sysfonts
#> Loading required package: showtextdb
#### functions ####
#fun little html function
html <- function(x, inline = FALSE) {
  container <- if (inline) htmltools::span else htmltools::div
  container(dangerouslySetInnerHTML = list("__html" = x))
}

#date issues: want mm-yyyy
dateInput2 <- function(inputId, label, minview = "days", maxview = "decades", ...) {
  d <- shiny::dateInput(inputId, label, ...)
  d$children[[2L]]$attribs[["data-date-min-view-mode"]] <- minview
  d$children[[2L]]$attribs[["data-date-max-view-mode"]] <- maxview
  d
}

#for maps, want range of values to be the same for all possible variables to make sizing easier 
rangeBrother <- function(x){
  if(max(x) == 0){
    3
  }
  else{
    (10^((x-min(x))/(max(x)-min(x))))*3
  }
}

#custom legend builder
#sheesh cleaned that up a lot
addLegendCustom <- function(map, weight, weightName, sys, legName,units, opacity = 0.7){
  n <- length(pretty(weight, n = breaksizer(weight)))
  col <- if_else(sys == "HL", "#fdbf6f",
                 if_else(sys == "GD", "#cab2d6",
                         if_else(sys == "GT", "#a6cee3",
                                 "#b2b2b2")
                         )
                 )
  title <- if_else(weightName == "TOTAL_RELEASE", paste0("Commodity Released (", units, ")"),
                   if_else(weightName == "TOTAL_COST_CURRENT", "Cost (2022 USD)",
                           if_else(weightName == "FATAL", "Deaths",
                                   "Deaths + Injuries"))
                   )
  prettyNumb <-pretty(weight, n = breaksizer(weight))
  colors <- rep(col, n)
  sizes <- rangeBrother(prettyNumb)
  colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, "px;","border-radius:50%;" )
  labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", comma(prettyNumb), "</div>")
  return(addLegend(map, 
                   title = title,
                   colors = colorAdditions, 
                   labels = labelAdditions, 
                   opacity = opacity, 
                   layerId = legName))
  
}

# well i simplified this more than i expected
breaksizer <- function(x){
  nq <- length(unique(x))
  nb <- if_else(nq <= 3, nq,as.integer(4))
  nb
}

## naming weights to be nicer 
weightName <- function(weight, system = "all"){
  unit <- if_else(system == "HL", "(BBL)","(mscf)")
  if(weight == "TOTAL_COST_CURRENT"){
    "Cost of Damage (Current USD)"
  }
  else if(weight == "TOTAL_RELEASE"){
    paste("Total Release Size", unit)
  }
  else if(weight == "FATAL"){
    "Deaths"
  }
  else if(weight == "humans"){
    "Deaths + Injuries"
  }
  else{
    "Error"
  }
}

#string split text wrap function 
#update to use a certain number of characters per line 
# ie if connected words + word <= x char, add word, else add /n
#maybe update so the 18 and 16 values are set by the function user 
wrapping <- function(text) {
  #split name into individual words
  name <- str_split(text, pattern = " ")[[1]]
  #create empty new name string
  newname <- ""
  newline <- ""
  i = 1
  while (i-1 < length(name) & nchar(newline) <= 18) {
    if(nchar(paste(newline, name[i]))>= 16){
      newline <- paste0(newline, " ","\n", name[i])
      newname <- paste0(newname, newline)
      newline <- ""
      i = i+1
    }
    else{
      newline <- paste0(newline, if_else(i == 1,""," "), name[i])
      i = i+1
    }
  }
  paste0(newname, " ", last(name))
}

## first and last months
dayRange <- function(date, period =c("m","y")) {
  if(period == "y"){
    start <- dmy(paste0("01-01-", year(date)))
    end <- dmy(paste0("31-12-", year(date))) -1
  }
  else{
    mo <- month(date)
    yr <- year(date)
    end <- ymd(paste(yr + (mo==12),c(1:12, 1)[mo+1],1,  sep="-")) - 1
    start <- date
  }
  c(start, end)
}

## color scaler for Repeat table
#palette can be any viridis palette a through h
colorScale <- function(x, pal, dir = -1, end = 1, begin = 0, scale = c("P","N")){
  #pick viridis or brewer gradient based on input
  if(pal %in% LETTERS[seq(1,8)]){
    cPal <- viridis(n = 101, option = pal, dir = dir, end = end, begin = begin)
  }
  else{
    cPal <- gradient_n_pal(brewer_pal(palette = pal)(5))(seq(0, 1, length.out = 101))
  }
  #scale x
  if(scale == "P"){
    if(max(x) <= 1){
      xScale <- round( x*100)
    }
    else{
      xScale <- round(x)
    }
  }
  else if(scale == "N"){
    xScale <- round(((x - min(x)) / (max(x)-min(x)))*100)
  }
  # get the colors 
  xCol <- cPal[xScale+1]
  return(xCol)
}

## y breaks and labs 
yBreak <- function(data, log=c(T,F), output = c("b", "l")){
  yMax <- max(data, na.rm = T)
  #setting the initial breaks
  if(log){
    lB <- log_breaks()(data[data >0])
    if(yMax < 1000){
      yNum <- c(0, lB[lB>=1])
    }
    else{
      yNum <- c(0, lB[lB>1])
    }
  }
  else{
    yNum <- pretty(data)
  }
  # cleaning the breaks 
  if(output == "b"){
    if( yMax < 3){
      c(0,1,2)
    }
    else if(yMax < 10){
      yNum[ abs(yNum-round(yNum) ) < 0.00000001 ]
    }
    else{
      yNum
    }
  }
  else{
    if(yMax < 3){
      yChr <- c(0,1,2)
    }
    else if(between(yMax,3,999)){
      yChr <- yNum[ abs(yNum-round(yNum) ) < 0.00000001 ]
    }
    else if(between(yMax,1000,999999)){
      yChr <- as.character(yNum)
      for(i in 1:length(yNum)){
        if(yNum[i] >=1000){
          yR <- round(yNum[i]/1000, 1)
          yChr[i] <- paste0(yR, "K")
        }
      }
    }
    else{
      yChr <- as.character(yNum)
      for(i in 1:length(yNum)){
        if(between(yNum[i], 1000, 999999)){
          yR <- round(yNum[i]/1000, 1)
          yChr[i] <- paste0(yR, "K")
        }
        else if(between(yNum[i],1000000,999999999.99)){
          yR <- round(yNum[i]/1000000, 1)
          yChr[i] <- paste0(yR, "M")
        }
        else if(yNum[i] >= 1000000000){
          yR <- round(yNum[i]/1000000000, 1)
          yChr[i] <- paste0(yR, "B")
        }
      }
    }
    yChr
  }
}

# theme clean adjustments to apply to ggplot
theme_pst <- function(font = "Arial") {
  (theme(
                       axis.line.x = element_line(
                         colour = "#182125",
                         size = 0.5,
                         linetype = "solid"
                       ),
                       axis.line.y = element_line(
                         colour = "#182125",
                         size = 0.5,
                         linetype = "solid"
                       ),
                       axis.ticks.y = element_line(
                         colour = "#182125",
                         size = 0.5,
                         linetype = "solid"
                       ),
                       text = element_text(family = font),
                       axis.text = element_text(colour = "#8C9394"),
                       axis.title = element_text(colour ="#C4C8C6"),
                       panel.grid.minor.y = element_line(linetype = "dotted", colour = "#29363D"),
                       panel.grid.major.y = element_line(colour = "#394C56", linetype = "dotted"),
                       panel.grid.major.x = element_blank(),
                       panel.grid.minor.x = element_blank(),
                       panel.background = element_blank(),
                       panel.border = element_blank(),
                       strip.background = element_rect(linetype = 0),
                       strip.text = element_blank(),
                       strip.text.x = element_blank(),
                       strip.text.y = element_blank(),
                       legend.text = element_text(colour = "#C4C8C6"),
                       legend.background = element_rect(fill = "#233239", color = "#182125"),
                       legend.title = element_text( face = "bold", colour = "#C4C8C6"),
                       legend.position = "right",
                       legend.key = element_blank(),
                       legend.margin = margin(4,12,4,6, "pt"),
                       #legend.background = element_blank(),
                       plot.background = element_rect(fill = "#233239"),
                       plot.title = element_text(face = "bold", colour = "#C4C8C6"),
                       plot.subtitle = element_text(colour = "#C4C8C6")
                     )
  )
}

