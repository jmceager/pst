library(ggplot2)

#pst 
darkBlue <- "#003E59"
midBlue <- "#00546F"
bluGreen <- "#459197"
liteGreen <- "#61A893"

#pst contrast 
honey <- "#FFAD05"
lilac <- "#C297B8"
crimson <- "#96140D"
liteBlue <- "#5CCEFF"
brown <- "#664400"

pstMax <- c(darkBlue, liteGreen, lilac, honey, liteBlue, brown, crimson, "gray55")
pstSys <- c(honey, darkBlue, liteGreen, lilac, liteBlue)

theme_pst <- function(baseSize=10) {
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
    text = element_text(family = pstFont,
                        lineheight = 2),
    axis.text = element_text(colour = "#8C9394",
                             size = baseSize * .8),
    axis.title = element_text(colour ="black", 
                              size= baseSize),
    panel.grid.minor.y = element_line(linetype = "dotted", 
                                      colour = "#C4C8C6"),
    panel.grid.major.y = element_line(colour = "#394C56", 
                                      linetype = "dotted"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    strip.background = element_rect(linetype = 0),
    strip.text = element_text(size = baseSize*.66,
                              colour = "gray25"),
    strip.text.x = element_blank(),
    strip.text.y = element_blank(),
    legend.text = element_text(colour = "black",
                               size = baseSize * .8),
    legend.background = element_rect(fill = NULL, 
                                     color = "#182125"),
    legend.title = element_text( face = "bold", 
                                 colour = "black",
                                 size = baseSize*.66),
    legend.position = "right",
    legend.key = element_blank(),
    #legend.spacing.y = unit(.125, 'cm'),
    legend.margin = margin(5,5,5,5, "pt"),
    #legend.background = element_blank(),
    plot.tag.position = c(.95,.97),
    plot.tag = element_text(size = baseSize *.5,
                            colour = "#8C9394"),
    plot.background = element_rect(fill = "#ffffff"),
    plot.title = element_text(face = "bold", 
                              colour = "black",
                              size = baseSize*1.2),
    plot.subtitle = element_text(colour = "#8C9394",
                                 size = baseSize),
    plot.caption = element_text(size = baseSize *.5)
  )
  )
}


theme_pst_map <- function(baseSize=bs) {
  (theme_void() + 
     theme(
       legend.direction = "horizontal",
       legend.position = "bottom",
       plot.tag.position = c(.97,.97),
       text = element_text(family = pstFont),
       legend.text = element_text(colour = "black",
                                  size = baseSize * .66),
       legend.background = element_rect(fill = NA, 
                                        color = NA),
       legend.title = element_text( face = "bold", 
                                    colour = "black",
                                    size = baseSize*.66),
       legend.key = element_blank(),
       plot.background = element_rect(fill = "#ffffff", colour = NA),
       plot.margin = unit(c(0,0,0,0), "cm"),
       plot.tag = element_text(size = baseSize *.33,
                               colour = "#8C9394"),
       plot.title = element_text(face = "bold", 
                                 colour = "black",
                                 size = baseSize*1.2),
       plot.subtitle = element_text(colour = "#8C9394",
                                    size = baseSize),
       plot.caption = element_text(size = baseSize *.33,
                                   lineheight = .33)
     )
   
   
  )
}