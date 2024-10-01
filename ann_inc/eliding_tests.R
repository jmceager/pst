library(ggplot2)
library(dplyr)
library(fiftystater)
library(readr)
library(sf)
library(sp)
library(rgdal)
library(albersusa)
library(maptools)
library(tigris)
library(showtext)

pstFont = "Montserrat"
font_add_google(pstFont)
showtext_auto()
showtext_opts(dpi=300) 


epsg <- "ESRI:102003"
datadir = "https://raw.githubusercontent.com/jmceager/pst/main/phmsa-clean/data/"


df <- read_csv(paste0(datadir, "clean/all_inc.csv")) %>%
  filter(IYEAR == 2022)%>%
  mutate(y = LOCATION_LATITUDE, 
         x = LOCATION_LONGITUDE,) %>%
  st_as_sf(coords = c("x", "y"), crs = 4326)%>%
  st_transform(epsg) %>%
  mutate(x = sf::st_coordinates(.)[,1],
         y = sf::st_coordinates(.)[,2])

df_elid <- read_csv(paste0(datadir, "clean/all_inc.csv")) %>%
  filter(IYEAR == 2022)%>%
  mutate(y = LOCATION_LATITUDE, 
         x = LOCATION_LONGITUDE,) %>%
  st_as_sf(coords = c("x", "y"), crs = 4326)%>%
  st_transform(epsg) %>%
  mutate(x = sf::st_coordinates(.)[,1],
         y = sf::st_coordinates(.)[,2])


rotation = function(a){
  r = a * pi / 180 #degrees to radians
  matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2, ncol = 2)
} 

us_sf_norm <- states(cb = T) %>% st_transform(epsg)
us_sf_adj <- usa_sf("laea") 
us_adj <- usa_composite("laea")

n_ak <- us_sf_adj %>% filter(name == "Alaska")
n_hi <- us_sf_adj %>% filter(name == "Hawaii")

ak_n_bb <- st_bbox(us_sf_norm %>% filter(NAME == "Alaska"))
ak_a_bb <- st_bbox(
  (st_geometry(n_ak) - st_centroid(st_geometry(n_ak))) * 
    rotation(-50) + st_centroid(st_geometry(n_ak))
                   ) 

ak_x_nr <- ak_n_bb$xmax[[1]] - ak_n_bb$xmin[[1]] 
ak_y_nr <- ak_n_bb$ymax[[1]] - ak_n_bb$ymin[[1]] 

ak_x_ar <- ak_a_bb$xmax[[1]] - ak_a_bb$xmin[[1]] 
ak_y_ar <- ak_a_bb$ymax[[1]] - ak_a_bb$ymin[[1]] 

hi_n_bb <- st_bbox(us_sf_norm %>% filter(NAME == "Hawaii"))
hi_a_bb <- st_bbox(us_sf_adj %>% filter(name == "Hawaii"))

hi_x_nr <- hi_n_bb$xmax[[1]] - hi_n_bb$xmin[[1]] 
hi_y_nr <- hi_n_bb$ymax[[1]] - hi_n_bb$ymin[[1]] 

hi_x_ar <- hi_a_bb$xmax[[1]] - hi_a_bb$xmin[[1]] 
hi_y_ar <- hi_a_bb$ymax[[1]] - hi_a_bb$ymin[[1]] 

df_hi <- filter(df, STATE %in% c("HI")) %>%
  st_drop_geometry()%>%
  select(REPORT_NUMBER, x, y)%>%
  mutate(x_dist = (x - hi_n_bb$xmin[[1]])/hi_x_nr,
         y_dist = (y - hi_n_bb$ymin[[1]])/hi_y_nr,
         x_adj = hi_a_bb$xmin[[1]] + (x_dist*hi_x_ar),
         y_adj = hi_a_bb$ymin[[1]] + (y_dist*hi_y_ar)) %>%
  st_as_sf(coords = c("x_adj", "y_adj"), crs = epsg)

df_ak <- filter(df, STATE %in% c("AK")) %>%
  st_drop_geometry()%>%
  select(REPORT_NUMBER, x, y)%>%
  mutate(x_dist = (x - ak_n_bb$xmin[[1]])/ak_x_nr,
         y_dist = (y - ak_n_bb$ymin[[1]])/ak_y_nr,
         x_adj = (ak_a_bb$xmin[[1]] + (x_dist*ak_x_ar))-400000,
         y_adj = (ak_a_bb$ymin[[1]] + (y_dist*ak_y_ar))) %>%
  st_as_sf(coords = c("x_adj", "y_adj"), crs = epsg) 


df_ak_elide <- elide(df %>% 
                     filter(STATE == "AK") %>% 
                       st_transform("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs")%>% 
                       as_Spatial(), 
                     scale = max(apply(sp::bbox(as_Spatial(n_ak)), 1, diff))/2.3, rotate = -50, bb = sp::bbox(as_Spatial(n_ak)))
df_ak_elide_shf <- sp::elide(df_ak_elide, shift = c(657000, -3368809)) 
proj4string(df_ak_elide_shf) <- CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs")
df_ak_elide_sf <- st_as_sf(df_ak_elide_shf, coords = c("x", "y"))

df_hi_elide <- elide(df %>% 
                       filter(STATE == "HI") %>% 
                       st_transform("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs")%>% 
                       as_Spatial(), 
                     rotate = -35, bb = sp::bbox(as_Spatial(n_hi)))
df_hi_elide_shf <- sp::elide(df_hi_elide, shift = c(5200000, 1728000)) 
proj4string(df_hi_elide_shf) <- CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs")
df_hi_elide_sf <- st_as_sf(df_hi_elide_shf, coords = c("x", "y"))

df %>%
  filter(!STATE %in% c("HI", "AK"))%>%
  rbind(st_transform(df_ak_elide_sf, epsg),
        st_transform(df_hi_elide_sf, epsg))%>%
  write_rds("air_df.rds")

  
  
df4gif <-   df %>%
  mutate(yrwk = lubridate::week(MDY),
         yrday = lubridate::yday(MDY),
         molab = lubridate::month(MDY, label = T, abbr = F))

inc_day <- df4gif %>% 
  st_drop_geometry()%>%
  count(yrday)%>%
  mutate(cs = cumsum(n))

baseSize = 12
dfgif <- ggplot(df4gif)+
  layer_spatial(us_sf_adj,fill = "gray75", colour = "white")+
  layer_spatial(df4gif, aes(colour = yrwk),size = 2.5, alpha = .65, stroke = 0)+
   coord_sf(xlim = c(us_box$xmin[[1]], us_box$xmax[[1]]),
            ylim = c(us_box$ymin[[1]], us_box$ymax[[1]]))+
  # geom_text(aes(label = month(week(day(parse_date_time(current_frame, "j"))),label = T, abbr = F ),
  #               x = us_box$xmin[[1]]+4000, y = us_box$ymax[[1]] - 5000))+
  theme_void()+
  theme(
    text = element_text(family = pstFont),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    strip.text.x = element_blank(),
    strip.text.y = element_blank(),
    legend.text = element_text(colour = "black",
                               size = baseSize * .8),
    legend.background = element_rect(fill = NULL, 
                                     color = "#182125"),
    legend.title = element_text( face = "bold", 
                                 colour = "black",
                                 size = baseSize),
    legend.position = "none",
    #legend.background = element_blank(),
    plot.tag.position = c(.97,.97),
    plot.tag = element_text(size = baseSize *.66,
                            colour = "#8C9394"),
    plot.background = element_blank(),
    plot.title = element_text(face = "bold", 
                              colour = "black",
                              size = baseSize*1.8),
    plot.subtitle = element_text(colour = "#8C9394",
                                 size = baseSize),
    plot.caption = element_text(size = baseSize *.8)
  )+
  scale_color_viridis()+
  transition_manual(yrday, cumulative = TRUE)+
  enter_fade()+
  exit_fade()+
    labs(#title = "Day: {current_frame}",
      title = paste0("{month(parse_date_time(current_frame, \"j\"), label = T, abbr = F)}"),
         tag = "PST 2023",
         caption = "PHMSA 2023 Incident Flagged Files")

anim_save("www/outfile.gif", animate(dfgif,start_pause = 5, end_pause = 20, 
                                     fps = 5,res = 150, device = "tiff",
                                     width = 1200, height = 1200/1.62)) 






#### clustering 
library(factoextra)
library(cluster)

df_clu <- df_elid %>%
  st_drop_geometry()%>%
  group_by(OPERATOR_ID)%>%
  summarize(inc = n(),
            fatal = sum(FATAL, na.rm = T),
            injure = sum(INJURE, na.rm = T),
            cost = sum(TOTAL_COST_CURRENT, na.rm = T),
            sig = sum(SIGNIFICANT == "YES"),
            ser = sum(SERIOUS == "YES"))%>%
  left_join(
    all_miles %>%
      filter(IYEAR == 2022)%>%
      distinct(OPERATOR_ID,NAME, pri.id, STATE, IYEAR, SYS,.keep_all = T)%>%
      group_by(OPERATOR_ID,NAME, pri.id, pri.name)%>%
      summarise(miles = sum(mileage, na.rm = T)),
    by = "OPERATOR_ID"
  ) %>%
  distinct(OPERATOR_ID, .keep_all = T)%>%
  select(-pri.name, -pri.id, -NAME)%>%
  mutate(ipm = inc / miles / 1000) 
  scale()


set.seed(123)

fviz_nbclust(df_clu, kmeans, method = "wss")
  
