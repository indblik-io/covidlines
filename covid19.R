library(tidyverse)
library(sf)
library(covid19germany)
library(stars)
library(gganimate)
library(fasterize)
library(extrafont)
library(landscapetools)

# load font ----
extrafont::ttf_import("Overpass_Mono/")
extrafont::loadfonts(quiet = TRUE)

# load covid and spatial data ----
cov_df <- covid19germany::get_RKI_timeseries()

con <- url("https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_DEU_3_sf.rds")
germany_sf <- readRDS(con)

# data cleaning ----
## clean weird naming of county names in covid dataframe
cov_df$Landkreis <- str_sub(cov_df$Landkreis, 4) 

## make covid data spatial 
germany_cov <- left_join(germany_sf, cov_df, by = c("NAME_3" = "Landkreis"))

## group by date and county and summarise number of new confirmed cases
germany_cov_clean <- germany_cov %>%
  group_by(Date, NAME_3) %>%
  summarise(SUM = sum(NumberNewTestedIll))

# data transformation ---
spatial_data <- map_dfr(na.omit(unique(germany_cov_clean$Date)), function(id_date){
  
  # filter for specific date
  cov_intermed <- germany_cov_clean %>% 
    filter(Date == as.Date(id_date)) 
  
  # since we only have polygons where NewIll > 0, we need to join the "empty" ones
  cov_intermed <- suppressWarnings(st_join(germany_sf, cov_intermed))
  
  # standardize by area
  cov_intermed$SUM <- (cov_intermed$SUM / st_area(cov_intermed))
  
  # and since "empty" actually means 0, we are going to do this
  cov_intermed$SUM <- cov_intermed$SUM %>% replace_na(0)
  
  # turn the polygons to a raster and coerce this raster to a tibble
  cov_r <- raster(cov_intermed, res = 0.03320453)
  cov_intermed <- fasterize(cov_intermed, cov_r, field = "SUM", fun="sum") %>% 
    landscapetools::util_raster2tibble()
  
  # and get the naming right
  cov_intermed <- cov_intermed %>%
    rename(lng = x, lat =y, value = z) #%>% 
  
  cov_intermed$date <- as.Date(id_date)
  cov_intermed
  
})

# animate as gif ----
## there is something wrong with the data before the 08th of march
## apparently, two dates have the same numer of newly positve tested person
## ... which causes a weird error from gganimate, so we start with this date
spatial_data_df <- spatial_data %>% 
  filter(date > as.Date("2020-03-08"))

p1 <- ggplot(spatial_data_df, aes(lng, lat + 6*(value/max(value, na.rm=TRUE)))) +
  geom_line(size=0.35, alpha=0.6, color='#5A3E37', aes(group=lat), na.rm=TRUE) +
  ggthemes::theme_map(base_family = "Overpass Mono") +
  theme(plot.title = element_text(size = 16), 
        plot.subtitle = element_text(size = 12)) +
  labs(
    title = 'Day: {frame_time}',
    subtitle = "Shown is the number of new confirmed COVID-19 cases for each day since March, 2020.",
    caption = "Visualization by Marco Sciaini • Data by RKI"
  ) +
  transition_time(date) +
  shadow_wake(wake_length = 0.1, colour = '#5A3E37')

gganimate::animate(p1,
                   render = gifski_renderer(),
                   nframes = length(unique(spatial_data_df$date)),
                   width= 3458,
                   height= 3858,
                   duration = 22,
                   res = 300)

anim_save("covidlines.gif")


# plot a single day ----
id_date = "2020-10-05"

spatial_data %>% 
  filter(date == as.Date(id_date)) %>% 
  ggplot(aes(lng, lat + 25*(value/max(value, na.rm=TRUE)))) +
  geom_line(size=0.4, alpha=0.8, color='#EEBCB1FF', aes(group=lat), na.rm=TRUE) +
  ggthemes::theme_map(base_family = "Overpass Mono") +
  labs(
    title = paste("Day:", id_date), 
    subtitle = "Shown is the number of new confirmed COVID-19 as latitudinal density.",
    caption = "Visualization by Marco Sciaini • Data by RKI"
  ) +
  theme(plot.background = element_rect(fill = "#e6eaeb", "#e6eaeb")) 


ggsave("covidlines.png", width = 6.58, height = 8.58)

