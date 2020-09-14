## Set WD and Libraries
setwd("data/snapchat/json/")

library(tidyverse)
library(ggthemes)
library(rjson)
library(readr)
library(sf)
library(maps)
library(usmap)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)

## Location Data
location_history <- fromJSON(file = "location_history.json")

df <- purrr::map_df(location_history, bind_rows)

# Getting Lat/Long For Mapping
lat_long <- df %>% 
  select(`Latitude, Longitude`) %>%
  drop_na() %>%
  separate(`Latitude, Longitude`,
           into = c("lat","long"),
           sep = ",") %>%
  separate(lat, into = c("lat","x"),"±") %>%
  separate(long, into = c("long","y"),"±") %>%
  mutate(lat=trimws(lat),long=trimws(long),
         lat=as.numeric(lat),long=as.numeric(long)) %>%
  select(-x,-y) %>% 
  print

## Plotting My Location Data
mi <- map_data('state', region = 'Michigan')

ggplot() +
  geom_polygon(aes(long,lat,group=group),
               colour='black',fill='lightblue',mi) +
  geom_point(aes(long,lat),lat_long,
             colour='red', size = 1.5) +
  # Big Rapids, MI
  geom_point(aes(x=-85.4837,y=43.6981),alpha=0.5,size=5) +
  geom_text(aes(x=-85.4837,y=43.6981,label="Big Rapids",family="serif"),
            hjust=0, vjust=-1) +
  # Grand Rapids, MI
  geom_point(aes(x=-85.6681,y=42.9634),alpha=0.5,size=5) +
  geom_text(aes(x=-85.6681,y=42.9634,label="Grand Rapids",family="serif"),
            hjust=0, vjust=1.75) +
  # Rockford, MI
  geom_point(aes(x=-85.56,y=43.12),alpha=0.5,size=5) +
  geom_text(aes(x=-85.56,y=43.12,label="Rockford",family="serif"),
            hjust=0,vjust=-1) +
  # Ann Arbor, MI
  geom_point(aes(x=-83.743,y=42.2808),alpha=0.35,size=5) +
  geom_text(aes(x=-83.743,y=42.2808,label="Ann Arbor",family="serif"),
            hjust=0,vjust=-1) +
  # KZoo, MI
  geom_point(aes(x=-85.5872,y=42.2917),alpha=0.4,size=5) +
  geom_text(aes(x=-85.5872,y=42.2917,label="Kalamazoo",family="serif"),
            hjust=0,vjust=-1) +
  theme_fivethirtyeight() +
  labs(x="Longitude",y="Lattitude") +
  ggtitle("Snapchat Location History: Simon") +
  theme(plot.title = element_text(hjust = 0.5)) 

## Story History Data
story_history <- fromJSON(file = "story_history.json")

dat <- purrr::map_df(story_history, bind_rows)

## Plotting Story History
dat %>%
  filter(`Media Type` != "") %>% 
  ggplot() +
    geom_bar(aes(View),fill='magenta') +
    facet_wrap(~`Media Type`) +
    coord_flip() +
    theme_fivethirtyeight() +
    ggtitle("Story Views, Counted, By Username: Simon") +
    theme(plot.title = element_text(hjust = 0.5))

## Profile Data
profile_data <- fromJSON(file = "user_profile.json")

engage <- purrr::map_df(test[["Engagement"]], bind_rows)

## Plotting Profile Data
ggplot(engage, aes(Event,Occurrences)) +
  geom_col(fill='lightgreen') +
  coord_flip() +
  theme_fivethirtyeight() +
  labs(x="Event",y="Occurrences") +
  ggtitle("Profile Information: Simon") +
  theme(plot.title = element_text(hjust = 0.5))

test <- fromJSON(file = "friends.json")
purrr::map_df(test[["Friends"]], bind_rows) %>% view
test[["Deleted Friends"]]
  