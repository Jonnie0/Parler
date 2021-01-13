library(tidyverse)
library(sf)
library(osmdata)
library(tmaptools)
library(ggtext)
library(showtext)

font_add_google('Oswald')
showtext_auto(enable = TRUE) 

data_parler <- read_csv('parler-videos-geocoded.csv')

sf_parler <- st_as_sf(data_parler, coords = c('Longitude', 'Latitude')) %>%
  mutate(st_sf(geometry, crs = 4326), Timestamp = as.POSIXct(Timestamp, tz="EST")) %>%
  filter(Timestamp >= as.POSIXct('2021-01-06 10:00:00', tz="EST") & Timestamp <= as.POSIXct('2021-01-06 20:00:00', tz="EST"))

bb <- bb_poly(c(xmin = -77.05, ymin = 38.88, xmax = -77.00, ymax = 38.901))

parler_dc <- st_crop(sf_parler, bb)


#c(-77.0537, 38.8811, -77.001, 38.9003)
# c(xmin, ymin, xmax, ymax)
streets <- opq(bb) %>%
  add_osm_feature(key = 'highway') %>%
  osmdata_sf() %>% trim_osmdata(bb)

theme_set(theme_void()) + theme_update(panel.background = element_rect(fill = "gray22", 
                                                                       color = NA),
                                       plot.background = element_rect(fill = "gray22", 
                                                                       color = NA),
                                       plot.title = element_markdown(family = 'Oswald', color = 'gray78', hjust = .5),
                                       plot.subtitle = element_markdown(family = 'Oswald', color = 'gray78', hjust = .5))

ggplot() + geom_sf(data = streets$osm_lines, color = "gray78") + 
  geom_sf(data = st_jitter(parler_dc), 
          mapping = aes(color = Timestamp), 
          stroke = 0, alpha = .7, show.legend = FALSE) + 
  scale_color_gradient(low = 'blue', high = 'red') +
  labs(title = "Tracking the Movement of the Capitol Mob",
       subtitle = "GPS metadata of videos uploaded to Parler from <b style = 'color:blue'>the start</b> to <b style = 'color:red'>the end</b> of January 6th")


ggsave('parler_map.png', dpi = 600)
