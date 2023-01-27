library(tidyverse)
library(leaflet)
library(viridis)

survivalists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/survivalists.csv')
loadouts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/loadouts.csv')
episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/episodes.csv')
seasons <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/seasons.csv')

colnames(episodes)
colnames(loadouts)
colnames(seasons)
colnames(survivalists)

combined <- merge(survivalists, loadouts, by = "name")
combined_map <- merge(seasons, survivalists, by = "season")

# ----
# map
map_df <- seasons %>% 
  select(
    season,
    location,
    lat,
    lon
  )

pal <- colorBin("viridis", map_df$season, 3, pretty = FALSE)

map <- leaflet(map_df) %>% 
                addTiles() %>% addProviderTiles(providers$CartoDB.Positron) %>% 
                addCircleMarkers(lng = ~ lon,
                                 lat = ~ lat,
                                 radius = 5)
map

# ----
# Graphing survival days

survival_days <- survivalists %>% 
  select(
    season,
    name,
    days_lasted
  ) %>% 
  group_by(season)


ggplot(survivalists) +
  geom_col(aes(x = season, y = days_lasted, fill = season)) +
  theme(
    axis.ticks.x = element_blank(),
    axis.text.x=element_blank()
  )


