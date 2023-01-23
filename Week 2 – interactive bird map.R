library(tidytuesdayR)
library(tidyverse)
library(leaflet)
library(rsconnect)
library(shiny)
library(modeldata)
library(DataExplorer)
library(plotly)
library(viridisLite)
library(viridis)

feederwatch <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-10/PFW_2021_public.csv')
site_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-10/PFW_count_site_data_public_2021.csv')

site_df <- site_data %>% 
  select(
    loc_id,
    hab_water_fresh,
    hab_water_salt,
    nearby_feeders,
    bird_baths_atleast,
  )

feeder_df <- feederwatch %>% 
  select(
    loc_id,
    subnational1_code,
    species_code, 
    how_many,
    latitude,
    longitude) %>% 
  filter(how_many >= 100, 
         subnational1_code != 'US-AK', 
         subnational1_code != 'CA-QC', 
         subnational1_code != 'CA-ON', 
         subnational1_code != 'CA-BC', 
         subnational1_code != 'CA-AB', 
         subnational1_code != 'CA-MB', 
         subnational1_code != 'CA-NB') 

map_df <- merge(feeder_df, site_df, 
                by = "loc_id")

map_df <- map_df %>% #change names of all the species. 
  mutate(species_code = str_replace(species_code, "eursta", "European Starling"), 
         species_code = str_replace(species_code, "doccor", "Double-crested Cormorant"), 
         species_code = str_replace(species_code, "bbwduc", "Black-bellied Whistling-Duck"), 
         species_code = str_replace(species_code, "pinsis", "Pine Siskin (Northern)"), 
         species_code = str_replace(species_code, "cedwax", "Cedar Waxwing"), 
         species_code = str_replace(species_code, "rewbla", "Red-winged Blackbird"), 
         species_code = str_replace(species_code, "rosfin", "rosy-finch sp"), 
         species_code = str_replace(species_code, "eutspa", "Eurasian Tree Sparrow"), 
         species_code = str_replace(species_code, "gcrfin", "Gray-crowned Rosy-Finch"), 
         species_code = str_replace(species_code, "cangoo", "Canada Goose"), 
         species_code = str_replace(species_code, "brebla", "GBrewer's Blackbird"), 
         species_code = str_replace(species_code, "amecro", "American Crow"), 
         species_code = str_replace(species_code, "houspa", "House Sparrow"), 
         species_code = str_replace(species_code, "lessca", "Lesser Scaup"), 
         species_code = str_replace(species_code, "ribgul", "Ring-billed Gull"), 
         species_code = str_replace(species_code, "bnhcow", "Brown-headed Cowbird"),
         species_code = str_replace(species_code, "comred", "Common Redpoll"), 
         species_code = str_replace(species_code, "comgra", "Common Grackle"), 
         species_code = str_replace(species_code, "evegro", "Evening Grosbeak"), 
         species_code = str_replace(species_code, "calqua", "California Quail"))

map_df <- map_df %>% 
  mutate(popup_info = paste("Species:", species_code, "<br/>", 
                            "Number of birds:", how_many, "<br/>", 
                            "Location ID:", loc_id, "<br/>",
                            "Nearby Feeders:", nearby_feeders, "<br/>",
                            "Bird Baths:", bird_baths_atleast))
pal <- colorBin("viridis", map_df$how_many, 3, pretty = FALSE)
map <- leaflet(map_df) %>% 
  addTiles() %>% addProviderTiles(providers$CartoDB.Positron) %>% 
  addCircleMarkers(lng = ~ longitude,
                   lat = ~ latitude,
                   radius = 5,
                   popup = ~ popup_info,
                   color = ~ pal(how_many))  
map
