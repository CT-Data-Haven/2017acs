library(tidyverse)
library(jsonlite)
library(cwi)
library(sf)
library(geojsonio)
library(rmapshaper)

json <- list.files(path = "./to_viz", pattern = "data_\\d+\\.json", full.names = T) %>%
  set_names(str_extract(., "\\w+(?=_data)")) %>%
  map(fromJSON) %>%
  map(as_tibble)

headings <- read_csv("./_utils/indicator_headings.txt") %>%
  rename(displayIndicator = display)

meta <- json[[1]] %>%
  distinct(topic, indicator, type, format, order, suborder) %>%
  rename(displayIndicator = indicator, displayTopic = topic) %>%
  mutate(topic = as_factor(displayTopic) %>%
           fct_relabel(str_to_lower) %>%
           fct_relabel(str_remove, " by age:") %>%
           fct_relabel(str_replace_all, "\\s", "_") %>%
           fct_relabel(str_replace, "children", "kids") %>%
           fct_recode(race = "race_and_ethnicity")) %>%
  inner_join(headings, by = c("topic", "displayIndicator")) %>%
  mutate(indicator = indicator %>%
           str_replace_all("\\s", "_") %>%
           str_replace("estimate", "num")) %>%
  select(order, suborder, topic, displayTopic, indicator, displayIndicator, type, format)

wide <- json %>%
  bind_rows(.id = "name") %>%
  select(city = name, town, name = neighborhood, geoType, topic, indicator, value) %>%
  rename(displayIndicator = indicator, displayTopic = topic) %>%
  left_join(meta, by = c("displayTopic", "displayIndicator")) %>%
  select(-starts_with("display"), -matches("order"), -type, -format) %>%
  mutate(indicator = as_factor(indicator)) %>%
  group_by(city, topic) %>%
  nest() %>%
  mutate(data = map(data, ~spread(., key = indicator, value = value))) %>%
  split(.$city) %>%
  map(select, -city)

write_json(wide, "./to_viz/nhood_data_wide.json")


# json %>%
#   bind_rows(.id = "name") %>%
#   select(name:value, town) %>%
#   rename(displayIndicator = indicator, displayTopic = topic) %>%
#   left_join(meta, by = c("displayTopic", "displayIndicator")) %>%
#   select(-starts_with("display"), -matches("order"), -type, -format) %>%
#   rename(city = name, name = neighborhood) %>%
#   group_by(city, topic, indicator) %>%
#   nest() %>%
#   write_json("./to_viz/nhood_data.json")

meta %>%
  arrange(topic, displayTopic) %>%
  # group_by(topic, displayTopic) %>%
  # nest() %>%
  write_json("./to_viz/nhood_meta.json")



# make topojson
bpt <- bridgeport_sf %>%
  ms_simplify(keep = 0.05, keep_shapes = T) %>%
  as_Spatial() %>%
  geojson::as.geojson() %>% 
  apply_mapshaper_commands(data = ., command = "-clean", force_FC = T, sys = T) %>%
  geojson_sf() %>%
  select(-rmapshaperid)

nhv <- new_haven_sf %>%
  ms_simplify(keep = 0.5, keep_shapes = T) %>%
  as_Spatial() %>%
  geojson::as.geojson() %>% 
  apply_mapshaper_commands(data = ., command = "-clean", force_FC = T, sys = T) %>%
  geojson_sf() %>%
  select(-rmapshaperid)

stam <- stamford_sf %>% 
  ms_simplify(keep = 0.25, keep_shapes = T)
  
geos <- list(
  bridgeport = bpt, 
  new_haven = nhv, 
  hartford = hartford_sf, 
  stamford = stam
)

geo_out <- geos %>% 
  map(st_transform, 4326) %>%
  imap(function(df, cty) {
    if ("town" %in% names(df)) {
      df
    } else {
      df %>% mutate(town = str_replace_all(cty, "_", " ") %>% str_to_title())
    }
  }) %>%
  map(arrange, town, name) %>%
  imap(~mutate(.x, city = .y)) %>%
  iwalk(~topojson_write(.x, geometry = "polygon", group = "city",
                        object_name = "city", 
                        file = sprintf("to_viz/%s_topo.json", .y)))

geo_out %>%
  imap(~mutate(.x, city = .y)) %>%
  reduce(rbind) %>%
  topojson_write(geometry = "polygon", group = "city", object_name = "cities",
                 file = "to_viz/topo_all.json")

# mapshaper -i combine-files to_viz/*_topo.json -o to_viz/topo_all.json
# system("mapshaper -i combine-files to_viz/*_topo.json -o to_viz/topo_all.json")


