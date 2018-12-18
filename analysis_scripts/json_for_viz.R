library(tidyverse)
library(jsonlite)

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

json %>%
  bind_rows(.id = "name") %>%
  select(name:value, town) %>%
  rename(displayIndicator = indicator, displayTopic = topic) %>%
  left_join(meta, by = c("displayTopic", "displayIndicator")) %>%
  select(-starts_with("display"), -matches("order"), -type, -format) %>%
  rename(city = name, name = neighborhood) %>%
  group_by(city, topic, indicator) %>%
  nest() %>%
  write_json("./to_viz/nhood_data.json")

write_json(meta, "./to_viz/nhood_meta.json")