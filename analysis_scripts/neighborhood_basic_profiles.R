library(tidyverse)
library(tidycensus)
library(camiller)
library(cwi)

################# VARIABLES ###############
acs_year <- 2017
###########################################

source("./_utils/weight_regions.R")

# will do new haven in separately, since it involves combining block groups & tracts depending on table
params <- list(
  bridgeport = list(
    region = "Fairfield County", town = "Bridgeport", df = bridgeport_tracts
  ),
  hartford = list(
    town = c("Hartford", "West Hartford"), df = hartford_tracts
  ),
  stamford = list(
    region = "Fairfield County", town = "Stamford", df = stamford_tracts
  )
)

filters <- c(
  "Connecticut",
  params %>% map(pluck, "town") %>% flatten_chr(),
  params %>% map(pluck, "region") %>% flatten_chr()
) %>% unique()

tracts_df <- params %>%
  map_dfr(pluck, "df", .id = "city") %>%
  mutate(town = coalesce(town, city) %>% str_to_title()) %>%
  select(-tract)

fetch_tract <- readRDS(str_glue("./output/acs_basic_{acs_year}_fetch_tracts.rds")) %>%
  map(~select(., -NAME, -moe)) %>%
  map(~rename(., geoid = GEOID)) %>%
  map(~weight_regions(., tracts_df, city, town, name, variable, join_cols = c("geoid"))) %>%
  map(label_acs, year = acs_year) %>%
  map(~mutate(., level = factor("5_tracts")))

fetch_town <- readRDS(str_glue("./output/acs_basic_{acs_year}_fetch.rds")) %>%
  map(~select(., level, name = NAME, variable, estimate)) %>%
  map(~filter(., name %in% filters)) %>%
  map(label_acs, year = acs_year)

fetch <- map2(fetch_town, fetch_tract, bind_rows) %>%
  map(~mutate(., level = as.factor(level))) %>%
  map(group_by, level, city, town, name)

out <- list()

# TOTAL POPULATION
# out$total_pop <- fetch$total_pop %>%
#   mutate(indicator = "total_pop") %>%
#   select(level, city, town, name, indicator, estimate)


# AGE
# population under 18, 65+
out$age <- fetch$sex_by_age %>%
  separate(label, into = c("total", "sex", "age"), sep = "!!") %>%
  filter(!is.na(age) | (is.na(age) & is.na(sex))) %>%
  replace_na(list(age = "total_pop")) %>%
  add_grps(list(total_pop = 1, ages0_17 = 2:5, ages65plus = 19:24), group = age) %>%
  calc_shares(group = age) %>%
  rename(indicator = age)


# RACE / HISPANIC
# hispanic, white non-hispanic, black non-hispanic, other non-hispanic
out$race <- fetch$race %>%
  add_grps(list(total_pop = 1, hispanic = 12, white = 3, black = 4, other_race = 5:9), group = label) %>%
  calc_shares(group = label) %>%
  rename(indicator = label)


# FOREIGN-BORN
out$foreign_born <- fetch$foreign_born %>%
  separate(label, into = c("total", "indicator"), sep = "!!") %>% 
  replace_na(list(indicator = "total_pop")) %>%
  add_grps(list(total_pop = 1, foreign_born = 5:6), group = indicator) %>%
  calc_shares(group = indicator)


# TENURE
# owner-occupied households
out$tenure <- fetch$tenure %>%
  add_grps(list(total_households = 1, owner_occupied = 2), group = label) %>%
  calc_shares(group = label, denom = "total_households") %>%
  rename(indicator = label)


# HOUSING COST
# cost-burdened, not by tenure
out$housing_cost <- fetch$housing_cost %>%
  mutate(label = str_remove(label, "Total!!")) %>%
  separate(label, into = c("tenure", "income", "indicator"), sep = "!!") %>% 
  filter(!is.na(indicator) | tenure == "Total") %>%
  replace_na(list(income = "all", indicator = "all")) %>%
  add_grps(list(total_households = 1, cost_burden = 4), group = indicator) %>%
  calc_shares(group = indicator, denom = "total_households") %>%
  filter(indicator != "total_households")


# POVERTY & LOW-INCOME
# poverty determined; below 1x fpl, below 2x fpl
out$poverty <- fetch$poverty %>%
  add_grps(list(poverty_status_determined = 1, poverty = 2:3, low_income = 2:7), group = label) %>%
  calc_shares(group = label, denom = "poverty_status_determined") %>%
  rename(indicator = label)


# POVERTY & LOW-INCOME BY AGE
# ages 0-17, ages 65+
pov_age <- fetch$pov_age %>%
  filter(str_detect(label, "!!")) %>%
  separate(label, into = c("total", "age", "ratio"), sep = "!!") %>%
  replace_na(list(ratio = "deter")) %>%
  mutate_at(vars(ratio, age), as_factor) %>%
  group_by(ratio, add = T) %>% 
  add_grps(list(ages0_17 = 1:3, ages65plus = 9:10), group = age) %>%
  group_by(level, city, town, name, age) %>% 
  add_grps(list(poverty_status_determined = 1, poverty = 2:4, low_income = 2:9), group = ratio) %>%
  calc_shares(group = ratio, denom = "poverty_status_determined") %>%
  unite("indicator", age, ratio, sep = "_", remove = F) %>%
  ungroup() %>%
  split(.$age)

out$income_kids <- pov_age$ages0_17 %>%
  select(-age, -ratio)
out$income_seniors <- pov_age$ages65plus %>%
  select(-age, -ratio)

indicators <- read_csv("./_utils/indicator_headings.txt") %>%
  mutate(topic = as_factor(topic))

# BIND ALL TOGETHER
out_df <- out %>%
  bind_rows(.id = "topic") %>%
  ungroup() %>%
  mutate(topic = as_factor(topic) %>%
           fct_collapse(households = c("tenure", "housing_cost")) %>%
           fct_recode(income = "poverty", immigration = "foreign_born")) %>%
  mutate_at(vars(topic, indicator), as_factor) %>%
  gather(key = type, value = value, estimate, share) %>%
  filter(!is.na(value)) %>%
  arrange(level, city, town, topic, indicator, type) %>%
  group_by(name) %>%
# want numbers indicating the order for reading in javascript--order indicators overall, and indicators within topics
  mutate(order = row_number()) %>%
  group_by(topic, add = T) %>%
  mutate(suborder = row_number()) %>%
  mutate(indicator = paste(type, indicator)) %>%
  mutate(type = fct_recode(type, table = "estimate", map = "share")) %>%
  mutate(format = fct_recode(type, "," = "table", ".0%" = "map")) %>%
# clean indicator display names
  left_join(indicators, by = c("indicator", "topic")) %>%
  select(-indicator) %>%
  rename(indicator = display) %>%
  mutate(displayTopic = topic %>%
           fct_relabel(~str_replace(., "^income_", "income by age: ") %>% str_replace("kids", "children")) %>%
           fct_relabel(str_replace_all, "_", " ") %>%
           fct_relabel(cap_first) %>%
           fct_recode("Race and ethnicity" = "Race")) %>%
  mutate(level = level %>%
           fct_relabel(str_remove_all, "\\d_") %>%
           fct_recode("1_neighborhood" = "tracts", "2_city" = "towns", "3_region" = "counties", "4_state" = "state")) %>%
  select(city, town, neighborhood = name, geoType = level, topic, displayTopic, indicator, value, type, format, order, suborder) %>%
  ungroup()

# split by city, then tack on state, region, and town
out_split <- out_df %>% split(.$city)




# BRIDGEPORT
bridgeport <- bind_rows(
  out_df %>% filter(neighborhood %in% c("Connecticut", params$bridgeport$town, params$bridgeport$region)),
  out_split$bridgeport
) %>%
  select(-city, -town)

# STAMFORD
stamford <- bind_rows(
  out_df %>% filter(neighborhood %in% c("Connecticut", params$stamford$town, params$stamford$region)),
  out_split$stamford
) %>%
  select(-city, -town)

# HARTFORD
hartford <- bind_rows(
  out_df %>% filter(neighborhood %in% c("Connecticut", params$hartford$town, params$hartford$region)),
  out_split$hartford
) %>% 
  select(-city)

# WRITE JSON FILES FOR WEB
prof_list <- lst(bridgeport, stamford, hartford) %>%
  map(~select(., -topic) %>% rename(topic = displayTopic)) %>%
  imap(~mutate(.x, city = str_to_title(.y))) %>%
  iwalk(~jsonlite::write_json(.x, str_glue("./to_viz/{.y}_data_{acs_year}.json")))

prof_wide <- prof_list %>%
  map(function(df) {
    df %>%
      select(-topic, -city, -type:-suborder) %>%
      rename(name = neighborhood) %>%
      arrange(geoType, name) %>%
      mutate_at(vars(name, indicator), as_factor) %>%
      distinct(name, indicator, .keep_all = T) %>%
      group_by(indicator) %>%
      mutate(row = row_number()) %>%
      spread(key = indicator, value = value) %>%
      select(-row) %>%
      select(geoType, name, everything())
  }) %>%
  iwalk(~write_csv(.x, str_glue("./to_distribute/{.y}_acs_basic_neighborhood_{acs_year}.csv"), na = ""))
