library(tidyverse)
library(tidycensus)
library(camiller)
library(cwi)

################# VARIABLES ###############
acs_year <- 2017
###########################################

source("./_utils/weight_regions.R")

# new haven uses a combo of block groups and tracts--when possible, use block groups
bgrp_nums <- c("sex_by_age", "race", "tenure", "poverty")
tract_nums <- c("foreign_born", "housing_cost", "pov_age")
table_names <- c(bgrp_nums, tract_nums)

filters <- c("Connecticut", "New Haven")

bgrps_df <- nhv_bgrps %>%
  select(name, geoid, weight)

tracts_df <- nhv_tracts %>%
  select(name, geoid, weight)

fetch_bgrp <- readRDS(str_glue("./output/acs_basic_{acs_year}_fetch_bgrps.rds")) %>%
  `[`(bgrp_nums) %>%
  map(~select(., -NAME, -moe)) %>%
  map(janitor::clean_names) %>%
  map(~weight_regions(., bgrps_df, name, variable, join_cols = c("geoid"))) %>%
  map(label_acs, year = acs_year) %>%
  map(~mutate(., level = factor("5_neighborhoods")))

fetch_tract <- readRDS(str_glue("./output/acs_basic_{acs_year}_fetch_tracts.rds")) %>%
  `[`(tract_nums) %>%
  map(~select(., -NAME, -moe)) %>%
  map(janitor::clean_names) %>%
  map(~weight_regions(., tracts_df, name, variable, join_cols = c("geoid"))) %>%
  map(label_acs, year = acs_year) %>%
  map(~mutate(., level = factor("5_neighborhoods")))

fetch_town <- readRDS(str_glue("./output/acs_basic_{acs_year}_fetch.rds")) %>%
  map(~select(., level, name = NAME, variable, estimate)) %>%
  map(~filter(., name %in% filters)) %>%
  map(label_acs, year = acs_year)

fetch_region <- readRDS(str_glue("./output/acs_basic_{acs_year}_fetch.rds")) %>%
  map(~select(., level, name = NAME, variable, estimate)) %>%
  map(~filter(., name %in% cwi::regions$`Greater New Haven`)) %>%
  map(~group_by(., level = factor("3_regions"), name = "Greater New Haven", variable)) %>%
  map(~summarise(., estimate = sum(estimate))) %>%
  map(label_acs, year = acs_year)

fetch_nhoods <- c(fetch_bgrp, fetch_tract)

fetch <- map(list(fetch_town, fetch_region, fetch_nhoods), magrittr::extract, table_names) %>%
  pmap(bind_rows) %>%
  map(~mutate(., level = as.factor(level))) %>%
  map(~mutate(., city = "New Haven")) %>%
  map(group_by, level, city, name)

out <- list()

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
  group_by(level, city, name, age) %>% 
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
  arrange(level, city, topic, indicator, type) %>%
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
           fct_recode("1_neighborhood" = "neighborhoods", "2_city" = "towns", "3_region" = "regions", "4_state" = "state")) %>%
  select(city, neighborhood = name, geoType = level, topic, displayTopic, indicator, value, type, format, order, suborder) %>%
  ungroup() %>%
  select(-topic) %>%
  rename(topic = displayTopic)

jsonlite::write_json(out_df, str_glue("./to_viz/new_haven_data_{acs_year}.json"))


prof_wide <- out_df %>%
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

write_csv(prof_wide, str_glue("./to_distribute/new_haven_acs_basic_neighborhood_{acs_year}.csv"), na = "")
