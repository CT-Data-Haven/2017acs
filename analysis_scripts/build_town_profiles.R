library(tidyverse)
library(tidycensus)
library(camiller)
library(cwi)

################# VARIABLES ##################
acs_year <- 2017
short_yr <- str_sub(acs_year, -2)
##############################################

fetch <- readRDS(str_glue("./output/acs_basic_{acs_year}_fetch.rds")) %>%
  map(~filter(., str_detect(level, "(state|counties|regions|towns)"))) %>%
  map(label_acs, acs_year) %>%
  map(~mutate(., NAME = ifelse(str_detect(level, "tracts"), GEOID, NAME))) %>%
  map(janitor::clean_names) %>%
  map(group_by, level, name, year) %>%
  map(~replace_na(., list(moe = 0)))
out <- list()


# TOTAL POPULATION
out$total_pop <- fetch$total_pop %>%
  mutate(grp = "total_pop") %>%
  select(level, name, year, grp, estimate, moe)


# SEX & AGE
# population under 18, 65+, male, female
# age
out$age <- fetch$sex_by_age %>%
  separate(label, into = c("total", "sex", "age"), sep = "!!") %>%
  filter(!is.na(age) | (is.na(age) & is.na(sex))) %>%
  replace_na(list(age = "total_pop")) %>%
  add_grps(list(total_pop = 1, ages0_17 = 2:5, ages65plus = 19:24), group = age, moe = moe) %>%
  calc_shares(group = age, moe = moe) %>%
  rename(grp = age)

# sex
out$sex <- fetch$sex_by_age %>%
  separate(label, into = c("total", "sex", "age"), sep = "!!") %>%
  filter(is.na(age)) %>%
  replace_na(list(sex = "total_pop")) %>%
  select(level, name, year, sex, estimate, moe) %>%
  calc_shares(group = sex, moe = moe) %>%
  mutate(sex = str_to_lower(sex)) %>%
  rename(grp = sex)


# RACE / HISPANIC
# hispanic, white non-hispanic, black non-hispanic, other non-hispanic
out$race <- fetch$race %>%
  add_grps(list(total_pop = 1, hispanic = 12, white = 3, black = 4, other_race = 5:9), group = label, moe = moe) %>%
  calc_shares(group = label, moe = moe) %>%
  rename(grp = label)


# FOREIGN-BORN
out$foreign_born <- fetch$foreign_born %>%
  separate(label, into = c("total", "grp"), sep = "!!") %>% 
  replace_na(list(grp = "total_pop")) %>%
  add_grps(list(total_pop = 1, foreign_born = 5:6), group = grp, moe = moe) %>%
  calc_shares(group = grp, moe = moe)


# TENURE
# owner-occupied households
out$tenure <- fetch$tenure %>%
  add_grps(list(total_households = 1, owner_occupied = 2), group = label, moe = moe) %>%
  calc_shares(group = label, denom = "total_households", moe = moe) %>%
  rename(grp = label)


# HOUSING COST
# cost-burdened, not by tenure
out$housing_cost <- fetch$housing_cost %>%
  mutate(label = str_remove(label, "Total!!")) %>%
  separate(label, into = c("tenure", "income", "grp"), sep = "!!") %>% 
  filter(!is.na(grp) | tenure == "Total") %>%
  replace_na(list(income = "all", grp = "all")) %>%
  add_grps(list(total_households = 1, cost_burden = 4), group = grp, moe = moe) %>%
  calc_shares(group = grp, denom = "total_households", moe = moe)


# VEHICLES
# households with at least 1 car
out$vehicles <- fetch$vehicles %>%
  add_grps(list(total_households = 1, has_vehicle = 3:6), group = label, moe = moe) %>%
  calc_shares(group = label, denom = "total_households", moe = moe) %>%
  rename(grp = label)


# EDUCATIONAL ATTAINMENT
# ages 25+; share with less than high school, bachelors+
out$education <- fetch$education %>%
  add_grps(list(ages25plus = 1, less_than_high_school = 2, bachelors_plus = 5:6), group = label, moe = moe) %>%
  calc_shares(group = label, denom = "ages25plus", moe = moe) %>%
  rename(grp = label)


# MEDIAN HOUSEHOLD INCOME
# drop regions
out$median_household_income <- fetch$median_income %>%
  filter(!str_detect(level, "regions")) %>%
  mutate(grp = "median_household_income") %>%
  select(level, name, year, grp, estimate, moe)


# POVERTY & LOW-INCOME
# poverty determined; below 1x fpl, below 2x fpl
out$poverty <- fetch$poverty %>%
  add_grps(list(poverty_status_determined = 1, poverty = 2:3, low_income = 2:7), group = label, moe = moe) %>%
  calc_shares(group = label, denom = "poverty_status_determined", moe = moe) %>%
  rename(grp = label)


# POVERTY & LOW-INCOME BY AGE
# ages 0-17, ages 65+
out$pov_age <- fetch$pov_age %>%
  filter(str_detect(label, "!!")) %>%
  separate(label, into = c("total", "age", "ratio"), sep = "!!") %>%
  replace_na(list(ratio = "deter")) %>%
  mutate_at(vars(ratio, age), as_factor) %>%
  group_by(ratio, add = T) %>% 
  add_grps(list(ages0_17 = 1:3, ages65plus = 9:10), group = age, moe = moe) %>%
  group_by(level, name, year, age) %>% 
  add_grps(list(poverty_status_determined = 1, poverty = 2:4, low_income = 2:9), group = ratio, moe = moe) %>%
  calc_shares(group = ratio, denom = "poverty_status_determined", moe = moe) %>%
  unite("grp", age, ratio)


# BIND ALL TOGETHER
out_df <- out %>%
  bind_rows(.id = "indicator")

out_wide <- out_df %>%
  ungroup() %>%
  mutate(name = as_factor(name) %>% fct_relabel(~str_replace(., "County", "County, Connecticut"))) %>%
  mutate(level = fct_relevel(level, "1_state", "3_regions", "2_counties", "4_towns")) %>%
  arrange(level, name) %>% 
  mutate(name = fct_inorder(name)) %>%
  select(-indicator, -level, -year) %>% 
  distinct() %>%
  rename(group = grp) %>%
  make_wide(estimate:sharemoe, group = group) %>%
  mutate(name = as.character(name))

# need meta stuff
prof16 <- read_csv("https://raw.githubusercontent.com/CT-Data-Haven/WebsiteIndicators/master/TownProfiles/5year2016town_profile_expanded_CWS.csv", col_types = cols(.default = "c"))

has_digits <- function(x) all(str_detect(x, "^\\d"), na.rm = TRUE)

is_meta <- !map_lgl(prof16, has_digits)
meta_text <- prof16 %>% 
  select_if(~!has_digits(.)) %>%
  mutate(Source = str_replace(Source, "1, 3, and 5-year", "1 and 5-year"))
cws_df <- prof16 %>% select(1:2, 5:15)

prof_done <- cws_df %>%
  left_join(out_wide, by = c("Town" = "name")) %>%
  left_join(meta_text, by = c("Town", "County")) %>%
  select(Town:County, 108:109, 
         # cws indicators
         3:13,
         `Demographic, Total Population`,
         total_pop_estimate:female_sharemoe,
         `Race and Ethnicity, Total Population`,
         hispanic_estimate:other_race_sharemoe,
         `Place of Birth, Total Population`,
         foreign_born_estimate:foreign_born_sharemoe,
         Households,
         total_households_estimate:has_vehicle_sharemoe,
         `Educational Attainment, Population 25 years and over`,
         ages25plus_estimate:bachelors_plus_sharemoe,
         `Median Income`,
         median_household_income_estimate:median_household_income_moe,
         `Poverty and Low-Income, Total Population`,
         poverty_status_determined_estimate:low_income_sharemoe,
         `Poverty and Low-Income, Population 0 to 17 years`,
         ages0_17_poverty_status_determined_estimate:ages0_17_low_income_sharemoe,
         `Poverty and Low-Income, Population 65 years and over`,
         ages65plus_poverty_status_determined_estimate:ages65plus_low_income_sharemoe,
         Source:`Demographic Characteristics`
         ) %>%
  mutate_at(vars(matches("share")), function(x) ifelse(is.na(x), NA_character_, round(x * 100, digits = 1) %>% paste0("%"))) %>%
  mutate_at(vars(ends_with("Characteristics")), ~str_replace_all(., "16_5YR", str_glue("{short_yr}_5YR"))) %>%
  setNames(names(prof16))

write_csv(prof_done, str_glue("output/5year{acs_year}town_profile_expanded_CWS.csv"), na = "")

saveRDS(out_df, str_glue("output/acs_town_basic_profile_{acs_year}.rds"))
