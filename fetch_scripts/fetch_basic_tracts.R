library(tidyverse)
library(tidycensus)
library(camiller)
library(cwi)

##################### VARIABLES ############
year <- 2017
############################################

# this gets all the data for neighborhood profiles for New Haven, Hartford/West Hartford, Bridgeport, and Stamford. 
# Hierarchy:
# State, Greater New Haven, Fairfield County, cities, all tracts in cities
# separately, do all block groups in New Haven for tables where available
regions_short <- cwi::regions[c("Greater New Haven")]
cities <- c("New Haven", "Bridgeport", "Hartford", "West Hartford", "Stamford")
counties <- fips_codes %>% 
  filter(state == "CT", str_detect(county, "(New Haven|Hartford|Fairfield)")) %>%
  pull(county_code)

# API is still crashing

# fetch <- cwi::basic_table_nums[2] %>%
#   map(~multi_geo_acs(table = ., year = year,
#                      towns = cities,
#                      regions = regions_short,
#                      counties = "all",
#                      tracts = "all")
#       )



cwi::basic_table_nums[1:9] %>%
  iwalk(function(tbl, nm) {
    f <- map_dfr(counties, function(cty) {
      httr::with_verbose(
        out <- get_acs("tract", table = tbl, year = year, state = "09", county = cty)
      )
      Sys.sleep(5)
      return(out)
    })
    saveRDS(f, str_glue("./output/fetch_tracts/{nm}_{year}_fetch_tracts.rds"))
  })

fetch <- list.files(path = "./output/fetch_tracts", pattern = "tracts\\.rds$", full.names = T) %>%
  map(function(long_path) {
    nm <- str_extract(long_path, "(?<=/)\\w+(?=_\\d)")
    setNames(long_path, nm)
  }) %>%
  flatten() %>%
  imap(~readRDS(.x))

saveRDS(fetch, str_glue("./output/acs_basic_{year}_fetch_tracts.rds"))

############ block groups, only New Haven County

cwi::basic_table_nums %>%
  iwalk(function(tbl, nm) {
    f <- map_dfr("New Haven", function(cty) {
      httr::with_verbose(
        out <- get_acs("block group", table = tbl, year = year, state = "09", county = cty)
      )
      Sys.sleep(5)
      return(out)
    })
    saveRDS(f, str_glue("./output/fetch_tracts/{nm}_{year}_fetch_bgrps.rds"))
  })

fetch_bg <- list.files(path = "./output/fetch_tracts", pattern = "bgrps\\.rds$", full.names = T) %>%
  map(function(long_path) {
    nm <- str_extract(long_path, "(?<=/)\\w+(?=_\\d)")
    setNames(long_path, nm)
  }) %>%
  flatten() %>%
  imap(~readRDS(.x))

saveRDS(fetch_bg, str_glue("./output/acs_basic_{year}_fetch_bgrps.rds"))
