library(tidyverse)
library(tidycensus)
library(camiller)
library(cwi)

##################### VARIABLES ############
year <- 2017
############################################

regions_short <- cwi::regions[c("Greater New Haven", "Greater Waterbury", "Greater Bridgeport", "Lower Naugatuck Valley")]

# had to rewrite this bc Census servers kept crashing

# fetch <- cwi::basic_table_nums %>%
  # map(~multi_geo_acs(table = ., year = year, towns = "all", regions = regions_short))

cwi::basic_table_nums[10:11] %>%
  iwalk(~multi_geo_acs(table = .x, year = year, towns = "all", regions = regions_short) %>%
          saveRDS(str_glue("./output/fetch/{.y}_{year}_fetch.rds")))

BRRR::skrrrahh()

# saveRDS(fetch, str_glue("./output/acs_basic_{year}_fetch.rds"))

fetch <- list.files(path = "./output/fetch", pattern = "\\.rds$", full.names = T) %>%
  map(function(long_path) {
    nm <- str_extract(long_path, "(?<=/)\\w+(?=_\\d)")
    setNames(long_path, nm)
  }) %>%
  flatten() %>%
  imap(~readRDS(.x))

saveRDS(fetch, str_glue("./output/acs_basic_{year}_fetch.rds"))
