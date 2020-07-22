library(tidyverse)

pop <- read_rds(here::here("data/FL-population-2020.rds"))
counties <- read_rds(here::here("data/county-testing.rds"))

pop <- pop %>%
  filter(Age %in% c("0-4", "5-17")) %>%
  group_by(County) %>%
  tally(`Population estimate 2020`)

per_capita <- map(counties, ~left_join(pop, .x, by = "County"))


