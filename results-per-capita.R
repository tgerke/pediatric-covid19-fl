library(tidyverse)

pop <- read_rds(here::here("data/FL-population-2020.rds"))
counties <- read_rds(here::here("data/county-testing.rds"))

# correct names for join
pop <- pop %>%
  mutate(County = str_replace(County, "Miami-Dade", "Dade"))
counties <- map(
  counties,
  ~ mutate(.x,
    County = str_replace(County, "St.Lucie", "St. Lucie")
  )
)

pop <- pop %>%
  filter(
    Age %in% c("0-4", "5-17"),
    County != "Florida"
  ) %>%
  group_by(County) %>%
  tally(`Population estimate 2020`)

per_capita <- map(counties, ~ left_join(pop, .x, by = "County")) %>%
  map(~ mutate(.x, "Tests per capita" = Total / n))

dates <- names(per_capita) %>%
  str_sub(start = -12, end = -5)

per_capita <- map2(
  per_capita,
  dates,
  ~ mutate(.x, date = lubridate::ymd(.y))
) %>%
  map_dfr(bind_rows)
