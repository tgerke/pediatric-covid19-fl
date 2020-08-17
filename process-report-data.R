library(tidyverse)

counties <- read_rds(here::here("data/county-testing.rds"))

# Primary wrangling needed:
# reports appear at staggered intervals 20200612 to 20200731
# daily reports begin 20200801
# starting 20200814 numbers are reported for 2 week intervals rather than cumulatively 
# Want: daily counts and cumulative counts on each day

# correct a county name
counties <- map(
  counties,
  ~ mutate(.x,
           County = str_replace(County, "St.Lucie", "St. Lucie")
  )
)

counties <- map2(
  .x = counties,
  .y = names(counties) %>% str_sub(start = -12, end = -5),
  ~ mutate(.x, date = lubridate::ymd(.y))
) %>%
  map_dfr(bind_rows)

counties %>% 
  group_by(County) %>%
  mutate(
    cumulative_cases = cases,
    cumulative_cases = if_else(
      date < lubridate::ymd("20200814"),
      cumulative_cases,
      cases - lag(cumulative_cases, 1) + lag(cumulative_cases, 14)
    )) %>% 
  ungroup() %>%
  filter(date == lubridate::ymd("20200814")) %>%
  View()

# check: 
# Alachua through 20200731 :: 259  
# Alachua through 20200812 :: 354
# Alachua through 20200813 :: 109
# new cases: 109 - (354-259) = 14
# cumulative: 354 + 14 = 368

  mutate(new_positives = cumulative_cases - lag(cumulative_cases, 1),
         new_negatives = cumulative_negative - lag(cumulative_negative, 1),
         new_tests = cumulative_total - lag(cumulative_total, 1),
         percent_pos = new_positives/new_tests) %>% 
  ungroup()
