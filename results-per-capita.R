library(tidyverse)
library(ggtext)
library(ggiraph)

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
  map(~ mutate(.x, "Tests per capita" = cumulative_total / n))

dates <- names(per_capita) %>%
  str_sub(start = -12, end = -5)

per_capita <- map2(
  per_capita,
  dates,
  ~ mutate(.x, date = lubridate::ymd(.y))
) %>%
  map_dfr(bind_rows)

per_capita <- per_capita %>%
  group_by(County) %>%
  mutate(new_positives = cumulative_cases - lag(cumulative_cases, 1),
         new_negatives = cumulative_negative - lag(cumulative_negative, 1),
         new_tests = cumulative_total - lag(cumulative_total, 1),
         percent_pos = new_positives/new_tests,
         new_tests_per_capita = new_tests/n) %>% 
  ungroup()

# plot --------------------------------------------------------------------

theme_set(
  firasans::theme_ipsum_fsc(
    axis_text_family = "Fira Sans Condensed",
    axis_text_size = 10, 
    axis_title_size = 14,
    axis_title_just = "cc") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.title = element_markdown(face = "plain"),
          plot.subtitle = element_markdown(),
          plot.caption = element_markdown(),
          plot.title.position = "plot")
)

g_new_tests <- per_capita %>%
  filter(!is.na(new_tests)) %>%
  ggplot() + 
  aes(x = date, y = new_tests, group = County) +
  geom_line_interactive(aes(tooltip = County)) +
  labs(
    title = "State of Florida pediatric COVID-19 test results",
    subtitle = "New tests by county for ages 0-17",
    caption = glue::glue(
      "Source: Florida DOH",
      "github.com/tgerke/pediatric-covid19-fl",
      .sep = "<br>"),
    x = NULL, y = NULL) 

girafe(ggobj = g_new_tests)

g_tests_per_capita <- per_capita %>%
  filter(!is.na(new_tests)) %>%
  ggplot() + 
  aes(x = date, y = `Tests per capita`, group = County) +
  geom_line_interactive(aes(tooltip = County)) + 
  labs(
    title = "State of Florida pediatric COVID-19 test results",
    subtitle = "Cumulative tests per capita by county for ages 0-17",
    caption = glue::glue(
      "Source: Florida DOH",
      "github.com/tgerke/pediatric-covid19-fl",
      .sep = "<br>"),
    x = NULL, y = NULL) 

girafe(ggobj = g_tests_per_capita)

g_new_tests_per_capita <- per_capita %>%
  filter(!is.na(new_tests)) %>%
  ggplot() + 
  aes(x = date, y = new_tests_per_capita, group = County) +
  geom_line_interactive(aes(tooltip = County)) +
  labs(
    title = "State of Florida pediatric COVID-19 test results",
    subtitle = "New tests per capita by county for ages 0-17",
    caption = glue::glue(
      "Source: Florida DOH",
      "github.com/tgerke/pediatric-covid19-fl",
      .sep = "<br>"),
    x = NULL, y = NULL) 

girafe(ggobj = g_new_tests_per_capita)

g_new_percent_pos <- per_capita %>%
  filter(!is.na(new_tests)) %>%
  filter(!is.nan(percent_pos)) %>%
  filter(!is.infinite(percent_pos)) %>%
  ggplot() + 
  aes(x = date, y = percent_pos, group = County) +
  geom_line_interactive(aes(tooltip = County)) +
  labs(
    title = "State of Florida pediatric COVID-19 test results",
    subtitle = "Percent positivity of new tests by county for ages 0-17",
    caption = glue::glue(
      "Source: Florida DOH",
      "github.com/tgerke/pediatric-covid19-fl",
      .sep = "<br>"),
    x = NULL, y = NULL) 

girafe(ggobj = g_new_percent_pos)

g_cumulative_percent_pos <- per_capita %>%
  filter(!is.na(new_tests)) %>%
  mutate(cumulative_perc_pos = str_remove(cumulative_perc_pos, "%"), 
         cumulative_perc_pos = as.numeric(cumulative_perc_pos)) %>%
  ggplot() + 
  aes(x = date, y = cumulative_perc_pos, group = County) +
  geom_line_interactive(aes(tooltip = County)) +
  labs(
    title = "State of Florida pediatric COVID-19 test results",
    subtitle = "Cumulative percent positivity by county for ages 0-17",
    caption = glue::glue(
      "Source: Florida DOH",
      "github.com/tgerke/pediatric-covid19-fl",
      .sep = "<br>"),
    x = NULL, y = NULL) 

girafe(ggobj = g_cumulative_percent_pos)
