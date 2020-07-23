library(tidyverse)
library(ggtext)

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

per_capita <- per_capita %>%
  group_by(County) %>%
  mutate(new_positives = Cases - lag(Cases, 1),
         new_tests = Total - lag(Total, 1),
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

per_capita %>%
  filter(date == lubridate::ymd("2020-07-17")) %>%
  rename(Positive = Cases) %>%
  pivot_longer(cols = c(Positive, Negative), 
               names_to = "status") %>%
  ggplot() +
  aes(x = County, y = value, fill = status) + 
  geom_bar(stat = "identity", position = "stack") + 
  coord_flip() + 
  labs(fill = NULL) +
  scale_fill_manual(
    values = c(
      Negative = "#dddddd",
      Positive = "#440154"
    )
  ) +
  # geom_text(data = . %>% filter(status == "Positive"),
  #           mapping = aes(label=scales::percent(percent_pos)),
  #           color = "white",
  #           position = position_dodge(width=0.9), 
  #           vjust = 1.5) + 
  scale_y_continuous(labels = grkmisc::format_pretty_num()) +
  #scale_x_datetime(date_breaks = "1 week", date_labels = "%b %d", expand = expansion()) +
  theme_minimal(14) +
  labs(
    title = "State of Florida pediatric COVID-19 test results",
    subtitle = "New tests and percent positivity for ages 0-17 beginning 2020-06-12",
    caption = glue::glue(
      "Source: Florida DOH",
      "github.com/tgerke/pediatric-covid19-fl",
      .sep = "\n"),
    x = NULL, y = NULL) 
