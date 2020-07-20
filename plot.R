# sources -----------------------------------------------------------------
# http://ww11.doh.state.fl.us/comm/_partners/covid19_report_archive/

library(tidyverse)
library(lubridate)
library(patchwork)

# tests -------------------------------------------------------------------

peds <- tribble(
  ~week, ~cases, ~negative, ~total_tests,
  "2020-07-10", 16797, 37225, 54022,
  "2020-07-03", 11515, 36096, 47611,
  "2020-06-29", 7197, 35090, 42287,
  "2020-06-19", 4809, 34261, 39070,
  "2020-06-12", 3407, 33804, 37211,
) %>%
  mutate(date = as_datetime(week)) %>%
  mutate(new_tests = total_tests - lead(total_tests, 1)) %>%
  mutate(new_positives = cases - lead(cases, 1)) %>%
  mutate(new_negatives = negative - lead(negative, 1)) %>%
  mutate(percent_pos = new_positives/new_tests) 

g_test <- peds %>% 
  pivot_longer(cols = c(new_positives, new_negatives), 
               names_to = "status") %>%
  mutate(status = 
           case_when(status == "new_positives" ~ "Positive",
                     status == "new_negatives" ~ "Negative",
                     TRUE ~ as.character(status))) %>%
  ggplot() +
  aes(x = date, y = value, fill = status) + 
  geom_bar(stat = "identity", position = "stack") + 
  labs(fill = NULL) +
  scale_fill_manual(
    values = c(
      Negative = "#dddddd",
      Positive = "#440154"
    )
  ) +
  geom_text(data = . %>% filter(status == "Positive"),
            mapping = aes(label=scales::percent(percent_pos)),
            color = "white",
            position = position_dodge(width=0.9), 
            vjust = 1.5) + 
  scale_y_continuous(labels = grkmisc::format_pretty_num()) +
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b %d", expand = expansion()) +
  theme_minimal(14) +
  theme(
    strip.text = element_text(face = "bold", size = 18),
    legend.position = c(0.1, .83),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    ) + 
  plot_annotation(
    title = "State of Florida pediatric COVID-19 test results",
    subtitle = "New tests and percent positivity for ages 0-17 beginning 2020-06-12",
    caption = glue::glue(
      "Source: Florida DOH",
      "github.com/tgerke/pediatric-covid19-fl",
      .sep = "\n"
    ),
    theme = theme(
      plot.title = element_text(hjust = 0, size = 18, face = "plain"),
      plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "lines"),
      plot.subtitle = element_text(margin = margin(b = 1.25, unit = "lines")),
      plot.caption = element_text(color = "#444444")
    )
  ) + 
  labs(x = NULL, y = NULL)

ggsave(here::here("plots", "testing.png"), 
       g_test, width = 5, height = 4, dpi = 150, scale = 1.5)

# hospitalizations --------------------------------------------------------

hosp <- tribble(
  ~date, ~age_group, ~cases, ~hospitalizations, ~deaths,
  "2020-07-10", "<1", 1120, 41, 0,
  "2020-07-10", "1-4", 2808, 46, 0,
  "2020-07-10", "5-9", 3739, 23, 0,
  "2020-07-10", "10-14", 4517, 42, 2,
  "2020-07-10", "15-17", 4889, 61, 2,
  "2020-07-03", "<1", 769, 34, 0,
  "2020-07-03", "1-4", 1976, 36, 0,
  "2020-07-03", "5-9", 2520, 20, 0,
  "2020-07-03", "10-14", 3003, 39, 1,
  "2020-07-03", "15-17", 3247, 54, 2,
  "2020-06-29", "<1", 456, 28, 0,
  "2020-06-29", "1-4", 1250, 32, 0,
  "2020-06-29", "5-9", 1591, 19, 0,
  "2020-06-29", "10-14", 1965, 33, 0,
  "2020-06-29", "15-17", 1935, 43, 2,
  "2020-06-19", "<1", 303, 25, 0,
  "2020-06-19", "1-4", 841, 27, 0,
  "2020-06-19", "5-9", 1082, 16, 0,
  "2020-06-19", "10-14", 1356, 29, 0,
  "2020-06-19", "15-17", 1227, 34, 0,
  "2020-06-12", "<1", 218, 21, 0,
  "2020-06-12", "1-4", 577, 22, 0,
  "2020-06-12", "5-9", 758, 12, 0,
  "2020-06-12", "10-14", 976, 24, 0,
  "2020-06-12", "15-17", 878, 24, 2,
) %>%
  mutate(date = as_datetime(date)) %>% 
  group_by(age_group) %>%
  mutate(new_cases = cases - lead(cases, 1),
         new_hospitalizations = hospitalizations - lead(hospitalizations, 1),
         percent_hosp = new_hospitalizations/new_cases) %>% 
  ungroup()
  
g_hosp <- hosp %>% 
  pivot_longer(cols = c(new_hospitalizations, new_cases), 
               names_to = "status") %>%
  mutate(status = 
           case_when(status == "new_hospitalizations" ~ "Hospitalizations",
                     status == "new_cases" ~ "Cases",
                     TRUE ~ as.character(status))) %>%
  mutate(age_group = 
           fct_relevel(age_group, '<1', '1-4', '5-9', '10-14', '15-17')) %>%
  filter(!is.na(value)) %>%
  ggplot() +
  aes(x = age_group, y = value, fill = status) + 
  geom_bar(stat = "identity", position = "stack") + 
  facet_wrap(~factor(date), nrow = 1) + 
  labs(fill = NULL) +
  scale_fill_manual(
    values = c(
      Cases = "#dddddd",
      Hospitalizations = "#440154"
    )
  ) +
  geom_text(data = . %>% filter(status == "Cases"),
            mapping = aes(label=scales::percent(percent_hosp, accuracy = .1)),
            color = "#440154",
            position = position_dodge(width=0.9), 
            vjust = 1.25) + 
  scale_y_continuous(labels = grkmisc::format_pretty_num()) +
  theme_minimal(14) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    legend.position = c(0.1, .83),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
  ) + 
  plot_annotation(
    title = "State of Florida pediatric COVID-19 hospitalizations",
    subtitle = "Proportion of new cases hospitalized by pediatric age group",
    caption = glue::glue(
      "Source: Florida DOH",
      "github.com/tgerke/pediatric-covid19-fl",
      .sep = "\n"
    ),
    theme = theme(
      plot.title = element_text(hjust = 0, size = 18, face = "plain"),
      plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "lines"),
      plot.subtitle = element_text(margin = margin(b = 1.25, unit = "lines")),
      plot.caption = element_text(color = "#444444")
    )
  ) + 
  labs(x = NULL, y = NULL)

ggsave(here::here("plots", "hospitalizations.png"), 
       g_hosp, width = 7, height = 4, dpi = 150, scale = 1.5)
