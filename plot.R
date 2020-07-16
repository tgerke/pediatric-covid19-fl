# http://ww11.doh.state.fl.us/comm/_partners/covid19_report_archive/
# http://ww11.doh.state.fl.us/comm/_partners/covid19_report_archive/pediatric_report_20200710.pdf
library(tidyverse)
library(lubridate)
library(patchwork)

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
