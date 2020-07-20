# data from
# https://www.bebr.ufl.edu/population/population-data/population-projections-age-sex-race-and-hispanic-origin-florida-and-its-4

library(tidyverse)

pop <- readxl::read_xlsx(
  here::here("population", "projections_2020_asrh.xlsx"),
  skip = 1
)

pop %>%
  rename(
    County = "and State",
    Age = "Age/Sex"
  ) %>%
  select(County, Age, "2020") %>%
  filter(!is.na(County) | !is.na(Age)) %>%
  mutate(
    County = case_when(
      str_detect(County, "[a-z]") ~ NA_character_,
      TRUE ~ as.character(County)
    )
  ) %>%
  fill(County) %>% 
  filter(Age %in% c("0-4", "5-17")) %>% 
  group_by(County) %>%
  slice(1:2) %>%
  ungroup()
