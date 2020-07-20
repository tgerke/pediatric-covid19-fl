# data from
# https://www.bebr.ufl.edu/population/population-data/population-projections-age-sex-race-and-hispanic-origin-florida-and-its-4

library(tidyverse)

pop <- readxl::read_xlsx(
  here::here("population", "projections_2020_asrh.xlsx"),
  skip = 1
)

pop <- pop %>%
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
  filter(!is.na(Age)) %>%
  group_by(County) %>%
  slice(1:8) %>%
  ungroup() %>%
  mutate(County = str_to_title(County),
         County = str_remove_all(County, "[*]"),
         Age = relevel(as_factor(Age), 
                       "Total",
                       "0-4",
                       "5-17",
                       "18-24",
                       "25-54",
                       "55-64",
                       "65-79",
                       "80+")) %>%
  rename("Population estimate 2020" = '2020')

write_csv(pop, here::here("data", "FL-population-2020.csv"))
