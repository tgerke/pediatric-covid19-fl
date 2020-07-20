library(tidyverse)

txt <- pdftools::pdf_text(
  here::here("reports", "pediatric_report_20200717.pdf")) %>%
  read_lines()

counties <- txt %>% 
  str_squish() %>%
  str_remove_all(",") %>%
  str_replace("Palm Beach", "Palm-Beach") %>%
  str_replace("Santa Rosa", "Santa-Rosa") %>%
  str_replace("St. Johns", "St.-Johns") %>%
  str_replace("St. Lucie", "St.Lucie") %>%
  str_replace("Indian River", "Indian-River") %>%
  read_delim(delim = " ", skip = 27, n_max = 35, col_names = FALSE)

counties <- bind_rows(
  counties %>% 
    select(X1:X5) %>%
    rename(County = X1,
           Cases = X2,
           Negative = X3,
           Total = X4, 
           'Percent Positive' = X5),
  counties %>% 
    select(X6:X10) %>%
    rename(County = X6,
           Cases = X7,
           Negative = X8,
           Total = X9, 
           'Percent Positive' = X10)
)
