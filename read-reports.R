library(tidyverse)

files <- fs::dir_ls(
  here::here("reports")
)
files <- files[-grep("archived", files)]

ingest_pdf <- function(file) {
  pdftools::pdf_text(file) %>%
    read_lines()
}

txt <- files %>%
  map(ingest_pdf)

process_counties <- function(txt) {
  row_start <- which(str_detect(txt, "Alachua"))[1] - 1
  row_end <- which(str_detect(txt, "Lake"))[1]
  
  counties <- txt %>%
    str_squish() %>%
    str_remove_all(",") %>%
    str_replace("Palm Beach", "Palm-Beach") %>%
    str_replace("Santa Rosa", "Santa-Rosa") %>%
    str_replace("St. Johns", "St.-Johns") %>%
    str_replace("St. Lucie", "St.Lucie") %>%
    str_replace("Indian River", "Indian-River") %>%
    read_delim(delim = " ", skip = row_start, n_max = row_end - row_start, 
               col_names = FALSE)

  counties <- bind_rows(
    counties %>%
      select(X1:X5) %>%
      rename(
        County = X1,
        cumulative_cases = X2,
        cumulative_negative = X3,
        cumulative_total = X4,
        cumulative_perc_pos = X5
      ),
    counties %>%
      select(X6:X10) %>%
      rename(
        County = X6,
        cumulative_cases = X7,
        cumulative_negative = X8,
        cumulative_total = X9,
        cumulative_perc_pos = X10
      )
  ) %>%
    filter(!is.na(County)) %>%
    mutate(County = str_replace(County, pattern = "[-]", " "))
}

counties <- txt %>%
  map(process_counties)

write_reports <- function(counties_obj, obj_names) {
  name_out <- obj_names %>%
    str_remove(here::here("reports")) %>%
    str_remove("/") %>%
    str_remove(".pdf") %>%
    paste0(".csv")
  
  write_csv(counties_obj, here::here("data", name_out))
}

walk2(counties, names(counties), write_reports)
write_rds(counties, here::here("data", "county-testing.rds"))