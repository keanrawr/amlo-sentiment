# initial setup ----------------------------------------------------------------
library(stringdist)
library(lubridate)
library(tidyverse)

raw <- read_csv("data/conferences-raw.csv")

months <- tibble(
  month = c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", 
            "agosto", "septiembre", "octubre", "noviembre", "diciembre"), 
  m = 1:12
)


# clean data -------------------------------------------------------------------

treated <- raw %>% 
  separate(date, c("month", "day", "year"), convert = TRUE) %>% 
  left_join(months) %>% 
  mutate(date = make_date(year, m, day)) %>% 
  select(date, content, speaker) %>% 
  mutate(content = str_remove(content, fixed(speaker)) )

treated %>% 
  mutate(speaker = str_extract(speaker, "([^,]+)"), 
         speaker = str_remove_all(speaker, "\\(([^\\)]+)\\)"),
         speaker = str_remove_all(speaker, "[.,\\/#!$%\\^&\\*;:{}=\\-_`~()]"), 
         speaker = str_remove_all(speaker, "\\d"),
         speaker = str_replace_all(speaker, "[^[:alnum:]]", " "),
         speaker = str_trim(speaker)) %>% 
  distinct(speaker) %>% 
  pull(speaker) -> all_names

# similarity <- 
crossing(names_1 = all_names, names_2 = all_names) %>% 
  mutate(dist = stringdist(names_1, names_2, "jw")) %>% 
  filter(dist < .2, dist > 0) %>% 
  # head(1) %>% 
  # as.data.frame()
  filter(!names_1 %in% unique(names_2))


treated %>% 
  filter(str_detect(speaker, "LUIS ANTONIO RAMÍREZ PINEDA")) %>% 
  count(speaker, sort = TRUE)


stringdist(
  c("sadsadas", "asdfghj", "a"), 
  c("sadassadg", "sdasertkm", "b"), 
  "jw"
)

"GRACIELA MÁRQUEZ COLÍN"