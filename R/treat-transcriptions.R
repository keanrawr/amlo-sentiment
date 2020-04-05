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

# remove special characters form speakers
treated <- raw %>% 
  separate(date, c("month", "day", "year"), convert = TRUE) %>% 
  left_join(months) %>% 
  mutate(date = make_date(year, m, day)) %>% 
  select(date, content, speaker) %>% 
  mutate(content = str_remove(content, fixed(speaker)) ) %>% 
  mutate(speaker = str_extract(speaker, "([^,]+)"), 
         speaker = str_remove_all(speaker, "\\(([^\\)]+)\\)"),
         speaker = str_remove_all(speaker, "[.,\\/#!$%\\^&\\*;:{}=\\-_`~()]"), 
         speaker = str_remove_all(speaker, "\\d"),
         speaker = str_replace_all(speaker, "[^[:alnum:]]", " "),
         speaker = str_trim(speaker))

# unify speaker names
all_names <- treated %>% 
  distinct(speaker) %>% 
  pull(speaker)

similar_names <- combn(all_names, 2) %>%
  t() %>% 
  as_tibble() %>% 
  rename(name_1 = V1, name_2 = V2) %>% 
  mutate(dist = stringdist(name_1, name_2, "jw")) %>% 
  filter(dist < .2, dist > 0) %>% 
  filter(!name_1 %in% name_2) %>% 
  select(clean_name = name_1, speaker = name_2)

treated <- treated %>% 
  left_join(similar_names) %>% 
  mutate(speaker = coalesce(clean_name, speaker), 
         speaker = str_to_title(speaker)) %>% 
  select(-clean_name)

# remove special characters from content text
treated <- treated %>% 
  mutate(content = str_remove_all(content, "[.,\\/#!$%\\^&\\*;:{}=\\-_`~()]"), 
         content = str_replace_all(content, "[^[:alnum:]]", " "),
         content = str_trim(content), 
         content = str_to_lower(content)) %>% 
  rename(text = content)

write_csv(treated, "data/conferences-treated.csv")
