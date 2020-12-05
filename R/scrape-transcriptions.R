# initial setup ----------------------------------------------------------------
library(tidyverse)
library(rvest)


# auxiliary function -----------------------------------------------------------

extract_speaker <- function(text) {
  preliminar <- str_extract(text, "^(.+:?):")
  preliminar[preliminar != str_to_upper(preliminar)] <- NA
  str_remove(preliminar, ":")
}


# extraction -------------------------------------------------------------------

# extract conference links
base_url <- "https://lopezobrador.org.mx/transcripciones/page/"
pages <- 1:165

links <- list()
total <- length(pages)
pb <- txtProgressBar(min = 0, max = total, style = 3)

for (i in pages) {
  url <- paste0(base_url, i)
  list_page <- read_html(url)
  links[[i]] <- list_page %>% 
    html_nodes(".entry-title a") %>% 
    html_attr("href")
  setTxtProgressBar(pb, i)
}
close(pb)

links <- unlist(links)
matutina_idx <- str_detect(str_to_lower(links), "matutina")
matutina_links <- links[matutina_idx]

# scrape for conference contents
contents <- list()
total <- length(matutina_links)
pb <- txtProgressBar(min = 0, max = total, style = 3)

for (i in 1:total) {
  page <- read_html(matutina_links[i])
  date <- page %>% 
    html_node(".entry-date a") %>% 
    html_text()
  content <- page %>% 
    html_nodes(".entry-content p") %>% 
    html_text()
  speaker <- extract_speaker(content)
  
  contents[[i]] <- tibble(date = date, content = content, speaker = speaker) %>%
    fill(speaker) %>% 
    filter(!is.na(speaker))
  setTxtProgressBar(pb, i)
}
close(pb)

contents <- bind_rows(contents)
write_csv(contents, "data/conferences-raw.csv")
