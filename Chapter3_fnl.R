library(httr)
library(stringr)
library(tidyverse) 
library(rvest)     
library(jsonlite)  
library(scales)

#It’s fine if show how you retrieved the data from the API (e.g. your GET command) and then how you converted into a tibble.
  resp<- GET("https://pokeapi.co/api/v2/ability/?limit=50&offset=20")
  rawToChar(resp$content) %>%
    fromJSON() %>% 
    .[[4]] %>% 
    .[] %>% 
    as_tibble()%>%
    head(10)



html <- read_html("https://www.rosebikes.de/fahrr%C3%A4der/kinder")
model_name <-  html %>% 
  html_nodes(css = ".catalog-product-tile__title") %>%
  html_text() %>%
  stringr::str_replace_all(pattern = "\n\n", replacement = " ") %>%
  stringr::str_extract("(?<=\\n).*(?=\\n)")

price <-  html %>% 
  html_nodes(css = ".product-tile-price__wrapper") %>%
  html_text()%>%
  stringr::str_extract(pattern = "[0-9]+") %>% 
  as.numeric() %>%
  dollar(big.mark = ".", 
         decimal.mark = ",", 
         prefix = "", 
         suffix = " €")

tibble(model_name,price) %>% 
  head(10)