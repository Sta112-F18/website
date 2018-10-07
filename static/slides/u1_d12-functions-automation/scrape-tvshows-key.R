# load packages ----------------------------------------------------------------
library(tidyverse)
library(rvest)

# read in http://www.imdb.com/chart/tvmeter ------------------------------------
page <- read_html("http://www.imdb.com/chart/tvmeter")

# years ------------------------------------------------------------------------
years <- page %>%
  html_nodes("a+ .secondaryInfo") %>%
  html_text() %>%
  str_remove("\\(") %>%
  str_remove("\\)") %>%
  as.numeric()

# scores -----------------------------------------------------------------------
scores <- page %>%
  html_nodes(".imdbRating") %>%
  html_text() %>%
  str_replace_all("\n", "") %>%
  str_trim() %>%
  as.numeric()

# names ------------------------------------------------------------------------
names <- page %>%
  html_nodes(".titleColumn") %>%
  html_text() %>%
  str_trim() %>%
  str_extract("^(.+?)\\n") %>%
  str_remove("\n")

# urls -------------------------------------------------------------------------
urls <- page %>%
  html_nodes(".titleColumn a") %>%
  html_attr("href") %>%
  paste("http://www.imdb.com", ., sep = "")

# tvshows dataframe ------------------------------------------------------------
tvshows <- tibble(
  rank = 1:100,
  name = names,
  year = years,
  score = scores,
  url = urls
)

# function: scrape_show_info ---------------------------------------------------
scrape_show_info <- function(x){
  
  y <- read_html(x) 
  
  title <- y %>%
    html_node("#title-overview-widget h1") %>%
    html_text() %>%
    str_trim()
  
  genres <- y %>%
    html_nodes(".see-more.canwrap~ .canwrap a") %>%
    html_text() %>%
    str_trim() %>%
    paste(collapse = ", ")
  
  runtime <- y %>%
    html_node("time") %>%
    html_text() %>%
    str_replace("\\n", "") %>%
    str_trim()
  
  episodes <- y %>%
    html_nodes(".np_right_arrow .bp_sub_heading") %>%
    html_text() %>%
    str_replace(" episodes", "") %>%
    as.numeric()
    
  keywords <- y %>%
    html_nodes(".itemprop") %>%
    html_text() %>%
    str_trim() %>%
    paste(collapse = ", ")
  
  tibble(title = title, runtime = runtime, genres = genres,
         episodes = episodes, keywords = keywords)
  
}

# map --------------------------------------------------------------------------
top_100_shows <- map_df(tvshows$url[1:5], scrape_show_info)


