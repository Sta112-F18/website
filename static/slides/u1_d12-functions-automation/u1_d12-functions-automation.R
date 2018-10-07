scrape_show_info <- function(x){
  y <- read_html(x) 
  title <- y %>%
    html_node("#title-overview-widget h1") %>%
    html_text() %>%
    str_trim()
  runtime <- y %>%
    html_node("time") %>%
    html_text() %>%
    str_replace("\\n", "") %>%
    str_trim()
  genres <- y %>%
    html_nodes(".txt-block~ .canwrap a") %>%
    html_text() %>%
    str_trim() %>%
    paste(collapse = ", ")
  tibble(title = title, runtime = runtime, genres = genres)
}