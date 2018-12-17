"https://www.cinemaxx.de/event" %>%
  read_html() %>%
  html_nodes(".underline--alt") %>%
  html_attr("href") -> urls


urls = paste0("https://www.cinemaxx.de",urls)  

getmovieDescription = function(url)
{
  url %>%
    read_html() %>%
    html_nodes(".wysiwyg h2") %>%
    html_text() -> x
  
  if(length(x)==0) {x = "keine Beschreibung"}
  
  x = as.character(x)
}

map_chr(urls, getmovieDescription)
