
url_immerhin <- "http://www.immerhin-wuerzburg.de/"
nodes_immerhin1 <- "section:nth-child(1) li a"
nodes_immerhin2 <- "section:nth-child(1) b"

raw_crawler <- function(url, nodes1, nodes2) {
  read_html(url) -> site
  site %>%
    html_nodes(nodes1) %>%
    html_text(trim = TRUE) -> date
  
  date = date[date!=""]
  
  site %>%
    html_nodes(nodes2) %>%
    html_text(trim = TRUE) -> description
  
  return(data.frame(date=date, description=description))
} 


#Raw Data
raw_immerhin <- raw_crawler(url_immerhin, nodes_immerhin1, nodes_immerhin2)

#Dataframe erstellen

raw_immerhin %>%
  filter(description != "Geschlossen") -> raw_immerhin

raw_immerhin %>%
  separate(date,into=c("day", "date_start", "type"), sep=" ") %>%
  mutate(description = paste (type, description)) %>%
  select(-type, -day) %>%
  mutate(url = url_immerhin) %>%
  mutate(lng = "9.93173") %>%
  mutate(lat = "49.80186") %>%
  mutate(city = "Wuerzburg") %>%
  mutate(street = "Bahnhofplatz 2 (Posthalle)") %>% 
  mutate(zip = "97080") %>%
  mutate("date_end" = date_start) %>%
  mutate("time_start" = NA) %>%
  mutate("time_end" = NA) %>%
  mutate(price = NA) %>%
  mutate(organizer = "Immerhin") %>%
  mutate(date_start = parse_date_time(date_start, "d!.m!.y!")) %>%
  mutate(date_end = parse_date_time(date_end, "d!.m!.y!")) %>%
  mutate(lng <- as.numeric(lng),
         lat <- as.numeric(lat),
         zip <- as.numeric(zip),
         time_start <- times(time_start),
         time_end <- times(time_end),
         price <- as.character(price),
         date_start <- as.Date(date_start),
         date_end <- as.Date(date_end)) -> tidy_immerhin


t=immerhin_crawler()
t
