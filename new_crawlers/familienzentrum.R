library(chron) 
library(tidyverse)

url <- "http://www.familienzentrum-wuerzburg.de/termine/"

url %>%
  read_html() %>%
  html_nodes(".csc-textpic-text p") %>%
  html_text(trim = T) -> data1
v2 <- as.data.frame(data1)


#edit data

v2$data <- str_remove_all(v2$data1, "[A-z]{2}[\\.]{1}\\s?[0-9]{1,2}[\\.]{1}[0-9]{1,2}[\\.]{1} ")
v2$title <- str_extract(v2$data, "(?<=- )[A-zÄäÖöÜüßéÉ[:punct:]\\s]{1,25} ")
v2$title <- gsub(",","",v2$title)

v2$url <- "http://www.familienzentrum-wuerzburg.de/termine/"
v2$url <- as.character(v2$url)

v2$data <- str_remove_all(v2$data1, "[[:space:]]{2,100}")
v2$description <-  str_extract_all(v2$data, "(?<= Uhr)[[:punct:]\\w ]*")
v2$description[v2$description == "  "] <- NA
v2$description[v2$description == "character(0)"] <- NA
v2$description[v2$description == ""] <- NA


v2$lng <- c(9.93684)
v2$lng <- as.numeric(v2$lng)

v2$lat <- c(49.76189)
v2$lat <- as.numeric(v2$lat)

v2$city <- "Wuerzburg-Heidingsfeld"
v2$city <- as.character(v2$city)

v2$street <- "Frau-Holle-Weg 27"
v2$street <- as.character(v2$street)

v2$zip <- c(97084)
v2$zip <- as.numeric(v2$zip)


v2$date_start <- str_extract(v2$data1, "[0-9]{1,2}[\\.][0-9]{1,2}")
v2$date_start <- paste0(v2$date_start,".2018")
v2$date_start <- as.Date(v2$date_start, format = "%d.%m.%Y")

v2$data <- str_remove_all(v2$data1, "[A-z]{2}[\\.]{1}\\s?[0-9]{1,2}[\\.]{1}[0-9]{1,2}[\\.]{1} ")

v2$date_end <- NA
v2$date_end  = as.Date(v2$date_end) 
v2$price <- NA

v2$time_start <- str_extract(v2$data, "[0-9]{1,2}[\\.]{1}[0-9]{1,2}") 
v2$time_end <- str_extract(v2$data, "(?<=- )[0-9]{1,2}[\\.]{1}[0-9]{1,2}")

v2$time_start <- gsub("\\.", ":", v2$time_start) 
v2$time_end <- gsub("\\.",":",v2$time_end)

v2$time_start <- times(paste0(v2$time_start, ":00"))
v2$time_end <- times(paste0(v2$time_end, ":00"))

v2$data <- str_remove_all(v2$data1, "[[:space:]]{2,100}")


organizer <- "Familienzentrum Würzburg e.V."

v2 %>%
  filter(!is.na(title)) -> v2

v2$data = NULL
v2$data1 = NULL

v2

meta_df = data.frame(url = url
                     , organizer = organizer)

names(meta_df)[names(meta_df) == 'url'] <- 'url_crawler'
names(v2)[names(v2) == 'url'] <- 'link'

v2[[3]] = as.character(v2[[3]])
#write to database
write_dataframes_to_database(v2, meta_df, conn)


# for (i in 1:ncol(crawled_df)) {
#   print(typeof(crawled_df[[i]]))
#   }
# crawled_df[[12]]
