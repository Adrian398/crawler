#Warning: Carwling might take some time (>1 minute)

franken_url <- "https://museum-franken.de/no_cache/veranstaltungen/kalender.html"

#month vector to convert month names to number
franken_months <- c("Januar","Februar","März","April","Mai","Juni","Juli","August","September","Oktober","November","Dezember")

has_content <- "T"
check_url <- franken_url
event_links_all <- list()

#event links are on monthly .htmls, so we have to check each month until there are no more events available
while(has_content == "T"){
  check_url %>%
    read_html(encoding = "UTF-8") -> check_html
  
  check_html %>%
    html_nodes("dt") %>%
    html_nodes("a") %>%
    html_attr("href") -> event_links
  
  event_links_all <- append(event_links_all, event_links)
  
  if(length(event_links) == 0){
    has_content <- "F"
  }
  
  check_html %>%
    html_node(".month-next") %>%
    html_node("a") %>%
    html_attr("href") -> check_url_ext
  
  check_url <- paste0("http://museum-franken.de/",check_url_ext)
}

#create full urls out of links
event_links_all <- paste0("http://museum-franken.de/", event_links_all)

#crawling function to get event data
crawl_franken_events <- function(url){
  url %>%
    read_html(encoding = "UTF-8") -> scraped_html
  
  scraped_html %>%
    html_nodes("#calendar-event div") %>%
    html_nodes("h1") %>%
    html_text() -> title
  
  scraped_html %>%
    html_nodes("div .datetime") %>%
    html_text() -> datetime
  
  url %>%
    str_extract("5Byear%5D=[0-9]{4}") %>%
    str_extract("[0-9]{4}") -> year
  
  scraped_html %>%
    html_nodes("div .csc-text") %>%
    html_text() -> description
  
  cast <- c(url, title, paste(datetime, year, sep = ", "), description)
}

#apply crawling function to event urls and save result as data.frame
franken_event_data <- lapply(event_links_all, crawl_franken_events) %>%
  as.data.frame() %>% t

#remove unnecessary row names
row.names(franken_event_data) <- NULL

#start-timestamp conversion function
franken_convert_start_timestamp <- function(raw_data){
  raw_data %>%
    str_extract("[0-9]{2}") -> day
  
  raw_data %>%
    str_extract("\\.\\s.*,") %>%
    str_extract("\\w+") %>%
    match(franken_months) -> month
  
  raw_data %>%
    str_extract("[0-9]{4}") -> year
  
  
  paste(year, month, day, sep="-") %>%
    as.Date() -> date_start
  
  raw_data %>%
    str_extract("[0-9]{2}:[0-9]{2}") -> time_base
  
  if(!is.na(time_base)){
    time_base %>%
      paste0(":00") %>%
      times() -> time_start
  } else {
    time_start <- times(NA)
  }
  
  cast <- data.frame(date_start, time_start)
}

#end-timestamp conversion function
franken_convert_end_timestamp <- function(raw_data){
  raw_data %>%
    str_extract("[0-9]{2}") -> day
  
  raw_data %>%
    str_extract("\\.\\s.*,") %>%
    str_extract("\\w+") %>%
    match(franken_months) -> month
  
  raw_data %>%
    str_extract("[0-9]{4}") -> year
  
  paste(year,month,day,sep="-") %>%
    as.Date() -> date_end
  
  raw_data %>%
    str_extract("-\\s[0-9]{2}:[0-9]{2}") %>%
    str_extract("[0-9]{2}:[0-9]{2}") -> time_base
  
  if(!is.na(time_base)){
    time_base %>%
      paste0(":00") %>%
      times() -> time_end
  } else {
    time_end <- times(NA)
  }
  
  cast <- data.frame(date_end,time_end)
}

#extract price out of description
franken_event_data[,4] %>%
  str_extract("(Erwachsene zahlen Eintritt zzgl. 2 € Führungsgebühr(.*))|(Kosten für Eintritt, Führung und Material: [0-9]+,[0-9]{1,2} €)") -> price

gsub(price[], "",franken_event_data[,4])

museum_franken_desc_price <- function(data){
  data %>%
    str_extract("(Erwachsene zahlen Eintritt zzgl. 2 € Führungsgebühr(.*))|(Kosten für Eintritt, Führung und Material: [0-9]+,[0-9]{1,2} €)") -> price
  
  if(!is.na(price)){
    gsub(price, "", data) -> description
  } else {
    description <- data
  }
  
  
  if(is.na(price)){
    price <- as.character(NA)
  }
  
  cast <- data.frame(description, price)
  
  
}


#get price info out of description
franken_event_data[,4] %>%
  as.vector() %>%
  map_df(museum_franken_desc_price) -> desc_price

#apply start-timestamp function 
franken_event_data[,3] %>%
  as.vector() %>%
  map_df(franken_convert_start_timestamp) -> start

#apply end-timestamp function
franken_event_data[,3] %>%
  as.vector() %>%
  map_df(franken_convert_end_timestamp) -> end

#add timestamp columns and remove unnecessary columns
franken_event_data[,-(3:4)] %>%
  cbind(desc_price, start, end) %>%
  cbind("Festung Marienberg, Oberer Burgweg",97082,"Wuerzburg",49.7897012, 9.920944, "Museum für Franken") %>%
  as.data.frame() -> franken_event_data_w_timestamps

#add correct names
names(franken_event_data_w_timestamps) <- c("url","title","description","price","date_start","time_start","date_end","time_end","street","zip","city","lat","lng","organizer")

#reorder data.frame
franken_event_data_w_timestamps <- franken_event_data_w_timestamps %>%
  unnest(title, date_start, date_end, url, description, lng, lat) %>%
  mutate(
    time_start = times(time_start),
    time_end = times(time_end),
    street = as.character(street),
    city = as.character(city),
    organizer = as.character(organizer),
    title = as.character(title),
    url = as.character(url)
  )