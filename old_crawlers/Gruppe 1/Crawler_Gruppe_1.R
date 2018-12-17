library(tidyverse)
library(lubridate)
library(rvest)
library(RSelenium)
library(stringr)
library(magrittr)
library(purrr)
library(ggmap)
library(jsonlite)
library(chron)

# Important Notice: The entire crawling process takes roughly 5 minutes

getEventAttribute = function(event, node) {
  html_node(event, node) %>%
    html_text()
}

getIGZEvents = function() {
  
  url = "https://www.igz.wuerzburg.de/igz/angebote/veranstaltungen/index.html"
  
  read_html(url) %>%
    html_nodes('.ev_title_verysimple') -> events
  
  title = c()
  date = c()
  link = c()
  fullText = c()
  startTime = c()
  s = html_session(url)
  for (i in 1:length(events)) {
    event = events[[i]]
    
    title[i] = html_node(event, '.c_title a') %>%
      html_text()
    
    eventLink = event %>%
      html_node(".c_title a") %>%
      html_attr("href")
    
    link[i] = eventLink
    
    eventText = s %>%
      jump_to(eventLink) %>%
      read_html() %>%
      html_node('td') %>%
      html_text()
    
    fullText[i] = eventText
    date[i] = str_extract(eventText, "[0-9]{2}.[0-9]{2}.[0-9]{4}")
    startTime[i] = str_extract(eventText, "[0-9]?[0-9]:[0-9]{2}")
  }
  
  df = data.frame(
    title = title,
    url = link,
    description = fullText,
    startTime = startTime,
    date_start = date
  )
  
  df %>%
    sapply(as.character) %>%
    as.data.frame() %>%
    mutate(url = paste0("https://www.igz.wuerzburg.de", url, sep = "")) %>%
    mutate(
      time_start = times(paste0(startTime, ":00")),
      date_start = dmy(date_start),
      startTime = NULL,
      description = as.character(fullText),
      lng = 9.99803,
      lat = 49.80379,
      title = as.character(title),
      organizer = "Innovations- & Gründerzentrum Würzburg",
      street = "Friedrich-Bergius-Ring 15",
      zip = 97076,
      city = "Wuerzburg"
    )
}

getAirportEvents = function() {
  url = "https://www.facebook.com/pg/airport.wue/events/"
  
  # Start Browser
  rD = rsDriver()
  remDr = rD[["client"]]
  remDr$navigate(url)
  
  site = read_html(remDr$getPageSource()[[1]])
  
  eventLinks = html_nodes(site, "#upcoming_events_card") %>%
    html_nodes("._4dmk a") %>%
    html_attr("href")
  
  currentYear = format(Sys.Date(), "%Y")
  title = c()
  link = c()
  time = c()
  description = c()
  startDate = c()
  endDate = c()
  startTime = c()
  endTime = c()
  s = html_session(url)
  Sys.setlocale("LC_ALL","German")
  
  for (i in 1:length(eventLinks)) {
    link[i] = paste0("https://www.facebook.com", eventLinks[i], sep = "")
    
    remDr$navigate(link[i])
    
    detailsPage = read_html(remDr$getPageSource()[[1]])
    
    title[i] = html_node(detailsPage, xpath = "//*[@id='seo_h1_tag']") %>%
      html_text()
    description[i] = html_node(detailsPage, "._63ew") %>%
      html_text()
    time[i] = html_node(detailsPage, xpath = "//*[@id='event_summary']/div/ul/li[1]/div/table/tbody/tr/td[2]/div/div/div[2]/div/div[2]") %>%
      html_text()
    
    dates = str_extract_all(time[i], "[0-9]?[0-9]{1}\\. (Januar|Februar|März|April|Mai|Juni|Juli|August|September|Oktober|November|Dezember)", simplify = TRUE)
    startDate[i] = as.character(paste(dates[1,1], currentYear, sep = " "))
    endDate[i] = as.character(paste(dates[1,2], currentYear, sep = " "))
    
    times = str_extract_all(time[i], "[0-9]{2}:[0-9]{2}", simplify = TRUE)
    startTime[i] = times[1,1]
    endTime[i] = times[1,2]
    Sys.sleep(0.5)
  }
  df = data.frame(
    title = title,
    description = description,
    url = link,
    date_start = startDate,
    date_end = endDate,
    time_start = startTime,
    time_end = endTime
  )
  
  df %>%
    mutate(
      date_start = as.Date(date_start, format = '%d. %B %Y'),
      date_end = as.Date(date_end, format = '%d. %B %Y'),
      time_start = times(paste0(startTime, ":00")),
      time_end = times(paste0(endTime, ":00")),
      title = as.character(title),
      description = as.character(description),
      url = as.character(url),
      lng = 9.9905,
      lat = 49.79472,
      street = "Gattingerstraße 17",
      zip = 97076,
      city = "Wuerzburg",
      organizer = "Airport"
    )
  
}

getHerbergeEvents = function() {
  
  url = "https://wuerzburg.jugendherberge.de/jugendherbergen/wuerzburg-281/reisen/familien"
  events = read_html(url) %>%
    html_nodes(".travel-program")
  
  title = c()
  link = c()
  daterange = c()
  description = c()
  price = c()
  s = html_session(url)
  for (i in 1:length(events)) {
    event = events[i]
    title[i] = getEventAttribute(event, '.card__title')
    daterange[i] = getEventAttribute(event, '.seasonal-text')
    eventLink = html_node(event, '.card__content a') %>%
      html_attr("href")
    
    link[i] = eventLink
    priceObject = html_nodes(event, '.total') %>%
      html_text()
    
    price[i] = paste(priceObject[1], priceObject[2], sep = ", ")
    
    description[i] = s %>%
      jump_to(eventLink) %>%
      read_html() %>%
      html_node('.crop-content') %>%
      html_text()
  }
  
  df = data.frame(
    title = title,
    url = link,
    daterange = daterange,
    description = description,
    price = price
  )
  
  df %>%
    separate(daterange, c("date_start", "date_end"), "-") %>%
    mutate(date_end = dmy(date_end)) %>%
    mutate(date_start = str_trim(as.character(date_start))) %>%
    mutate(date_start = ifelse(nchar(date_start) == 6, paste(date_start, year(date_end), sep = ""), date_start)) %>%
    mutate(date_start = dmy(date_start)) %>%
    mutate(
      url = paste0("https://wuerzburg.jugendherberge.de", url, sep = ""),
      lng = 9.9251022,
      lat = 49.790162,
      description = as.character(description),
      title = as.character(title),
      organizer = "DJH-Landesverband Bayern e.V.",
      street = "Fred-Joseph-Platz 2",
      zip = 97082,
      city = "Wuerzburg",
      price = as.character(price)
    )
}

getToscanasaalEvents = function() {
  url = "https://www-sbhome1.zv.uni-wuerzburg.de/qisserver/rds?state=wplan&act=Raum&pool=Raum&show=plan&P.vx=mittel&raum.rgid=82"
  # Start Browser
  rD = rsDriver()
  remDr = rD[["client"]]
  remDr$navigate(url)
  
  dfWrapper = data.frame()
  
  site = read_html(remDr$getPageSource()[[1]])
  df = toscanaCrawler(site, url)
  if (is.data.frame(df)) {
    dfWrapper = rbind(dfWrapper, df)
  }
  
  run <- TRUE
  i <- 1
  while (run) {
    tryCatch({
      remDr$findElement(
        'xpath',
        "//*[@id='wrapper']/div[6]/div[2]/table/tbody/tr/td/fieldset/form/input[3]"
      )$clickElement()
      site = read_html(remDr$getPageSource()[[1]])
      df = toscanaCrawler(site, url)
      if (is.data.frame(df)) {
        dfWrapper = rbind(dfWrapper, df)
      }
    },
    error = function(c) {
      run <<- F
    },
    warning = function(w) {
      run <<- F
    },
    finally = print(paste("Pressed button", i, "times")))
    i <- i + 1
    Sys.sleep(1)
    
  }
  
  return(dfWrapper)
  
}

toscanaCrawler = function(site, url) {
  eventNodes = html_nodes(site, ".plan2")
  
  events = html_text(eventNodes)
  
  eventLinks = html_node(eventNodes, "a") %>%
    html_attr("href")
  
  singleBookings = str_detect(events, "Einzel")
  events = events[singleBookings] %>%
    str_replace_all("[\r\n\t]" , "")
  
  eventLinks = eventLinks[singleBookings]
  
  title = c()
  timeFrame = c()
  startDate = c()
  endDate = c()
  s = html_session(url)
  if (!is.na(nchar(events[1]))) {
    for (i in 1:length(events)) {
      event = events[i]
      timeFrame[i] = str_extract(event, "[0-9]{2}:[0-9]{2} - [0-9]{2}:[0-9]{2}")
      dates = str_extract_all(event, "[0-9]{2}.[0-9]{2}.[0-9]{4}", simplify = TRUE)
      startDate[i] = dates[1, 1]
      endDate[i] = dates[1, 2]
      
      title[i] = s %>%
        jump_to(eventLinks[i]) %>%
        read_html() %>%
        html_node('.form h1') %>%
        html_text()
    }
    
    df = data.frame(
      title = title,
      timeFrame = timeFrame,
      date_start = startDate,
      date_end = endDate,
      url = eventLinks
    )
    
    df %>%
      separate(timeFrame, c("time_start", "time_end"), " - ") %>%
      mutate(
        title = as.character(title),
        url = as.character(url),
        date_start = dmy(date_start),
        date_end = dmy(date_end),
        time_start = times(paste0(time_start, ":00")),
        time_end = times(paste0(time_end, ":00")),
        lng = 9.93844,
        lat = 49.79331,
        organizer = "Dekanat Philosophische Fakultät",
        street = "Residenzplatz 2 (Toscanasaal)",
        city = "Wuerzburg",
        zip = 97070
      )
  } else {
    return(NULL)
  }
}

getArteNoahEvents = function() {
  #Arte Noah (currently) has no events on its website, therefore exhibitions are crawled and entries for the event calendar created based on opening times
  arte_noah_url <- paste0("http://www.kunstverein-wuerzburg.de/cms/website.php?id=/de/index/ausstellungen/jahr", format(Sys.Date(), "%Y"), ".htm")
  
  #crawl event url endings from site
  arte_noah_url %>%
    read_html() %>%
    html_nodes("div .overview") %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    as.vector() -> exhibition_url_ending_unfiltered
  
  #filter out links to top of page
  exhibition_url_ending_unfiltered[!grepl("#oben", exhibition_url_ending_unfiltered)] -> exhibition_url_ending
  
  #crawl whole div
  arte_noah_url %>%
    read_html() %>%
    html_nodes("div .overview") -> arte_noah_crawled_html
  
  #extract exhibition information
  arte_noah_crawled_html[2] %>%
    html_nodes("div,.MsoNormal, h2, p") -> exhibition_divs
  
  #get item number of dividing divs
  grep('<div class="cleardiv">', exhibition_divs) + 1 -> exhibition_info_block_stop_lines
  
  #create data frame with data for further crawling process
  data.frame(exhibition_url_ending, exhibition_info_block_stop_lines) -> crawl_df
  
  #prepare crawl df
  as.character(crawl_df[,1]) -> crawl_df[,1]
  
  rbind(c("dummy",1),crawl_df) -> crawl_df
  
  as.numeric(crawl_df[,2]) -> crawl_df[,2]
  
  #function that crawls info from exhibition pages
  crawl_exhibition_info_arte_noah <- function(counter){
    
    crawl_df[counter-1,2]+1 -> start_line
    crawl_df[counter,2]-2 -> end_line
    
    exhibition_divs[start_line:end_line] -> exhib_source
    
    exhib_source %>%
      html_nodes("div") %>%
      html_nodes("img") %>%
      html_attr("src") %>%
      extract2(1) %>%
      str_replace("..","")-> img_url_ending
    
    paste0("http://www.kunstverein-wuerzburg.de", img_url_ending) -> img_url
    
    exhib_source[1] %>%
      html_text() -> title
    
    exhib_source[2] %>%
      html_text() %>%
      str_split(" - |/") %>%
      unlist -> timestamp
    
    if(nchar(timestamp[1]) == 6){
      timestamp[1] <- paste0(timestamp[1], substr(timestamp[2],7,10))
    }
    
    exhib_source[c(8,9,11)] %>%
      html_text() -> descr
    
    gsub("\\r|\\n","",descr) %>%
      toString() -> descr_clean
    
    seq(from=dmy(timestamp[1]), to=dmy(timestamp[2]), by = "days") -> lubridate_interval
    
    #find thursdays to saturdays
    lubridate_interval[which(wday(lubridate_interval) %in% seq(5,7, by = 1))] -> sundays
    
    #find sundays
    lubridate_interval[which(wday(lubridate_interval) == 1)] -> other_days
    
    paste(sundays, " 12:00") %>%
      ymd_hm() -> sunday_start
    
    paste(sundays, " 18:00") %>%
      ymd_hm() -> sunday_end
    
    paste(other_days, " 15:00") %>%
      ymd_hm() -> other_days_start
    
    paste(other_days, " 18:00") %>%
      ymd_hm() -> other_days_end
    
    data.frame(sunday_start, sunday_end)  -> sundays_df
    
    data.frame(other_days_start, other_days_end)  -> other_days_df
    
    names(sundays_df) <- c("start","end")
    names(other_days_df) <- c("start","end")
    
    rbind(sundays_df, other_days_df) -> start_end_df
    
    cbind(title, descr_clean, img_url,start_end_df) -> cast
  }
  
  arte_noah_events_df <- data.frame()
  
  #loop instead of apply so function can use previous (i-1) exhibition's row number in crawl_df as reference
  for(i in 2:nrow(crawl_df)) {
    rbind(arte_noah_events_df, crawl_exhibition_info_arte_noah(i)) -> arte_noah_events_df
  }
  
  #convert timestamps to new date/time format
  arte_noah_events_df$start %>%
    as.Date() -> arte_noah_events_df$date_start
  
  arte_noah_events_df$start %>%
    format("%H:%M") %>%
    paste0(":00") %>%
    times() -> arte_noah_events_df$time_start
  
  arte_noah_events_df$end %>%
    as.Date() -> arte_noah_events_df$date_end
  
  arte_noah_events_df$end %>%
    format("%H:%M") %>%
    paste0(":00") %>%
    times() -> arte_noah_events_df$time_end
  
  #add lat and long info and convert factor columns to character
  lat <- 49.788855759704
  lon <- 9.9272410635377
  city <- "Wuerzburg"
  street <- "Oskar-Laredo-Platz 1"
  zip <- 97080
  price <- as.character(NA)
  organizer <- "Kunstverein Würzburg"
  
  map_df(arte_noah_events_df[,1:2], as.character) %>%
    cbind(arte_noah_events_df[,4:7],arte_noah_url,street,zip,city, lat, lon, price, organizer) -> arte_noah_final
  
  #assign correct column names
  names(arte_noah_final) <- c("title","description","date_start","time_start","date_end","time_end","url","street","zip","city","lat","lng","price","organizer")
  
  arte_noah_final = arte_noah_final %>%
    mutate(
      organizer = as.character(organizer),
      price = as.character(price),
      url = as.character(url),
      street = as.character(street),
      date_start = as.Date(date_start),
      time_start = times(format(time_start, "%H:%M:%S")),
      time_startTemp = time_start,
      time_start = time_end,
      time_end = time_startTemp,
      time_startTemp = NULL,
      city = as.character(city)
    )
  
  return(arte_noah_final)
}

getTrinitatisEvents = function() {
  trinitatis_url <-"http://www.trinitatiskonzerte.de"
  
  #Month vector to convert month names to number
  trinitatis_months <- c("Januar","Februar","März","April","Mai","Juni","Juli","August","September","Oktober","November","Dezember")
  
  #Events are in multiple parts of the site, so site is first scraped and then processed in multiple steps
  trinitatis_html <- read_html(trinitatis_url)
  
  #crawl event url from two seperate areas of page to get all links
  trinitatis_html %>%
    html_nodes(".art-postheader") %>%
    html_nodes("a") %>%
    html_attr("href") -> trinitatis_event_urls1
  
  trinitatis_html %>%
    html_nodes(".mod-articles-category-title") %>%
    html_attr("href") -> trinitatis_event_urls2
  
  #create consolidtaed url list
  trinitatis_event_urls1 %>%
    append(trinitatis_event_urls2) -> trinitatis_event_urls_all
  
  #extend links to full URLS
  trinitatis_event_urls_all_full <- paste0("http://www.trinitatiskonzerte.de", trinitatis_event_urls_all)
  
  #event data scraping function
  scrape_event_data <- function(url){
    url %>%
      read_html() %>%
      html_nodes(".art-article") -> nodes
    
    cast <- c(1:3) %>%
      lapply(function(item) {
        nodes %>%
          html_nodes("span") %>%
          extract2(item) %>%
          html_text()
      })
  }
  
  #apply scrapring function to event urls
  trinitatis_event_data <- sapply(trinitatis_event_urls_all_full, scrape_event_data) %>%
    t
  
  #move url from row names to column
  trinitatis_event_data_finished <- cbind(urls = row.names(trinitatis_event_data),trinitatis_event_data)
  row.names(trinitatis_event_data_finished) <- NULL
  
  #remove unnecessary data and split heterogeneous column
  trinitatis_event_data_separated <- cbind(trinitatis_event_data_finished[, -3], str_split_fixed(trinitatis_event_data_finished[,3], " Uhr, ", n = 2))
  
  #fzunction to convert starttime to usable timestamp
  trinitatis_convert_start_date <- function(raw_data){
    raw_data %>%
      str_extract("[0-9]{1,2}") -> day
    
    raw_data %>%
      str_extract("\\.\\s.*,") %>%
      str_extract("\\w+") %>%
      match(trinitatis_months) -> month
    
    raw_data %>%
      str_extract("[0-9]{4}") -> year
    
    paste(year,month,day, sep="/") %>%
      as.Date() -> start_date
    
    start_date  -> cast
  }
  
  trinitatis_convert_start_time <- function(raw_data){
    raw_data %>%
      str_extract("[0-9]{2}.[0-9]{2}") %>%
      str_replace("\\.",":") %>%
      paste0(":00") %>%
      times() -> start_time
    
    start_time  -> cast
  }
  
  #apply timestamp conversion function to data
  trinitatis_event_data_separated[,4] %>%
    as.vector() %>%
    lapply(trinitatis_convert_start_date) %>%
    cbind(trinitatis_event_data_separated) %>%
    as.data.frame()-> trinitatis_event_data_w_starttime
  
  trinitatis_event_data_w_starttime[,5] %>%
    as.vector() %>%
    lapply(trinitatis_convert_start_time) %>%
    unlist() %>%
    cbind(trinitatis_event_data_w_starttime) %>%
    as.data.frame()-> trinitatis_event_data_w_starttime
  
  #combine duration and price columns to form event description
  trinitatis_event_data_w_starttime[,5] <- 
    paste0("Dauer: ",trinitatis_event_data_w_starttime[,4])
  
  end_date <- as.Date(NA)
  end_time <- NA
  lat <- 49.72048
  lng <- 9.96731
  city <- "Wuerzburg"
  street <- "Unterer Kirchplatz 3"
  zip <- 97084
  organizer <- "Trinitatis-Konzerte"
  
  #add lat and long, and remove unnecessary columns
  cbind(trinitatis_event_data_w_starttime[,-6],end_date, end_time, street, zip, city, lat, lng, organizer) -> trinitatis_event_data_full
  
  #add correct names
  names(trinitatis_event_data_full) <- c("time_start","date_start","url","title","description","price","date_end","time_end", "street","zip","city","lat","lng","organizer")
  
  
  trinitatis_event_data_full = unnest(trinitatis_event_data_full, title, date_start, url, price ) %>%
    mutate(date_end = NULL,
           organizer = as.character(organizer),
           city = as.character(city),
           street = as.character(street),
           time_end = NULL,
           time_start = times(time_start))
  
  return(trinitatis_event_data_full)
}

getFrankenMusemEvents = function() {
  #Warning: Carwling might take some time (>1 minute)
  
  franken_url <- "http://museum-franken.de/no_cache/veranstaltungen/kalender.html"
  
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
  
  return(franken_event_data_w_timestamps)
}

getRoentgenEvents = function() {
  #NO EVENTS AIVALABLE, INSTEAD OPENING TIMES CRAWLED
  roentgen_url <- "http://wilhelmconradroentgen.de/de/oeffnungszeiten"
  
  #weekday vector to convert day to number
  roentgen_days <- c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag")
  
  #crawl base html
  roentgen_url %>%
    read_html() %>%
    html_nodes("p") -> roentgen_crawled_html
  
  roentgen_crawled_html %>%
    html_text() -> roentgen_text_list
  
  #unlist opening hours page content
  paste(unlist(roentgen_text_list), collapse="") %>%
    str_extract_all("((Montag|Dienstag|Mittwoch|Donnerstag|Freitag|Samstag|Sonntag): [0-9]{1,2} – [0-9]{1,2})|((Montag|Dienstag|Mittwoch|Donnerstag|Freitag|Samstag|Sonntag) – (Montag|Dienstag|Mittwoch|Donnerstag|Freitag|Samstag|Sonntag): [0-9]{1,2} – [0-9]{1,2})", simplify = T) %>%
    t() %>%
    as.vector -> roentgen_opening_vector
  
  #extract all weekdays the museum is open
  roentgen_opening_vector %>%
    str_extract_all("(Montag|Dienstag|Mittwoch|Donnerstag|Freitag|Samstag|Sonntag)", simplify = T) -> roentgen_weekdays
  
  #build current year end timestamp
  curr_year_end <- paste(format(Sys.Date(), "%Y"), 12, 31, sep = "-")
  
  #build time interval from today to end of year
  seq(from=Sys.Date(), to=ymd(curr_year_end), by = "days") -> lubridate_interval
  
  day_vector <- vector()
  start_end_df <- data.frame()
  
  #for all weekdays found, generate timestamps for today until the end of the year
  for (i in 1:nrow(roentgen_weekdays)){
    
    match(roentgen_weekdays[i,], roentgen_days) -> roentgen_day_index
    
    if(!is.na(roentgen_day_index[2]))  {
      lubridate_interval[which(wday(lubridate_interval) %in% seq(roentgen_day_index[1],roentgen_day_index[2], by = 1))] -> day_vector
    } else {
      lubridate_interval[which(wday(lubridate_interval) == roentgen_day_index[1])]  -> day_vector
    }
    roentgen_opening_vector[i] %>%
      str_extract("[0-9]{1,2}") %>%
      paste0(":00:00") -> start
    
    roentgen_opening_vector[i] %>%
      str_extract("– [0-9]{1,2}") %>%
      str_extract("[0-9]{1,2}") %>%
      paste0(":00:00") -> end
    
    day_vector %>%
      paste(start, sep = " ") %>%
      ymd_hm() -> start_timestamps
    
    day_vector %>%
      paste(end, sep = " ") %>%
      ymd_hm() -> end_timestamps
    
    day_vector %>%
      as.Date() -> days
    
    cbind(days, times(start), days, times(end)) %>%
      rbind(start_end_df) -> start_end_df
    
    #cbind(start_timestamps, end_timestamps) %>%
    #rbind(start_end_df) -> start_end_df
  }
  
  street <- "Roentgenring 8"
  city <- "Wuerzburg"
  zip <- 97070
  price <- as.character(NA)
  organizer <- "Röntgen-Gedachtnisstätte"
  url <- "http://wilhelmconradroentgen.de/de/oeffnungszeiten"
  title <- "Röntgen-Gedachtnisstätte geöffnet"
  lat <- 49.79988
  lng <- 9.93182
  
  #add more info to opening times
  cbind(title,start_end_df, paste(unlist(roentgen_text_list), collapse=""), url,street, zip, city, price, lat, lng, organizer, stringsAsFactors=F) -> roentgen_final_df
  
  #add correct names to data frame
  names(roentgen_final_df) <- c("title", "date_start","time_start", "date_end","time_end", "description", "url", "street","zip","city","price","lat","lng","organizer")
  
  roentgen_final_df = roentgen_final_df %>%
    mutate(
      date_end = as.Date(date_end, origin = '1970-01-01'),
      date_start = as.Date(date_start, origin = '1970-01-01'),
      time_start = times(time_start),
      time_end = times(time_end)
    )
  
  return(roentgen_final_df)
}

getDeutschhausKircheEvents = function() {
  #website is very messy, so cralwing results are not reliable
  #at the time of writing this crawler, four different date/time crawling functions
  #had to be written to extract date/times for four events
  
  dhk_url <- "https://deutschhauskirche-wuerzburg.de/archives/category/aktuelles"
  
  #scrape basic html
  dhk_url %>%
    read_html %>%
    html_nodes("div #content") -> dhk_events_scraped
  
  #scrape links to event pages
  dhk_events_scraped %>%
    html_nodes("h2") %>%
    html_nodes("a") %>%
    html_attr("href") -> dhk_event_links
  
  #remove false entries in event links list
  dhk_event_links[-grep("wp-content", dhk_event_links)] -> dhk_event_links_filtered
  
  #event data crawling function
  get_dhk_event_data <- function(url){
    #month vector to match months to numbers
    dhk_months <- c("Januar","Februar","März","April","Mai","Juni","Juli","August","September","Oktober","November","Dezember")
    
    #crawl event html
    url %>%
      read_html() -> crawled_html
    
    #crawl basic info about event
    crawled_html %>%
      html_node("h1") %>%
      html_text() -> title
    
    crawled_html %>%
      html_nodes("div .entry-content") %>%
      html_nodes("img") %>%
      html_attr("src") -> img_url
    
    crawled_html %>%
      html_nodes("div .entry-content") %>%
      html_text() -> event_text
    
    #event pages are very unstrucutred. based on events currently available, four different crawling algorithms are applied to find start and and times
    
    #1
    event_text %>%
      str_extract("Nächster Terminen: [0-9]{2}\\.[0-9]{2}\\.[0-9]{4}, [0-9]{2} Uhr") -> crawled_datetime_unformatted
    
    gsub("Nächster Terminen: |Uhr|,", "", crawled_datetime_unformatted) %>%
      str_sub(1,-2) -> start_base
    
    if(!is.na(start_base)){
      start_base %>%
        paste0(":00") -> start_base
    }
    
    if(!is.na(start_base)){
      paste(str_sub(start_base,7,10),str_sub(start_base,4,5),str_sub(start_base,1,2), sep = "/") %>%
        paste(str_sub(start_base,12,16)) %>%
        ymd_hm(tz = "Europe/Berlin") %>%
        matrix -> lubridate_timestamp
    } else {
      NA -> lubridate_timestamp
    }
    
    if(!is.na(lubridate_timestamp)){
      lubridate_timestamp %>%
        cbind(NA) -> lubridate_timestamp
      names(lubridate_timestamp) <- c("start", "end")
    }
    
    #2
    
    if(is.na(lubridate_timestamp)){
      
      event_text %>%
        str_extract("(Montag|Dienstag|Mittwoch|Donnerstag|Freitag|Samstag|Sonntag), [0-9]{2}\\. (Januar|Februar|März|April|Mai|Juni|Juli|August|September|Oktober|November|Dezember) [0-9]{4}") -> crawled_base_date
      
      crawled_base_date %>% 
        str_extract("(Januar|Februar|März|April|Mai|Juni|Juli|August|September|Oktober|November|Dezember)") %>%
        match(dhk_months) %>%
        as.character()-> month
      
      crawled_base_date %>%
        str_sub(-4) %>%
        as.character() -> year
      
      crawled_base_date %>%
        str_extract("[0-9]{1,2}") %>%
        as.character()-> day
      
      event_text %>%
        str_extract("[0-9]{2}\\.[0-9]{2}\\ Uhr") %>%
        str_sub(1,-5) %>%
        str_replace("\\.",":") -> time
      
      
      if(!is.na(month) & !is.na(year) & !is.na(day) & !is.na(time)){
        paste(year, month, day, sep = "/") %>%
          paste(time, sep = " ") %>%
          ymd_hm(tz = "Europe/Berlin") -> lubridate_timestamp
      } else {
        lubridate_timestamp <- NA
      }
      
      if(!is.na(lubridate_timestamp)){
        lubridate_timestamp %>%
          cbind(NA) -> lubridate_timestamp
        names(lubridate_timestamp) <- c("start", "end")
      }
    }
    #3
    
    if(is.na(lubridate_timestamp)){
      event_text %>%
        str_extract_all("((Montag|Dienstag|Mittwoch|Donnerstag|Freitag|Samstag|Sonntag), [0-9]{1,2}\\. (Januar|Februar|März|April|Mai|Juni|Juli|August|September|Oktober|November|Dezember) von [0-9]{1,2}\\.[0-9]{2} Uhr – [0-9]{1,2}\\.[0-9]{2} Uhr und von [0-9]{1,2}\\.[0-9]{2} – ([0-9]{1,2}|[0-9]{1,2}\\.[0-9]{2}) Uhr)", simplify=T) -> base_time
      
      base_time %>%
        as.vector() -> base_time_vector
      
      base_time[2] %>%
        as.character()-> item
      
      get_dhk_timestamps_two_dates <- function(item){  
        
        item %>%
          str_extract_all("[0-9]{1,2}\\. (Januar|Februar|März|April|Mai|Juni|Juli|August|September|Oktober|November|Dezember)") %>%
          str_extract("[0-9]{1,2}") -> day
        
        item %>%
          str_extract_all("(Januar|Februar|März|April|Mai|Juni|Juli|August|September|Oktober|November|Dezember)") %>%
          match(dhk_months) -> month
        
        item %>% 
          str_extract_all("[0-9]{1,2}\\.[0-9]{2}", simplify = T) %>%
          as.character() -> hrs
        
        item %>%
          str_extract_all(" [0-9]{1,2} Uhr", simplify = T) %>%
          str_extract("[0-9]{1,2}") %>%
          paste0(".00") -> formatted_hrs
        
        c(hrs, formatted_hrs) %>%
          str_replace("\\.",":") %>%
          matrix(ncol = 2) -> full_hrs_matrix
        
        timestamps <- matrix(ncol=2)
        
        for (i in 1:nrow(full_hrs_matrix)){
          format(Sys.Date(), "%Y") %>%
            paste(month, day, sep="/") -> date_base
          
          date_base %>%
            paste(full_hrs_matrix[i,1], sep= " ") %>%
            ymd_hm(tz="Europe/Berlin") -> start_ts
          
          date_base %>%
            paste(full_hrs_matrix[i,2], sep= " ") %>%
            ymd_hm(tz="Europe/Berlin") -> end_ts
          
          timestamps %>%
            rbind(c(start_ts, end_ts)) -> timestamps
        }
        timestamps %>%
          as.data.frame() %>%
          na.omit() -> lubridate_timestamp
        names(lubridate_timestamp) <- c("start","end")
        
        lubridate_timestamp -> cast
      }
      
      map_df(base_time_vector, get_dhk_timestamps_two_dates) -> lubridate_timestamp
    }
    #4
    if(is_empty(lubridate_timestamp)){
      event_text %>%
        str_extract("[0-9]{2}\\. (Januar|Februar|März|April|Mai|Juni|Juli|August|September|Oktober|November|Dezember) [0-9]{4}, [0-9]{1,2}\\:[0-9]{2} (a|p)\\.m\\.") -> crawled_base_date
      
      crawled_base_date %>%
        str_extract("(Januar|Februar|März|April|Mai|Juni|Juli|August|September|Oktober|November|Dezember)") %>%
        match(dhk_months) %>%
        as.character() -> month
      
      crawled_base_date %>%
        str_extract("[0-9]{4}") %>%
        as.character() -> year
      
      crawled_base_date %>%
        str_sub(1,2) %>%
        as.character() -> day
      
      crawled_base_date %>%
        str_extract("[0-9]{1,2}:[0-9]{2}") -> base_time
      
      crawled_base_date %>%
        str_sub(-4,-4) -> am_pm
      
      if(!is.na(am_pm)){
        if(am_pm == "p"){
          base_time %>%
            str_extract(".:") %>%
            str_sub(1, -2) %>%
            as.numeric() -> numeric_hr
          
          numeric_hr + 12 -> numeric_hr
          
          paste0(numeric_hr,str_sub(base_time, -3)) -> time
        } else {
          base_time -> time
        }
      }
      if(!is.na(time)){
        paste(year, month, day, sep = "/") %>%
          paste(time, sep = " ") %>%
          ymd_hm(tz = "Europe/Berlin") -> timestamp
        
        c(timestamp, NA) %>%
          matrix() %>%
          t() -> lubridate_timestamp
        
        names(lubridate_timestamp) <- c("start","end")
      }
    }
    
    # last steps to create the full df
    
    if(is_empty(img_url)){
      img_url <- NA
    }
    
    lat = 49.7943162
    long = 9.9218645
    
    #prevent errors should no timestamp be found
    if(is.na(lubridate_timestamp)){
      start <- NA
      end <- NA
      data.frame(start, end) -> lubridate_timestamp
    }
    
    lubridate_timestamp %>%
      cbind(title, url, img_url, event_text, lat, long) %>%
      as.matrix() -> cast
    
  }
  
  #create empty martix to be filled with event data
  dhk_event_matrix <- matrix(ncol = 8)
  
  #loop instead of apply or map_df used, as these functions were not compatible with the data format returned from function
  for(i in 1:length(dhk_event_links_filtered)){
    dhk_event_links_filtered[i] %>%
      get_dhk_event_data -> result
    
    dhk_event_matrix %>%
      rbind(result) -> dhk_event_matrix
  }
  
  #convert matrix to data frame
  dhk_event_matrix %>%
    data.frame() -> dhk_event_df
  
  #remove uneccesarry first row
  dhk_event_df[-1,] -> dhk_event_df_full
  
  #add correct names to data frame
  names(dhk_event_df_full) <- c("start","end","title","url","img","description", "lat","lng")
  
  #convert date types
  
  #start and end times are converted speratly from rest of df, as factors have to be converted three times to get POSIXct format
  dhk_event_df_full$start %>%
    as.character() %>%
    as.integer() %>%
    as.POSIXct(origin=lubridate::origin) -> start
  
  start %>%
    as.Date() -> date_start
  
  start %>%
    format("%H:%M:%S") %>%
    times() -> time_start
  
  dhk_event_df_full$end %>%
    as.character() %>%
    as.integer() %>%
    as.POSIXct(origin=lubridate::origin) -> end
  
  end %>%
    as.Date() -> date_end
  
  end %>%
    format("%H:%M:%S") %>%
    times() -> time_end
  
  street <- "Schottenanger 13"
  zip <- 97082
  city <- "Wuerzburg"
  organizer <- "Evang. Luth. Kirchengemeinde Deutschhaus-Erlöser"
  price <- as.character(NA)
  
  map_df(dhk_event_df_full[,-c(1,2,5)], as.character) %>%
    cbind(street,zip,city,price,date_start,time_start,date_end,time_end,organizer,stringsAsFactors=F) -> dhk_event_df_final
  
  map_df(dhk_event_df_final[,4:5], as.numeric) -> dhk_event_df_final[,4:5]
  
  return(dhk_event_df_final)
}

getKompetenzzentrumEvents = function() {
  #save URL
  Kompetenzzentrum_URL= "http://www.energietechnik-hwk.de"
  
  #read HTML
  html = Kompetenzzentrum_URL %>%
    read_html() 
  
  #get links from html 
  html_nodes(html, ".list-group-item") %>%
    html_attr("href") -> links
  links = links[!is.na(links)]
  
  url = paste(Kompetenzzentrum_URL, links, sep = "")
  
  
  #get Titel, start,end and description
  html_text = html_nodes(html,".listgroup-text")
  html_text
  
  #get title
  html_nodes(html_text, "strong") %>%
    html_text(trim = T) -> title
  title
  
  
  #function get start and end Date
  
  get_Date <- function(arg1){
    arg2 = strsplit(as.character(arg1),"<br>")[[1]]
    arg2
    
    #get start and end
    subDate = arg2[1]
    
    subDate = strsplit(subDate,"[<>]")[[1]][3]
    
    extractedDates =  strsplit(subDate, " ")[[1]][-2]
    extractedDates = gsub('\\.', '/', extractedDates)
    
    
    extractedDates
    
    return(extractedDates)
  }
  
  #get description
  get_description <- function(arg1){
    arg2 = strsplit(as.character(arg1),"<br>")[[1]]
    arg2
    
    
    description = paste(arg2[4:length(arg2)], collapse = " ")
    description = substr(description, 1  , nchar(description)-6)
    
    description
    return(description)
  }
  
  #get lat lng
  #same location
  
  lng = 9.9083
  lat = 49.79923
  city = "Wuerzburg"
  zip = 97082
  organizer = "Handwerkskammer für Unterfranken"
  price = NA
  street = NA
  time_start = NA
  time_end = NA
  
  
  #create Dataframe
  description =map(as.character(html_text), get_description)
  
  description = sapply( description, paste0, collapse="")
  
  
  Date = map(as.character(html_text), get_Date)
  date_start = map(Date, 1) 
  date_end = map(Date, 2) 
  
  date_start = sapply( date_start, paste0, collapse="")
  date_end = sapply( date_end, paste0, collapse="")
  
  date_start = as.Date(date_start, format='%d/%m/%Y')
  date_end = as.Date(date_end, format='%d/%m/%Y')
 
  df = data.frame(title, url, description, lng, lat, city, zip, street, date_start, date_end, time_start, time_end, price, organizer) %>%
    mutate(title = as.character(title),
           url = as.character(url),
           description = as.character(description),
           street = "Dieselstr. 10",
           time_start = NULL,
           time_end = NULL,
           organizer = as.character(organizer),
           price = NULL,
           city = as.character(city)
    )
   
}

getNeunerplatzTheaterEvents = function() {
  #save URL
  Theater_URL= "http://www.neunerplatz.de/spielplan/"
  
  #read HTML
  html = Theater_URL %>%
    read_html() 
  
  #get link, Titel, start,end and description
  html_text = html_nodes(html,".wp_theatre_event")
  html_text
  
  #get link
  html_text %>%
    html_nodes("a") %>%
    html_attr("href") -> url
  s = seq(1,length(url),3)
  url = url[s]
  
  #get img
  html_text %>%
    html_nodes(".wp-post-image") %>%
    html_attr("src")-> img
  
  
  
  #get title
  html_nodes(html_text, ".wp_theatre_event_title") %>%
    html_text(trim = T) -> title
  title
  
  # get price 
  price = html_node(html_text, ".wp_theatre_event_prices") %>%
    html_text()
  
  #function get start and end Date
  get_Date <- function(arg1){
    arg2 = strsplit(as.character(arg1),", ")[[1]]
    
    #get start and end
    subDate = arg2[2]
    subDate = strsplit(subDate," ")[[1]]
    
    day = substr(subDate[1],1,nchar(subDate[1])-1)
    
    list = setNames(as.list(c(seq(1,12,1))), c("Januar", "Februar", "M?rz", "April", "Mai", "Juni", "Juli", "August", "September", "Oktober", "November", "Dezember"))
    
    month = eval(parse(text= paste0("list$", subDate[2])))
    
    year = format(Sys.Date(), "%Y")
    
    date = paste0(c(day, month, year), seq="", collapse = "/" )
    time1 = "00"
    
    datetime = paste0(c(date, subDate[4], time1), seq="", collapse = ":" )
    
    mydatetime = strptime(datetime,format='%d/%m/%Y:%H:%M:%S')
    
    return(mydatetime)
  }
  
  #get description
  html_nodes(html_text, ".wp_theatre_prod_excerpt") %>%
    html_text(trim = T) -> description
  
  #get lat lng
  #same location
  
  lng = 9.9158
  lat = 49.79639
  city = "Wuerzburg"
  zip = 97082
  organizer = "Theater am Neunerplatz"
  price = price
  street = "Adelgundenweg 2a"
  time_start = NA
  time_end = NA
  date_end = NA
  
  #create Dataframe
  
  html_nodes(html_text, ".wp_theatre_event_datetime") %>%
    html_text(trim = T) -> event_date
  event_date
  
  date_start = map(as.character(event_date), get_Date)
  date_start = sapply( date_start, paste0, collapse="")
  
  df = data.frame(title, url, description, lng, lat, city, zip, street, date_start, date_end, time_start, time_end, price, organizer)
  
  df = df %>%
    separate(date_start, c("date_start", "time_start"), " ") %>%
    mutate(
      date_start = ymd(date_start),
      time_start = times(time_start),
      title = as.character(title),
      url = as.character(url),
      description = as.character(description),
      city = as.character(city),
      street = as.character(street),
      organizer = as.character(organizer),
      date_end = NULL,
      time_end = NULL,
      price = as.character(price)
    )
  
}

getBotanischerGartenEvents = function() {
  
  url = "http://www.bgw.uni-wuerzburg.de/fuehrungen-und-veranstaltungen/veranstaltungen/"
  nPages = read_html(url) %>%
    html_node('.news-list .page-navigation') %>%
    html_node('p') %>%
    html_text() %>%
    str_replace_all("[\r\n\t]" , "") %>%
    str_sub(start = -2, -2)
  
  df = botanischerGartenCrawler(url)
  
  for(i in 2:nPages) {
    url = paste0("http://www.bgw.uni-wuerzburg.de/fuehrungen-und-veranstaltungen/veranstaltungen/page/", i, sep = "" )
    newData = botanischerGartenCrawler(url)
    df = bind_rows(df, newData)
  }
  
  df$time_start = times(df$time_start)
  df$date_end = as.Date(df$date_end, origin = '1970-01-01')
  return(df)
}

botanischerGartenCrawler = function(url) {
  
  read_html(url) %>%
    html_nodes('.news-list__item') -> events
  
  title = c()
  date = c()
  link = c()
  description = c()
  startDate = c()
  time_start = c()
  date_end = c()
  s = html_session(url)
  
  for (i in 1:length(events)) {
    event = events[[i]]
    title[i] = html_node(event, 'a') %>%
      html_attr("title")
    
    eventLink = event %>%
      html_node('a') %>%
      html_attr("href")
    
    link[i] = eventLink
    
    dateTimeText = html_node(event, '.bodytext') %>%
      html_text()
      
    type1 = str_extract(dateTimeText, "[0-9]{2}\\.[0-9]{2}\\.[0-9]{2} - [0-9]?[0-9] Uhr")
    if(!is.na(type1)) {
      time_startTemp = str_extract(type1, "[0-9]?[0-9] Uhr") %>%
        substr(1, nchar(.)-4) %>%
        paste0(":00:00") %>%
        times()
      time_start[i] = time_startTemp
      date_end[i] = NA
    } else {
      time_start[i] = times(NA)
      type2 = str_extract(dateTimeText, "[0-9]{2}\\.[0-9]{2}\\. bis [0-9]{2}\\.[0-9]{2}\\.[0-9]{2}")
      if(!is.na(type2)) {
        date_end[i] = str_extract(type2, "[0-9]{2}\\.[0-9]{2}\\.[0-9]{2}") %>%
          dmy()
      } else {
        date_end[i] = NA
      }
    }
    
    detailsPage = s %>%
      jump_to(eventLink) %>%
      read_html()
      
    eventText = detailsPage %>%
      html_nodes('p.intro') %>%
      html_text()
    
    startDate[i] = detailsPage %>%
      html_node('.news-single__item-date') %>%
      html_text()
    
    description[i] = eventText
  }
  
  df = data.frame(
    title = title,
    url = link,
    description = description,
    date_start = dmy(startDate),
    date_end = date_end,
    time_start = time_start
  )
  
  df %>%
    as.data.frame() %>%
    mutate(
      url = paste0("http://www.bgw.uni-wuerzburg.de", url, sep = ""),
      description = as.character(description),
      lng = 9.93287,
      lat = 49.76477,
      title = as.character(title),
      street = "Julius-von-Sachs-Platz 4",
      city = "Wuerzburg",
      zip = 97082
    )
}

odeoncrawler <- function(){
  
  
  #get highlight
  odeonlounge2 = "http://www.odeon-lounge.de/programm.html"
  odeonlounge2 %>%
    read_html()%>%
    html_nodes("#printitbigone .eventname")%>%
    html_text(trim = T)->htitle
  htitle
  odeonlounge2%>%
    read_html()%>%
    html_nodes(".eventhide")%>%
    html_text(trim = T)->hdescription
  hdescription
  
  
  odeonlounge2%>%
    read_html()%>%
    html_nodes("#printitbigone aside")%>%
    html_text(trim = T)%>%
    str_replace_all("Januar","1.")%>%
    str_replace_all("Februrar","2.")%>%
    str_replace_all("Mai","5.")%>%
    str_replace_all(" ","")%>%
    str_replace_all("[:alpha:]+,","")%>%
    as.Date(format = "%d.%m.%Y")->hstartDate
  hstartDate
  
  "http://www.odeon-lounge.de/programm.html"->hurl
  9.94123 -> hlong
  49.79821 -> hlat
  highlight <- data.frame(
    title = htitle,
    description = hdescription,
    date_start = hstartDate,
    date_end = NA,
    time_start = NA,
    time_end = NA,
    url = hurl,
    lng = hlong,
    lat = hlat
  )
  
  
  
  odeonlounge = "https://gastro.e2n.de/api/odeon/termine.json"
  
  fromJSON(txt=odeonlounge)->events
  events$beginn%>%
    as.character()%>%
    str_replace_all("[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] ","")%>%
    times()->time_start
  events$ende%>%
    as.character()%>%
    str_replace_all("[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] ","")%>%
    times()->time_end
  df <- data.frame(
    title = events$name,
    description = events$beschreibung,
    date_start = as.Date(events$beginn),
    date_end = as.Date(events$ende),
    time_start = time_start,
    time_end = time_end,
    url = hurl,
    lng = hlong,
    lat = hlat
  )
  rbind(highlight,df)->fullevents
  fullevents = fullevents %>%
    mutate(city="Wuerzburg",
           street="Augustinerstrasse",
           price = NULL,
           organizer = "Odeon Wuerzburg",
           zip=97070,
           date_end = as.Date(date_end, origin = '1970-01-01'),
           url = "http://www.odeon-lounge.de/programm.html",
           title = as.character(title),
           description = as.character(description))
  
}

vogelcrawler <- function(){
  title = c()
  url = c()
  description = c()
  lng=c()
  lat=c()
  city=c()
  street=c()
  zip=c()
  time = c()
  img = c()
  date_start = c()
  date_end = c()
  startTime = c()
  endTime = c()
  vogelconventioncenter <- "https://www.messe.ag/Messekalender?Text=vogel+convention+center#searchResult"
  vogelconventioncenter%>%
    read_html()->rawdata
  rawdata%>%
    html_nodes("#searchResult article a")%>%
    html_attr("href") -> url
  
  url%>%
    read_html()%>%
    html_nodes("#Template_ctl11_Content")%>%
    html_text(trim=T)->description
  
  description
  
  
  rawdata%>%
    html_nodes("b:nth-child(1)")%>%
    html_text()->zipcity
  zipcity%>%
    str_extract("[0-9][0-9][0-9][0-9][0-9]")->zip
  zipcity%>%
    str_extract("[:alpha:]+")->city
  
  rawdata%>%
    html_nodes("#searchResult article")%>%
    html_text(trim = T)->test
  test
  
  rawdata%>%
    html_nodes("#searchResult a")%>%
    html_text(trim = T)->title
  
  test
  str_extract_all(test, "[0-9][0-9].[0-9][0-9].[0-9][0-9][0-9][0-9]")->dates
  length(dates)
  
  # for(i in 1:length(dates)){  
  #dates[[i]]%>%
  # as.Date(format = "%d.%m.%Y")->dates[[i]]
  #}
  
  for(j in 1:length(dates)){
    dates[[j]][1] -> date_start[j]
    dates[[j]][2] -> date_end[j]
  }
  date_start%>%
    as.Date(format = "%d.%m.%Y")->date_start
  date_end%>%
    as.Date(format = "%d.%m.%Y")->date_end
  test
  
  df = data.frame(
    title = title,
    url = url,
    description = description,
    city = city,
    zip = zip,
    date_start = date_start,
    date_end = date_end
  )
  df = df %>%
    mutate(
      lat=49.79704,
      lng=9.90075,
      title = as.character(title),
      url = as.character(url),
      city = as.character(city),
      zip = as.numeric(as.character(zip)),
      description = as.character(description)
      #start = dmy_hm(paste(date, "00:00", sep = " ")),
    )
  
  
}

kellerperlencrawler <- function(){
  title = c()
  link = c()
  time = c()
  description = c()
  img = c()
  startDate = c()
  endDate = c()
  endDate = c()
  startTime = c()
  endTime = c()
  lat=c()
  long=c()
  
  url <- "http://www.kellerperle.de/#/uebersicht"
  rD <- rsDriver()
  remDr <- rD[["client"]]
  remDr$navigate(url)
  site = read_html(remDr$getPageSource()[[1]])
  html_nodes(site,"#uebersicht .ng-scope a")->nodes
  html_text(nodes, trim=T)->text
  html_nodes(site,"strong")%>%
    html_text()->titles
  titles[6:length(titles)]->title
  title
  html_nodes(site,".small")%>%
    html_text()->description
  description[5:length(description)]->description
  description
  html_nodes(site,".right")%>%
    html_text(trim=T)->startDate
  startDate
  #
  nodes%>%
    html_attr("href")->links
  paste0("http://www.kellerperle.de/",links)->urls 
  link <- urls
  
  for(i in 1:length(urls)){
    remDr$navigate(urls[i])
    read_html(remDr$getPageSource()[[1]])->rawdata
    rawdata%>%
      html_nodes(".text-md-right")%>%
      html_text(trim=T)->infos
    infos[4]
    str_extract(infos[4],"Beginn \\d\\d:\\d\\d")%>%
      str_replace_all("Beginn ","")->startTime[i]
    str_extract(infos[4],"Ende \\d\\d:\\d\\d")%>%
      str_replace_all("Ende ","")->endTime[i]
    
    49.7861264->lat[i]
    9.9342507->long[i]
  }
  #always the same times for some reason!! bug
  startTime
  endTime
  #
  #dmy_hm(paste0(str_extract(startDate, "[0-9]{2}\\.[0-9]{2}"), ".", year(Sys.Date()), " 00:00"))
  
  df = data.frame(
    title = title,
    url = link,
    description = description,
    date_start = dmy_hm(paste0(str_extract(startDate, "[0-9]{2}\\.[0-9]{2}"), ".", year(Sys.Date()), " 00:00")),
    date_end = NA,
    time_start = startTime,
    time_end = endTime,
    lng=long,
    lat=lat,
    price = NA,
    organizer = "Kellerperle",
    city = "Wuerzburg",
    street = "Am Studentenhaus",
    zip = 97072
  )
  #
  
  df = df %>%
    mutate(
      title = as.character(title),
      description = as.character(description),
      url = as.character(url),
      time_start = NULL,
      time_end = NULL,
      price = NULL,
      street = as.character(street),
      city = as.character(city),
      organizer = as.character(organizer),
      date_start = as.Date(date_start, origin = '1970-01-01'),
      date_end = NULL
    )
  
  # url2 <- "http://www.kellerperle.de/#/1455"
  #remDr$navigate(url2)
  #site2 <-  read_html(remDr$getPageSource()[[1]])
  #html_nodes(site2,".text-md-right")%>%
  #  html_text(trim=T)->text2
  #text2[4]
  #str_extract(text2[4],"Beginn \\d\\d:\\d\\d")%>%
  #  str_replace_all("Beginn ","")->start
  #str_extract(text2[4],"Ende \\d\\d:\\d\\d")%>%
  #  str_replace_all("Ende ","")->end
  return(df)
}

getJuliusSpitalEvents = function() {
  #We want to crawl all events from today to the end of the next year
  
  #First, we need the dates to construct our base url
  paste(substr(Sys.Date(),9,10),substr(Sys.Date(),6,7),substr(Sys.Date(),1,4),sep=".") -> today
  
  Sys.Date() %>%
    format("%Y") %>%
    as.numeric()+1 -> next_year
  
  paste("31","12",next_year,sep=".") -> end_next_year
  
  julius_url <- paste0("https://www.juliusspital.de/aktuelles/veranstaltungskalender/index.html?ev%5Bstart%5D=",today,"&ev%5Bend%5D=",end_next_year,"&ev%5Bsearch%5D=&ev%5Bcat%5D=&ev%5Bsubcat%5D")
  
  #crawl maximum page number from site
  julius_url %>%
    read_html %>%
    html_nodes("div .pager") %>%
    html_nodes("a") %>%
    html_attr("rel") %>%
    str_extract_all("page[0-9]{1,2}") %>%
    str_extract_all("[0-9]{1,2}") %>%
    as.numeric() %>%
    max() -> max_page_no
  
  #create event calender page urls from crawled number
  paste0(julius_url, "=&page=", seq(from=0, to=(max_page_no-1))) -> julius_page_links
  
  #function to crawl event urls from calendar pages
  julius_get_event_urls <- function(url){
    url %>%
      read_html() %>%
      html_nodes(".contentwidth div") %>%
      html_nodes(".evdetails") %>%
      html_nodes("a") %>%
      html_attr("href") %>%
      as.list()-> cast
  }
  
  #crawl all pages for event links
  #may take some time (<1 minute)
  lapply(julius_page_links, julius_get_event_urls) -> julius_base_event_urls
  
  #convert event links to list for further use
  julius_base_event_urls %>%
    unlist() -> julius_base_event_urls_list
  
  #create full urls from base urls
  paste0("https://www.juliusspital.de/",julius_base_event_urls_list) -> julius_full_event_urls_list
  
  #function to crawl event pages for details
  julius_get_event_data <- function(url){
    #crawl part of event page relevant for us
    url %>%
      read_html() %>%
      html_nodes("div .eventdetail") -> crawled_html
    
    #get title
    crawled_html %>%
      html_nodes(".ev-title") %>%
      html_text() -> title
    
    #get description
    crawled_html %>%
      html_nodes(".ev-content") %>%
      html_text() -> description
    
    description %>%
      str_extract("[0-9]+,-") -> price
    
    #Set description to NA if none is available
    if(description=="Zurück"){
      description <- NA
    }
    
    #get date/time in unformatted form
    crawled_html %>%
      html_nodes(".col-md-6") %>%
      html_text() -> datetime_base
    
    #extract date
    datetime_base[1] %>%
      str_extract("[0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{4}") %>%
      dmy(tz="Europe/Berlin") + (24*60*60)  -> date_start_base
    
    date_start_base %>%
      as.Date() -> date_start
    
    #extract time
    datetime_base[1] %>%
      str_extract("[0-9]{1,2}:[0-9]{1,2}") %>%
      paste0(":00") -> time_start_base
    
    time_start <- try(times(time_start_base))
    
    if("try-error" %in% class(time_start)) {
      time_start <- NA
    }
    
    #get location of event
    crawled_html %>%
      html_nodes(".evadress") %>%
      html_text() -> location_string
    
    #get coordinates from google maps if address available, else choose default cooridnates
    if(!is.na(str_extract(location_string, "[0-9]{5}"))){
      geocode(location_string) -> coordinates
      str_extract(location_string, "[0-9]{5}.*") -> city_base
      str_extract(city_base, "[0-9]{5}") %>%
        as.numeric() -> zip
      str_extract(city_base, "[^0-9]+.+") %>%
        as.character() -> city
      data.frame(coordinates, zip, city) -> location
    }
    
    #if no coordinates found or no address available, use default coordinates
    if(is.na(str_extract(location_string, "[0-9]{5}")) || is.na(coordinates)) {
      lon <- 9.92992
      lat <- 49.7979
      zip <- 97070
      city <- "Wuerzburg"
      data.frame(lon, lat, zip, city) -> location
    }
    
    #ggmaps returns longitude in column "lon", so we have to convert names
    names(location) <- c("lng","lat","zip","city")
    
    #this information is not provided, added to get our common data format
    date_end <- as.Date(NA)
    time_end <- NA
    organizer <- "Juliusspital Würzburg"
    #bind information and cast
    cbind(title,date_start,time_start,date_end,time_end,description,price,url,location, organizer) -> cast
  }
  
  julius_get_event_data(julius_full_event_urls_list[3]) -> test
  
  #get all event infos and output a data frame
  #may take a LONG time to crawl due to high number of events (~200 for 18 months when created)
  
  #don't display type coercion warnings
  options(warn=-1)
  map_df(julius_full_event_urls_list,julius_get_event_data) -> julius_event_df_final
  options(warn=0)
  
  julius_event_df_final = julius_event_df_final %>%
    mutate(organizer = as.character(organizer),
           time_end = NULL,
           date_end = NULL)
  
  return(julius_event_df_final)
}

dfToscanasaal = getToscanasaalEvents()
dfHerberge = getHerbergeEvents()
dfAirport = getAirportEvents()
dfIGZ = getIGZEvents()
dfArteNoah = getArteNoahEvents()
dfTrinitatis = getTrinitatisEvents()
dfFrankenMuseum = getFrankenMusemEvents()
dfRoentgen = getRoentgenEvents()
dfDeutschhausKirche = getDeutschhausKircheEvents()
dfKompetenzzentrum = getKompetenzzentrumEvents()
dfNeunerplatzTheater = getNeunerplatzTheaterEvents()
dfBotanischerGarten = getBotanischerGartenEvents()
dfOdeonLounge = odeoncrawler()
dfVogelConventionCenter = vogelcrawler()
dfKellerperle = kellerperlencrawler()
dfJuliusSpital = getJuliusSpitalEvents()

dfCombined = bind_rows(
  dfHerberge,
  dfIGZ,
  dfToscanasaal,
  dfAirport,
  dfArteNoah,
  dfTrinitatis,
  dfFrankenMuseum,
  dfRoentgen,
  dfDeutschhausKirche,
  dfKompetenzzentrum,
  dfNeunerplatzTheater,
  dfBotanischerGarten,
  dfOdeonLounge,
  dfVogelConventionCenter,
  dfKellerperle,
  dfJuliusSpital
)

dfCombined$time_start = times(dfCombined$time_start)
dfCombined$time_end = times(dfCombined$time_end)

# Export Events-Data-Frame to csv
write.csv2(dfCombined, file = "Events_Gruppe_1.csv")
