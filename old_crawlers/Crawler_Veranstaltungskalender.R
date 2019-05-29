library(RMariaDB)
library(dbConnect)
library(tidyverse)
library(rvest)
library(xml2)
library(stringr)
library(tidyr)
library(knitr)
library(chron)
library(lubridate)
library(plyr)
library(qdapRegex)
library(RSelenium)
library(rowr)
library(purrr)
library(gsubfn)
library(RJSONIO)


###### Crawler ######


getEventAttribute = function(event, node) {
  html_node(event, node) %>%
    html_text()
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
      price = as.character(price),
      time_start=NA,
      time_end=NA
      
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
  remDr$close()
  rm(rD)
  gc()
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
  description = c()
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
      
      description[i]= s %>%
        read_html() %>%
        html_node('tr:nth-child(1) .mod_n') %>%
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
        zip = 97070,
        price=NA,
        description=NA
        
      )
  } else {
    return(NULL)
  }
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
      time_start = NA,
      time_end = NA,
      price = NA,
      street = as.character(street),
      city = as.character(city),
      organizer = as.character(organizer),
      date_start = as.Date(date_start, origin = '1970-01-01'),
      date_end = NA
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
  remDr$close()
  rm(rD)
  gc()
  
  return(df)
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
           time_start = NA,
           time_end = NA,
           organizer = as.character(organizer),
           price = NA,
           city = as.character(city)
    )
  return(df)
  
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
    time_start = time_start,
    time_end = NA,
    price=NA,
    organizer="Botanischer Garten"
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
getJuliusSpitalEvents = function() {
  print("Julius")
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
    # if(!is.na(str_extract(location_string, "[0-9]{5}"))){
    #   geocode(location_string) -> coordinates
    #   str_extract(location_string, "[0-9]{5}.*") -> city_base
    #   str_extract(city_base, "[0-9]{5}") %>%
    #     as.numeric() -> zip
    #   str_extract(city_base, "[^0-9]+.+") %>%
    #     as.character() -> city
    #   data.frame(coordinates, zip, city) -> location
    # }
    
    #if no coordinates found or no address available, use default coordinates
    # if(is.na(str_extract(location_string, "[0-9]{5}")) || is.na(coordinates)) {
    #   lon <- 9.92992
    #   lat <- 49.7979
    #   zip <- 97070
    #   city <- "Wuerzburg"
    #   data.frame(lon, lat, zip, city) -> location
    # }
    
    lon <- 9.92992
    lat <- 49.7979
    zip <- 97070
    city <- "Wuerzburg"
    data.frame(lon, lat, zip, city) -> location
    
    
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
           time_end = NA,
           date_end = NA,
           street = "Juliuspromenade 19, 97070 Würzburg")
  
  return(julius_event_df_final)
} 
getCairo=function() {
  print("Cairo")
  jugendkulturhaus <- "https://cairo.wue.de/"
  
  jugendkulturhaus %>%
    read_html() -> raw_data
  
  raw_data %>%
    html_nodes("time") %>%
    html_text() -> datum4
  
  raw_data %>%
    html_nodes("#block-views-block-veranstaltungen-frontpage-block-1 .titlefr") %>%
    html_text() -> title4
  
  lat <- c(49.78961)
  lng <- c(9.92466)
  
  link <- "https://cairo.wue.de/"
  link <- as.character(link)
  
  city <- ("Wuerzburg")
  city <- as.character(city)
  
  street <- ("Fred-Joseph-Platz 3")
  street <- as.character(street)
  
  zip <- c(97082)
  
  organizer <- ("Jugendkulturhaus Cairo")
  organizer <- as.character(organizer)
  
  veranstaltungenjugendkulturhaus <- data.frame(title = title4, url = link, date_start = datum4, lat = lat, lng = lng, city = city, street = street, zip = zip)
  
  veranstaltungenjugendkulturhaus$date_start <- paste0(datum4, ".2018")
  gsub("[A-z]{1,10}","", veranstaltungenjugendkulturhaus$date_start) -> veranstaltungenjugendkulturhaus$date_start
  veranstaltungenjugendkulturhaus$date_start <- str_extract(veranstaltungenjugendkulturhaus$date_start,"[0-9]{1,2}/[0-9]{1,2}.[0-9]{1,4}")
  
  gsub("/", ".", veranstaltungenjugendkulturhaus$date_start, fixed = TRUE) -> date_start
  
  date_start1 <- as.Date(date_start, format = "%d.%m.%Y")
  
  veranstaltungenjugendkulturhaus <- data.frame(title = title4, url = link, description = NA, lng = lng, lat = lat, city = city, street = street, zip = zip, date_start = date_start1, date_end = NA, time_start = NA, time_end = NA, price = NA, organizer = organizer)
  return(veranstaltungenjugendkulturhaus)
}
getBuergerbraeu = function(){
  print("BürgerbrÃ¤u")
  buergerbraeu <- "https://www.buergerbraeu-wuerzburg.de/aktuelles-archive/was-kommt.html"
  
  buergerbraeu %>%
    read_html() -> raw_data
  
  raw_data %>%
    html_nodes("time") %>%
    html_text() -> datum5
  dfdatum5 <- data.frame(datum5)
  
  raw_data %>%
    html_nodes(".first-headline span") %>%
    html_text() -> title5
  dftitle5 <- data.frame(title5)
  
  raw_data %>%
    html_nodes(".teaser-text p") %>%
    html_text() -> description
  
  lat <- 49.7937172
  lon <- 9.894353
  
  link <- "https://www.buergerbraeu-wuerzburg.de/aktuelles-archive/was-kommt.html"
  link <- as.character(link)
  
  city <- ("Wuerzburg")
  city <- as.character(city)
  
  street <- ("Frankfurter Strasse 87")
  street <- as.character(street)
  
  zip <- c(97082)
  
  organizer <- ("BürgerbrÃ¤u")
  organizer <- as.character(organizer)
  
  veranstaltungenbuergerbraeu <- data.frame(title = title5, url = link, date_start = datum5, date_end = NA, time_end = NA, description = description, lat = lat, lng = lon, city = city, street = street, zip = zip, organizer = organizer, price = NA)
  
  
  veranstaltungenbuergerbraeu$time_start <- str_extract(veranstaltungenbuergerbraeu$date_start,"[0-9]{1,2}:[0-9]{1,2}")
  veranstaltungenbuergerbraeu$date_start <- str_extract(veranstaltungenbuergerbraeu$date_start,"[0-9]{1,2}.[0-9]{1,2}.[0-9]{1,4}")
  
  date_start <- veranstaltungenbuergerbraeu$date_start
  time_start <- veranstaltungenbuergerbraeu$time_start
  time_start <- as.character(time_start)
  
  
  date_start <- as.Date(date_start, format = "%d.%m.%Y")
  
  time_start <- times(paste0(time_start, ":00"))
  
  
  veranstaltungenbuergerbraeu <- data.frame(title = title5, url = link, description = description, lng = lon, lat = lat, city = city, street = street, zip = zip, date_start = date_start, date_end = NA, time_start = time_start, time_end = NA, price = NA, organizer = organizer)
  
}
getMainfrankenTheater = function() {
  print("Mainfrankentheater")
  mft_url <- "https://www.theaterwuerzburg.de/index.php?option=com_mftplayground&view=repertoires&Itemid=116"
  mft_url %>%
    read_html() %>%
    html_nodes("#spielplan")  %>%
    map_df(~list(Event = html_nodes(.x, ".repdate") %>%
                   html_text(trim = T) %>%
                   {if(length(.) == 0) NA else .},
                 title = html_nodes(.x, ".title") %>%
                   html_text(trim = T) %>%
                   {if(length(.) == 0) NA else .})) ->mft_df
  #Clean and edit data 
  mft_df$date_start <- str_extract(mft_df$Event, "[0-9]{1,2}[\\.][0-9]{1,2}[\\.]")
  mft_df$date_start[mft_df$date_start == "character(0)"] <- NA
  mft_df$Event <- gsub("[A-Za-z]{1,2} \\|{1} [0-9]{1,2}[\\.][0-9]{1,2}[\\.]", "", mft_df$Event) #remove Mo und date
  mft_df$Ort <- str_extract_all(mft_df$Event, "^[a-zA-Z???????[:punct:][:space:]]{1,50}")
  mft_df$Einfuehrung <- str_extract_all(mft_df$Event, "(?<=\\|{1}) (EF:){1} [0-9]{1,2}[\\.]{1}[0-9]{1,2}") #?<= suche aber matche nicht
  mft_df$Einfuehrung[mft_df$Einfuehrung == "character(0)"] <- NA
  mft_df$Event <- gsub("\\|{1} (EF:){1} [0-9]{1,2}[\\.]{1}[0-9]{1,2}", "", mft_df$Event)
  mft_df$Uhrzeit <- str_extract_all(mft_df$Event, "[0-9]{1,2}[\\.]{1}[0-9]{1,2}(.*[0-9]{1,2}[\\.]{1}[0-9]{1,2})?")
  mft_df$time_start <- str_extract(mft_df$Uhrzeit, "[0-9]{1,2}[\\.]{1}[0-9]{1,2}")
  mft_df$time_end <- str_extract(mft_df$Uhrzeit, "(?<=ca\\. )[0-9]{1,2}[\\.]{1}[0-9]{1,2}")
  mft_df$description <- str_extract_all(mft_df$Event, "(?<=(.{0,100}\\|){2,3}).{0,100}") #?<= bracht finite Anzahl an WErten kein * etc
  mft_df <- unite(mft_df, description, c(Einfuehrung, Ort, description), remove = T)
  mft_df$description <- str_remove_all(mft_df$description, "NA_")
  mft_df_clean <- subset(mft_df, select = -c(Event, Uhrzeit))
  mft_df_clean$date_end <- NA
  
  #Formatierung
  sysyear <- year(Sys.Date())
  mft_df_clean$date_start <- as.Date(mft_df_clean$date_start, format = "%d.%m.")
  year(mft_df_clean$date_start) <- ifelse(month(mft_df_clean$date_start) < month(Sys.Date()), 
                                          year(mft_df_clean$date_start) <- sysyear+1,
                                          year(mft_df_clean$date_start) <- sysyear) 
  mft_df_clean$date_end <- as.Date(mft_df_clean$date_end)
  mft_df_clean$time_start %>%
    str_replace_all("\\.", ":") %>%
    paste0(":00") %>%
    times() -> mft_df_clean$time_start
  mft_df_clean$time_end %>%str_replace_all("\\.", ":") %>%
    paste0(":00") %>%
    times() -> mft_df_clean$time_end
  
  
  #erweitern mit liste
  
  mft_df_final <- data.frame(mft_df_clean, organizer = "Mainfranken Theater Würzburg",
                             street = "Theaterstrasse 21", zip = 97070, url = "http://www.mainfrankentheater.de/",
                             lat = 49.79541, lng = 9.937 ,stringsAsFactors=FALSE)
  mft_df_final$organizer <- as.character(mft_df_final$organizer)
  mft_df_final$street <- as.character(mft_df_final$street)
  mft_df_final$zip <- as.numeric(mft_df_final$zip)
  mft_df_final$url <- as.character(mft_df_final$url)
  mft_df_final$price <- as.character(NA) 
  mft_df_final$city <- "Wuerzburg"
  
  return(mft_df_final)
}
getBBK = function() {
  print("BBK")
  bbk_url <- "http://www.bbk-unterfranken.de/ausstellungen_bbk.html"
  bbk_url %>%
    read_html() %>%
    html_nodes(".box-info") %>% #W?hle gesamtes Ereignis
    map_df(~list(description = html_nodes(.x, ".blau") %>%
                   html_text(trim = T) %>%
                   {if(length(.) == 0) NA else .}, #replace legth 0 elements with NA
                 title = html_nodes(.x, ".title") %>%
                   html_text(trim = T) %>%
                   {if(length(.) == 0) NA else .},
                 Datum = html_nodes(.x, ".event-date") %>%
                   html_text(trim = T) %>%
                   {if(length(.) == 0) NA else .}))  -> bbk_df
  #edit data
  bbk_df$Datum <- str_remove_all(bbk_df$Datum, "[A-z]")
  bbk_df$date_start <- unlist(str_extract_all(bbk_df$Datum, "^.{10}"))
  bbk_df$date_end <- gsub(".* -*", "", bbk_df$Datum)  
  bbk_df_clean <- subset(bbk_df, select = -c(Datum))
  bbk_df_clean$time_start <- NA
  bbk_df_clean$time_end <- NA
  
  
  #Formatierung
  bbk_df_clean$date_start <- as.Date(bbk_df_clean$date_start, format = "%d.%m.%y")
  bbk_df_clean$date_end <- as.Date(bbk_df_clean$date_end, format = "%d.%m.%y")
  bbk_df_clean$time_start <- times(bbk_df_clean$time_start)
  bbk_df_clean$time_end <- times(bbk_df_clean$time_end)
  
  
  bbk_df_final <- data.frame(bbk_df_clean, organizer = "BBK-Galerie",
                             street = "Oskar-Laredo-Platz 1", zip = 97078, url = "http://www.bbk-unterfranken.de/",
                             lat = 49.8158268, lng = 9.9102608 ,stringsAsFactors=FALSE)
  bbk_df_final$organizer <- as.character(bbk_df_final$organizer)
  bbk_df_final$street <- as.character(bbk_df_final$street)
  bbk_df_final$zip <- as.numeric(bbk_df_final$zip)
  bbk_df_final$url <- as.character(bbk_df_final$url)
  bbk_df_final$price <- as.character(NA)
  bbk_df_final$city <- "Wuerzburg"
  
  return(bbk_df_final)
}
getFamilienzentrum = function() {
  print("Familienzentrum")
  
  url46 <- "http://www.familienzentrum-wuerzburg.de/termine/"
  
  url46 %>%
    read_html() %>%
    html_nodes(".csc-textpic-text p") %>%
    html_text(trim = T) -> data1
  v2 <- as.data.frame(data1)
  
  
  #edit data
  
  v2$data <- str_remove_all(v2$data1, "[A-z]{2}[\\.]{1}\\s?[0-9]{1,2}[\\.]{1}[0-9]{1,2}[\\.]{1} ")
  v2$title <- str_extract(v2$data, "(?<=- )[A-zÃ¤Ã¶üss[:punct:]\\s]{1,25} ")
  v2$title <- gsub(",","",v2$title)
  
  v2$url <- "http://www.familienzentrum-wuerzburg.de/termine/"
  v2$url <- as.character(v2$url)
  
  v2$data <- str_remove_all(v2$data1, "[[:space:]]{2,100}")
  v2$description <-  str_extract_all(v2$data, "(?<= Uhr)[[:punct:]\\w ]*")
  v2$description[v2$description == "  "|v2$description == "character(0)"|v2$description == ""] <- NA
  #v2$description[v2$description == "character(0)"] <- NA
  #v2$description[v2$description == ""] <- NA
  v2$description=unlist(lapply(v2$description, `[[`, 1))
  
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
  
  v2$price <- NA
  
  v2$time_start <- str_extract(v2$data, "[0-9]{1,2}[\\.]{1}[0-9]{1,2}") 
  v2$time_end <- str_extract(v2$data, "(?<=- )[0-9]{1,2}[\\.]{1}[0-9]{1,2}")
  
  v2$time_start <- gsub("\\.", ":", v2$time_start) 
  v2$time_end <- gsub("\\.",":",v2$time_end)
  
  v2$time_start <- times(paste0(v2$time_start, ":00"))
  v2$time_end <- times(paste0(v2$time_end, ":00"))
  
  v2$data <- str_remove_all(v2$data1, "[[:space:]]{2,100}")
  
  
  v2$organizer <- "Familienzentrum Würzburg e.V."
  v2$organizer <- as.character(v2$organizer)
  
  v2 %>%
    filter(!is.na(title)) -> v2
  
  v2$data1 <- NULL
  v2$data <- NULL
  
  return(v2)
}
getFraunhofer = function() {
  print("Fraunhofer")
  url <- "https://www.isc.fraunhofer.de/de/messen-und-termine/messen-und-termine-2018.html?q=w%C3%BCrzburg#search-events"
  
  getEvents <- function(url) {
    url %>%
      read_html() -> raw_data
    
    raw_data %>%
      html_nodes(".internal") %>%
      html_text()  -> title
    
    raw_data %>%
      html_nodes(".place") %>%
      html_text()  -> ort
    
    raw_data %>%
      html_nodes(".date") %>%
      html_text()  -> datum
    
    df <- data.frame(title=title, Ort=ort, Date=datum)
  }
  
  events_df <- getEvents(url)
  
  ## Organize dataframe
  events_df$organizer <- "Fraunhofer Institut für Silicatforschung ISC"
  events_df$lng <- 9.91769
  events_df$lat <- 49.79763
  events_df$street <- "Neunerplatz 2"
  
  events_df$date_start<- str_extract(events_df$Date, "[0-9]{1,2}[\\.][0-9]{1,2}[\\.](20[0-9]{2})?")
  events_df$date_end <- str_extract(events_df$Date, "(?<=-.{1,2})[0-9]{1,2}[\\.][0-9]{1,2}[\\.]20[0-9]{2}")
  events_df$time_start <- str_extract(events_df$Date, "(?<=,.{1,15})[0-9]{1}")
  events_df$time_end <- str_extract(events_df$Date, "(?<=-.{1,2})[0-9]{1,2}[\\ ]")
  
  # get links of each event - url column
  pg <- read_html(url)
  links <- html_attr(html_nodes(pg, "a"), "href")
  links <- as.vector(links)
  links <- links[grep("^/de/messen-und-termine/messen-und-termine-2018/.*", links)]
  links <- unique(links)
  url_list <- data.frame(paste('https://www.isc.fraunhofer.de/',links,sep=""))
  colnames(url_list) <- c("url")
  
  
  Frau_Ins_events <- data.frame(title = events_df$title, url = url_list, description = NA, lng = events_df$lng,
                                lat= events_df$lat, city= "Wuerzburg", street= events_df$street, zip = 97082, date_start = events_df$date_start, date_end = events_df$date_end,
                                time_start = NA, time_end =NA, price = NA, organizer = events_df$organizer)
  
  ## Change dataformat
  # date
  Frau_Ins_events$date_start <- as.Date(Frau_Ins_events$date_start, "%d.%m.%Y")
  Frau_Ins_events$date_end   <- as.Date(Frau_Ins_events$date_start, "%d.%m.%Y")
  # time
  t <- as.POSIXct(Frau_Ins_events$time_start, tz = "", format = "%H:%M", usetz = FALSE)
  Frau_Ins_events$time_start <-times(format(t, "%H:%M:%S"))
  t <- as.POSIXct(Frau_Ins_events$time_end, tz = "", format = "%H:%M", usetz = FALSE)
  Frau_Ins_events$time_end <- times(format(t, "%H:%M:%S"))
  # to character
  Frau_Ins_events[] <- lapply(Frau_Ins_events, function(x) if (is.factor(x)) as.character(x) else {x})
  
  return(Frau_Ins_events)
  
}
getUniBib = function(){
  
  #### Änderung Adrian ####
  year=as.numeric(format(Sys.Date(), "%Y"))
  years=c(year, year, year, year)
  years[]
  month=as.numeric(format(Sys.Date(), "%m"))
  #month=9
  month1=month+1
  if(month==12){
    month1=1
    years[2]=year+1}
  
  month2=month1+1
  if(month1==12){
    month2=1
    years[3]=year+1}
  if(years[2]==years+1){
    years[3]=years[2]
  }
  
  month3=month2+1
  if(month2==12){
    month3=1
    years[4]=year+1}
  if(years[3]==years+1){
    years[4]=years[3]
  }
  months=c(month, month1, month2, month3)
  
  ####
  
  urls_set <- paste('https://www.bibliothek.uni-wuerzburg.de/ueber-uns/veranstaltungen/veranstaltungskalender/zeitraum/',years,'/',months,'/', sep ="")  
  
  ## ** RSelenium - activate
  rD <- rsDriver()
  remDr <- rD[["client"]]
  
  # Get Values
  title_selector <- ".news-list__item-header a"
  date_selector <- ".news-single__item-value"
  ort_selector <- ".news-list__item-event-location"
  details_selector <- ".news-list__item-value"
  
  getData_sel <- function(url) {
    
    remDr$navigate(urls_set[1])
    site <- read_html(remDr$getPageSource()[[1]])
    # Get Nodes
    site %>%
      html_nodes(".news-list__item") -> node_data
    
    node_data %>%
      html_node(title_selector) %>%
      html_text() -> title
    
    node_data %>%
      html_node(date_selector) %>%
      html_text() -> date
    
    node_data %>%
      html_node(ort_selector) %>%
      html_text() -> ort
    
    node_data %>%
      html_node(details_selector) %>%
      html_text() -> details
    
    # Merge
    df <- data.frame(title = title,
                     url = url,
                     Details = details,
                     Ort = ort,
                     Date = date)
    
    # Clean up
    df %>%
      filter(!is.na(title)) -> df_clean
  }
  
  events_list <- map_df(urls_set,getData_sel)
  # Shut down selenium
  remDr$close()
  rm(rD)
  gc()
  
  ## Organize dataframe
  events_df <- events_list
  events_df$organizer <- "Universitätsbibliothek Würzburg"
  events_df$lng <- 9.9707861
  events_df$lat <- 49.7815829
  events_df$street <- "Am Hubland"
  
  events_df$Ort <- gsub("Ort:", "", events_df$Ort)
  events_df$date_start <- str_extract( events_df$Date, "[0-9]{1,2}[\\.][0-9]{1,2}[\\.](20[0-9]{2})?")
  events_df$date_end <- str_extract(events_df$Date, "(?<=-.{1,2})[0-9]{1,2}[\\.][0-9]{1,2}[\\.]20[0-9]{2}")
  events_df$time_start <- str_extract(events_df$Date, "[0-9]{1,2}[\\:][0-9]{1,2}")
  events_df$time_end <- str_extract_all(events_df$Date, "(?<=-.{1,15})[0-9]{1,2}[\\:][0-9]{1,2}")
  
  bib_events <- data.frame(title = events_df$title, url = events_df$url, description = events_df$Details, lng = events_df$lng,
                           lat= events_df$lat, city= "Wuerzburg", street= events_df$street, zip = 97074, date_start = events_df$date_start, date_end = events_df$date_end,
                           time_start = events_df$time_start, time_end = as.character(events_df$time_end), price = NA, organizer = events_df$organizer)
  
  ## Change dataformat
  # date
  bib_events$date_start <- as.Date(bib_events$date_start, "%d.%m.%Y")
  bib_events$date_end   <- as.Date(bib_events$date_start, "%d.%m.%Y")
  # time
  t <- as.POSIXct(bib_events$time_start, tz = "", format = "%H:%M", usetz = FALSE)
  bib_events$time_start <-times(format(t, "%H:%M:%S"))
  t <- as.POSIXct(bib_events$time_end, tz = "", format = "%H:%M", usetz = FALSE)
  bib_events$time_end <- times(format(t, "%H:%M:%S"))
  # to character
  bib_events[] <- lapply(bib_events, function(x) if (is.factor(x)) as.character(x) else {x})
  return(bib_events)
  
}
getStStephan = function() {
  sts_get <- function(url){
    url %>%
      read_html() %>%
      html_nodes("#et_content_container div") %>% #W?hle gesamtes Ereignis
      map_df(~list(date = html_nodes(.x, ".et_content_date") %>%
                     html_text(trim = T) %>%
                     {if(length(.) == 0) NA else .}, #replace legth 0 elements with NA
                   title = html_nodes(.x, ".et_link_title") %>%
                     html_text(trim = T) %>%
                     {if(length(.) == 0) NA else .},
                   city = html_nodes(.x, ".et_city") %>%
                     html_text(trim = T) %>%
                     {if(length(.) == 0) NA else .},
                   description = html_nodes(.x, ".et_placename") %>%
                     html_text(trim = T) %>%
                     {if(length(.) == 0) NA else .})) -> df
    df_clean <- df[rowSums(is.na(df)) < 3,] #keep rows with < 2 NAs
  }
  sts_urls <- paste0("http://wuerzburg-ststephan.de/gemeindeleben/?pageID=", 
                     1:11, "&until=yes")
  map_df(sts_urls, sts_get) -> sts_df
  
  #Clean Data
  sts_df$date <- str_remove_all(sts_df$date, "[A-Z]|[?]")
  sts_df$date_start <- str_extract(sts_df$date, "[0-9]{1,2}\\.[0-9]{1,2}\\.")
  sts_df$date <- str_remove(sts_df$date, "[0-9]{1,2}\\.[0-9]{1,2}\\.")
  sts_df$date_end <- str_extract_all(sts_df$date, "[0-9]{1,2}\\.[0-9]{1,2}\\.")
  sts_df$date_end[sts_df$date_end == "character(0)"] <- NA
  sts_df$time_start <- str_extract(sts_df$date, "[0-9]{1,2}(:[0-9]{1,2})?")
  sts_df$time_end <- str_extract(sts_df$date, "(?<=-)[0-9]{1,2}(:[0-9]{1,2})?")
  filter(sts_df, !is.na(date_start)) -> sts_df
  sts_df_clean <- subset(sts_df, select = -c(date))
  
  
  #formate dates (umwandeln und jahr anpassen)
  sysyear <- year(Sys.Date())
  sts_df_clean$date_start <- as.Date(sts_df_clean$date_start, format = "%d.%m.")
  year(sts_df_clean$date_start) <- ifelse(month(sts_df_clean$date_start) < month(Sys.Date()), 
                                          year(sts_df_clean$date_start) <- sysyear+1,
                                          year(sts_df_clean$date_start) <- sysyear) 
  sts_df_clean$date_end <- unlist(sts_df_clean$date_end)
  sts_df_clean$date_end <- as.Date(sts_df_clean$date_end, format = "%d.%m.")
  year(sts_df_clean$date_end) <- ifelse(month(sts_df_clean$date_end) < month(Sys.Date()), 
                                        year(sts_df_clean$date_end) <- sysyear+1,
                                        year(sts_df_clean$date_end) <- sysyear) 
  
  sts_df_clean$time_start <- ifelse(is.na(str_extract(sts_df_clean$time_start, ":")),
                                    paste0(sts_df_clean$time_start, ":00"), 
                                    sts_df_clean$time_start)
  sts_df_clean$time_start <- ifelse(is.na(str_extract(sts_df_clean$time_start, ".{2}:")),
                                    paste0("0", sts_df_clean$time_start),
                                    sts_df_clean$time_start)
  sts_df_clean$time_start <- times(paste0(sts_df_clean$time_start, ":00"))
  sts_df_clean$time_end <- ifelse(is.na(str_extract(sts_df_clean$time_end, ":")),
                                  paste0(sts_df_clean$time_end, ":00"), 
                                  sts_df_clean$time_end)
  sts_df_clean$time_end <- ifelse(is.na(str_extract(sts_df_clean$time_end, ".{2}:")),
                                  paste0("0", sts_df_clean$time_end),
                                  sts_df_clean$time_end)
  sts_df_clean$time_end <- times(paste0(sts_df_clean$time_end, ":00"))
  
  #erweitern mit liste
  sts_df_final <- data.frame(sts_df_clean, organizer = "St. Stephan",
                             street = "Wilhelm-Schwinn-Platz 1", zip = 97070, url = "http://wuerzburg-ststephan.de/gemeindeleben/",
                             lat = 49.7894, lng = 9.9348 ,stringsAsFactors=FALSE)
  sts_df_final$organizer <- as.character(sts_df_final$organizer)
  sts_df_final$street <- as.character(sts_df_final$street)
  sts_df_final$zip <- as.numeric(sts_df_final$zip)
  sts_df_final$url <- as.character(sts_df_final$url)
  sts_df_final$lat <- as.numeric(levels(sts_df_final$lat))[sts_df_final$lat]
  sts_df_final$lng <- as.numeric(levels(sts_df_final$lng))[sts_df_final$lng]
  sts_df_final$price <- as.character(NA)
  
  return(sts_df_final)
}
getHFM = function() {
  hfm_url <- "http://www.hfm-wuerzburg.de/veranstaltungen"
  
  rD = rsDriver()
  remDr = rD[["client"]]
  remDr$navigate(hfm_url)
  
  
  #click "mehr zeigen"
  i <- 1
  run <- T
  while (run) {
    tryCatch(remDr$findElement(using = "css selector", ".more-link")$clickElement(),
             error = function(c){run <<- F},
             warning = function(w){run <<- F},
             finally = print(paste("Pressed button", i, "times"))
    )
    i <- i + 1
    Sys.sleep(5)
  }
  
  #Get HTML, Nodes and Values
  site_hfm <- read_html(remDr$getPageSource()[[1]])
  site_hfm %>%
    html_nodes(".cntblk") %>% #Wähle gesamtes Ereignis
    map_df(~list(title = html_nodes(.x, "h2") %>%
                   html_text(trim = T) %>%
                   {if(length(.) == 0) NA else .}, #replace legth 0 elements with NA
                 description = html_nodes(.x, ".alist-item-body") %>%
                   html_text(trim = T) %>%
                   {if(length(.) == 0) NA else .})) -> hfm_df 
  remDr$close
  rm(rD)
  gc
  
  #clean data
  hfm_df$date_start <-  str_extract_all(hfm_df$description, "[0-9]{1,2}[\\.] [a-zA-z]{1,9} (20[1-2]{1}[0-9]{1})?")
  hfm_df$time_start <-  str_extract_all(hfm_df$description, "[0-9]{1,2}[\\:][0-9]{1,2} ")
  hfm_df$description <- str_extract_all(hfm_df$description, "(?<=Uhr).*") 
  hfm_df %>%
    filter(hfm_df$date_start != "character(0)") ->hfm_df_clean
  hfm_df_clean$date_end <- as.Date(NA)
  hfm_df_clean$time_end <- NA
  hfm_df_clean$price <- as.character(NA)
  
  #formate dates (umwandeln und jahr anpassen)
  hfm_df_clean$date_start <- str_replace_all(hfm_df_clean$date_start, c("Januar" = "01", "Februar" = "02", "März" = "03",
                                                                        "April" = "04","Mai" = "05","Juni" = "06",
                                                                        "Juli" = "07","August" = "08","September" = "09",
                                                                        "Oktober" = "10","November" = "11","Dezember" = "12"))
  hfm_df_clean$date_start <- as.Date(hfm_df_clean$date_start, format = "%d. %m %Y")
  hfm_df_clean$time_start <- times(paste0(hfm_df_clean$time_start, ":00"))
  hfm_df_clean$time_end <- times(hfm_df_clean$time_end)
  hfm_df_final <- data.frame(hfm_df_clean, organizer = "Hochschule für Musik",
                             street = "Ebracher Gasse 1", zip = 97070, url = "http://www.hfm-wuerzburg.de",
                             lat = 49.79247, lng = 9.93394 ,stringsAsFactors=FALSE)
  hfm_df_final$organizer <- as.character(hfm_df_final$organizer)
  hfm_df_final$street <- as.character(hfm_df_final$street)
  hfm_df_final$zip <- as.numeric(hfm_df_final$zip)
  hfm_df_final$url <- as.character(hfm_df_final$url)
  hfm_df_final$lat <- as.numeric(levels(hfm_df_final$lat))[hfm_df_final$lat]
  hfm_df_final$lng <- as.numeric(levels(hfm_df_final$lng))[hfm_df_final$lng]
  #hfm_df_final$description <- unlist(map(hfm_df_final$description, 1))
  
  #unlist(hfm_df_final$description)
  hfm_df_final$city <- "Wuerzburg"
  
  return(hfm_df_final)
}
getGnadenkirche = function(){
  print("Gnadenkirche")
  url <- "http://www.gnadenkirche-wuerzburg.de/Termine.html"
  Eventtabelle <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(Eventtabelle) <- c("X1","X2","Uhrzeit","art")
  
  count <- c(1:10)
  count2 <- c(1:10)
  
  bereiche = function(j,url){
    
    url %>%
      read_html() -> raw_data
    colnames(Eventtabelle) <- c("X1","X2")
    string2 <- as.character(paste('//*[@id="article-290"]/div/div[',j,']', sep = ""))
    string2
    raw_data2 <- raw_data
    raw_data2 %>%
      html_nodes(xpath = string2) -> raw_data3
    return(raw_data3)
  }       
  
  tabeling = function(i,raw_data3){ 
    
    raw_data3 %>%
      html_node("h2") %>%
      html_text() -> vsb
    vsb <- data.frame(vsb)
    uhrzeit <- as.character(str_extract_all(vsb$vsb,"[0-9]{2}:[0-9]{2}"))
    art <-  gsub("\\s*\\([^\\)]+\\)","",as.character(vsb$vsb))
    
    string <- as.character(paste("table[",i+1,"]", sep = ""))
    raw_data3 %>%
      html_nodes(xpath = string) %>%
      html_table() -> text
    text <- data.frame(text)
    text$uhrzeit <- rep(uhrzeit,nrow(text)) 
    text$art <- rep(art,nrow(text))
    
    return(text)
  }  
  
  for (j in count2) {
    raw_data3 = bereiche(j,url)
    for (i in count) {
      text <- tabeling(i,raw_data3)
      Eventtabelle <- rbind(Eventtabelle,text)
      assign('Eventtabelle',Eventtabelle,envir=.GlobalEnv)
    }
  }
  Eventtabelle$X1 <- as.character(str_extract(Eventtabelle$X1,"[0-9]{2}.[0-9]{2}"))
  
  
  # --- Einlesen des unteren Bereiches
  
  Eventtabelle2 <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(Eventtabelle2) <- c("X1","X2","Uhrzeit","art")
  
  url %>%
    read_html() -> raw_data
  
  raw_data %>%
    html_node('.ce_text.last') %>%
    html_node('table') %>%
    html_table() -> vsbb
  vsbb <- data.frame(vsbb)
  
  count <-nrow(vsbb)
  
  for (i in 1:count) {
    test <- vsbb$X2[i]
    datum <- str_extract_all(test,"[0-3][0-9][^ :-][0-3][0-9]")
    date <- unlist(datum)
    
    zaehler <- character(0)
    if(identical(date, character(0))){
      if(str_detect(test, "Sonntag")){
        searchday <- 7
      }
      if(str_detect(test, "Samstag")){
        searchday <- 6
      }
      if(str_detect(test, "Freitag")){
        searchday <- 5
      }
      if(str_detect(test, "Donnerstag")){
        searchday <- 4
      }
      if(str_detect(test, "Mittwoch")){
        searchday <- 3
      }
      if(str_detect(test, "Dienstag")){
        searchday <- 2
      }
      if(str_detect(test, "Montag")){
        searchday <- 1
      }
      heute <- Sys.Date()
      zaehler <- heute
      numdayh <- wday(zaehler)
      numnaechst <- searchday - numdayh +1
      datenaechst <- zaehler + numnaechst
      zaehler <- datenaechst
      day <- substring(datenaechst, 9,10)
      monat <- substring(datenaechst, 6,7)
      naechstevent <- paste0(day,'.',monat,collapse = NULL)
      date <- c(date, naechstevent)
    }
    
    text <- vsbb$X1[i]
    
    uhrzeit <- as.character(str_extract(test,"[0-9]+.[0-9]{2}-[0-9]{2}.[0-9]{2}"))
    
    if (is.na(uhrzeit)) {
      uhrzeit <- as.character(str_extract(test,"[0-9]+.[0-9]{2}"))
    }
    Beschreibung <- NA
    bla <- cbind.fill(date, Beschreibung, uhrzeit,text)
    colnames(bla) <- c("X1","X2","uhrzeit","art")
    Eventtabelle2 <- rbind(Eventtabelle2,bla)
  }
  Eventtabelle2 <- Eventtabelle2[-nrow(Eventtabelle2),] 
  
  #----------------------------------------------------------------------
  KompletteEventtabelle <- rbind(Eventtabelle,Eventtabelle2)
  colnames(KompletteEventtabelle) <- c("date_start","description","Uhrzeit","title")
  KompletteEventtabelle$date_start <-  gsub(",",".",KompletteEventtabelle$date_start)
  KompletteEventtabelle$city <- rep("Würzburg",nrow(KompletteEventtabelle))
  KompletteEventtabelle$zip <- rep(97072,nrow(KompletteEventtabelle))
  KompletteEventtabelle$street <- rep("Danziger Strasse 10",nrow(KompletteEventtabelle))
  KompletteEventtabelle$lng <- rep(9.93654,nrow(KompletteEventtabelle)) 
  KompletteEventtabelle$lat <- rep(49.77727,nrow(KompletteEventtabelle))
  KompletteEventtabelle$url <- rep("http://www.gnadenkirche-wuerzburg.de/Termine.html",nrow(KompletteEventtabelle))
  KompletteEventtabelle$price <- rep(NA,nrow(KompletteEventtabelle))
  KompletteEventtabelle$organizer <- rep("Gnadenkirche Würzburg",nrow(KompletteEventtabelle))
  
  
  KompletteEventtabelle <- separate(data = KompletteEventtabelle, col = Uhrzeit, into = c("time_start","time_end"), sep = "-")
  KompletteEventtabelle <- separate(data = KompletteEventtabelle, col = date_start, into = c("date_start","date_end"), sep = "-")
  
  KompletteEventtabelle$date_start <- as.Date(KompletteEventtabelle$date_start, format= "%d.%m")
  KompletteEventtabelle$date_end <- as.Date(KompletteEventtabelle$date_end, format= "%d.%m")
  
  KompletteEventtabelle$time_start <- paste(KompletteEventtabelle$time_start,':00', sep = '')
  #KompletteEventtabelle$Startzeit <- times(KompletteEventtabelle$Startzeit)
  
  for (i in 1:nrow(KompletteEventtabelle)) {
    
    if(is.na(KompletteEventtabelle$date_end[i])){
      KompletteEventtabelle$date_end[i] <- KompletteEventtabelle$date_start[i]
    }
    assign('KompletteEventtabelle',KompletteEventtabelle,envir = .GlobalEnv)
    
  }
  for (i in 1:nrow(KompletteEventtabelle)) {
    
    if(!is.na(KompletteEventtabelle$time_end[i])){
      KompletteEventtabelle$time_end[i] <- paste(KompletteEventtabelle$time_end[i],':00', sep = '')
      KompletteEventtabelle$time_end[i] <- as.character(times(KompletteEventtabelle$time_end[i]))
      
    }
  }
  
  return(KompletteEventtabelle)
}
getLindleinsmühle = function(){
  
  li_url <- "http://lindleinsmuehle.pg-albert-jakobus.de/neuaktuelles/#page1"
  li_titel_voll <- vector()
  li_datum_voll <- vector()
  li_links_voll <- vector()
  
  rD <- rsDriver()
  remDr <- rD[["client"]]
  remDr$navigate(li_url)
  
  run <- TRUE
  i <- 1
  while (run){
    tryCatch(
      remDr$findElement(using = 'css selector', ".nav-next .ui-icons-block")$clickElement(),
      error= function(c) {run <<- F},
      warning = function(w) {run <<- F},
      finally = print(paste("Pressed button", i, "times"))
    )
    raw_data <- read_html(remDr$getPageSource()[[1]])
    
    raw_data %>%
      html_nodes("#eventslist .itemtitle a") %>%
      html_text(trim = T) %>%
      ifelse(. == "", NA, .) -> li_titel
    li_titel_voll <- c(li_titel_voll, li_titel)
    
    raw_data %>%
      html_nodes(".datetime") %>%
      html_text(trim = T) %>%
      ifelse(. == "", NA, .) -> li_datum
    li_datum_voll <- c(li_datum_voll, li_datum)
    
    raw_data %>%
      html_nodes("#eventslist .itemtitle a") %>%
      html_attr("href")-> li_links
    li_links_voll <- c(li_links_voll, li_links)
    
    i <- i + 1
    Sys.sleep(5)
    
  }
  
  # Shut down selenium
  remDr$close()
  rm(rD)
  gc()
  
  #Links erweitern
  li_links_voll <- paste0("http:", li_links_voll)
  
  
  
  #Kopfzeile
  li_url%>%
    read_html() %>%
    html_nodes(".calendarcontainer .itemtitle a") %>%
    html_text() -> titel2
  if(identical(titel2, character(0))) {
    titel2 <- NA
  }
  
  li_url %>%
    read_html() %>%
    html_nodes(".calendarcontainer .date , .function") %>%
    html_text(trim = T) -> datum2
  if(identical(datum2, character(0))){
    datum2 <- NA
  }
  
  li_url %>%
    read_html() %>%
    html_nodes(".calendarcontainer .itemtitle a") %>%
    html_attr("href")-> links2
  if(identical(links2, character(0))){
    links2 <- NA
  }
  
  
  
  links2 <- paste0("http:", links2)
  datum2 <- cbind(Datum = datum2[1],Startuhrzeit = datum2[2])
  
  
  #Data frame Tabelle
  df_li <- data.frame(title= li_titel_voll, url = li_links_voll, date = li_datum_voll, adresse ="Frankenstrasse 21, 97078 Wuerzburg", lat = 49.8084818 , lng = 9.9634205, organizer = "Pfarrei St. Albert W?rzburg - Lindleinsm?hle")
  
  
  #Data frame Kopfzeile
  df_li2 <- data.frame(title = titel2, url = links2, date = datum2, adresse ="Frankenstrasse 21, 97078 Wuerzburg", lat = 49.8084818 , lng = 9.9634205, organizer = "Pfarrei St. Albert Wuerzburg - Lindleinsmuehle")
  
  colnames(df_li2)[4] <- "time_start"
  colnames(df_li2)[3] <- "date"  
  
  #Spalte Datum aufteilen
  df_li<-separate(data=df_li, col=date, into=c("date", "time_start"), sep = "\\,")
  
  #Dataframes verbinden
  df_li_to <- rbind(df_li2, df_li)
  
  #Erste Zeile, falls leer l?schen
  if(is.na(df_li_to$title[1])){
    df_li_to <- df_li_to[-1,]
  }
  
  #Spalte Datum in Start- und Enddatum aufteilen
  df_li_to <- separate(data=df_li_to, col=date, into=c("date_start", "date_end"), sep = "bis ")
  
  #Spalte adresse aufspalten in city, street, zip
  df_li_to <- separate(data=df_li_to, col=adresse, into=c("street", "zipcity"), sep="\\, ")
  df_li_to$zip<- str_extract_all(df_li_to$zipcity, "([0-9]+)")
  df_li_to$city<- str_extract_all(df_li_to$zipcity, "([A-z,?]+)")
  
  df_li_to$date_start<- str_replace_all(df_li_to$date_start, "[A-z]*[\\,] ","")
  
  
  #Spalte Beschreibung einf?gen
  df_li_to$time_end <- NA
  df_li_to$price <- NA
  df_li_to$description <- NA
  df_li_to <- df_li_to[c("title", "url", "description", "lng","lat", "city", "street", "zip", "date_start", "date_end", "time_start", "time_end", "price", "organizer")]
  
  
  #Enddatum = Startdatum falls na
  for (i in 1:nrow(df_li_to)){
    if (is.na(df_li_to[i,]$date_end)){
      df_li_to[i,]$date_end<- df_li_to[i,]$date_start
    }
  }
  
  # Enduhrzeit = Startuhrzeit falls na
  for (i in 1:nrow(df_li_to)){
    if (is.na(df_li_to[i,]$time_end)){
      df_li_to[i,]$time_end <- as.character(df_li_to[i,]$time_start)
    }
  }
  
  #Zip to numeric
  df_li_to$zip <- as.numeric(df_li_to$zip)
  
  #dates to date Format
  df_li_to$date_end <- as.Date(df_li_to$date_end,format='%d.%m.%Y')
  df_li_to$date_start <- as.Date(df_li_to$date_start,format='%d.%m.%Y')
  
  #time to times Format
  df_li_to$time_end <- str_extract_all(df_li_to$time_end,"[0-9]{2}:[0-9]{2}")
  for (i in 1:nrow(df_li_to)){
    if (is.na(df_li_to[i,]$time_end)){
    }
    else {
      df_li_to[i,]$time_end<- paste0(df_li_to[i,]$time_end,":00")
    }
  }
  df_li_to$time_end <- times(df_li_to$time_end)
  
  df_li_to$time_start <- str_extract_all(df_li_to$time_start,"[0-9]{2}:[0-9]{2}")
  for (i in 1:nrow(df_li_to)){
    if (is.na(df_li_to[i,]$time_start)){
    }
    else {
      df_li_to[i,]$time_start<- paste0(df_li_to[i,]$time_start,":00")
    }
  }
  df_li_to$time_start <- times(df_li_to$time_start)
  
  #----------------------------------------------------------------------
  return(df_li_to)
  
}
getIHK = function() {
  
  mh_url <- "https://www.wuerzburg.ihk.de/veranstaltungen.html?seite=1"
  title_voll <- vector()
  datum_voll <- vector()
  ort_voll <- vector()
  text_voll <- vector()
  link_voll1 <- vector()
  
  rD <- rsDriver()
  remDr <- rD[["client"]]
  remDr$navigate(mh_url)
  
  run <- TRUE
  i <- 1
  while (i<=50){
    tryCatch(
      remDr$findElement(using = 'css selector', "#bottom_pbrowser li:nth-child(12) a")$clickElement(),
      error= function(c) {run <<- F},
      warning = function(w) {run <<- F},
      finally = print(paste("Pressed button", i, "times"))
    )
    
    raw_data <- read_html(remDr$getPageSource()[[1]])
    
    raw_data %>% html_nodes("#eventlist li") -> node_data3
    
    title_selector3 <- "h4"
    datum_selector3 <- ".calendar-event-date"
    ort_selector3 <- ".calendar-event-datelocation"
    text_selector3 <- "p"
    tag_selector3 <- ".calendar-event-category"
    link_selector3 <- "#eventlist a"
    
    node_data3 %>%
      html_node(title_selector3) %>%
      html_text() %>%
      ifelse(. == "", NA, .) -> t3
    title_voll <- c(title_voll, t3)
    
    node_data3 %>%
      html_node(datum_selector3) %>%
      html_text(trim = T)  %>%
      ifelse(. == "", NA, .) -> d3
    as.Date(d3, "%b %d %Y") -> d3
    format(d3, format="%d.%m.%Y") -> d3
    datum_voll <- c(datum_voll, d3)
    
    node_data3 %>%
      html_node(ort_selector3) %>%
      html_text(trim = T) %>%
      ifelse(. == "", NA, .) -> o3
    ort_voll <- c(ort_voll, o3)
    
    node_data3 %>%
      html_node(text_selector3) %>%
      html_text(trim = T) %>%
      ifelse(. == "", NA, .) -> txt3
    text_voll <- c(text_voll, txt3)
    
    #keine Daten
    # 
    # node_data3 %>%
    #   html_node("#eventlist a") %>%
    #   html_attr("href") -> l3
    # 
    # link_voll1 <- c(link_voll1, paste0("https://www.wuerzburg.ihk.de",l3))
    
    
    i <- i + 1
    Sys.sleep(3)
  }
  
  # Shut down selenium
  remDr$close()
  rm(rD)
  gc()
  
  df3 <- data.frame(title=title_voll, description= text_voll, Startdatum= datum_voll, time_start="",time_end="",organizer=ort_voll )
  df3 <- separate(df3, col=Startdatum, into = c("date_start","date_end"), sep="\\-")
  as.character(df3$organizer) -> df3$organizer
  
  df3$date_start <- as.Date(df3$date_start, format = "%d.%m.%Y")
  
  
  # for (i in 1:nrow(df3)) {
  #   if (is.na(df3[i,]$date_end)) {
  #     df3[i,]$date_end <- df3[i,]$date_start
  #   }
  #   
  # }
  
  ####Unterschiedliche Adressen einfügen, Woher Long und Lat, wenn nicht aus Excel?  
  
  df3Adresse <- character(0) 
  for (i in 1:nrow(df3)) {
    if(df3[i,]$organizer=="IHK-Bildungszentrum Schweinfurt"){
      df3Adresse <- cbind(df3Adresse, street="Karl-Götz-Straße 7, 97424. Schweinfurt")
      
    }
    else{
      df3Adresse <- cbind(df3Adresse, street="Mainaustraße 35, 97082. Würzburg")
    }
    
  }
  df3Long <- character(0) 
  for (i in 1:nrow(df3)) {
    if(df3[i,]$organizer=="IHK-Bildungszentrum Schweinfurt"){
      df3Long <- cbind(df3Long, lng= 10.19685)
      
    }
    else{
      df3Long <- cbind(df3Long, lng=9.90881)
    }
  }
  
  df3Lat <- character(0) 
  for (i in 1:nrow(df3)) {
    if(df3[i,]$organizer=="IHK-Bildungszentrum Schweinfurt"){
      df3Lat <- cbind(df3Lat, lat=50.05785)
      
    }
    else{
      df3Lat <- cbind(df3Lat, lat=49.80109)
    }
  }
  
  
  
  inverse <- t(df3Adresse)
  as.character(inverse) -> inverse1
  df3Long <- t(df3Long)
  as.character(df3Long) ->df3Long
  df3Lat <- t(df3Lat)
  as.character(df3Lat) -> df3Lat
  
  veranstaltungenIHK <- data.frame()
  veranstaltungenIHK <- data.frame(df3,url= "https://www.wuerzburg.ihk.de/veranstaltungen.html", price=NA, Adresse=inverse1, lng=df3Long, lat=df3Lat)
  veranstaltungenIHK <- separate(data= veranstaltungenIHK, col=Adresse, into = c("street", "zip"), sep=", " )
  veranstaltungenIHK <- separate(data= veranstaltungenIHK, col=zip, into = c("zip", "city"), sep=". " )
  veranstaltungenIHK$zip<-as.numeric(veranstaltungenIHK$zip)
  #-------------------
  
  return(veranstaltungenIHK)
}
getPGSanderau = function(){
  
  url <- paste0("http://www.pg-sanderau.de/aktuelles---termine/")
  Eventtabelle <- data.frame(matrix(ncol = 12, nrow = 0))
  colnames(Eventtabelle) <- c("title", "description", "date_start","date_end", "time_start", "time_end","url","price","organizer","city","street","zip")
  
  rD <- rsDriver()
  remDr <- rD[["client"]]
  remDr$navigate(url)
  
  run <- TRUE
  
  tryCatch(
    #remDr$findElement(using = 'css selector', ".nav-text")$clickElement(),
    error= function(c) {run <<- F},
    warning = function(w) {run <<- F}
    #finally = print(paste("Pressed button", i, "times"))
  )
  pg  <- read_html(remDr$getPageSource()[[1]])
  pg %>%
    html_node('.pagenumber') %>%
    html_nodes('a') %>%
    html_attr("href")-> urls
  
  urls <- paste0("http://www.pg-sanderau.de/aktuelles---termine/",urls)  
  
  remDr$close()
  rm(rD)
  gc()  
  
  Events <- data.frame(matrix(ncol = 1, nrow = 0))
  
  
  for (i in urls) {
    urlseite <- i
    rD <- rsDriver()
    remDr <- rD[["client"]]
    remDr$navigate(urlseite)
    run <- TRUE
    tryCatch(
      #remDr$findElement(using = 'css selector', ".itemtitel")$clickElement(),
      error= function(c) {run <<- F},
      warning = function(w) {run <<- F}
      #finally = print(paste("Pressed button", i, "times"))
    )
    Sys.sleep(5)
    pg <- read_html(remDr$getPageSource()[[1]])
    pg %>%
      html_nodes(".itemtitle a") %>%
      html_attr('href')-> Links
    Links <- data.frame(Links)
    Links$Links <- paste("http:", Links$Links, sep="")
    Events <- rbind(Events,Links)
    
    remDr$close()
    rm(rD)
    gc()  
    
  }
  crawling = function(links) {
    
    linkad <- links
    versuch <- links %>%
      read_html() -> raw_data
    
    raw_data %>%
      html_nodes("p") %>%
      html_text() -> Beschreibung
    Beschreibung  <- Beschreibung[2]
    
    raw_data %>%
      html_nodes(".datetime") %>%
      html_text() -> Datum
    Datum <- Datum[2]
    Datum <-  strsplit(Datum, ",", fixed=TRUE)
    
    dates <- as.character(str_extract_all(Datum,"[0-9]{2}.[0-9]{2}.[0-9]{4}", simplify = TRUE)) #
    uhrzeiten <- as.character(str_extract_all(Datum,"[0-9]+:[0-9]{2}", simplify = TRUE)) #
    
    dates <- data.frame(dates)
    colnames(dates) <- c("Datum")
    uhrzeiten <- data.frame(uhrzeiten)
    colnames(uhrzeiten) <- c("Uhrzeiten")
    startzeit <- uhrzeiten$Uhrzeiten[1]
    endzeit <- uhrzeiten$Uhrzeiten[2]
    
    Startdatum <- dates$Datum[1]
    Startdatum <- as.Date(Startdatum, format= "%d.%m.%Y")
    Enddatum <- dates$Datum[2]
    Enddatum <- as.Date(Enddatum, format= "%d.%m.%Y")
    
    if(is.na(Enddatum)){ #
      Enddatum <- Startdatum #
    } #
    
    ort <- NA #Kann nicht ausgelesen werden
    
    raw_data %>%
      html_nodes("h1") %>%
      html_text() -> Titel
    Titel <- Titel[2]
    
    price <- NA
    organizer <- "PG Sanderau"
    
    startzeit <- gsub(".",":",startzeit, fixed = TRUE)
    startzeit <- as.character(startzeit)
    endzeit <- gsub(".",":",endzeit, fixed = TRUE)
    endzeit <- as.character(endzeit)
    if(!is.na(startzeit)){
      startzeit <- paste(startzeit,':00', sep = '')
      #startzeit <- times(startzeit)
    }
    
    if(!is.na(endzeit)){
      endzeit <- paste(endzeit,':00', sep = '')
      endzeit <- as.character(times(endzeit))
      
    }
    city <- "Wuerzburg"
    street <- "Traubengasse 27"
    zip <- 97072
    
    test <- data.frame(Titel,Beschreibung,Startdatum,Enddatum, startzeit, endzeit,linkad,price,organizer,city,street,zip)
    colnames(test) <- c("title", "description", "date_start","date_end", "time_start", "time_end","url","price","organizer","city","street","zip")
    
    return(test)
  }
  for (i in Events$Links) {
    test = crawling(i)
    Eventtabelle <- rbind(Eventtabelle,test)
    assign('Eventtabelle',Eventtabelle,envir=.GlobalEnv)
  }
  Eventtabelle$lng <- rep(9.93632,nrow(Eventtabelle)) 
  Eventtabelle$lat <- rep(49.78475,nrow(Eventtabelle)) 
  
  return(Eventtabelle)
}
getBoot = function() {
  
  url_boot <- "https://www.das-boot.com/programm"
  
  url_boot %>%
    read_html() %>%
    html_nodes(".btn-u") %>%
    html_attr("href") -> links
  
  links <- links[!is.na(links)]
  
  getDataTitel <- function(links){
    links %>%
      read_html() %>%
      html_nodes(".termin-detail-title") %>%
      html_text(trim = T) -> titel
  }
  map(links, getDataTitel) -> Titel_List
  
  
  getDataDatum <- function(links){
    links %>%
      read_html() %>%
      html_nodes(".month , .month+ h1") %>%
      html_text(trim = T) -> datum
  }
  map(links, getDataDatum) -> Datum_List
  
  
  getDataUhrzeit <- function(links){
    links %>%
      read_html() %>%
      html_nodes(".time_age") %>%
      html_text(trim = T) -> uhrzeit
    uhrzeit <- substr(uhrzeit, 1, 9)
  }
  map(links, getDataUhrzeit) -> Uhrzeit_List
  
  
  getDataBeschreibung <- function(links){
    links %>%
      read_html() %>%
      html_nodes(".specials") %>%
      html_text(trim = T) -> Beschreibung
  }
  map(links, getDataBeschreibung) -> Beschreibung_List
  
  getDataPreis <- function(links){
    links%>%
      read_html() %>%
      html_nodes(".list-specials li:nth-child(1)") %>%
      html_text(trim = T) %>%
      ifelse(. == "", NA, .) -> Preis
  }
  map(links, getDataPreis) -> Preis_List
  
  
  Titel_List <- unlist(Titel_List, use.names=FALSE)
  Datum_List <- unlist(Datum_List, use.names=FALSE)
  Uhrzeit_List <- unlist(Uhrzeit_List, use.names=FALSE)
  Beschreibung_List <- unlist(Beschreibung_List, use.names=FALSE)
  Preis_List <- unlist(Preis_List, use.names=FALSE)
  
  
  i <- 1
  x <- 1
  Monat <- list()
  Tag <- list()
  
  while (i < length(Datum_List)) {
    Monat[[x]] <-c(Datum_List[i])
    Tag[[x]] <-c(Datum_List[i+1])
    x <- x+1
    i <- i+2
  }
  
  Monat <- unlist(Monat, use.names=FALSE)
  Tag <- unlist(Tag, use.names=FALSE)
  
  boot_df <- data.frame(Titel_List, Uhrzeit_List, Beschreibung_List, Monat, Tag)
  
  boot_df$Monat <- boot_df$Monat %>%
  { gsub("Jan","01", .) } %>%
  { gsub("Feb","02", .) } %>%
  { gsub("Mär","03", .) } %>%
  { gsub("Apr","04", .) } %>%
  { gsub("Mai","05", .) } %>%
  { gsub("Jun","06", .) } %>%
  { gsub("Jul","07", .) } %>%
  { gsub("Aug","08", .) } %>%
  { gsub("Sep","09", .) } %>%
  { gsub("Okt","10", .) } %>%
  { gsub("Nov","11", .) } %>%
  { gsub("Dez","12", .) }
  
  
  boot_df$Tag <- as.numeric(gsub("\\D", "", boot_df$Tag)) 
  
  boot_df$Uhrzeit_List <- str_sub(boot_df$Uhrzeit_List, str_length(boot_df$Uhrzeit_List) - 6, str_length(boot_df$Uhrzeit_List)-1)
  #Uhrzeit bekommen
  time_start=times(paste0(boot_df$Uhrzeit_List,":00"))
  boot_df$Tag <- as.character(boot_df$Tag)
  boot_df <- boot_df %>%
    mutate(Datum = if_else(str_length(boot_df$Tag) == 1, as.character(paste("0",boot_df$Tag, sep = "")), boot_df$Tag))
  
  boot_df <- mutate(boot_df, StartDatum = paste(boot_df$Datum, boot_df$Monat, "2018", sep = "."))
  #Datum erstellen
  
  
  boot_latitude <- 49.8018
  boot_longitude <- 9.92291 
  boot_city <- "Würzburg"
  boot_street <- "Veitshöchheimer Straße 14"
  boot_zip <- 97080
  boot_organizer <- "Das Boot"
  boot_EndUhrzeit <- NA
  #Latitude und Longitude deklarieren
  date=boot_df$StartDatum
  boot_df$StartDatum=as.Date(date, format="%d.%m.%Y")
  
  df_preis <- data.frame(Preis_List)
  df_preis <- mutate(df_preis, Preis = if_else(grepl("Eintritt", df_preis$Preis_List) == T, df_preis$Preis_List,as.integer(1)))
  #Preis herausfinden
  
  df_boot_final <- data.frame(title=boot_df$Titel_List, url = links, description = boot_df$Beschreibung_List, 
                              lng = boot_longitude, lat = boot_latitude, city = boot_city, street = boot_street, 
                              zip = boot_zip, date_start = boot_df$StartDatum, date_end = boot_df$StartDatum, 
                              time_start = time_start, time_end = boot_EndUhrzeit, price = NA, 
                              organizer = boot_organizer)
  df_boot_final$title=as.character(df_boot_final$title)
  df_boot_final$url=as.character(df_boot_final$url)
  df_boot_final$description=as.character(df_boot_final$description)
  df_boot_final$city=as.character(df_boot_final$city)
  df_boot_final$street=as.character(df_boot_final$street)
  df_boot_final$date_start=as.Date(df_boot_final$date_start,format="%d.%m")
  df_boot_final$date_end=as.Date(df_boot_final$date_start,format="%d.%m")
  df_boot_final$time_start=times(df_boot_final$time_start)
  df_boot_final$time_end=times(df_boot_final$time_end)
  df_boot_final$organizer=as.character(df_boot_final$organizer)
  
  #fertig
  df_boot_final <- unique(df_boot_final)
  
  return(df_boot_final)
}
getMH = function() {
  
  mh_url <- "http://www.me-haus.de/startseite/kalender"
  mh_titel_voll <- vector()
  mh_datum_voll <- vector()
  mh_date_voll <- vector()
  
  rD <- rsDriver()
  remDr <- rD[["client"]]
  remDr$navigate(mh_url)
  
  run <- TRUE
  i <- 1
  while (i <= 30){
    tryCatch(
      remDr$findElement(using = 'css selector', ".pagingbox-navigate-right")$clickElement(),
      error= function(c) {run <<- F},
      warning = function(w) {run <<- F},
      finally = print(paste("Pressed button", i, "times"))
    )
    raw_data <- read_html(remDr$getPageSource()[[1]])
    raw_data %>%
      html_nodes(".date") %>%
      html_text(trim = T) %>%
      ifelse(. == "", NA, .) -> mh_date
    mh_datum_voll <- c(mh_datum_voll, mh_date)
    raw_data %>%
      html_nodes(".itemtitle a") %>%
      html_text(trim = T) %>%
      ifelse(. == "", NA, .) -> mh_titel
    mh_titel_voll <- c(mh_titel_voll, mh_titel)
    raw_data %>%
      html_nodes(".function") %>%
      html_text(trim = T) %>%
      ifelse(. == "", NA, .) -> mh_date
    mh_date2 <- rep(mh_date,length(mh_titel))
    mh_date_voll <- c(mh_date_voll, mh_date2)
    
    i <- i + 1
    Sys.sleep(7)
  }
  
  # Shut down selenium
  remDr$close()
  rm(rD)
  gc()
  
  df_mh <- data.frame(mh_titel_voll, mh_datum_voll, mh_date_voll)
  
  df_mh <- df_mh %>%
    separate(mh_datum_voll, c("StartUhrzeit", "EndUhrzeit"), sep=" bis ")
  
  df_mh$EndUhrzeit <- gsub(" Uhr", "", df_mh$EndUhrzeit)
  time_start=times(paste0(df_mh$StartUhrzeit,":00"))
  time_end=times(paste0(df_mh$EndUhrzeit,":00"))
  
  df_mh <- df_mh %>%
    separate(mh_date_voll, c("A", "StartDatum"), sep=", ")
  
  mh_latitude <- rep(49.7978,nrow(df_mh))
  mh_longitude <- rep(9.93509,nrow(df_mh)) 
  mh_city <- rep("Würzburg",nrow(df_mh))
  mh_street <- rep("Kolpingstraße 11",nrow(df_mh))
  mh_zip <- rep(97070,nrow(df_mh))
  mh_Beschreibung <- rep("",nrow(df_mh))
  mh_links <- rep("http://www.me-haus.de/startseite/kalender",nrow(df_mh))
  mh_organizer <- rep("Matthias-Ehrenfried-Haus",nrow(df_mh))
  #Latitude und Longitude deklarieren
  date=df_mh$StartDatum
  df_mh$StartDatum=as.Date(date, format="%d.%m.%Y")
  
  df_mh_final <- data.frame(title=df_mh$mh_titel_voll, url = mh_links, description = mh_Beschreibung, lng = mh_longitude, lat = mh_latitude, city = mh_city, street = mh_street, zip = mh_zip, date_start = df_mh$StartDatum, date_end = df_mh$StartDatum, time_start = time_start, time_end = time_end, price = mh_Beschreibung, organizer = mh_organizer)
  df_mh_final <- unique(df_mh_final)
  df_mh_final$title=as.character(df_mh_final$title)
  df_mh_final$url=as.character(df_mh_final$url)
  df_mh_final$description=as.character(df_mh_final$description)
  df_mh_final$city=as.character(df_mh_final$city)
  df_mh_final$street=as.character(df_mh_final$street)
  df_mh_final$organizer=as.character(df_mh_final$organizer)
  df_mh_final$price=as.character(df_mh_final$price)
  #-------------------
  
  return(df_mh_final)
}
getThomas = function() {
  
  mh_url <- "http://www.thomaskirche-wuerzburg.de/cm/veranstaltungen/"
  title_voll <- vector()
  datum_voll <- vector()
  ort_voll <- vector()
  link_voll <- vector()
  
  rD <- rsDriver()
  remDr <- rD[["client"]]
  remDr$navigate(mh_url)
  
  run <- TRUE
  i <- 1
  while (run){
    tryCatch(
      remDr$findElement(using = 'css selector', "#et_filter_container+ .et_pager_container a:nth-child(6)")$clickElement(),
      error= function(c) {run <<- F},
      warning = function(w) {run <<- F},
      finally = print(paste("Pressed button", i, "times"))
    )
    raw_data <- read_html(remDr$getPageSource()[[1]])
    
    raw_data %>% html_nodes(".et_content_row") -> node_data2
    
    title_selector2 <- ".et_link_title"
    datum_selector2 <- ".et_content_date"
    ort_selector2 <- ".et_placename"
    
    node_data2 %>%
      html_node(title_selector2) %>%
      html_text() %>%
      ifelse(. == "", NA, .) -> t2
    title_voll <- c(title_voll, t2)
    
    node_data2 %>%
      html_node(title_selector2) %>%
      html_attr("href") -> l2
    link_voll <- c(link_voll, paste0("http://www.thomaskirche-wuerzburg.de/cm/veranstaltungen/",l2))
    
    
    node_data2 %>%
      html_node(datum_selector2) %>%
      html_text() %>%
      ifelse(. == "", NA, .) -> d2
    datum_voll <- c(datum_voll, d2)
    
    node_data2 %>%
      html_node(ort_selector2) %>%
      html_text(trim = T) %>%
      ifelse(. == "", NA, .) -> o2
    ort_voll <- c(ort_voll, o2)
    
    i <- i + 1
    Sys.sleep(2)
  }
  
  # Shut down selenium
  remDr$close()
  rm(rD)
  gc()
  
  regex_date1 = "[0-9]{1,2}[\\./]{1}[0-9]{1,2}[\\./]?(20[1-2]{1}[0-9]{1})?"
  regex_time1 = "[0-9]{1,2}([\\:/]{1})?([\\-/]{1})?([0-9]{1,2})? Uhr"
  
  datum_voll %>%
    str_extract_all(regex_date1) -> d21
  d21 <- as.character.Date(d21)
  
  
  datum_voll %>%
    str_extract_all(regex_time1) -> time2
  time2 <- as.character.Date(time2)
  
  
  df2 <-
    data.frame(
      title = title_voll,
      url= link_voll,
      description=ort_voll,
      Datum = d21,
      time_start = time2,
      street = "Schiestlstraße 54",
      city = "Würzburg",
      zip =97080,
      lng=9.95034,
      lat=49.80377,
      price= NA,
      organizer="Thomaskirche Würzburg"
    )
  
  df2 <- separate(data= df2, col=time_start, into = c("time_start", "time_end"), sep="\\-" )
  df2 <- separate(df2, col=Datum, into = c("date_start","date_end"), sep="\\-")
  
  as.Date(df2$date_start, "%d.%m") -> df2$date_start
  as.Date(df2$date_end, "%d.%m") -> df2$date_end
  
  df2$time_start <- sub(pattern = " Uhr", replacement = "", x=df2$time_start)
  df2$time_end <- sub(pattern = " Uhr", replacement = "", x=df2$time_end)
  
  
  testi <- paste0(df2$time_start, ":00:00")
  testi1 <- str_extract_all(testi, "[0-9]{1,2}\\:[0-9]{1,2}\\:[0-9]{1,2}")
  testi1<- as.character(testi1)
  
  
  df2$time_start <- times(testi1)
  df2$time_end <- times(paste0(df2$time_end, ":00:00"))
  
  df2$price <- as.numeric(df2$price)
  
  # for (i in 1:nrow(df2)) {
  #   if (is.na(df2[i,]$date_end)) {
  #     df2[i,]$date_end <- df2[i,]$date_start
  #   }
  #   
  # }
  # 
  # for (i in 1:nrow(df2)) {
  #   if (is.na(df2[i,]$time_end)) {
  #     df2[i,]$time_end <- df2[i,]$time_start
  #   }
  #   
  # }
  
  #-------------------
  
  return(df2)
}
getAugustinerKirche = function() {
  
  agk_monat_voll <- vector()
  agk_beschreibung_voll <- vector()
  agk_tag_voll <- vector()
  agk_zeit_voll <- vector()
  agk_titel_voll <- vector()
  agk_links_voll <- vector()
  
  agk_url <- "http://augustinerkirche-wuerzburg.de/veranstaltungen-3/"
  agk_url %>%
    read_html() -> raw_data_agk
  # rD <- rsDriver()
  # remDr <- rD[["client"]]
  # remDr$navigate(agk_url)
  # Smarter: Click button until all sites are loaded
  #run <- TRUE
  #i <- 1
  #while (run){
    # tryCatch(
    #   remDr$findElement(using = 'css selector', ".ai1ec-pull-left .ai1ec-next-page")$clickElement(),
    #   error= function(c) {run <<- F},
    #   warning = function(w) {run <<- F}
    #   finally = print(paste("Pressed button", i, "times"))
    # )
    #raw_data_agk <- read_html(remDr$getPageSource()[[1]])
    raw_data_agk %>%
      html_nodes(".ai1ec-month") %>%
      html_text(trim = T) -> agk_monat
    agk_monat_voll <- c(agk_monat_voll, agk_monat)
    raw_data_agk %>%
      html_nodes(".ai1ec-event-description") %>%
      html_text(trim = T) %>%
      ifelse(. == "", NA, .) -> agk_beschreibung  
    agk_beschreibung_voll <- c(agk_beschreibung_voll, agk_beschreibung)
    
    raw_data_agk %>%
      html_nodes(".ai1ec-load-event") %>%
      html_text(trim = T) %>%
      ifelse(. == "", NA, .) -> agk_titel
    agk_titel <- agk_titel[!is.na(agk_titel)]
    agk_titel_voll <- c(agk_titel_voll, agk_titel)
    
    raw_data_agk %>%
      html_nodes(".ai1ec-event-time") %>%
      html_text(trim = T) -> agk_zeit
    agk_zeit_voll <- c(agk_zeit_voll, agk_zeit)
    
    raw_data_agk %>%
      html_nodes(".ai1ec-day") %>%
      html_text(trim = T) -> agk_tag
    agk_tag_voll <- c(agk_tag_voll, agk_tag)
    
    raw_data_agk %>%
      html_nodes(".ai1ec-load-event") %>%
      html_attr("href") %>%
      ifelse(. == "", NA, .) -> agk_links
    agk_links <- agk_links[!is.na(agk_links)]
    agk_links_voll <- c(agk_links_voll, agk_links)
  
    # i <- i + 1
    # Sys.sleep(7)
  
  
  # Shut down selenium
  # remDr$close()
  # rm(rD)
  # gc()
  
  df_agk <- data.frame(Titel = agk_titel_voll, Beschreibung = agk_beschreibung_voll, Monat = agk_monat_voll, Tag = agk_tag_voll, Zeit = agk_zeit_voll)
  
  df_agk$Monat <- df_agk$Monat %>%
  { gsub("Jan","01", .) } %>%
  { gsub("Feb","02", .) } %>%
  { gsub("Mär","03", .) } %>%
  { gsub("Apr","04", .) } %>%
  { gsub("Mai","05", .) } %>%
  { gsub("Jun","06", .) } %>%
  { gsub("Jul","07", .) } %>%
  { gsub("Aug","08", .) } %>%
  { gsub("Sep","09", .) } %>%
  { gsub("Okt","10", .) } %>%
  { gsub("Nov","11", .) } %>%
  { gsub("Dez","12", .) }
  
  df_agk$Zeit <- str_sub(df_agk$Zeit, str_length(df_agk$Zeit) - 5, str_length(df_agk$Zeit))
  #Uhrzeit bekommen
  
  df_agk <- mutate(df_agk, StartDatum = paste(df_agk$Tag,df_agk$Monat))
  df_agk$StartDatum <- df_agk$StartDatum %>%
  { gsub(" ",".", .) }
  #Datum erstellen
  
  df_agk <- select(df_agk,-Monat)
  df_agk <- select(df_agk,-Tag)
  #Monat & Tag löschen
  
  agk_latitude <- rep(49.7963,nrow(df_agk))
  agk_longitude <- rep(9.93119,nrow(df_agk)) 
  agk_city <- rep("Würzburg",nrow(df_agk))
  agk_street <- rep("Dominikanerplatz 2",nrow(df_agk))
  agk_zip <- rep(97070,nrow(df_agk))
  agk_EndUhrzeit <- NA
  time_start=times(paste0(df_agk$Zeit,":00"))
  
  agk_organizer <- rep("Augustinerkirche",nrow(df_agk))
  
  #Latitude und Longitude deklarieren
  agk_links_voll <- unique(agk_links_voll)
  
  df_agk$StartDatum <- as.character(df_agk$StartDatum)
  date_start=paste0(df_agk$StartDatum, ".2018")
  #df_agk$StartDatum <- if_else(str_length(df_agk$StartDatum) == 4, as.character(paste("0",df_agk$StartDatum, sep = "")), df_agk$StartDatum)
  df_agk$StartDatum=as.Date(date_start, format="%d.%m.%Y")
  
  df_agk_final <- data.frame(title=df_agk$Titel, url = agk_links_voll, description = df_agk$Beschreibung, lng = agk_longitude, lat = agk_latitude, city = agk_city, street = agk_street, zip = agk_zip, date_start = df_agk$StartDatum, date_end = df_agk$StartDatum, time_start = time_start, time_end = agk_EndUhrzeit, price = NA, organizer = agk_organizer)
  df_agk_final <- unique(df_agk_final)
  df_agk_final$title=as.character(df_agk_final$title)
  df_agk_final$url=as.character(df_agk_final$url)
  df_agk_final$description=as.character(df_agk_final$description)
  df_agk_final$city=as.character(df_agk_final$city)
  df_agk_final$street=as.character(df_agk_final$street)
  df_agk_final$organizer=as.character(df_agk_final$organizer)
  df_agk_final$price=as.character(df_agk_final$price)  
  return(df_agk_final)
}
getDomschule = function(){
  
  #---------Domschule----------------------
  do_url <- "https://www.domschule-wuerzburg.de/akademie/alle-veranstaltungen"
  do_titel_voll <- vector()
  do_links_voll <- vector()
  do_datum_voll <- vector()
  
  rD <- rsDriver()
  remDr <- rD[["client"]]
  remDr$navigate(do_url)
  
  #Vor Aktualisierung gab es zwei Seiten mit Veranstaltungen
  run <- TRUE
  i <- 1
  while (i <= 1){
    tryCatch(
      remDr$findElement(using = 'css selector', "#topcontrol")$clickElement(),
      error= function(c) {run <<- F},
      warning = function(w) {run <<- F},
      finally = print(paste("Pressed button", i, "times"))
    )
    raw_data <- read_html(remDr$getPageSource()[[1]])
    raw_data %>%
      html_nodes(".eventcontent a") %>%
      html_text(trim = T) %>%
      ifelse(. == "", NA, .) -> do_titel
    do_titel_voll <- c(do_titel_voll, do_titel)
    do_titel_voll <- do_titel_voll[seq(1, length(do_titel_voll), 2)]
    
    raw_data %>%
      html_nodes(".eventcontent a") %>%
      html_attr("href")-> do_links
    do_links_voll <- c(do_links_voll, do_links)
    do_links_voll <- do_links_voll[seq(1, length(do_links_voll), 2)]
    
    raw_data %>%
      html_nodes(".blog-info li:nth-child(1)") %>%
      html_text(trim = T) %>%
      ifelse(. == "", NA, .) -> do_datum
    do_datum_voll <- c(do_datum_voll, do_datum)
    
    i <- i + 1
    Sys.sleep(3)
  }
  
  # Shut down selenium
  remDr$close()
  rm(rD)
  gc()
  
  #Links erweitern
  do_links_voll <- paste0("http:", do_links_voll)
  
  
  df_do <- data.frame(title= do_titel_voll, url = do_links_voll, date_do = do_datum_voll, adresse ="Frankenstrasse 21, 97078 Wuerzburg", lat = 49.8084818 , lng = 9.9634205)
  
  #Datum 
  #Wochentag entfernen
  df_do$date_do <- str_replace_all(df_do$date_do, "([A-z]+, )", "")
  #Monate durch Zahlen ersetzen
  df_do$date_do <- str_replace_all(df_do$date_do," Januar ","01.")
  df_do$date_do <- str_replace_all(df_do$date_do," Februar ","02.")    
  df_do$date_do <- str_replace_all(df_do$date_do," März ","03.")  
  df_do$date_do <- str_replace_all(df_do$date_do," April ","04.")
  df_do$date_do <- str_replace_all(df_do$date_do," Mai ","05.")  
  df_do$date_do <- str_replace_all(df_do$date_do," Juni ","06.")  
  df_do$date_do <- str_replace_all(df_do$date_do," Juli ","07.")  
  df_do$date_do <- str_replace_all(df_do$date_do," August ","08.")
  df_do$date_do <- str_replace_all(df_do$date_do," September ","09.")  
  df_do$date_do <- str_replace_all(df_do$date_do," Oktober ","10.")  
  df_do$date_do <- str_replace_all(df_do$date_do," November ","11.") 
  df_do$date_do <- str_replace_all(df_do$date_do," Dezember ","12.") 
  #Aufspalten Start, Ende
  df_do <- separate(data=df_do, col=date_do, into=c("date_start", "date_end"), sep="\\- ")
  df_do <- separate(data=df_do, col=date_start, into=c("date_start", "time_start"), sep="\\, ")
  df_do$time_end <- str_extract_all(df_do$date_end, "[0-9]+:[0-9]+")
  df_do$date_end <- str_extract_all(df_do$date_end, "([0-9]+.[0-9]+.[0-9]+)")
  #character(0) -> NA
  df_do$date_end <- as.character(df_do$date_end)
  for(i in 1:length(df_do$date_end)){
    if(identical(df_do[i,]$date_end, "character(0)")){
      df_do[i,]$date_end<- NA
    }
  }
  #Enddatum = Startdatum falls na
  for (i in 1:nrow(df_do)){
    if (is.na(df_do[i,]$date_end)){
      df_do[i,]$date_end<- df_do[i,]$date_start
    }
  }
  #dates to date Format
  df_do$date_end <- as.Date(df_do$date_end,format='%d.%m.%Y')
  df_do$date_start <- as.Date(df_do$date_start,format='%d.%m.%Y')
  
  
  #Zeit
  df_do$time_start <- str_extract_all(df_do$time_start, "[0-9]+:[0-9]+")
  df_do$time_start <- paste0(df_do$time_start, ":00")
  df_do$time_start <- times(df_do$time_start)
  df_do$time_end <- paste0(df_do$time_end, ":00")
  df_do$time_end <- times(df_do$time_end)
  
  #Ort
  df_do$city = "Würzburg"
  df_do$zip = 97070
  df_do$street = "Am Bruderhof 1"
  
  #Kosten aus Link
  links_d<- as.character(df_do$url)
  getDataKosten <- function(links_d){
    links_d %>%
      read_html() %>%
      html_nodes(".f_cost .f_value") %>%
      html_text(trim = T) -> kosten
  }
  Kosten_List1 <- map(links_d, getDataKosten) 
  Kosten_List1 <- unlist(Kosten_List1, use.names=FALSE)
  df_do$price <- Kosten_List1
  
  df_do$description <- NA 
  df_do$organizer <- "Domschule Würzburg"
  
  df_do_to <- df_do[c("title","url", "description","lng", "lat", "city", "street", "zip", "date_start", "date_end", "time_start", "time_end","price","organizer")]
  
  #--------------------------------------------------
  
  return(df_do_to)
}
getVHS = function(){
  
  #Gesellschaft
  
  vhs1_url <- "https://www.vhs-wuerzburg.info/programm/gesellschaft.html"
  vhs1_titel_voll <- vector()
  vhs1_beschreibung_voll <- vector()
  vhs1_datum_voll <- vector()
  vhs1_ort_voll <- vector()
  vhs1_links_voll <- vector()
  
  rD <- rsDriver()
  remDr <- rD[["client"]]
  remDr$navigate(vhs1_url)
  
  run <- TRUE
  i <- 1
  while (i <= 11){
    tryCatch(
      remDr$findElement(using = 'css selector', ".switch-page-up")$clickElement(),
      error= function(c) {run <<- F},
      warning = function(w) {run <<- F},
      finally = print(paste("Pressed button", i, "times"))
    )
    raw_data <- read_html(remDr$getPageSource()[[1]])
    raw_data %>%
      html_nodes(".bold .title") %>%
      html_text(trim = T) %>%
      ifelse(. == "", NA, .) -> vhs1_titel
    if(identical(vhs1_titel, character(0))){
      vhs1_titel<- NA
    }
    vhs1_titel_voll <- c(vhs1_titel_voll, vhs1_titel)
    
    raw_data %>%
      html_nodes(".startDate") %>%
      html_text(trim = T) %>%
      ifelse(. == "", NA, .) -> vhs1_datum
    vhs1_datum_voll <- c(vhs1_datum_voll, vhs1_datum)
    
    raw_data %>%
      html_nodes(".venues\\.city") %>%
      html_text(trim = T) %>%
      ifelse(. == "", NA, .) -> vhs1_ort
    vhs1_ort_voll <- c(vhs1_ort_voll, vhs1_ort)
    
    raw_data %>%
      html_nodes(".bold .title") %>%
      html_attr("href")-> vhs1_links
    vhs1_links_voll <- c(vhs1_links_voll, vhs1_links)
    
    
    i <- i + 1
    Sys.sleep(10)
  }
  
  # Shut down selenium
  remDr$close()
  rm(rD)
  gc()
  
  #Link erweitern
  vhs1_links_voll <- paste0("https:", vhs1_links_voll)
  
  #Data Frame1
  df_vhs1 <- data.frame(title= vhs1_titel_voll, url=vhs1_links_voll, description = NA, date = vhs1_datum_voll, city = vhs1_ort_voll, Adresse ="", lat = 49.7821, lng = 9.938)
  
  df_vhs1$date <- str_replace_all(df_vhs1$date, "[A-z]{2}[.][,][ ]","")
  
  df_vhs1$date_start <- str_extract_all(df_vhs1$date,"[0-9]{2}[.][0-9]{2}[.][0-9]{4}")
  df_vhs1$time_start <- str_extract_all(df_vhs1$date, "[0-9]{2}[:][0-9]{2}")
  
  
  
  # Beruf
  
  vhs2_url <- "https://www.vhs-wuerzburg.info/programm/beruf.html"
  vhs2_titel_voll <- vector()
  vhs2_beschreibung_voll <- vector()
  vhs2_datum_voll <- vector()
  vhs2_ort_voll <- vector()
  vhs2_links_voll <- vector()
  
  
  rD <- rsDriver()
  remDr <- rD[["client"]]
  remDr$navigate(vhs2_url)
  
  run <- TRUE
  i <- 1
  while (i <= 5){
    tryCatch(
      remDr$findElement(using = 'css selector', ".switch-page-up")$clickElement(),
      error= function(c) {run <<- F},
      warning = function(w) {run <<- F},
      finally = print(paste("Pressed button", i, "times"))
    )
    raw_data <- read_html(remDr$getPageSource()[[1]])
    raw_data %>%
      html_nodes(".bold .title") %>%
      html_text(trim = T) %>%
      ifelse(. == "", NA, .) -> vhs2_titel
    vhs2_titel_voll <- c(vhs2_titel_voll, vhs2_titel)
    raw_data %>%
      html_nodes(".startDate") %>%
      html_text(trim = T) %>%
      ifelse(. == "", NA, .) -> vhs2_datum
    vhs2_datum_voll <- c(vhs2_datum_voll, vhs2_datum)
    raw_data %>%
      html_nodes(".venues\\.city") %>%
      html_text(trim = T) %>%
      ifelse(. == "", NA, .) -> vhs2_ort
    vhs2_ort_voll <- c(vhs2_ort_voll, vhs2_ort)
    raw_data %>%
      html_nodes(".bold .title") %>%
      html_attr("href")-> vhs2_links
    vhs2_links_voll <- c(vhs2_links_voll, vhs2_links)
    
    
    i <- i + 1
    Sys.sleep(10)
  }
  
  # Shut down selenium
  remDr$close()
  rm(rD)
  gc()
  
  #Link erweitern
  vhs2_links_voll <- paste0("https:", vhs2_links_voll)
  
  df_vhs2 <- data.frame(title= vhs2_titel_voll, url=vhs2_links_voll, description = NA, date = vhs2_datum_voll, city = vhs2_ort_voll, Adresse ="", lat = 49.7821, lng = 9.938)
  
  df_vhs2$date <- str_replace_all(df_vhs2$date, "[A-z]{2}[.][,][ ]","")
  
  df_vhs2$date_start <- str_extract_all(df_vhs2$date,"[0-9]{2}[.][0-9]{2}[.][0-9]{4}")
  df_vhs2$time_start <- str_extract_all(df_vhs2$date, "[0-9]{2}[:][0-9]{2}")
  
  # Sprachen
  vhs3_url <- "https://www.vhs-wuerzburg.info/programm/sprachen.html"
  vhs3_titel_voll <- vector()
  vhs3_beschreibung_voll <- vector()
  vhs3_datum_voll <- vector()
  vhs3_ort_voll <- vector()
  vhs3_links_voll <- vector()
  
  
  rD <- rsDriver()
  remDr <- rD[["client"]]
  remDr$navigate(vhs3_url)
  
  run <- TRUE
  i <- 1
  while (i <= 16){
    tryCatch(
      remDr$findElement(using = 'css selector', ".switch-page-up")$clickElement(),
      error= function(c) {run <<- F},
      warning = function(w) {run <<- F},
      finally = print(paste("Pressed button", i, "times"))
    )
    raw_data <- read_html(remDr$getPageSource()[[1]])
    raw_data %>%
      html_nodes(".bold .title") %>%
      html_text(trim = T) %>%
      ifelse(. == "", NA, .) -> vhs3_titel
    vhs3_titel_voll <- c(vhs3_titel_voll, vhs3_titel)
    raw_data %>%
      html_nodes(".startDate") %>%
      html_text(trim = T) %>%
      ifelse(. == "", NA, .) -> vhs3_datum
    vhs3_datum_voll <- c(vhs3_datum_voll, vhs3_datum)
    raw_data %>%
      html_nodes(".venues\\.city") %>%
      html_text(trim = T) %>%
      ifelse(. == "", NA, .) -> vhs3_ort
    vhs3_ort_voll <- c(vhs3_ort_voll, vhs3_ort)
    raw_data %>%
      html_nodes(".bold .title") %>%
      html_attr("href")-> vhs3_links
    vhs3_links_voll <- c(vhs3_links_voll, vhs3_links)
    
    
    i <- i + 1
    Sys.sleep(10)
  }
  
  # Shut down selenium
  remDr$close()
  rm(rD)
  gc()
  
  
  #Link erweitern
  vhs3_links_voll <- paste0("https:", vhs3_links_voll)
  
  df_vhs3 <- data.frame(title= vhs3_titel_voll, url=vhs3_links_voll, description = NA, date = vhs3_datum_voll, city = vhs3_ort_voll, Adresse ="", lat = 49.7821, lng = 9.938)
  
  df_vhs3$date <- str_replace_all(df_vhs3$date, "[A-z]{2}[.][,][ ]","")
  
  df_vhs3$date_start <- str_extract_all(df_vhs3$date,"[0-9]{2}[.][0-9]{2}[.][0-9]{4}")
  df_vhs3$time_start <- str_extract_all(df_vhs3$date, "[0-9]{2}[:][0-9]{2}")
  
  
  
  # Gesundheit
  vhs4_url <- "https://www.vhs-wuerzburg.info/programm/gesundheit.html"
  vhs4_titel_voll <- vector()
  vhs4_beschreibung_voll <- vector()
  vhs4_datum_voll <- vector()
  vhs4_ort_voll <- vector()
  vhs4_links_voll <- vector()
  
  
  rD <- rsDriver()
  remDr <- rD[["client"]]
  remDr$navigate(vhs4_url)
  
  run <- TRUE
  i <- 1
  while (i <= 29){
    tryCatch(
      remDr$findElement(using = 'css selector', ".switch-page-up")$clickElement(),
      error= function(c) {run <<- F},
      warning = function(w) {run <<- F},
      finally = print(paste("Pressed button", i, "times"))
    )
    raw_data <- read_html(remDr$getPageSource()[[1]])
    raw_data %>%
      html_nodes(".bold .title") %>%
      html_text(trim = T) %>%
      ifelse(. == "", NA, .) -> vhs4_titel
    vhs4_titel_voll <- c(vhs4_titel_voll, vhs4_titel)
    raw_data %>%
      html_nodes(".startDate") %>%
      html_text(trim = T) %>%
      ifelse(. == "", NA, .) -> vhs4_datum
    vhs4_datum_voll <- c(vhs4_datum_voll, vhs4_datum)
    raw_data %>%
      html_nodes(".venues\\.city") %>%
      html_text(trim = T) %>%
      ifelse(. == "", NA, .) -> vhs4_ort
    vhs4_ort_voll <- c(vhs4_ort_voll, vhs4_ort)
    raw_data %>%
      html_nodes(".bold .title") %>%
      html_attr("href")-> vhs4_links
    vhs4_links_voll <- c(vhs4_links_voll, vhs4_links)
    
    
    i <- i + 1
    Sys.sleep(10)
  }
  
  # Shut down selenium
  remDr$close()
  rm(rD)
  gc()
  
  #Link erweitern
  vhs4_links_voll <- paste0("https:", vhs4_links_voll)
  
  df_vhs4 <- data.frame(title= vhs4_titel_voll, url=vhs4_links_voll, description = NA, date = vhs4_datum_voll, city = vhs4_ort_voll, Adresse ="", lat = 49.7821, lng = 9.938)
  
  df_vhs4$date <- str_replace_all(df_vhs4$date, "[A-z]{2}[.][,][ ]","")
  
  df_vhs4$date_start <- str_extract_all(df_vhs4$date,"[0-9]{2}[.][0-9]{2}[.][0-9]{4}")
  df_vhs4$time_start <- str_extract_all(df_vhs4$date, "[0-9]{2}[:][0-9]{2}")
  
  
  
  # Kultur
  vhs5_url <- "https://www.vhs-wuerzburg.info/programm/kultur.html"
  vhs5_titel_voll <- vector()
  vhs5_beschreibung_voll <- vector()
  vhs5_datum_voll <- vector()
  vhs5_ort_voll <- vector()
  vhs5_links_voll <- vector()
  
  
  rD <- rsDriver()
  remDr <- rD[["client"]]
  remDr$navigate(vhs5_url)
  
  run <- TRUE
  i <- 1
  while (i <= 12){
    tryCatch(
      remDr$findElement(using = 'css selector', ".switch-page-up")$clickElement(),
      error= function(c) {run <<- F},
      warning = function(w) {run <<- F},
      finally = print(paste("Pressed button", i, "times"))
    )
    raw_data <- read_html(remDr$getPageSource()[[1]])
    raw_data %>%
      html_nodes(".bold .title") %>%
      html_text(trim = T) %>%
      ifelse(. == "", NA, .) -> vhs5_titel
    vhs5_titel_voll <- c(vhs5_titel_voll, vhs5_titel)
    raw_data %>%
      html_nodes(".startDate") %>%
      html_text(trim = T) %>%
      ifelse(. == "", NA, .) -> vhs5_datum
    vhs5_datum_voll <- c(vhs5_datum_voll, vhs5_datum)
    raw_data %>%
      html_nodes(".venues\\.city") %>%
      html_text(trim = T) %>%
      ifelse(. == "", NA, .) -> vhs5_ort
    vhs5_ort_voll <- c(vhs5_ort_voll, vhs5_ort)
    raw_data %>%
      html_nodes(".bold .title") %>%
      html_attr("href")-> vhs5_links
    vhs5_links_voll <- c(vhs5_links_voll, vhs5_links)
    
    
    i <- i + 1
    Sys.sleep(10)
  }
  
  # Shut down selenium
  remDr$close()
  rm(rD)
  gc()
  
  #Link erweitern
  vhs5_links_voll <- paste0("https:", vhs5_links_voll)
  
  df_vhs5 <- data.frame(title= vhs5_titel_voll, url=vhs5_links_voll, description = NA, date = vhs5_datum_voll, city = vhs5_ort_voll, Adresse ="", lat = 49.7821, lng = 9.938)
  
  df_vhs5$date <- str_replace_all(df_vhs5$date, "[A-z]{2}[.][,][ ]","")
  
  df_vhs5$date_start <- str_extract_all(df_vhs5$date,"[0-9]{2}[.][0-9]{2}[.][0-9]{4}")
  df_vhs5$time_start <- str_extract_all(df_vhs5$date, "[0-9]{2}[:][0-9]{2}")
  
  
  
  # Grundbildung
  vhs6_url <- "https://www.vhs-wuerzburg.info/programm/grundbildung.html"
  vhs6_titel_voll <- vector()
  vhs6_beschreibung_voll <- vector()
  vhs6_datum_voll <- vector()
  vhs6_ort_voll <- vector()
  vhs6_links_voll <- vector()
  
  rD <- rsDriver()
  remDr <- rD[["client"]]
  remDr$navigate(vhs6_url)
  
  run <- TRUE
  i <- 1
  while (i <= 1){
    tryCatch(
      remDr$findElement(using = 'css selector', ".switch-page-up")$clickElement(),
      error= function(c) {run <<- F},
      warning = function(w) {run <<- F},
      finally = print(paste("Pressed button", i, "times"))
    )
    raw_data <- read_html(remDr$getPageSource()[[1]])
    raw_data %>%
      html_nodes(".bold .title") %>%
      html_text(trim = T) %>%
      ifelse(. == "", NA, .) -> vhs6_titel
    vhs6_titel_voll <- c(vhs6_titel_voll, vhs6_titel)
    raw_data %>%
      html_nodes(".startDate") %>%
      html_text(trim = T) %>%
      ifelse(. == "", NA, .) -> vhs6_datum
    vhs6_datum_voll <- c(vhs6_datum_voll, vhs6_datum)
    raw_data %>%
      html_nodes(".venues\\.city") %>%
      html_text(trim = T) %>%
      ifelse(. == "", NA, .) -> vhs6_ort
    vhs6_ort_voll <- c(vhs6_ort_voll, vhs6_ort)
    raw_data %>%
      html_nodes(".bold .title") %>%
      html_attr("href")-> vhs6_links
    vhs6_links_voll <- c(vhs6_links_voll, vhs6_links)
    
    
    i <- i + 1
    Sys.sleep(10)
  }
  
  # Shut down selenium
  remDr$close()
  rm(rD)
  gc()
  
  #Link erweitern
  vhs6_links_voll <- paste0("https:", vhs6_links_voll)
  
  df_vhs6 <- data.frame(title= vhs6_titel_voll, url=vhs6_links_voll, description = NA, date = vhs6_datum_voll, city = vhs6_ort_voll, Adresse ="", lat = 49.7821, lng = 9.938)
  
  df_vhs6$date <- str_replace_all(df_vhs6$date, "[A-z]{2}[.][,][ ]","")
  
  df_vhs6$date_start <- str_extract_all(df_vhs6$date,"[0-9]{2}[.][0-9]{2}[.][0-9]{4}")
  df_vhs6$time_start <- str_extract_all(df_vhs6$date, "[0-9]{2}[:][0-9]{2}")
  
  
  #Alle VHS-Kurse zusammenfassen
  df_vhs_to <- rbind(df_vhs1, df_vhs2, df_vhs3, df_vhs4, df_vhs5, df_vhs6)
  
  #Enddatum, Enduhrzeit, Organizer einf?gen
  df_vhs_to$time_end <- NA
  df_vhs_to$date_end <- NA
  df_vhs_to$organizer <- "vhs Wuerzburg & Umgebung e. V. "
  df_vhs_to$zip <- 97070
  df_vhs_to$city <- "Würzburg"
  df_vhs_to$street <- "Münzstraße 1"
  df_vhs_to$price <- NA
  df_vhs_to <- df_vhs_to[c("title","url", "description","lng", "lat", "city", "street", "zip", "date_start", "date_end", "time_start", "time_end","price","organizer")]
  
  #Wenn time_start = character(0), dann NA zuweisen
  for(i in 1:length(df_vhs_to$time_start)){
    if(identical(df_vhs_to[i,]$time_start, character(0))){
      df_vhs_to[i,]$time_start<- NA
    }
  }
  
  #Endatum = Startdatum und Enduhrzeit = Startuhrzeit
  for (i in 1:nrow(df_vhs_to)){
    if (is.na(df_vhs_to[i,]$date_end)){
      df_vhs_to[i,]$date_end <- df_vhs_to[i,]$date_start
    }
  }
  
  # Enduhrzeit = Startuhrzeit falls na
  for (i in 1:nrow(df_vhs_to)){
    if (is.na(df_vhs_to[i,]$time_end)){
      df_vhs_to[i,]$time_end <- as.character(df_vhs_to[i,]$time_start)
    }
  }
  
  
  #dates to date Format
  df_vhs_to$date_end <- as.character(df_vhs_to$date_end)
  df_vhs_to$date_end <- as.Date(df_vhs_to$date_end,format="%d.%m.%Y")
  df_vhs_to$date_start <- as.character(df_vhs_to$date_start)
  df_vhs_to$date_start <- as.Date(df_vhs_to$date_start,format="%d.%m.%Y")
  
  #time to times Format
  for (i in 1:nrow(df_vhs_to)){
    if (is.na(df_vhs_to[i,]$time_end)){
    }
    else {
      df_vhs_to[i,]$time_end<- paste0(df_vhs_to[i,]$time_end,":00")
    }
  }
  df_vhs_to$time_end <- times(df_vhs_to$time_end)
  
  
  for (i in 1:nrow(df_vhs_to)){
    if (is.na(df_vhs_to[i,]$time_start)){
    }
    else {
      df_vhs_to[i,]$time_start<- paste0(df_vhs_to[i,]$time_start,":00")
    }
  }
  df_vhs_to$time_start <- times(df_vhs_to$time_start)
  
  #----------------------------------------------------
  #Aus Links Adresse   -> Fehlermeldung da Zeit?berschreitung
  #links_v1 <- as.character(df_vhs_to$url)
  #getDataAdresse <- function(links_v1){
  # links_v1 %>%
  # read_html() %>%
  # html_nodes("p span") %>%
  # html_text(trim = T) -> adresse
  #}
  #Adresse_List1 <- map(links_v1, getDataAdresse) 
  # <- unlist(Adresse_List1, use.names=FALSE)
  #---------------------------------------------------------
  
  return(df_vhs_to)
  
}
getMuseumAmDom = function(){
  print("Museum am Dom")
  url <- "https://www.museum-am-dom.de/veranstaltungen.htm"
  
  # ziehe Tag und Uhrzeit
  url %>%
    read_html() %>%
    html_nodes(".calDate") %>%
    html_text() -> date_time
  
  # suche Datum
  date <- unlist(strapply(date_time, "\\d{1,2}\\.\\d{1,2}\\."))
  
  # Startdatum immer gleich Enddatum, also identisch
  date_start <- sapply(date, function(x) paste0(x, "2018"))
  date_end <- date_start
  
  # ziehe Zeit und füge, falls es fehlt ":00" hinzu
  time_start <- sapply(date_time, function(x) str_extract(x, '\\d{1,2}:\\d{1,2}(?=\\sUhr)|\\d{1,2}(?=\\sUhr)'))
  time_start <- sapply(time_start, function(x) ifelse(grepl(":", x), x, paste0(x, ":00"))) 
  time_end <- rep(NA, length(date_start))
  
  date_start <- as.Date(date_start, format="%d.%m.%Y")
  date_end <- as.Date(date_end, format="%d.%m.%Y")
  time_start <- times(paste0(time_start,":00"))
  
  
  # ziehe Namen 
  url %>%
    read_html() %>%
    html_nodes(".calDescr a") %>%
    html_text() -> title

  # ziehe Infos 
  url %>%
    read_html() %>%
    html_nodes("p+ p") %>%
    html_text() -> info
  
  # Preis kommt immer nach "Kosten:". Manchmal endet er erst mit dem nÃ¤chsten Satz ".Bitte anmelden"
  preis_info <- function(string){
    if(grepl(".Bitte anmelden", string)){
      price = str_extract(string, "(?<=Kosten:\\s).+(?=.Bitte)")
    }
    else {
      price = str_extract(string, "(?<=Kosten:\\s).+")
    }
    return(price)
  }
  price <- sapply(info, preis_info)
  
  # Location ist immer identisch
  organizer <- rep("Museum am Dom", length(title))
  
  city <- rep("Wuerzburg", length(title))
  street <- rep("Domerschulstrasse 2", length(title))
  zip <- rep(97070, length(title))
  lng <- rep(9.9316742, length(title))
  lat <- rep(49.793606, length(title))
  
  # link so oft wie es Titel gibt
  link <- rep("https://www.museum-am-dom.de/veranstaltungen.htm", length(title))
  
  # mache dataframe daraus
  mus_df <- data.frame(title, link, info, lng, lat, city, street, zip, date_start, date_end, time_start, time_end, price, organizer)
  
  colnames(mus_df) <- c("title", "url", "description", "lng", "lat", "city", "street", "zip","date_start", "date_end", "time_start","time_end","price", "organizer")
  mus_df$title=as.character(title)
  mus_df$url=as.character(url)
  mus_df$city=as.character(city)
  mus_df$street=as.character(street)
  mus_df$organizer=as.character(organizer)
  mus_df$price=as.character(price)
  
  rownames(mus_df) <- NULL
  
  return(mus_df)
  
  
}
getClubKatze <- function(){
  print("Katze")
  url_samstag <- "http://katze-club.de/portfolio_page/haltdieklappe/"
  url_montag <- "http://katze-club.de/portfolio_page/montaggetraenkepyramide/"
  
  get_data <- function(url,date_start){
    
    
    # Get HTML
    url %>% 
      read_html() -> raw_data
    
    raw_data%>%
      html_node(".title_subtitle_holder span")%>%
      html_text(trim = TRUE) -> title
    
    raw_data%>%
      html_node("strong")%>%
      html_text(trim = TRUE) -> zeit_price
    
    raw_data%>%
      html_node("h5+ p")%>%
      html_text(trim = TRUE) -> description
    
    #Adresse erstmal hardcoded, lookup wird noch erstellt und dann nachgetragen
    
    lng <- 9.92626
    
    lat <- 49.79761
    
    city <- "Wuerzburg"
    
    street <- "Gerberstrasse 14"
    
    zip <- 97070
    
    organizer <- "Club Katze"
    
    
    
    str_split(zeit_price, "\n", simplify = FALSE) -> daten_split
    
    as.character(daten_split)
    
    daten_split[[1]] -> uhrzeit
    
    uhrzeit[1] -> uhrzeit
    
    
    regex_uhrzeit  = "[0-9]{2}"
    
    uhrzeit %>%
      str_extract_all(regex_uhrzeit, simplify = TRUE) -> uhrzeit
    
    uhrzeit[1] -> time_start
    uhrzeit[2] -> time_end
    
    paste0(time_start, ":00") -> time_start
    paste0(time_end, ":00") -> time_end
    
    daten_split[[1]] -> price_raw
    
    price_raw[2] -> price_raw
    
    price_raw %>%
      str_replace("Eintritt: ", "") -> price
    
    date_end <- date_start
    
    df <- data.frame(title, url, description, lng, lat, city, street, zip, date_start, date_end, time_start, time_end, price, organizer)
    
    return(df)
    
  }
  
  #Auf der Webseite nur Montag und Freitag gegeben keine Daten - deshalb getMondays und getSaturdays
  
  getAllMondays <- function(year) {
    days <- as.POSIXlt(paste(year, 1:366, sep="-"), format="%Y-%j")
    Ms <- days[days$wday==1]
    Ms[!is.na(Ms)]  # Needed to remove NA from day 366 in non-leap years
  }
  
  getAllSaturdays <- function(year) {
    days <- as.POSIXlt(paste(year, 1:366, sep="-"), format="%Y-%j")
    Ms <- days[days$wday==6]
    Ms[!is.na(Ms)]
  }
  
  
  
  getAllMondays(2018) -> all_mondays
  getAllSaturdays(2018) -> all_saturdays
  
  get_data(url_montag,all_mondays) -> data_monday
  get_data(url_samstag,all_saturdays) -> data_saturdays
  
  
  df_katze_events <- bind_rows(data_monday, data_saturdays)
  
  #time zu times format mit chron package
  df_katze_events$time_start <- times(paste0(df_katze_events$time_start, ":00"))
  df_katze_events$time_end <- times(paste0(df_katze_events$time_end, ":00"))
  
  #Factors zu chr casten
  df_katze_events$title <- as.character(df_katze_events$title)
  df_katze_events$city <- as.character(df_katze_events$city)
  df_katze_events$street <- as.character(df_katze_events$street)
  df_katze_events$organizer <- as.character(df_katze_events$organizer)
  df_katze_events$price <- as.character(df_katze_events$price)
  as.Date(df_katze_events$date_start)->df_katze_events$date_start
  as.Date(df_katze_events$date_end)->df_katze_events$date_end
  
  return(df_katze_events)
  
}
getZauberberg <- function(){
  
  print("Zauberberg")
  
  url_zauberberg <- "http://www.zauberberg.info/cls_blu-e.cfm?cmd=04"
  nodes_zauberberg <- ".text div+ div , .teaser div"
  
  raw_crawler <- function(url, nodes) {
    read_html(url) %>%
      html_nodes(nodes) %>%
      html_text(trim = TRUE) 
  } 
  
  #Raw Data
  raw_zauberberg <- raw_crawler(url_zauberberg, nodes_zauberberg)
  
  #Dataframe erstellen
  tidy_zauberberg <- as.data.frame(split(raw_zauberberg, 1:6))
  tidy_zauberberg <- tidy_zauberberg %>%
    select(date_start = X1, title = X3, description = X5) %>%
    separate(title, c("title", "time_start"), sep = " ab ") %>%
    mutate(url = url_zauberberg) %>%
    mutate(lng = "9.9217") %>%
    mutate(lat = "49.80271") %>%
    mutate(city = "Wuerzburg") %>%
    mutate(street = "Veitshoechheimer Str. 20") %>% 
    mutate(zip = "97080") %>%
    mutate(date_end = date_start) %>%
    mutate(time_end = NA) %>%
    mutate(price = NA) %>%
    mutate(organizer = "Zauberberg")
  
  #Anpassung des Inhalts 
  tidy_zauberberg$title <- gsub("\\s+$", "", tidy_zauberberg$title)
  tidy_zauberberg$title <- substr(tidy_zauberberg$title, 1, nchar(tidy_zauberberg$title)-1)
  
  tidy_zauberberg$time_start <- gsub("^\\s+", "", tidy_zauberberg$time_start)
  tidy_zauberberg$time_start <- str_sub(tidy_zauberberg$time_start, 1, 9) 
  tidy_zauberberg$time_start <- gsub("Uhr", "", tidy_zauberberg$time_start)
  tidy_zauberberg$time_start <- gsub("\\s+$", "", tidy_zauberberg$time_start)
  tidy_zauberberg$time_start <- paste0(tidy_zauberberg$time_start, ":00") 
  tidy_zauberberg$time_start <- substr(tidy_zauberberg$time_start, 0, 5)
  
  tidy_zauberberg$date_start <- str_sub(tidy_zauberberg$date_start, 4)
  tidy_zauberberg$date_start <- dmy(tidy_zauberberg$date_start)
  tidy_zauberberg$date_end <- str_sub(tidy_zauberberg$date_end, 4)
  tidy_zauberberg$date_end <- dmy(tidy_zauberberg$date_end)
  
  #Anpassung der Datenformate 
  tidy_zauberberg$description <- as.character(tidy_zauberberg$description)
  tidy_zauberberg$lng <- as.numeric(tidy_zauberberg$lng)
  tidy_zauberberg$lat <- as.numeric(tidy_zauberberg$lat)
  tidy_zauberberg$zip <- as.numeric(tidy_zauberberg$zip)
  tidy_zauberberg$time_end <- as.character(tidy_zauberberg$time_end)
  tidy_zauberberg$price <- as.character(tidy_zauberberg$price)
  tidy_zauberberg$time_start <- times(paste0(tidy_zauberberg$time_start, ":00"))
  tidy_zauberberg$time_end <- times(tidy_zauberberg$time_end)
  
  
  #Reihenfolge der Spalten
  tidy_zauberberg <- tidy_zauberberg[,c(2,5,4,6,7,8,9,10,1,11,3,12,13,14)] 
  
  return(tidy_zauberberg)
  
}
getDeutschaus = function() {
  print("Deutschhaus")
  # Anfang: Veranstalterinformationen herausfinden 
  url_dh <- "http://www.deutschhaus.de/veranstaltungen-amp-news/"
  
  url_dh %>%
    read_html() %>%
    html_nodes("#Logo div") %>%
    html_text() -> impressum
  
  impressum %>%
    str_replace_all("\\t", "") %>%
    str_replace_all("\\n", "") %>%
    str_split("\\\r") %>%
    unlist() -> impressum2
  
  veranstalter <- impressum2[2]
  strasse <- impressum2[3]
  ort2 <- impressum2[4]
  
  ort2 %>%
    str_extract("\\d{5}") -> PLZ
  ort2 %>%
    str_extract(" .+") %>%
    trimws("both") -> ortsname
  
  preVeranstaltrUndAdresse <- str_c(veranstalter, strasse, ort2, sep = ",")
  
  #LatAndLong <- geocode(preVeranstaltrUndAdresse)
  
  #lat <- LatAndLong[2]
  #lon <- LatAndLong[1]
  lat <- 49.794
  lon <- 9.92062
  
  # Ende: Veranstalterinformationen herausfinden 
  
  
  url_dh %>% 
    read_html() -> raw_dh
  
  titel_selector <- "#Content a"
  datum_selector <- ".cal_list_date"
  
  raw_dh %>%
    html_nodes(titel_selector) %>%
    html_text(trim = T) -> titel
  raw_dh %>%
    html_nodes(datum_selector) %>%
    html_text() -> datum
  
  titel %>%
    str_replace_all("[:space:]{4,}", "|") %>%
    str_split_fixed("\\|", 2) -> data_dh
  
  # muss klammer am ende sein und die klammer muss mit zwei zahlen starten
  uhrzeit_match <- "\\((\\d{2})(.*)\\)$"
  titel %>%
    str_extract_all(uhrzeit_match) -> uhrzeit2 
  
  uhrzeit3 <- unlist(lapply(uhrzeit2,function(x) if(identical(x,character(0))) ' ' else x))
  uhrzeit3 %>%
    str_replace_all("\\(", "") %>%
    str_replace_all("\\)", "") %>%
    str_replace_all(" ", "") -> uhrzeit3
  
  # eigentlich relevanter match, hole nur die Zeitspannen die klar definiert sind 
  uhrzeit_match3 <- "((([0-9]|0[0-9]|1[0-9]|2[0-3]):[0-5][0-9])[:space:]?-[:space:]?(([0-9]|0[0-9]|1[0-9]|2[0-3]):[0-5][0-9]))"
  
  uhrzeit3 %>%
    str_extract_all(uhrzeit_match3) -> uhrzeit4
  uhrzeit4 <- unlist(lapply(uhrzeit4,function(x) if(identical(x,character(0))) ' ' else x))
  
  # aufsplitten der Zeiten / quasi in von und bis feld
  uhrzeit4 %>%
    str_replace_all(" ", "") %>%
    str_split_fixed("-", 2) -> uhrzeit5
  
  # :00 bei verfügbaren Zeiten anhÃ¤ngen
  for(i in 1:nrow(uhrzeit5)){
    for(j in 1:ncol(uhrzeit5)){
      if(uhrzeit5[i,j]!=""){
        uhrzeit5[i,j] <- paste0(uhrzeit5[i,j], ":00")
      }
    }
  }
  
  start_uhrzeit <- uhrzeit5[,1]
  end_uhrzeit <- uhrzeit5[,2]
  
  # NA eintrag entfernen
  #data_dh <- data_dh[-1,]
  
  data_dh2 <- cbind(data_dh, start_uhrzeit, end_uhrzeit)
  
  colnames(data_dh2) <- c("Datum", "Titel", "Start_Uhrzeit", "End_Uhrzeit")
  
  data_dh2 <- as.data.frame(data_dh2,stringsAsFactors=FALSE)
  
  data_dh2$Datum <- str_replace_all(data_dh2$Datum, " ", "")
  
  data_dh2 %>%
    separate(Datum, into = c("von", "bis"), sep= "\\-") -> data_dh3
  
  # Test für Jahreswechsel: data_dh3 <- rbind(data_dh3, c("30.12","03.01"))
  
  # fülle leere felder mit NA auf
  for(i in 1:nrow(data_dh3)){
    for(j in 1:ncol(data_dh3)){
      if(data_dh3[i,j]== " "||data_dh3[i,j]== ""||is.na(data_dh3[i,j])){
        data_dh3[i,j] <- NA
      }
    }
  }
  
  # füge enddatum gleich anfangsdatum wenn kein zeitraum
  for(i in 1:nrow(data_dh3)){
    if(is.na(data_dh3[i,"bis"])){
      data_dh3[i,"bis"] <- data_dh3[i,"von"]
    }
  }
  
  monat_aktuell <- month(as.POSIXlt(Sys.Date(), format="%Y/%m/%d"))
  
  # konvertiere von/bis datum zu Date
  data_dh3$von <- as.Date(data_dh3$von, format ='%d.%m')
  data_dh3$bis <- as.Date(data_dh3$bis, format ='%d.%m')
  
  # Jahreswechsel für "von" Spalte
  for(i in 1:nrow(data_dh3)){
    monat_veranstaltung <- month(as.POSIXlt(data_dh3[i, "von"], format="%Y/%m/%d"))
    
    if(!is.na(data_dh3[i, "von"])){
      if(monat_veranstaltung < monat_aktuell){
        year(data_dh3[i, "von"]) <- year(data_dh3[i, "von"]) + 1
      }
    }
  }
  # Jahreswechsel für "bis" Spalte
  for(i in 1:nrow(data_dh3)){
    monat_veranstaltung <- month(as.POSIXlt(data_dh3[i, "bis"], format="%Y/%m/%d"))
    
    if(!is.na(data_dh3[i, "bis"])){
      if(monat_veranstaltung < monat_aktuell){
        year(data_dh3[i, "bis"]) <- year(data_dh3[i, "bis"]) + 1
      }
    }
  }
  
  
  df_dh <- data.frame(title = data_dh3[,"Titel"],
                      url = url_dh,
                      description = NA,
                      lng = lon,
                      lat = lat,
                      city = ortsname,
                      street = strasse,
                      zip = PLZ,
                      date_start = data_dh3[,"von"],
                      date_end = as.Date(data_dh3[, "bis"]),
                      time_start = data_dh3[,"Start_Uhrzeit"],
                      time_end = data_dh3[, "End_Uhrzeit"],
                      price = NA,
                      organizer = veranstalter,
                      stringsAsFactors = F
  )
  
  df_dh <- df_dh[-1,]
  data_dh <- df_dh
  
  return(data_dh)
}
getAuferstehung = function() {
  print("Auferstehung")
  veranstalter_auf <- "Evang.-Luth. Auferstehungskirche"
  
  adresse_auf <- "Hans-LÃ¶ffler-Strasse 33"
  
  plz_auf <- "97074"
  
  ortsname_auf <- "Würzburg"
  
  #Lat Long Dynamisch
  # lat_long_temp <- geocode(paste(veranstalter_auf, adresse_auf, plz_auf, ortsname_auf, sep = ", "))
  # lat_auf <- lat_long_temp$lat
  # long_auf <- lat_long_temp$lon
  
  lat_auf <- 49.7728
  long_auf <- 9.95733
  
  url_auf <- link_temp <- "https://www.auferstehung-wue.de/?seite=15"
  
  auferstehung <- url_auf %>%
    read_html() %>%
    html_nodes("#main div , h4") 
  
  #Jeder zweite Eintrag aus 'auferstehung' ist wichtig
  counter <- seq(from =1, to = length(auferstehung),by = 2)
  
  #Iteriere durch alle wichtigen EintrÃ¤ge
  for(i in counter){
    
    #Textblock abspeichern
    auf_temp <- html_text(auferstehung[i])
    
    #titel aus nÃ¤chstem Eintrag auslesen
    titel_temp <- auferstehung[i+1] %>%
      html_text(trim = T)
    
    #Datum
    start_date_temp <- end_date_temp <- as.Date(sub(".*(\\d{2}.\\d{2}.\\d{4}).*","\\1", auf_temp), "%d.%m.%Y")
    
    #Falls kein Datum, setze NA
    if(!grepl(".*(\\d{2}.\\d{2}.\\d{4}).*", auf_temp)){
      start_date_temp <- as.Date(NA)
      end_date_temp <- as.Date(NA)
    }
    
    #Uhrzeit
    if(grepl(".* (\\d{2}:\\d{2}) .*", auf_temp)){
      start_time_temp <- times(paste(sub(".* (\\d{2}:\\d{2}) .*","\\1", auf_temp),"00", sep = ":"))
    } else {
      start_time_temp <- times(NA)
    }
    end_time_temp <- times(NA)
    
    
    #Links
    link_temp <- auferstehung[i] %>%
      html_nodes("a") %>%
      html_attr("href")
    
    check <- 0
    
    #Errorhandling falls kein Link existiert, setze leeren String
    if(identical(link_temp, character(0))|| is.na(link_temp)){
      
      link_temp <- ""
      check <- 1 #setze check auf 1 für die Beschreibung
      
    }
    
    #Link zusammenfügen
    link_temp <- paste("https://www.auferstehung-wue.de/",link_temp, sep = "")
    
    
    #Beschreibung (nur falls check = 0 ist, suche nach der Beschreibung, sonst NA)
    beschreibung_temp <- NA
    if(check==0){
      beschreibung_temp <- link_temp %>%
        read_html() %>%
        html_nodes("p") %>%
        html_text() %>%
        noquote() %>%
        paste(beschreibung_temp, collapse ="\n")
    }
    
    #Merge into Dataframe
    df_auf <- data.frame(titel = titel_temp,
                         url = link_temp,
                         description = beschreibung_temp,
                         lng = long_auf,
                         lat = lat_auf,
                         city = ortsname_auf,
                         street = adresse_auf,
                         zip = plz_auf,
                         date_start = start_date_temp,
                         date_end = end_date_temp,
                         time_start = start_time_temp,
                         time_end = end_time_temp,
                         price = as.character(NA),
                         organizer = veranstalter_auf, stringsAsFactors = F)
    
    # df_auf <- rbind(df_all, df_all2)
    
  }
  
  colnames(df_auf) <- c("title", "url", "description", "lng", "lat", "city", "street", "zip", "date_start", "date_end", "time_start", "time_end", "price", "organizer")
  df_auf$zip <- as.numeric(df_auf$zip)
  
  return(df_auf)
}
getHofkeller = function(){
  print("Hofkeller")
  hofkellerVeranstalter = hofkellerDatumVon = hofkellerDatumBis = hofkellerStartzeit = hofkellerEndzeit = hofkellerTitel = hofkellerBeschreibung = 
    hofkellerAdresse = hofkellerPLZ = hofkellerOrtsname = hofkellerLat = hofkellerLong = hofkellerLink = hofkellerEintritt = NA
  
  
  hofkellerVeranstalter <- "Staatlicher Hofkeller Würzburg"
  
  "https://www.hofkeller.de/kontakt" %>% 
    read_html() %>% 
    html_nodes("p:nth-child(1)") %>% 
    str_split("<br>\n") %>% 
    unlist() %>% 
    str_remove_all("<[^>]*>") -> hofkellerAnschrift
  
  hofkellerAnschrift %>% 
    tail(n=2) %>% 
    .[1] -> hofkellerAdresse
  
  hofkellerAnschrift %>% 
    tail(n=2) %>% 
    .[2] %>% 
    gsub("([0-9]{5}) (.*)", "\\1", .) -> hofkellerPLZ
  
  hofkellerAnschrift %>% 
    tail(n=2) %>% 
    .[2] %>% 
    gsub("([0-9]{5}) (.*)", "\\2", .) -> hofkellerOrtsname
  
  if ( identical(hofkellerAdresse, character(0)) ){
    hofkellerAdresse <- NA
  }
  if ( identical(hofkellerPLZ, character(0)) ){
    hofkellerPLZ <- NA
  }
  if ( identical(hofkellerOrtsname, character(0)) ){
    hofkellerOrtsname <- NA
  }
  
  preHofkellerVeranstaltrUndAdresse <- paste(hofkellerVeranstalter, hofkellerAdresse, hofkellerPLZ, hofkellerOrtsname, sep = ", ")
  
  # hofkellerLatAndLong <- geocode(preHofkellerVeranstaltrUndAdresse)
  # 
  # hofkellerLat <- hofkellerLatAndLong[2]
  # hofkellerLong <- hofkellerLatAndLong[1]
  hofkellerLat <- 49.79387
  hofkellerLong <- 9.938243
  
  
  getHofkellerEvents <- function(url) {
    
    hofkellerLink <- url
    
    url %>% 
      read_html() %>% 
      html_nodes("#product_addtocart_form h1") %>% 
      html_text() -> hofkellerDatumUndTitel
    
    if( grepl("[0-9]{1,2}\\.[0-9]{1,2}\\.20[0-9]{2}", hofkellerDatumUndTitel) ){
      hofkellerDatumUndTitel %>% 
        gsub("^([0-9]{1,2}\\.[0-9]{1,2}\\.20[0-9]{2}) (.*)", "\\1", .) %>% 
        parse_date("%d.%m.%Y",locale=locale("de")) -> hofkellerDatumVon
      
      hofkellerDatumBis <- hofkellerDatumVon
      
    } else if( grepl("[0-9]{1,2}\\.[0-9]{2}\\.[0-9]{2} ", hofkellerDatumUndTitel) ){
      hofkellerDatumUndTitel %>% 
        gsub("^([0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{2}) (.*)", "\\1", .) %>% 
        parse_date("%d.%m.%y",locale=locale("de")) -> hofkellerDatumVon
      
      hofkellerDatumBis <- hofkellerDatumVon
    } else {
      hofkellerDatumVon <- as.Date(NA)
      hofkellerDatumBis <- as.Date(NA)
    }
    
    if( grepl("^([0-9]{1,2}\\.[0-9]{1,2}\\.20[0-9]{2}) (.*)", hofkellerDatumUndTitel) ) {
      hofkellerDatumUndTitel %>%
        gsub("^([0-9]{1,2}\\.[0-9]{1,2}\\.20[0-9]{2}) (.*)", "\\2", .) -> hofkellerTitel
      
    } else if( grepl("[0-9]{1,2}\\.[0-9]{2}\\.[0-9]{2}", hofkellerDatumUndTitel) ) {
      hofkellerDatumUndTitel %>%
        gsub("([0-9]{1,2}\\.[0-9]{2}\\.[0-9]{2}) (.*)", "\\2", .) -> hofkellerTitel
    } else{
      hofkellerTitel <- NA
    }
    
    
    url %>% 
      read_html() %>% 
      html_nodes("#product_addtocart_form .std") %>% 
      html_text() -> preHofkellerZeit
    
    if( grepl("[0-9]{1,2}(\\.|:)[0-9]{2} bis [0-9]{1,2}(\\.|:)[0-9]{2}", preHofkellerZeit) )  {
      preHofkellerZeit %>% 
        str_extract("[0-9]{1,2}(\\.|:)[0-9]{2} bis [0-9]{1,2}(\\.|:)[0-9]{2}") %>% 
        gsub("([0-9]{1,2}(\\.|:)[0-9]{2}) bis ([0-9]{1,2}(\\.|:)[0-9]{2})", "\\1",.) %>% 
        str_replace_all("\\.", ":") -> hofkellerStartzeit
      
      hofkellerStartzeit <- times(paste0(hofkellerStartzeit, ":00"))
      
      preHofkellerZeit %>% 
        str_extract("[0-9]{1,2}(\\.|:)[0-9]{2} bis [0-9]{1,2}(\\.|:)[0-9]{2}") %>% 
        gsub("([0-9]{1,2}(\\.|:)[0-9]{2}) bis ([0-9]{1,2}(\\.|:)[0-9]{2})", "\\3",.) %>% 
        str_replace_all("\\.", ":") -> hofkellerEndzeit
      
      hofkellerEndzeit <- times(paste0(hofkellerEndzeit, ":00"))
      
    } else if( grepl("ab [0-9]{1,2}(\\.|:)[0-9]{2}", preHofkellerZeit) ){
      preHofkellerZeit %>% 
        gsub(".* ab ([0-9]{1,2}(\\.|:)[0-9]{2}) .*", "\\1",.) %>% 
        str_replace_all("\\.", ":") -> hofkellerStartzeit
      
      hofkellerStartzeit <- times(paste0(hofkellerStartzeit, ":00"))
      
      hofkellerEndzeit <- times(NA)
      
    } else {
      hofkellerStartzeit <- times(NA)
      hofkellerEndzeit <- times(NA)
    }
    
    if(hofkellerTitel == "Ã–WP" | hofkellerTitel == "Ã–wp"){
      hofkellerTitel <- "Ã–ffentliche Weinprobe"
    }
    
    url %>%
      read_html() %>% 
      html_nodes(".box-description .std") %>% 
      html_text() %>% 
      unlist() %>% 
      str_replace_all("[[:space:]]{2,}", " ") %>% 
      str_trim("both") %>% 
      noquote() -> hofkellerBeschreibung
    
    
    url %>% 
      read_html() %>% 
      html_nodes(".price") %>% 
      html_text() %>% 
      str_extract("[0-9]{1,4},[0-9]{2}") %>% 
      str_replace(",", "\\.") %>% 
      as.character() -> hofkellerEintritt
    
    hofkellerDf2 <- data.frame(title = hofkellerTitel, url = hofkellerLink, description = hofkellerBeschreibung, lng = hofkellerLong, 
                               lat = hofkellerLat, city = hofkellerOrtsname, street = hofkellerAdresse, zip = hofkellerPLZ, 
                               date_start = hofkellerDatumVon, date_end = hofkellerDatumBis, time_start = hofkellerStartzeit, 
                               time_end = hofkellerEndzeit, price = hofkellerEintritt, organizer = hofkellerVeranstalter, 
                               stringsAsFactors = F)
    colnames(hofkellerDf2) <- c("title", "url", "description", "lng", "lat", "city", "street", "zip", "date_start", "date_end", "time_start", "time_end", "price", "organizer")
    hofkellerDf2
  }
  
  "https://shop.hofkeller.de/veranstaltungen?dir=asc&limit=100&order=position" %>% 
    read_html() %>%
    html_nodes(".link-learn") %>% 
    html_attr("href") -> hofkellerLinksammlung
  
  hofkellerDf <- map_df(hofkellerLinksammlung, getHofkellerEvents)
  
  if( !all(is.na(hofkellerDf$time_start)) ){
    hofkellerDf$time_start <- times(hofkellerDf$time_start)
  }
  if( !all(is.na(hofkellerDf$time_end)) ){
    hofkellerDf$time_end <- times(hofkellerDf$time_end)
  }
  
  return(hofkellerDf)
}
getWuf = function(){
  print("Wuf")
  "http://www.wufzentrum.de/index.php" %>% 
    read_html() -> urlWUF
  
  # fuer die naechsten 15 Jahre >>> CSS-Tag wird vom Admin monatlich erhÃ¶ht
  seq(1994, 2180) %>% 
    paste0("#c", .) %>% 
    paste(collapse = ' , ') -> monate_div
  
  urlWUF %>% 
    html_nodes(monate_div) %>% 
    html_text(trim = T) -> rawWUF
  
  "https://www.wufzentrum.de/index.php?id=78" %>% 
    read_html() %>% 
    html_nodes("#rightbar .bodytext:nth-child(3)")  %>%
    as.character() %>% 
    strsplit("\">|<br>") %>% 
    unlist() -> wufLocation
  
  wufLocation %>% 
    .[2] -> wufAdresse
  
  wufLocation %>% 
    .[3] %>% 
    gsub("([0-9]{5}) (.*)", "\\1", .) %>% 
    as.numeric()-> wufPLZ
  
  wufLocation %>% 
    .[3] %>% 
    gsub("([0-9]{5}) (.*)", "\\2", .) -> wufOrtsname
  
  wufVeranstalter <- "Schwulesbisches Zentrum Würzburg"
  
  wufLink <- "http://www.wuf-zentrum.de"
  
  preWufVeranstaltrUndAdresse <- paste(wufVeranstalter, wufAdresse, wufPLZ, wufOrtsname, sep = ", ")
  
  # wufLatAndLong <- geocode(preWufVeranstaltrUndAdresse)
  # 
  # wufLat <- wufLatAndLong[2]
  # wufLong <- wufLatAndLong[1]
  wufLat <- 49.7952
  wufLong <- 9.92005
  wufDatumVon = wufDatumBis = wufZeitVon = wufZeitBis = wufTitel = wufBeschreibung = wufEintritt = NA
  
  wufdf <- data.frame(stringsAsFactors = F)
  
  for (i in 1:length(rawWUF)){
    year <- gsub("^[A-z]{3,9} (20.{2}).*", "\\1", rawWUF[i])
    rawWUF[i] %>% 
      gsub("[A-zÃ¤]{3,9} 20.{2}", "", .) %>% 
      gsub("(\\n|\\t)", "", .) %>%
      str_split("\\r", simplify = F) %>% 
      unlist() %>% 
      gsub("!^[\\d]", "", .) %>% 
      str_trim("both") %>% 
      grep("^[0-9]{2}\\.[0-9]{2}", value = T, .)  -> rawSplitWufDigits
    
    #rawSplitWufDigits <- rawSplitWuf[rawSplitWuf %in% grep(paste0("\\d", collapse = "|"), rawSplitWuf, value = T)] 
    
    
    for (variable in rawSplitWufDigits) {
      
      variable %>% 
        as.character() -> variable
      
      #print(variable)
      
      variable %>% 
        noquote() %>% 
        str_trim() %>% 
        str_replace_all("\\s", " ") -> variable
      
      #print(variable)
      
      # DATUMRAUM
      if(grepl("^([0-9]{2}\\.[0-9]{2}\\.?)(-[0-9]{2}\\.[0-9]{2}\\.?) .*", variable)){
        
        wufDatumVon <- as.Date(gsub("\\.", "/",paste0(gsub("^([0-9]{2}\\.[0-9]{2}\\.?)(-[0-9]{2}\\.[0-9]{2}\\.?) .*", "\\1", variable), year)), "%d/%m/%Y")
        wufDatumBis <- as.Date(gsub("\\.", "/",paste0(gsub("^([0-9]{2}\\.[0-9]{2}\\.?)-([0-9]{2}\\.[0-9]{2}\\.?) .*", "\\2", variable), year)), "%d/%m/%Y")
        
        # Datumraum + Zeitraum
        if(grepl("^[0-9]{2}\\.[0-9]{2}\\.?-[0-9]{2}\\.[0-9]{2}\\.? ([0-9]{1,2}:[0-9]{2}-[0-9]{1,2}:[0-9]{2}).*", variable)){
          wufZeitVon <- gsub("^[0-9]{2}\\.[0-9]{2}\\.?-[0-9]{2}\\.[0-9]{2}\\.? ([0-9]{1,2}:[0-9]{2})-([0-9]{1,2}:[0-9]{2}).*", "\\1", variable)
          wufZeitVon <- times(paste0(wufZeitVon, ":00"))
          wufZeitBis <- gsub("^[0-9]{2}\\.[0-9]{2}\\.?-[0-9]{2}\\.[0-9]{2}\\.? ([0-9]{1,2}:[0-9]{2})-([0-9]{1,2}:[0-9]{2}).*", "\\2", variable)
          wufZeitBis <- times(paste0(wufZeitBis, ":00"))
          
          wufTitel <- gsub("^[0-9]{2}\\.[0-9]{2}\\.?-[0-9]{2}\\.[0-9]{2}\\.? [0-9]{1,2}:[0-9]{2}-[0-9]{1,2}:[0-9]{2} (.*)", "\\1", variable)
          
          # Datumraum + Zeitpunkt
        } else if (grepl("^[0-9]{2}\\.[0-9]{2}\\.?-[0-9]{2}\\.[0-9]{2}\\.? ([0-9]{1,2}:[0-9]{2}).*", variable)){
          wufZeitVon <- gsub("^[0-9]{2}\\.[0-9]{2}\\.?-[0-9]{2}\\.[0-9]{2}\\.? ([0-9]{1,2}:[0-9]{2}).*", "\\1", variable)  
          wufZeitVon <- times(paste0(wufZeitVon, ":00"))
          wufZeitBis <- times(NA)
          
          wufTitel <- gsub("^[0-9]{2}\\.[0-9]{2}\\.?-[0-9]{2}\\.[0-9]{2}\\.? [0-9]{1,2}:[0-9]{2} (.*)", "\\1", variable)
          
          # Datenraum + Zeitpunkt/Zeitraum unbekannt
        } else {
          wufZeitVon <- times(NA)
          wufZeitBis <- times(NA)
          wufTitel <- gsub("^[0-9]{2}\\.[0-9]{2}\\.?-[0-9]{2}\\.[0-9]{2}\\.? (.*)", "\\1", variable)
          
        }
        
        wufdf2 <- data.frame(title = wufTitel, url = wufLink, description = wufBeschreibung, lng = wufLong, lat = wufLat, city = wufOrtsname, 
                             street = wufAdresse, zip = wufPLZ, date_start = wufDatumVon, date_end = wufDatumBis, time_start = wufZeitVon, 
                             time_end = wufZeitBis, price = wufEintritt, organizer = wufVeranstalter, stringsAsFactors = F)    
        colnames(wufdf2) <- c("title", "url", "description", "lng", "lat", "city", "street", "zip", "date_start", "date_end", "time_start", "time_end", "price", "organizer")
        wufdf <- rbind(wufdf, wufdf2)
        
        # DATUMSPUNKT
      } else if (grepl("^([0-9]{2}\\.[0-9]{2}\\.) .*", variable)){
        
        wufDatumVon <- as.Date(gsub("\\.", "/", paste0(gsub("^([0-9]{2}\\.[0-9]{2}\\.).*", "\\1", variable), year)), "%d/%m/%Y")
        wufDatumBis <- as.Date(gsub("\\.", "/", paste0(gsub("^([0-9]{2}\\.[0-9]{2}\\.).*", "\\1", variable), year)), "%d/%m/%Y")
        
        
        # Datumspunkt + Zeitraum
        if(grepl("^[0-9]{2}\\.[0-9]{2}\\. ([0-9]{1,2}:[0-9]{2}-[0-9]{1,2}:[0-9]{2}).*", variable)){
          wufZeitVon <- gsub("^[0-9]{2}\\.[0-9]{2}\\. ([0-9]{1,2}:[0-9]{2})-([0-9]{1,2}:[0-9]{2}).*", "\\1", variable)
          wufZeitVon <- times(paste0(wufZeitVon, ":00"))
          wufZeitBis <- gsub("^[0-9]{2}\\.[0-9]{2}\\. ([0-9]{1,2}:[0-9]{2})-([0-9]{1,2}:[0-9]{2}).*", "\\2", variable)
          wufZeitBis <- times(paste0(wufZeitBis, ":00"))
          
          wufTitel <- gsub("^[0-9]{2}\\.[0-9]{2}\\. [0-9]{1,2}:[0-9]{2}-[0-9]{1,2}:[0-9]{2} (.*)", "\\1", variable)
          # Datumpunkt + Zeitpunkt
        } else if(grepl("^[0-9]{2}\\.[0-9]{2}\\. ([0-9]{1,2}:[0-9]{2}).*", variable)){
          wufZeitVon <- gsub("^[0-9]{2}\\.[0-9]{2}\\. ([0-9]{1,2}:[0-9]{2}).*", "\\1", variable)
          wufZeitVon <- times(paste0(wufZeitVon, ":00"))
          wufZeitBis <- times(NA)
          
          wufTitel <- gsub("^[0-9]{2}\\.[0-9]{2}\\. [0-9]{1,2}:[0-9]{2} (.*)", "\\1", variable)
        } else {
          wufZeitVon <- times(NA)
          wufZeitBis <- times(NA)
          
          wufTitel <- gsub("^[0-9]{2}\\.[0-9]{2}\\. (.*)", "\\1", variable)
        }
        ###
        wufdf2 <- data.frame(title = wufTitel, url = wufLink, description = wufBeschreibung, lng = wufLong, lat = wufLat, city = wufOrtsname, 
                             street = wufAdresse, zip = wufPLZ, date_start = wufDatumVon, date_end = wufDatumBis, time_start = wufZeitVon, 
                             time_end = wufZeitBis, price = wufEintritt, organizer = wufVeranstalter, stringsAsFactors = F)
        colnames(wufdf2) <- c("title", "url", "description", "lng", "lat", "city", "street", "zip", "date_start", "date_end", "time_start", "time_end", "price", "organizer")
        wufdf <- rbind(wufdf, wufdf2)
        # NANANA
      } else{
        #print("no")
      }    
      
    } 
    
  }
  return(wufdf)
}
getFuture = function() {
  print("Future")
  fututoFuturo <- function(url) {
    
    
    futureVeranstalter = futureDatumVon = futureDatumBis = futureStartzeit = futureEndzeit = futureTitel = futureBeschreibung = 
      futureAdresse = futurePLZ = futureOrtsname = futureLat = futureLong = futureLink = futureEintritt = NA
    
    
    futureVeranstalter <- "Futurekids Computerkurse"
    
    futureDf <- data.frame()
    
    futureLink <- url
    
    futureLink %>% 
      read_html() -> futureURL
    
    futureURL %>% 
      html_nodes(".uk-accordion-title") %>% 
      html_text()-> futureKurstitelSammlung
    
    futureURL %>% 
      html_nodes(".uk-accordion-content") -> futureKurse
    
    "http://www.futurekids-wue.de/ueber-uns.html" %>% 
      read_html() %>% 
      html_nodes(".extradiv") %>% 
      grep("970[0-9]{2} W", value = T, .) %>% 
      gsub("<.*?>", "", .) %>% 
      str_split("\\n") %>% 
      unlist() -> futureAnschrift
    
    futureAnschrift %>% 
      grep("([A-zÃ¶Ã¤ü]+str.)|(Strasse)|(Strasse)|(strasse)|(strasse)", value = T,.) %>% 
      str_trim("both") -> futureAdresse 
    
    futureAnschrift %>% 
      grep("970[0-9]{2} W", value = T, .) %>%
      str_trim("both") %>% 
      #str_match("970[0-9]{2}")
      gsub("^(970[0-9]{2}) ([A-zü]*)", "\\1", .) -> futurePLZ
    
    futureAnschrift %>% 
      grep("970[0-9]{2} W", value = T, .) %>%
      str_trim("both") %>% 
      #str_match("970[0-9]{2}")
      gsub("^(970[0-9]{2}) ([A-zÃ¤Ã¶ü]*)", "\\2", .) -> futureOrtsname
    
    preFutureVeranstaltrUndAdresse <- paste(futureVeranstalter, futureAdresse, futurePLZ, futureOrtsname, sep = ", ")
    
    # futureLatAndLong <- geocode(preFutureVeranstaltrUndAdresse)
    # 
    # futureLat <- futureLatAndLong[2]
    # futureLong <- futureLatAndLong[1]
    futureLat <- 49.797070
    futureLong <- 9.9354519
    
    
    for (n in 1:length(futureKurse)) {
      
      futureKurse[n] %>%  
        html_nodes(".agetimediv") %>% 
        html_text(trim = T) -> testObFixerTermin 
      
      if( any(grepl("(Mo\\. |Di\\. |Mi\\. |Do\\. |Fr\\. |Sa\\. |So\\.)[0-9]{2}\\.[0-9]{2}\\.[0-9]{2}", testObFixerTermin)) ){
        
        futureTitel <- futureKurstitelSammlung[n] %>% 
          as.character() %>% 
          noquote()
        
        testObFixerTermin %>% 
          str_split("\\n") %>% 
          #gsub("[[:space:]]{2,}", "", .) %>% 
          unlist() %>% 
          str_replace_all("[[:space:]]{2,}","") %>% 
          str_replace_all(" oder$", "") -> futureDatumUndAltersempfehlung
        
        futureDatumUndAltersempfehlung %>% 
          grep("(Mo\\. |Di\\. |Mi\\. |Do\\. |Fr\\. |Sa\\. |So\\.)[0-9]{2}\\.[0-9]{2}\\.[0-9]{2}", value = T,.) -> DatumsListeChar
        
        futureDatumUndAltersempfehlung %>% 
          grep("(Mo\\. |Di\\. |Mi\\. |Do\\. |Fr\\. |Sa\\. |So\\.)[0-9]{2}\\.[0-9]{2}\\.[0-9]{2}", value = T, invert = T, .) -> futureAltersempfehlung
        
        futureKurse[n] %>% 
          html_nodes("p") %>% 
          html_text(trim = T) %>% 
          gsub("  ", "",.) %>% 
          gsub("\\n", "",.) %>% 
          noquote() -> futurePreBeschreibung
        
        futureKurse[n] %>% 
          html_nodes("strong:nth-child(3)") %>% 
          html_text() %>% 
          str_replace_all("\\.- ", "\\.00 ") %>% 
          str_match_all("[0-9]{1,4}\\.[0-9]{2}") %>% 
          unlist() %>% 
          as.numeric() %>% 
          sum() %>% 
          as.character() -> futureEintritt
        
        for (variable in DatumsListeChar) {
          
          variable %>% 
            str_match("[0-9]{2}\\.[0-9]{2}\\.[0-9]{2}") %>% 
            as.Date("%d.%m.%y") -> futureDatumVon
          
          futureDatumBis <- futureDatumVon
          
          variable %>% 
            str_match("[0-9]{2}:[0-9]{2}.{1,4}[0-9]{2}:[0-9]{2}") %>%
            gsub("([0-9]{2}:[0-9]{2}) . ({1,4}[0-9]{2}:[0-9]{2})", "\\1",.) %>% 
            gsub(" ", "", .) %>% 
            as.character()-> futureStartzeit
          
          futureStartzeit <- times(paste0(futureStartzeit, ":00"))
          
          variable %>% 
            str_match("[0-9]{2}:[0-9]{2}.{1,4}[0-9]{2}:[0-9]{2}") %>% 
            gsub("([0-9]{2}:[0-9]{2}) . ({1,4}[0-9]{2}:[0-9]{2})", "\\2",.) %>% 
            gsub(" ", "", .) %>% 
            as.character()-> futureEndzeit
          
          futureEndzeit <- times(paste0(futureEndzeit, ":00"))
          
          futureBeschreibung <- paste(futureAltersempfehlung[1], futurePreBeschreibung, "Weiterfolgende Termine:", sep = ";  ") %>% as.character()
          futureBeschreibung <- paste(futureBeschreibung, variable, sep = " ") %>% as.character()
          
          futureDf2 <- data.frame(title = futureTitel, url = futureLink, description = futureBeschreibung, lng = futureLong, 
                                  lat = futureLat, city = futureOrtsname, street = futureAdresse, zip = futurePLZ, 
                                  date_start = futureDatumVon, date_end = futureDatumBis, time_start = futureStartzeit, 
                                  time_end = futureEndzeit, price = futureEintritt, organizer = futureVeranstalter, 
                                  stringsAsFactors = F)
          
          futureDf <- rbind(futureDf, futureDf2)
          
        }
        
        
      } 
      
    }
    futureDf
  }
  
  futureAlleLinks <- c("http://www.futurekids-wue.de/kids.html", "http://www.futurekids-wue.de/erwachsene.html")
  
  
  futureDfFINAL <- map_df(futureAlleLinks, fututoFuturo)
  
  colnames(futureDfFINAL) <- c("title", "url", "description", "lng", "lat", "city", "street", "zip", "date_start", "date_end", "time_start", "time_end", "price", "organizer")
  
  if( !all(is.na(futureDfFINAL$time_start)) ){
    futureDfFINAL$time_start <- times(futureDfFINAL$time_start)
  }
  if( !all(is.na(futureDfFINAL$time_end)) ){
    futureDfFINAL$time_end <- times(futureDfFINAL$time_end)
  }
  
  return(futureDfFINAL)
}
getUniKlinikum = function() {
  
  df_all <- data.frame(title = character(),
                       url = character(),
                       description = character(),
                       lng = numeric(),
                       lat = numeric(),
                       city = character(),
                       street = character(),
                       zip = numeric(),
                       date_start = as.Date(character()),
                       date_end = as.Date(character()),
                       time_start =times(),
                       time_end = times(),
                       price = character(),
                       organizer = character(),
                       stringsAsFactors = F)
  
  veranstalter_ukw <- "Universitätsklinikum Würzburg"
  
  adresse_ukw <- "Josef-Schneider-Straße 11"
  
  plz_ukw <- "97080"
  
  ortsname_ukw <- "Würzburg"
  
  #Lat Long Dynamisch
  #lat_long_temp <- geocode(paste(veranstalter_ukw, adresse_ukw, plz_ukw, ortsname_ukw, sep = ", "))
  # lat_ukw <- lat_long_temp$lat
  # long_ukw <- lat_long_temp$lon
  lat_ukw <- 49.80328
  long_ukw <- 9.95565
  #URL
  url_ukw <- "http://www.ukw.de/patienten-besucher/veranstaltungskalender/"
  
  #Browser Setup (RSelenium)
  rD <- rsDriver(browser = "chrome")
  remDr <- rD[["client"]]
  remDr$navigate(url_ukw)
  
  #Klicke Button 
  run <- TRUE
  i <- 1
  while (run){
    tryCatch(
      ukwDr$findElement(using = 'css selector', ".jscroll-next")$clickElement(),
      error= function(c) {run <<- F},
      warning = function(w) {run <<- F},
      finally = print(paste("Pressed button", i, "times"))
    )
    i <- i + 1
    Sys.sleep(2)
  }
  
  #Selenium auslesen und als einzelne Listenelemente speichern
  ukw_list <- read_html(remDr$getPageSource()[[1]]) %>%
    html_nodes("div.scroll-item")
  
  #ALTERNATIV: Regulärer Seitenzugriff (erste 7 Elemente)
  #ukw_list <- url_ukw %>%
  #read_html() %>%
  #html_nodes("div.scroll-item")
  
  #durch alle Elemente iterieren
  for(i in 1:length(ukw_list)){
    
    #Datum
    datum_temp <- ukw_list[i] %>%
      html_node(".date") %>%
      html_text
    
    #Datum auslesen
    if(grepl(".*(\\d{2}.\\d{2}.\\d?{4}).+(\\d{2}.\\d{2}.\\d{4}).*", datum_temp)){
      
      #String nach Leerzeichen trennen und als Liste speichern
      timeframe <- str_split(datum_temp, "[\n\r\t]+") %>%
        .[[1]]
      
      counter = T
      
      #für jedes Element der Liste nach Datumsformat checken
      for(t in timeframe){
        
        #das erste Element im Datumformat = Anfangsdatum
        if(grepl("\\d{2}\\.\\d{2}\\.\\d?{4}", t) && counter){ 
          
          start_date_temp <- sub(".*(\\d{2}\\.\\d{2}\\.).*", "\\1", t) #start_date
          counter <- F
          
          #das zweite Element ist das Enddatum 
        } else if(grepl("\\d{2}\\.\\d{2}\\.\\d{4}", t) && !counter){ 
          
          end_date_temp <- as.Date(sub(".*(\\d{2}\\.\\d{2}\\.\\d{4}).*", "\\1", t),"%d.%m.%Y") #end_date
          jahr <- sub(".*(\\d{4}).*", "\\1", t) #das Jahr auslesen
          
        }
      }
      
      #start_date mit Jahreszahl in ein Datumsformat überführen
      start_date_temp <- as.Date(paste(start_date_temp, jahr, sep = ""), "%d.%m.%Y")
      
      #Zeitpunkt auslesen
    } else if(grepl(".*(\\d{2}.\\d{2}.\\d{4}).*", datum_temp)) {
      
      #Single
      start_date_temp <- as.Date(sub(".*(\\d{2}.\\d{2}.\\d{4}).*", "\\1", datum_temp),"%d.%m.%Y")
      end_date_temp <- start_date_temp
      
      #kein Datum || nicht lesbar
    } else {
      
      #Single
      datum_temp <- NA
      
    }
    
    #Uhrzeit
    zeit_temp <- ukw_list[i] %>%
      html_node(".time") %>%
      html_text(trim=T)
    
    #Zeitspanne auslesen
    if(grepl(".*\\d{2}:\\d{2}.*-[[:space:]]*\\d{2}:\\d{2}.*",zeit_temp)){
      
      time_clean <- gsub("[[:space:]]|[\r\n\t]|Uhr","", zeit_temp)
      start_time_temp <- times(paste(gsub("(\\d{2}:\\d{2}).*","\\1", time_clean),"00", sep = ":"))
      end_time_temp <- times(paste(gsub(".*(\\d{2}:\\d{2})","\\1", time_clean),"00", sep = ":"))
      
      
      #Zeitpunkt auslesen    
    } else if(grepl(".*(\\d{2}:\\d{2}).*",zeit_temp)){
      
      start_time_temp <- times(paste(gsub("[[:space:]]|[\r\n\t]|Uhr","", zeit_temp),"00",sep =":"))
      end_time_temp <- times(NA)
      
      #keine Zeitangabe  
    } else {
      
      start_time_temp <- times(NA)
      end_time_temp <- times(NA)
      
    }
    
    #Beschreibung
    beschreibung_temp <- ukw_list[i] %>%
      html_node(".text") %>%
      html_text(trim = T) %>%
      noquote()
    
    #Falls als Liste vorliegt, diese Zusammenmergen mit einem Absatz
    beschreibung_temp <- paste(beschreibung_temp, collapse ="\n\n") 
    
    #zu kurze Beschreibungen rausfiltern
    if(nchar(beschreibung_temp) < 30){
      
      beschreibung_temp <- NA
      
    }
    
    #titel
    titel_temp <- ukw_list[i] %>%
      html_node(".title") %>%
      html_text(trim = T)
    
    #Link
    link_temp <- ukw_list[i] %>%
      html_node(".print a") %>%
      html_attr("href") %>%
      paste("https://www.ukw.de",., sep = "")
    
    #Veranstalter in der Schleife neu setzen
    veranstalter_ukw <- "Universitätsklinikum Würzburg"
    
    #Unterveranstalter auslesen (z.B. Poliklinik)
    veran <-  ukw_list[i] %>%
      html_node(".organizer") %>%
      html_text(trim = T)
    
    #Veranstalter zusammenfügen
    if (!is.na(veran) && veran!="Universitätsklinikum Würzburg"){
      
      veranstalter_ukw <- paste(veranstalter_ukw, veran, sep = " - ")
      
    }
    
    #Die Veranstaltung für jedes Datum abspeichern
    
    df_all2 <- data.frame(title = titel_temp,
                          url = link_temp,
                          description = beschreibung_temp,
                          lng = long_ukw,
                          lat = lat_ukw,
                          city = ortsname_ukw,
                          street = adresse_ukw,
                          zip = plz_ukw,
                          date_start = start_date_temp,
                          date_end = end_date_temp,
                          time_start = start_time_temp,
                          time_end = end_time_temp,
                          price = as.character(NA),
                          organizer = veranstalter_ukw,
                          stringsAsFactors = F)
    
    df_all <- rbind(df_all, df_all2)
    
  }
  remDr$close()
  rm(rD)
  gc()
  return(df_all)
}
getLoma = function() {
  
  lomaVeranstalter = lomaDatumVon = lomaDatumBis = lomaStartzeit = lomaEndzeit = lomaTitel = lomaBeschreibung = 
    lomaAdresse = lomaPLZ = lomaOrtsname = lomaLat = lomaLong = lomaLink = lomaEintritt = NA
  
  
  lomaDf <- data.frame()
  
  "http://www.loma-bar.com/wochenprogramm.html" -> lomaLink
  
  lomaLink %>% 
    read_html() -> lomaURL
  
  lomaVeranstalter <- "LOMA"
  
  "http://www.loma-bar.com/kontakt.html" %>% 
    read_html() %>% 
    html_nodes(".Stil6+ .Stil5") %>% 
    html_text() %>% 
    str_split("\\n|Telefon") %>% 
    unlist() %>%
    str_trim("both") -> lomaAnschrift
  
  lomaAnschrift %>% 
    grep("str\\.|(S|s)tra(ss|ß)e", value = T,.) %>% 
    .[1] -> lomaAdresse
  
  lomaAnschrift %>% 
    grep("^[0-9]{5} ", value = T,.) %>%
    .[1] %>% 
    gsub("([0-9]{5}) (.*)", "\\1", .) -> lomaPLZ
  
  lomaAnschrift %>% 
    grep("^[0-9]{5} ", value = T,.) %>% 
    .[1] %>% 
    gsub("([0-9]{5}) (.*)", "\\2", .) -> lomaOrtsname
  
  preLomaVeranstaltrUndAdresse <- paste(lomaVeranstalter, lomaAdresse, lomaPLZ, lomaOrtsname, sep = ", ")
  
  # lomaLatAndLong <- geocode(preLomaVeranstaltrUndAdresse)
  # 
  # lomaLat <- lomaLatAndLong[2]
  # lomaLong <- lomaLatAndLong[1]
  lomaLat <- 49.78937
  lomaLong <- 9.93054
  
  
  # gib mir das Programm der nächsten 7 Tage (inkl heute)
  for (x in 0:6) {
    # Öffnungszeiten auf der Homepage nicht angegeben, daher hardcoded
    lomaStartzeit <- "19:00"
    lomaStartzeit <- times(paste0(lomaStartzeit, ":00"))
    lomaEndzeit <- "03:00"
    lomaEndzeit <- times(paste0(lomaEndzeit, ":00"))
    lomaDatumVon <- Sys.Date()+x
    lomaDatumBis <- lomaDatumVon
    tagKuerzel <- substr(weekdays(Sys.Date()+x), 1,2) 
    
    bildName <- paste0("cal_", tagKuerzel,".jpg")
    
    # lomaURL %>% 
    #  html_node("td td table") %>%
    #   html_table() -> a
    
    xml_find_all(lomaURL, './/tr') %>% 
      grep(bildName, value = T, ignore.case = T, .) %>% 
      tail(n=1) %>% 
      str_replace_all("<.*?>", " ") %>%
      str_replace_all("[[:space:]]{2,}", " ") %>% 
      str_trim("both") -> lomaTitelundBeschreibung
    
    lomaTitelundBeschreibung %>% 
      str_extract(".*:") %>% 
      str_replace(":", "") -> lomaTitel
    
    #print(lomaTitel)
    
    lomaTitelundBeschreibung %>% 
      str_extract(":.*") %>% 
      str_replace(":", "") %>% 
      str_trim("both") -> lomaBeschreibung
    
    #print(lomaBeschreibung)
    
    
    lomaDf2 <- data.frame(title = lomaTitel, url = lomaLink, description = lomaBeschreibung, lng = lomaLong, 
                          lat = lomaLat, city = lomaOrtsname, street = lomaAdresse, zip = lomaPLZ, 
                          date_start = lomaDatumVon, date_end = lomaDatumBis, time_start = lomaStartzeit, 
                          time_end = lomaEndzeit, price = lomaEintritt, organizer = lomaVeranstalter, 
                          stringsAsFactors = F)
    
    lomaDf <- rbind(lomaDf, lomaDf2)
    
  }
  
  colnames(lomaDf) <- c("title", "url", "description", "lng", "lat", "city", "street", "zip", "date_start", "date_end", "time_start", "time_end", "price", "organizer")
  
  if( !all(is.na(lomaDf$time_start)) ){
    lomaDf$time_start <- times(lomaDf$time_start)
  }
  if( !all(is.na(lomaDf$time_end)) ){
    lomaDf$time_end <- times(lomaDf$time_end)
  }
  
  return(lomaDf)
}
getEisbahndisco_eisbahn=function (sDate, sDesc, url){
  oStartDate=as.Date(sDate,format="%d.%m.%Y")
  oEndDate=NA
  startTime=NA
  endTime=NA
  aTime=str_extract_all(sDesc,"\\d{2}:\\d{2}",simplify = TRUE)
  if (length(aTime)>=1 && !is.na(aTime[1])){
    startTime=times(paste0(aTime[1],":00"))
    
  }
  if (length(aTime)>=2 && !is.na(aTime[2])){
    oEndDate=oStartDate
    endTime=times(paste0(aTime[2],":00"))
  }
  event=data.frame(title="Eisbahndisco",description="Eisbahndisco", organizer=NA, city=NA, street=NA,zip=NA, lat=49.7746187, lng=9.9090821, date_start=oStartDate, date_end=oEndDate, url=url, time_start=startTime, time_end=endTime, price=NA)
  event$time_start=times(event$time_start)
  event$time_end=times(event$time_end)
  return (event)
}
getEvents_eisbahn=function(){
  print("Eisbahn")
  #readEventLinks
  url="https://www.wvv.de/de/privatkunden/baeder/unser-angebot/eisbahn/eishockey/"
  url %>%
    read_html() -> raw_data
  raw_data %>%
    html_nodes(".w670 table") %>%
    html_table()->oTable
  oRes=data.frame()
  if (length(oTable)>=1){
    oTable=oTable[[1]]
    if (length(oTable)==6){
      for (i in 1:length(oTable[[1]])){
        oTemp=data.frame();
        sDate=str_extract(oTable[[1]][[i]],"\\d{2}\\.\\d{2}\\.\\d{4}")
        sBeginn=oTable[[2]][[i]]
        if (startsWith(sBeginn,"Eisbahndisco")){
          event=getEisbahndisco_eisbahn(sDate,sBeginn,url)
        }else if (startsWith(sBeginn,"ABGESAGT")){
          next;
        }else{
          sBeginn=str_extract(sBeginn,"\\d{2}:\\d{2}")
          oStartDate=as.Date(sDate,format="%d.%m.%Y")
          startTime=times(paste0(sBeginn,":00"))
          oEndDate=NA
          endTime=NA
          sDesc=NA
          sGegner=oTable[[3]][[i]]
          sMannschaft=oTable[[4]][[i]]
          sLetzerEinlass=oTable[[5]][[i]]
          Eislaufende=oTable[[6]][[i]]
          
          if (startsWith(sGegner,"Decathlon")){
            sDesc=sGegner
            event=data.frame(title=sDesc,description=sDesc, organizer=NA, city=NA, street=NA,zip=NA, lat=49.7746187, lng=9.9090821, date_start=oStartDate, date_end=oEndDate, url=url, time_start=startTime, time_end=endTime, price=NA)
            event$time_start=times(event$time_start)
            event$time_end=times(event$time_end)      
          }else{
            sDesc=paste("Gegner: ",sGegner,"\n")
            sMannschaft=paste0("Mannschaft: ",sMannschaft)
            sDesc=paste(sDesc,sMannschaft,"\n")
            sLetzerEinlass=paste0("Letzter Einlass: ",sLetzerEinlass);
            sDesc=paste(sDesc,sLetzerEinlass,"\n")
            if (Eislaufende!=""){
              Eislaufende=paste("Eislaufende: ",Eislaufende)
              sDesc=paste(sDesc,Eislaufende,"\n")
            }
            event=data.frame(title="Eishockeyspiel",description=sDesc, organizer=NA, city=NA, street=NA,zip=NA, lat=49.7746187, lng=9.9090821, date_start=oStartDate, date_end=oEndDate, url=url, time_start=startTime, time_end=endTime, price=NA)
            event$time_start=times(event$time_start)
            event$time_end=times(event$time_end)  
            
          }
          
        } 
        oRes=data.frame(rbind((oRes), (event)))
      }
    }
  }
  
  
  return (oRes)
}
getOberthürschule = function(){
  print("Oberthürschule")
  eventsurl = "https://www.franz-oberthuer-schule.de/events/event/"
  getMaxPages <- function(url, i=1)
  {
    code <- read_html(url)
    mainBlock <- html_node(code, xpath="//*[@id='primary']")
    
    morePages <- grepl("SpÃ¤tere Termine", mainBlock, fixed = TRUE)
    
    if(morePages == TRUE)
    {
      i = i + 1
      url <- paste0("https://www.franz-oberthuer-schule.de/events/event/page/", i)
      print(url)
      getMaxPages(url, i)
    }
    
    return (i)
  }
  
  maxPages = getMaxPages(eventsurl)
  
  pages <- seq(1, maxPages, 1)
  print(pages)
  pageUrls <- paste0("https://www.franz-oberthuer-schule.de/events/event/page/", pages)
  print(pageUrls)
  
  
  getEvents <- function(url)
  {
    code <- read_html(url)
    mainBlock <- html_node(code, xpath="//*[@id='primary']")
    events <- html_nodes(code, ".hentry")
    
    startDates = c()
    
    # pagedfschule = data.frame(Eventname=character(), Eventbeschreibung=character(), Eventveranstalter=character(), Ort=character(),
    #                           Lon = numeric(), Lat =numeric(), Eventstart=as.POSIXct(character()),  
    #                           Eventende=as.POSIXct(character()) , Eventurl=character(), Eventkalenderurl=character(), Preis = numeric())
    
    pagedfschule = data.frame(title=character(), url = character(), description=character(), lng = numeric(), lat =numeric(), city=character(), street = character(), zip = character(), 
                              date_start=as.Date(character()), date_end=as.Date(character()) , time_start=times(character()), time_end = character(), price=character(), organizer=character())
    
    for (i in 1:length(events))
    {
      print("-------------------")
      title_node <- html_node(events[i], ".entry-title span")
      title = html_text(title_node)
      print(title)
      dateRow <- html_node(events[i], "p")
      timeRowNode <- html_node(events[i], ".eo-event-date , time")
      timeTag <- html_nodes(timeRowNode, "time")
      dtValues <- html_attr(timeTag, "datetime")
      dtValues <- str_replace(dtValues, "\\+[\\d]{2}:[\\d]{2}", "")
      
      startTime <- NA
      endTime <- NA
      if (nchar(dtValues) >= 12)
      {
        #print("a")
        dateFormatStrFromSchool = "%Y-%m-%dT%H:%M:%S"
        dateFormatStr = "%d.%m.%Y %H:%M"
        
        dtValues <- as.POSIXct(dtValues, dateFormatStrFromSchool, tz="")
        
        startTime <- strftime(dtValues[[1]], "%H:%M:%S")
        print(startTime)
        startTime <- times(startTime)
        print(startTime)
        
        endTime <- strftime(dtValues[[2]], "%H:%M:%S")
        endTime <- times(endTime)
        
      }
      else if((nchar(dtValues) <= 11) & (nchar(dtValues) >= 9))
      {
        dateFormatStrFromSchool = "%Y-%m-%d"
        dateFormatStr = "%d.%m.%Y"
        dtValues <- as.POSIXct(dtValues, dateFormatStrFromSchool, tz="")
      }
      startDt <- dtValues[[1]]
      startDate <- as.Date(startDt)
      endDt <- dtValues[[2]]
      endDate <- as.Date(endDt)
      
      #print(typeof(startDt))
      #print(typeof(endDt))
      # 
      # subdfschule = data.frame(Eventname= c(title), Eventbeschreibung=c(NA), Eventveranstalter=c("Franz-Oberthuer-Schule"), Ort=c(NA),
      #                           Lon = c(9.95439), Lat =c(49.7865), Eventstart=c(startDt),  
      #                           Eventende= c(endDt), Eventurl=c(NA), Eventkalenderurl=c("https://www.franz-oberthuer-schule.de/events/event/"), Preis = c(NA))
      
      print(startTime)
      subdfschule = data.frame(title=title, url = eventsurl, description=NA, lng = 9.95439, lat =49.7865, city="Würzburg", street = "Zwerchgraben 2", zip = "97074", 
                               date_start=startDate, date_end=endDate , time_start=times(startTime), time_end = times(endTime), price=NA, organizer="Franz-Oberthuer-Schule")
      
      pagedfschule <- rbind(pagedfschule, subdfschule)
      
    }
    
    #print("blub")
    #print(length(pagedfschule))
    return (pagedfschule)
  }
  
  
  maindfschule = data.frame(title=character(), url = character(), description=character(), lng = numeric(), lat =numeric(), city=character(), street = character(), zip = character(), 
                            date_start=as.Date(character()), date_end=as.Date(character()) , time_start=times(character()), time_end = character(), price=character(), organizer=character())
  
  allpageDFs <- map(pageUrls, getEvents)
  print(length(allpageDFs))
  
  for (i in 1:length(allpageDFs)){
    maindfschule <- rbind(maindfschule, allpageDFs[[i]])
  }
  #View(allpageDFs[[1]])
  #View(maindfschule)
  return (maindfschule)
}
getZFK = function() {
  print("ZFK")
  url1 = "https://www.zfk-wuerzburg.de/tagesstaette/events/"
  url1 %>%
    read_html() -> rawData1
  
  
  rawData1 %>%
    html_nodes("h4") %>% 
    html_text() -> title
  
  rawData1 %>%
    html_nodes("h4+ p") %>% 
    html_text() -> beschreibung
  
  length(beschreibung) -> length
  
  c(rep(url1, length)) -> url
  gsub("https://", "", url) -> url
  
  rawData1 %>%
    html_nodes("p:nth-child(9)") %>% 
    html_text() -> ort1
  gsub("\n", " ", ort1) -> ort
  c(rep(ort, length)) -> ort
  
  c(rep("Zentrum für KÃ¶rperbehinderte", length)) -> veranstalter
  
  addressToGeoLoc <- function(address)
  {
    address = str_replace_all(address, " ", "")
    gurl <- paste0("https://maps.googleapis.com/maps/api/geocode/json?address=",address,"&key=AIzaSyAx0fpXEbPEYrKIQfv3so2TFU96-tUcaww")
    print(gurl)
    req <- fromJSON(gurl)
    
    if (req$status == "OK") {
      print("OK")
      location = req$results[[1]]$geometry$`location`
      lat = location[[1]]
      lon = location[[2]]
      #print(typeof(lat))
      #print(lon)
      print(location)
    } else {
      location <- NA
    }
    Sys.sleep(0.3)
    return(location)
  }
  data <- lapply(ort, addressToGeoLoc)
  unlist(data) -> data
  stringr::str_extract_all(data, "\\d+[.]\\d+") -> data
  unlist(data) -> data
  stringr::str_extract_all(data, "^\\d{1}[.]\\d+") -> long
  stringr::str_extract_all(data, "\\d{2}[.]\\d+") -> lat
  unlist(long) -> long
  unlist(lat) -> lat
  
  ort1
  stringr::str_extract_all(ort1, "\\d{5}.*") -> city
  stringr::str_extract_all(city, "\\D+") -> city
  gsub(" ", "", city) -> city
  c(rep(city, length)) -> city
  
  
  stringr::str_extract_all(ort1, "\\d{5}") -> zip
  unlist(zip) -> zip
  c(rep(zip, length)) -> zip
  
  stringr::str_extract_all(ort1, "[\n].*[\n]") -> street
  unlist(street) -> street
  gsub("\n", "", street) -> street
  c(rep(street, length)) -> street
  
  df_zfk <- data.frame(title = title, url = url, description = beschreibung, lng = long, lat = lat, city = city, street= street , zip= zip, date_start = NA, date_end = NA, time_start = NA, time_end = NA, price = NA, organizer = veranstalter)
  
  return(df_zfk)
}
getMariannhill = function(){
  print("Mariannhill")
  url_mariannhill <-"http://www.kirchenmusik-mariannhill.de/programm.html"
  
  url_mariannhill %>%
    read_html() %>%
    html_nodes('.texteng td+ td , .texteng .texteng , .texteng~ tr+ tr .texteng') %>%
    html_text() %>%
    str_trim() %>%
    as.list() -> Mariannhill
  
  
  Mariannhill <- as.data.frame(str_split_fixed(str_subset(Mariannhill,pattern=""), ",", 3))
  Mariannhill$V1 <- NULL
  Mariannhill <- subset(Mariannhill, Mariannhill$V2!="")
  Mariannhill <- cbind(Mariannhill, as.list(as.data.frame(gsub("(Uhr).*","\\1",Mariannhill$V3))))
  colnames(Mariannhill)[1] <- "date_start"
  colnames(Mariannhill)[2] <- "title"
  colnames(Mariannhill)[3] <- "time_start"
  
  Mariannhill %>%
    mutate_all(as.character) -> Mariannhill
  
  replaceTime_MH <-list(" " = "", "Uhr" = "")
  Mariannhill$time_start %>%
    gsubfn(paste(names(replaceTime_MH),collapse="|"),replaceTime_MH,.) %>%
    paste0(., ":00:00") %>%
    times(.)-> Mariannhill$time_start
  
  Mariannhill$title %>%
    gsub("\\d|(Uhr)|\\s{2}|(\n)","",.) %>%
    gsub("([a-z])([A-Z])", "\\1 \\2", .) %>%
    gsub(" Musik",", Musik", .) %>%
    str_trim("both") -> Mariannhill$title
  
  Mariannhill$title %>%
    gsub("^[A-zÃ¤Ã¶ü0-9 ]*, (.*)", "\\1", .) %>%
    gsub(" Leitung",", Leitung", .) %>%
    gsub("Steinmeyer-Orgel",", Steinmeyer-Orgel", .) -> Mariannhill$description
  
  Mariannhill$title <- gsub("^([A-zÃ¤Ã¶ü0-9 ]*), (.*)", "\\1", Mariannhill$title)
  
  replaceMonth_MH <-list(". Januar " = "jan", ". Februar " = "feb",". MÃ¤rz " = "mar",". April " = "apr",
                         ". Mai " = "mai",". Juni " = "jun",". Juli " = "jul",". August " = "aug",
                         ". September " = "sep",". Oktober " = "okt",". November " = "nov",". Dezember " = "dez")
  
  Mariannhill$date_start   %>%
    gsubfn(paste(names(replaceMonth_MH),collapse="|"),replaceMonth_MH,.) %>%
    as.Date(. ,"%d%B%Y") -> Mariannhill$date_start  
  
  Mariannhill$time_end <- times(NA)
  Mariannhill$date_end <- Mariannhill$date_start
  Mariannhill$organizer <- "Mariannhill"
  Mariannhill$lat <- as.numeric("49.79348")
  Mariannhill$lng <- as.numeric("9.9545")
  Mariannhill$price <- as.character(NA)
  Mariannhill$url <- url_mariannhill
  Mariannhill$city <- "Wuerzburg"
  Mariannhill$street <- "Mariannhillstrasse 1"
  Mariannhill$zip <- as.numeric("97074")
  
  Mariannhill <- Mariannhill[,c(2, 11, 4, 9, 8, 12, 13, 14, 1, 6, 3, 5, 10, 7)]
  return(Mariannhill)
}
getFrankenwarte = function() {
  print("Frankenwarte")
  frankenwarte_link <- "https://www.frankenwarte.de/unser-bildungsangebot/aktuell.html"
  fw_sublinks <- frankenwarte_link %>% read_html() %>% html_nodes(".intern") %>% html_attr('href') %>% unique()
  fw_sublinks <- paste0("https://www.frankenwarte.de/", fw_sublinks)
  
  
  gethtml_files_fw <- function(sublink){
    html <- tryCatch({
      sublink %>% read_html()
    }, error = function(e){
      return(NA)
    })
    
    return(html)
  }
  
  getdesc_fw <- function(html){
    desc <- tryCatch({
      html %>% html_nodes('p')  %>% html_text()
    }, error= function(e){
      return(NA)
    })
    
    html %>% html_nodes('p')  %>% html_text()
    return(desc[1])
  }
  
  getprice_fw <- function(html){
    price <- tryCatch({
      html %>% html_nodes('p:nth-child(7)')  %>% html_text()  
    }, error= function(e){
      return(NA)
    })
    
    return(price[1])
  }
  
  getstreet_fw <- function(html){
    street <- tryCatch({
      street <-  html%>% html_nodes('img+ p')  %>% html_text() %>% gsub(., pattern="97082 Würzburg", replacement="", fixed=T) %>% gsub(., pattern="Akademie Frankenwarte", replacement="", fixed=T)
    }, error= function(e){
      return(NA)
    })
    
    return(street)
  }
  
  
  fw <- frankenwarte_link %>% read_html() %>% html_nodes(".va-item") %>% html_text()
  
  fw <- map(fw, function(fw) return(strsplit(fw, "\r\n\t"))) %>% unlist() 
  fw <- fw[fw != ""] 
  fw <- fw[fw != "\t"]
  x <- as.data.frame(fw)
  sel <- seq(4,nrow(x),4)
  x <- x[-sel,]
  fw <- as.vector(x)
  
  fw <- matrix(fw,ncol=3, byrow=TRUE) %>% as.data.frame()
  
  fw$V3 <- as.character(fw$V3)
  y <- map(fw$V3, function(fw) if(nchar(fw) <13){return(paste(fw," - ",fw))} else{return(fw)})%>% unlist()
  y <- map(y, function(fw) return(strsplit(fw, split=" - ", fixed =T)))%>% unlist() %>% matrix(., ncol=2, byrow = TRUE) %>% as.data.frame()
  
  
  fw_startdate <- map(y$V1, function(fw) return(as.Date(fw, format="%d.%m."))) %>% do.call(c, .)
  fw_enddate <- map(y$V2, function(fw) return(as.Date(fw, format="%d.%m."))) %>% do.call(c, .)
  
  html_files_fw <- map(fw_sublinks, gethtml_files_fw)
  
  
  descriptions_fw <- map(html_files_fw, getdesc_fw) %>% unlist()
  prices_fw <- map(html_files_fw, getprice_fw) %>% unlist()
  #zipcode_fw <- map(html_files_fw, getzipcode_fw) %>% unlist()
  street_fw <- map(html_files_fw, getstreet_fw) %>% unlist()
  
  
  frankenwarte  <- data.frame("title" = fw$V2,
                              "url" = fw_sublinks,
                              "description" = descriptions_fw,
                              "lng" = 9.90694,
                              "lat" = 49.78187,
                              "city" = "Wuerzburg",
                              "street" = street_fw,
                              "zip" = 97082,
                              "date_start" = fw_startdate, 
                              "date_end" = fw_enddate, 
                              "time_start" = NA, 
                              "time_end" = NA, 
                              "price" = prices_fw,
                              "organizer" = "Frankenwarte")
  return(frankenwarte)
}
getJugendbildungszentrum = function() {
  print("Jugendbildungszentrum")
  jubi_raw_month=c(month(today()), month(today())+1, month(today())+2)
  jubi_year=c(year(today()), year(today()+30), year(today()+60))
  jubi_month = formatC(jubi_raw_month, width = 2, format = "d", flag = "0")
  jubi_url_raw=list()
  jubi_url=list()
  jubi_kurse=list()
  jubi_links=list()
  jubi_start=list()
  jubi_ort=list()
  jubi_ende=list()
  jubi_beschreibung=list()
  jubi_links_raw=list()
  i=1
  
  
  while(i<=length(jubi_year)){
    b = paste0("http://www.jubi-unterfranken.de/events/", jubi_year[i],"-", jubi_month[i])
    jubi_url_raw[[i]] = b
    jubi_url=unlist(jubi_url_raw)
    jubi_url[i] %>%
      read_html() -> jubi_data
    
    jubi_data %>%
      html_nodes(".tribe-events-month-event-title") %>%
      html_text() -> jubi_a
    jubi_kurse=append(jubi_kurse,jubi_a)
    
    jubi_data %>%
      html_nodes(".tribe-events-has-events > div > h3 > a") %>%
      html_attr("href") -> jubi_b
    jubi_c = unique(jubi_b)
    jubi_links_raw=append(jubi_links_raw,jubi_b)
    jubi_links=append(jubi_links,jubi_c)
    i=i+1
  }
  
  jubi_kurse = unlist(jubi_kurse)
  jubi_links = unlist(jubi_links)
  jubi_links_raw=unlist(jubi_links_raw)
  jubi_year2=append(jubi_year, jubi_year)
  
  j=1
  while(j<=length(jubi_links_raw)){
    
    jubi_links_raw[[j]] %>%
      read_html() -> jubi_data
    
    jubi_data %>%
      html_nodes(".tribe-events-schedule .tribe-event-date-start") %>%
      html_text() -> jubi_c
    jubi_start = append(jubi_start, jubi_c)
    
    jubi_data %>%
      html_nodes(".tribe-postal-code , .tribe-locality , .tribe-street-address") %>%
      html_text() -> jubi_o
    jubi_ort = append(jubi_ort, jubi_o)
    
    jubi_data %>%
      html_nodes(".tribe-events-schedule .tribe-event-date-end") %>%
      html_text() -> jubi_d
    jubi_ende = append(jubi_ende,jubi_d)
    
    j=j+1
  }
  
  jubi_start=unlist(jubi_start)
  jubi_ende=unlist(jubi_ende)
  
  jubi_start_tag=list()
  jubi_start_monat=list()
  jubi_start_jahr=list()
  jubi_start_uhrzeit=list()
  
  jubi_ende_tag=list()
  jubi_ende_monat=list()
  jubi_ende_jahr=list()
  jubi_ende_uhrzeit=list()
  
  k=1
  while(k<=length(jubi_start)){
    
    jubi_start[k] %>%
      strsplit(" ") %>%
      unlist() -> a
    jubi_start_tag[k] = a[1]
    jubi_start_monat[k] = a[2]
    jubi_start_jahr[k] = jubi_year2[k]
    jubi_start_uhrzeit[k] = a[4]
    
    jubi_ende[k] %>%
      strsplit(" ") %>%
      unlist() -> b
    jubi_ende_tag[k] = b[1]
    jubi_ende_monat[k] = b[2]
    jubi_ende_jahr[k] = jubi_year2[k]
    jubi_ende_uhrzeit[k] = b[4]
    
    k=k+1
    
  }
  
  jubi_ende_monat= unlist(jubi_ende_monat)
  jubi_ende_uhrzeit= unlist(jubi_ende_uhrzeit)
  jubi_ende_tag=unlist(jubi_ende_tag)
  jubi_ende_jahr= unlist(jubi_ende_jahr)
  jubi_start_monat=unlist(jubi_start_monat)
  jubi_start_tag=unlist(jubi_start_tag)
  jubi_start_uhrzeit=unlist(jubi_start_uhrzeit)
  jubi_start_jahr=unlist(jubi_start_jahr)
  
  jubi_date_start = paste0(jubi_start_tag, jubi_start_monat, ".", jubi_start_jahr)
  jubi_date_ende = paste0(jubi_ende_tag, jubi_ende_monat, ".", jubi_ende_jahr)
  
  #jubi_start_final = paste0(jubi_start_tag, jubi_start_monat, ".", jubi_start_jahr, " ", jubi_start_uhrzeit, ":00")
  #jubi_ende_final = paste0(jubi_ende_tag, jubi_ende_monat, ".", jubi_ende_jahr, " ", jubi_ende_uhrzeit, ":00")
  
  
  jubi_time_start = times(paste0(jubi_start_uhrzeit, ":00"))
  jubi_time_ende = times(paste0(jubi_ende_uhrzeit, ":00"))
  
  jubi_date_start = as.Date(jubi_date_start,format = "%d.%B.%Y")
  jubi_date_ende = as.Date(jubi_date_ende,format = "%d.%B.%Y")
  
  jubi_ort=unlist(jubi_ort[1])
  jubi_ort=paste0(jubi_ort[1], ", ", jubi_ort[3], " ",jubi_ort[2])
  
  Jugendbildungszentrumsveranstaltungen = data.frame("title"=jubi_kurse, "url"=jubi_links_raw, "description"= NA,"lng"=9.9547, "lat"=49.73939,
                                                     "city"="Würzburg", "street"="Berner Strasse 14", "zip"=97084, "date_start"=jubi_date_start,
                                                     "date_end"=jubi_date_ende, "time_start"=jubi_time_start, "time_end"=jubi_time_ende , "price"=NA,
                                                     "organizer"="Jugendbildungszentrum Unterfranken")
  
  return(Jugendbildungszentrumsveranstaltungen)
}
getSalon77 = function(){
  url_s77WK <- "https://www.salon77.de/index.php?nav=wk&mod=wk&bc1=Wochenkurse"
  
  url_s77WK %>%
    read_html() %>%
    html_nodes("#innercontent a") %>%
    html_attr("href") %>%
    paste0("https://www.salon77.de", .) -> links_s77WK
  
  cleanURLs <- function (link) {
    link <- gsub(" ", "%20", link, fixed = TRUE)
    return(link)
  }
  
  links_s77WK <- unlist(map(links_s77WK, cleanURLs))
  
  GetTitle_s77WK <- function(link){
    s77WK_title <- tryCatch({
      link %>%
        read_html() %>%
        html_nodes("h1") %>%
        html_text(trim = T)
    }, error = function(e) {
      return(NA)
    })
    return (s77WK_title)
  }
  
  s77WK <- map(links_s77WK, GetTitle_s77WK)
  
  s77WK %>%
    unlist() %>%
    as.data.frame() %>%
    mutate_all(as.character) -> s77WK
  
  colnames(s77WK)[1] <- "title"
  
  GetPrice_s77WK <- function(link) {
    s77WK_price <- tryCatch({
      link %>%
        read_html() %>%
        html_nodes("#innercontent") %>%
        html_text(trim = T)  %>%
        as.list() %>%
        as.data.frame() %>%
        mutate_all(as.character) %>%
        str_extract(., "(.){6}(Euro)") %>%
        str_trim()
    }, error = function(e) {
      return(NA)
    })
    return(s77WK_price)
  }
  
  s77WK_price <- map(links_s77WK, GetPrice_s77WK)
  
  s77WK_price %>%
    unlist() -> s77WK$price
  
  GetContent_s77WK <- function(link){
    s77WK_content <- tryCatch({
      link %>%
        read_html() %>%
        html_nodes("h2+ strong") %>%
        html_text(trim = T)
    }, error = function(e) {
      return(NA)
    })
    return (s77WK_content)
  }
  
  
  GetContent_s77WK(links_s77WK[[1]])
  
  s77WK_content <- map(links_s77WK, GetContent_s77WK)
  
  s77WK_content %>%
    unlist() %>%
    as.data.frame() -> s77WK_content
  
  s77WK_content <- separate(data = s77WK_content, col = ., into = c("day", "time", "loc"), sep = "\\|")
  s77WK_content <- separate(data = s77WK_content, col = time, into = c("start", "end"), sep = "\\-")
  
  s77WK_content$start %>%
    gsub("(Uhr)|\\s*|(\n)","",.) %>%
    paste0(., ":00") %>%
    times(.) -> s77WK$time_start
  
  s77WK_content$end %>%
    gsub("(Uhr)|\\s*|(\n)","",.) %>%
    paste0(., ":00") %>%
    times(.) -> s77WK$time_end
  
  s77WK$date_start <- as.Date(NA ,"%d%B%Y")
  s77WK$date_end <- as.Date(NA ,"%d%B%Y")
  
  initializeWeek <- function() {
    
    week <- data.frame(monday=as.Date(NA),
                       tuesday=as.Date(NA),
                       wednesday=as.Date(NA),
                       thursday=as.Date(NA),
                       friday=as.Date(NA),
                       saturday=as.Date(NA),
                       sunday=as.Date(NA),
                       stringsAsFactors = FALSE)
    
    if (weekdays(Sys.Date()) == "Montag") {
      week$monday <- Sys.Date()
      week$tuesday<- Sys.Date()+1
      week$wednesday<- Sys.Date()+2
      week$thursday<- Sys.Date()+3
      week$friday<- Sys.Date()+4
      week$saturday<- Sys.Date()+5
      week$sunday<- Sys.Date()+6
    } else if (weekdays(Sys.Date()) == "Dienstag") {
      week$monday <- Sys.Date()+6
      week$tuesday<- Sys.Date()
      week$wednesday<- Sys.Date()+1
      week$thursday<- Sys.Date()+2
      week$friday<- Sys.Date()+3
      week$saturday<- Sys.Date()+4
      week$sunday<- Sys.Date()+5
    } else if (weekdays(Sys.Date()) == "Mittwoch") {
      week$monday <- Sys.Date()+5
      week$tuesday<- Sys.Date()+6
      week$wednesday<- Sys.Date()
      week$thursday<- Sys.Date()+1
      week$friday<- Sys.Date()+2
      week$saturday<- Sys.Date()+3
      week$sunday<- Sys.Date()+4
    } else if (weekdays(Sys.Date()) == "Donnerstag") {
      week$monday <- Sys.Date()+4
      week$tuesday<- Sys.Date()+5
      week$wednesday<- Sys.Date()+6
      week$thursday<- Sys.Date()
      week$friday<- Sys.Date()+1
      week$saturday<- Sys.Date()+2
      week$sunday<- Sys.Date()+3
    } else if (weekdays(Sys.Date()) == "Freitag") {
      week$monday <- Sys.Date()+3
      week$tuesday<- Sys.Date()+4
      week$wednesday<- Sys.Date()+5
      week$thursday<- Sys.Date()+6
      week$friday<- Sys.Date()
      week$saturday<- Sys.Date()+1
      week$sunday<- Sys.Date()+2
    } else if (weekdays(Sys.Date()) == "Samstag") {
      week$monday <- Sys.Date()+2
      week$tuesday<- Sys.Date()+3
      week$wednesday<- Sys.Date()+4
      week$thursday<- Sys.Date()+5
      week$friday<- Sys.Date()+6
      week$saturday<- Sys.Date()
      week$sunday<- Sys.Date()+1
    } else if (weekdays(Sys.Date()) == "Sonntag") {
      week$monday <- Sys.Date()+1
      week$tuesday<- Sys.Date()+2
      week$wednesday<- Sys.Date()+3
      week$thursday<- Sys.Date()+4
      week$friday<- Sys.Date()+5
      week$saturday<- Sys.Date()+6
      week$sunday<- Sys.Date()
    }
    return (week)
  }
  
  setDay <- function(weekday) {
    
    if (weekday == "Montags") {
      s77WK$date_start<- week$monday
    } else if (weekday == "Dienstags") {
      s77WK$date_start<- week$tuesday
    }else if (weekday == "Mittwochs") {
      s77WK$date_start<- week$wednesday
    } else if (weekday == "Donnerstags") {
      s77WK$date_start<- week$thursday
    }else if (weekday == "Freitags") {
      s77WK$date_start<- week$friday
    }else if (weekday == "Samstags") {
      s77WK$date_start<- week$saturday
    }else if (weekday == "Sonntags") {
      s77WK$date_start<- week$sunday
    } else {s77WK$date_start<- NA}
  }
  
  for (i in 1: nrow(s77WK_content)) {
    s77WK_content[c(i),] <- gsub(" ", "", s77WK_content[c(i),], fixed = TRUE)
  }
  
  week <- initializeWeek()
  
  map(s77WK_content$day, setDay) %>%
    as.data.frame() %>%
    t() %>%
    as.data.frame() %>%
    mutate_all(as.character) %>%
    mutate_all(as.Date) -> startDates
  
  s77WK$date_start <- startDates$V1
  
  s77WK$url <- links_s77WK
  s77WK$description <- paste0("Immer ", sapply(s77WK_content$day, tolower), ", Ort: ", s77WK_content$loc)
  s77WK$organizer <- "Salon77"
  s77WK_content %>% rowwise() %>% mutate (lat = ifelse(grepl("Posthalle",loc),as.numeric("49.802087"),as.numeric("49.79304")),
                                          lng = ifelse(grepl("Posthalle",loc),as.numeric("9.9337239"),as.numeric("9.95808")),
                                          street = ifelse(grepl("Posthalle",loc),"Bahnhofsplatz 2","Richard-Wagner-Str. 60"),
                                          zip = ifelse(grepl("Posthalle",loc),"97070","97074")) -> s77WK_content
  
  
  s77WK$city <- "Würzburg"
  s77WK$zip = s77WK_content$zip
  s77WK$street <- s77WK_content$street
  s77WK$lng = s77WK_content$lng
  s77WK$lat = s77WK_content$lat
  
  #check_Address <- as.data.frame(grepl("(Salon77)|(salon77)", s77WK$description))
  
  #for (i in 1:nrow(s77WK)) {
  #  if (check_Address[i, ] == FALSE) {
  #    s77WK$street[i] <- NA
  #    s77WK$zip[i] <- NA
  #  }
  #}
  
  s77WK <- s77WK[,c(1, 7, 8, 11, 10, 12, 13, 14, 5, 6, 3, 4, 2, 9)]
  
  return(s77wk)
}
getLaViva = function() {
  viva_url = "http://www.la-viva-danceclub.de/events-laviva-danceclub"
  
  
  #binman::list_versions("seleniumserver")
  rD <- rsDriver()
  remDr <- rD[["client"]]
  remDr$navigate(viva_url)
  
  viva_data <- read_html(remDr$getPageSource()[[1]])
  
  remDr$close()
  rm(rD)
  gc()
  
  viva_data %>%
    html_nodes(".date") %>%
    html_text()-> viva_d
  
  viva_d <- lapply(viva_d, gsub, pattern = " ", replacement = "", fixed = TRUE)
  viva_date=str_extract(viva_d, '\\d{2}.\\d{2}.\\d{4}')
  
  viva_data %>%
    html_nodes(".details") %>%
    html_text() -> viva_details
  
  viva_data %>%
    #html_nodes(".details div:nth-child(1)") %>%
    html_nodes("#accordion p") %>%
    html_text() -> viva_beschreibung
  
  viva_details <- lapply(viva_details, gsub, pattern = " ", replacement = "", fixed = TRUE)
  viva_uhrzeit=str_extract(viva_details, '[0-2][0-9].[0-5][0-9]')
  
  viva_data %>%
    html_nodes("h2") %>%
    html_text()-> viva_title
  
  viva_time_start = unlist(times(paste0(viva_uhrzeit, ":00")))
  
  viva_date = as.Date(viva_date,format = "%d.%m.%Y")
  
  
  
  
  
  
  LaVivaDanceclub = data.frame("title"=viva_title, "url"="http://www.la-viva-danceclub.de/events-laviva-danceclub", "description"= NA,
                               "lng"=9.970704, "lat"=49.79563, "city"="Würzburg", "street"="Nürnberger Straße 72-74", "zip"=97076, 
                               "date_start"=viva_date, "date_end"=viva_date, "time_start"=viva_time_start, "time_end"=NA , "price"=NA, "organizer"="LaViva Danceclub")
  
  
  return(LaVivaDanceclub)
}

##### s.Oliver Baskets & Theater Hobbit #####
# getSOliver = function() {
#   sol_url <- "https://www.soliver-wuerzburg.de/saison/spielplan/"
#   sol_url %>%
#     read_html() %>%
#     html_nodes(".csc-table") %>% #W?hle gesamtes Ereignis
#     map_df(~list(title = html_nodes(.x, "td.spieltag") %>%
#                    html_text(trim = T) %>%
#                    {if(length(.) == 0) NA else .}, #replace legth 0 elements with NA
#                  Datum = html_nodes(.x, "td.date") %>%
#                    html_text(trim = T) %>%
#                    {if(length(.) == 0) NA else .},
#                  Heim = html_nodes(.x, "td.team-home") %>%
#                    str_extract(. , "(alt=\").*(\")") %>%
#                    gsub("alt=", "", .) %>%
#                    {if(length(.) == 0) NA else .},
#                  Gast = html_nodes(.x, "td.team-gast") %>%
#                    str_extract(. , "(alt=\").*(\")") %>%
#                    gsub("alt=", "", .) %>%
#                    {if(length(.) == 0) NA else .},
#                  Details = html_nodes(.x, ".result") %>%
#                    html_text(trim = T) %>%
#                    {if(length(.) == 0) NA else .}))  -> sol_df
#   sol_df$title <- paste0("Spieltag ", sol_df$title)
#   sol_df$date_start <- str_extract(sol_df$Datum, "[0-9]{1,2}\\.[0-9]{1,2}\\.20[0-9]{2}")
#   sol_df$time_start <- str_extract(sol_df$Datum, "[0-9]{1,2}:[0-9]{1,2}")
#   sol_df <- unite(sol_df, description, c(Heim, Gast, Details), remove = T)
#   sol_df_clean <- subset(sol_df, select = -c(Datum))
#   sol_df_clean$date_end <- as.Date(NA)
#   sol_df_clean$time_end <- times(NA)
#   
#   #formatierung
#   sol_df_clean$date_start <- as.Date(sol_df_clean$date_start, format = "%d.%m.%Y")
#   sol_df_clean$time_start %>%
#     paste0(":00") %>%
#     times() -> sol_df_clean$time_start
#   sol_df_clean$time_end <- times(sol_df_clean$time_end)
#   
#   sol_df_final <- data.frame(sol_df_clean, organizer = "s.Oliver-Arena",
#                              street = "Stettiner Straße 1", zip = 97072, url = "http://www.soliver-wuerzburg.de/home/",
#                              lat = 49.77431, lng = 9.94059 ,stringsAsFactors=FALSE)
#   sol_df_final$organizer <- as.character(sol_df_final$organizer)
#   sol_df_final$street <- as.character(sol_df_final$street)
#   sol_df_final$zip <- as.numeric(sol_df_final$zip)
#   sol_df_final$url <- as.character(sol_df_final$url)
#   sol_df_final$price <- as.character(NA)
#   sol_df_final$city <- "Wuerzburg"
#   
#   return(sol_df_final)
# }
# getTheaterHobbit = function(){
#   
#   url1 = "http://www.theater-hobbit.de/"
#   url1 %>%
#     read_html() -> rawData
#   
#   url2 = "http://www.theater-hobbit.de/Kontakt.html"
#   url2 %>%
#     read_html() -> rawData2
#   
#   url3 = "http://www.theater-hobbit.de/preise.html"
#   url3 %>%
#     read_html() -> rawData3
#   
#   rawData %>% 
#     html_nodes(".stacks_right .stacks_out+ .stacks_out span") %>%
#     html_text(trim = T) -> beschreibung
#   beschreibung
#   
#   rawData %>%
#     html_nodes("strong+ span , .stacks_right a") %>% 
#     html_attr("href") -> link
#   gsub("//www.","www.", link) -> link
#   link
#   
#   rawData2 %>% 
#     html_nodes(".com_yourhead_stack_header_stack span") %>%
#     html_text(trim = T) -> ort
#   
#   ort
#   stringr::str_extract_all(ort, "\\d{5}.*") -> city
#   stringr::str_extract_all(city, "\\D+") -> city
#   gsub(" ", "", city) -> city
#   c(rep(city, length)) -> city
#   
#   stringr::str_extract_all(ort, "\\d{5}") -> zip
#   unlist(zip) -> zip
#   c(rep(zip, length)) -> zip
#   
#   stringr::str_extract_all(ort, "[\n].*[\n]") -> street
#   unlist(street) -> street
#   gsub("\n", "", street) -> street
#   c(rep(street, length)) -> street
#   
#   gsub("\n", "", ort) -> ort
#   
#   
#   rawData %>%
#     html_nodes("strong") %>% 
#     html_text(trim = T) -> daten
#   stringr::str_extract_all(daten, "\\d{2}[:]\\d{2}") -> Uhrzeit
#   unlist(Uhrzeit) -> time
#   time <- times(paste0(time, ":00"))
#   time
#   
#   
#   stringr::str_extract_all(daten, "\\d{2}[.]\\d{2}[.]") -> Datum
#   unlist(Datum) -> date
#   date <- as.POSIXct(date,format="%d.%m.")
#   date
#   
#   rawData %>%
#     html_nodes("strong+ span , .stacks_right a") %>% 
#     html_text(trim = T) -> title
#   title
#   
#   rawData3 %>%
#     html_nodes("strong:nth-child(3)") %>% 
#     html_text(trim = T) -> preis
#   preis
#   
#   rawData %>%
#     html_nodes("#stacks_in_974_page3 , span+ strong") %>% 
#     html_text(trim = T) -> test
#   test
#   test[1] -> test2
#   stringr::str_extract_all(test2, "\\d{2}[.]\\d{2}[.]") -> Datum
#   unlist(Datum) -> datetest
#   length(datetest) -> length
#   length
#   
#   c(rep(title[1],length),title[2], title[3], title[4])-> alltitles
#   c(rep(link[1],length), link[2], link[3], link[4]) -> alllinks
#   c(rep(beschreibung[1], length), beschreibung[2], beschreibung[3], beschreibung[4]) -> allbeschreibungen
#   
#   length(alltitles) -> length1
#   
#   
#   gsub("http://", "", url) -> url
#   c(rep("Plastisches Theater HOBBIT",length1)) -> veranstalter
#   c(rep(preis, length1)) -> preis
#   
#   city <- c(rep(city[1], length1))
#   street <- c(rep(street[1], length1))
#   zip <- c(rep(zip[1], length1))
#   
#   
#   addressToGeoLoc <- function(address)
#   {
#     address = str_replace_all(address, " ", "")
#     gurl <- paste0("https://maps.googleapis.com/maps/api/geocode/json?address=",address,"&key=AIzaSyAx0fpXEbPEYrKIQfv3so2TFU96-tUcaww")
#     print(gurl)
#     req <- fromJSON(gurl)
#     
#     if (req$status == "OK") {
#       print("OK")
#       location = req$results[[1]]$geometry$`location`
#       lat = location[[1]]
#       lon = location[[2]]
#       #print(typeof(lat))
#       #print(lon)
#       print(location)
#     } else {
#       location <- NA
#     }
#     Sys.sleep(0.3)
#     return(location)
#   }
#   data <- lapply(ort, addressToGeoLoc)
#   unlist(data) -> data
#   stringr::str_extract_all(data, "\\d+[.]\\d+") -> data
#   unlist(data) -> data
#   stringr::str_extract_all(data, "^\\d{1}[.]\\d+") -> long
#   stringr::str_extract_all(data, "\\d{2}[.]\\d+") -> lat
#   unlist(long) -> long
#   unlist(lat) -> lat
#   
#   long <- c(rep(long, length1))
#   lat <- c(rep(lat, length1))
#   
#   df_hobbit <- data.frame(title = alltitles, url = alllinks, description= allbeschreibungen, lng = long, lat = lat, city = city, street = street, zip = zip, date_start = date, date_end = date, time_start = time, time_end = NA, price = preis, organizer = veranstalter)
#   
#   return(df_hobbit)
# }

###### Zusammenfügen der Dataframes #####
HerbergeEvents=getHerbergeEvents()
KompetenzzentrumEvents=getKompetenzzentrumEvents()
BotanischerGartenEvents=getBotanischerGartenEvents()
JuliusSpitalEvents=getJuliusSpitalEvents()
Cairo=getCairo()
Buergerbraeu=getBuergerbraeu()
MainfrankenTheater=getMainfrankenTheater()
BBK=getBBK()
Familienzentrum=getFamilienzentrum()
Fraunhofer=getFraunhofer()
Gnadenkirche=getGnadenkirche()
MuseumAmDom=getMuseumAmDom()
ClubKatze=getClubKatze()
Zauberberg=getZauberberg()
Deutschaus=getDeutschaus()
Auferstehung=getAuferstehung()
Hofkeller=getHofkeller()
Wuf=getWuf()
Future=getFuture()
Events_eisbahn=getEvents_eisbahn()
Oberthürschule=getOberthürschule()
ZFK=getZFK()
Mariannhill=getMariannhill()
Frankenwarte=getFrankenwarte()
Jugendbildungszentrum=getJugendbildungszentrum()
LaViva=getLaViva()
Toscana=getToscanasaalEvents()
Kellerperle=kellerperlencrawler()
UniBib=getUniBib()
StStephan=getStStephan()
Lindleinsmühle=getLindleinsmühle()
PGSanderau=getPGSanderau()
IHK=getIHK()
Boot=getBoot()
MH=getMH()
Thomas=getThomas()
Augustinerkirche=getAugustinerKirche()
Domschule=getDomschule()
VHS=getVHS()
UniKlinik=getUniKlinikum()
Loma=getLoma()
HFM=getHFM()


final_Events= rbind(
  HerbergeEvents,
  KompetenzzentrumEvents,
  BotanischerGartenEvents,
  JuliusSpitalEvents,
  Cairo,
  Buergerbraeu,
  MainfrankenTheater,
  BBK,
  Familienzentrum,
  Fraunhofer,
  Gnadenkirche,
  MuseumAmDom,
  ClubKatze,
  Zauberberg,
  Deutschaus,
  Auferstehung,
  Hofkeller,
  Wuf,
  Future,
  Events_eisbahn,
  Oberthürschule,
  ZFK,
  Mariannhill,
  Frankenwarte,
  Jugendbildungszentrum,
  LaViva, 
  Toscana,
  Kellerperle, 
  UniBib,
  StStephan,
  Lindleinsmühle,
  PGSanderau,
  IHK,
  Boot,
  MH,
  Thomas,
  Augustinerkirche,
  Domschule,
  VHS,
  UniKlinik,
  Loma,
  HFM)