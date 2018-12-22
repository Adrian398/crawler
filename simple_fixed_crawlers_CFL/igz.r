url <-"https://www.igz.wuerzburg.de/igz/angebote/veranstaltungen/index.html"
url %>%
  read_html() %>%
  html_nodes("#embhl a") %>%
  html_text -> title_igz


url <-"https://www.igz.wuerzburg.de/igz/angebote/veranstaltungen/index.html"
url %>%
  read_html() %>%
  html_nodes("#embhl a") %>%
  html_attr("href") ->links_igz
creating_links <- function(link) {
  url <- paste0("https://www.igz.wuerzburg.de", link)
}
url_igz <- sapply(links_igz, creating_links)


## META DATA
url <-"https://www.igz.wuerzburg.de/igz/angebote/veranstaltungen/index.html"
# url %>%
#   read_html() %>%
#   html_nodes(".ev_title_verysimple") %>%
#   html_text() ->meta_igz

url %>%
  read_html() %>%
  html_nodes("hr+ strong") %>%
  html_text() ->datum
##DATE
str_extract_all(datum, "[0-9]?[0-9]\\.[0-9]?[0-9]\\.201[8-9]") -> date_start_igz
unlist(date_start_igz) -> date_start_igz
date_start_igz <- as.Date(date_start_igz, format="%d.%m.%Y")
date_end_igz <- date_start_igz

##TIME
get_time_start <- function(link) {
  url <- link
  url %>%
    read_html() %>%
    html_nodes("td") %>%
    html_text %>%
    str_extract_all("(?<=Uhrzeit:\\s)[0-9][0-9]:[0-9][0-9]") 
}
sapply(url_igz, get_time_start)-> time_igz
time_igz <- unlist(time_igz)
names(time_igz) <- NULL
time_start_igz <- times(paste0(time_igz,":00"))

get_time_end <- function(link) {
  url <- link
  url %>%
    read_html() %>%
    html_nodes("td") %>%
    html_text %>%
    str_extract_all("((?<=Ende:\\s)[0-9]?[0-9].[0-9][0-9])|((?<=[0-9]?[0-9]\\s(h)\\s(-)\\s)[0-9]?[0-9].[0-9][0-9](?=\\s(h)))") 
}
sapply(url_igz, get_time_end)-> time_end_igz
sapply(time_end_igz,function(x){gsub("\\.",":",x)}) -> time_end_igz
times(paste0(time_end_igz,":00")) -> time_end_igz



##DESCRIPTION
url <-"https://www.igz.wuerzburg.de/igz/angebote/veranstaltungen/index.html"
url %>%
  read_html() %>%
  html_nodes("tr div:nth-child(2)") %>%
  html_text(trim=T) -> description_igz
gsub("\t","",description_igz) -> description_igz
gsub("/r","",description_igz) -> description_igz

##LOCATION
city_igz <- "Wuerzburg"
street_igz <- "Friedrich-Bergius-Ring 15" 
zip_igz <- "97076"
lng_igz <- 9.9981454
lat_igz <- 49.8034612
organizer_igz <- "Innovations- und Gründerzentrum Würzburg"
# First crawl
website <- "https://www.dom-wuerzburg.de/aktuelles/gottesdienste"

website %>%
  read_html() -> raw_read

raw_read %>%
  html_nodes("span") %>%
  html_text(trim = T) -> Zeit

raw_read %>%
  html_nodes(".eventcontent .title a") %>%
  html_text(trim = T) -> title

raw_read %>%
  html_nodes(".title+ p") %>%
  html_text(trim = T) -> description

# Cleaning time info
Zeit <- Zeit[-c(1,2)]

# setting up data in iterative loop
for (x in 1:(length(Zeit))){
  # initializing
  if (x == 1){
    date_start = c()
    date_end = c()
    time_start = c()
    time_end = c()
    city = c()
    street = c()
    zip = c()
    organizer = c()
    price = c()
    lat = c()
    lng = c()
    url = c()
  }
  
  # formluating date format --------------------------------------------------------
  temp_date = trimws(strsplit(Zeit, "[,]")[[x]][2], "l")
  temp_date = gsub("\\.", "", temp_date)
  temp_date = gsub("[[:space:]]", ".", temp_date)
  
  # month tranform to date form
  { temp_date = gsub("Januar", "01", temp_date)
    temp_date = gsub("Februar", "02", temp_date)
    temp_date = gsub("März", "03", temp_date)
    temp_date = gsub("April", "04", temp_date)
    temp_date = gsub("Mai", "05", temp_date)
    temp_date = gsub("Juni", "06", temp_date)
    temp_date = gsub("Juli", "07", temp_date)
    temp_date = gsub("August", "08", temp_date)
    temp_date = gsub("September", "09", temp_date)
    temp_date = gsub("Oktober", "10", temp_date)
    temp_date = gsub("November", "11", temp_date)
    temp_date = gsub("Dezember", "12", temp_date)
  }
  temp_date <- as.Date(temp_date, "%d.%m.%Y")
  date_start = c(date_start, temp_date)
  date_end = c(date_end, temp_date)
  
  
  # formluating time format --------------------------------------------------------
  temp_time = gsub("Uhr", "", strsplit(Zeit, "[,]")[[x]][3])
  temp_time = gsub("[[:space:]]", "", temp_time)
  temp_time = paste(temp_time, ":00")
  time_start = c(time_start, chron(times = times(temp_time)))
  time_end = c(time_end, NA)
  
  
  city = c(city, "Würzburg")
  street = c(street, "Kiliansplatz 1")
  zip = c(zip, 97070)
  organizer = c(organizer, "Dom St. Kilian")
  price = c(price, NA)
  lat = c(lat, 49.79369)
  lng = c(lng, 9.93159)
  url = c(url, "https://www.dom-wuerzburg.de/aktuelles/veranstaltungen")
  
}

# final formation for date and time
date_start <- chron(dates = date_start)
date_end <- chron(dates = date_end)
time_start <- chron(times = time_start)
time_end <- chron(times = time_end)


# creating list
eventkalendar_DSK <- data.frame(date_start, date_end, time_start, time_end, title, description, price, 
                                city, street, zip, organizer, lat, lng, url)


##### VKU #####

tmp <- NULL
tmp.name <- ""
tmp <- setNames(data.frame(matrix(ncol = 14, nrow = 1)),  colnames(events))
tmp <- data.frame(matrix(ncol = 14, nrow = 1))


url <- "http://vku-kunst.de/"

links <- read_html(url)%>%html_nodes("a.ai1ec-read-more")%>%html_attr("href")
cat <- read_html(url)%>%html_nodes(".ai1ec-color-swatch")%>%html_attr("title")
events=""
tmp.name=""
for(i in 1:length(links)){
  name <- read_html(links[i])%>%html_node(".entry-title")%>%html_text()
  if(tmp.name != name){
    if(cat[i]!="Konzert"){
      #read part where start & end Date is hidden
      dt <- read_html(links[i])%>%html_node(".su-column-inner")%>%html_nodes("p")%>%html_text()
      #get part where Date is 
      dt <- lapply(dt, function(x) if(str_detect(x,"Ausstellung")) x)%>%unlist
      #pattern for Date extraction
      p="\\s*(?:\\b\\d{4}\\b)|(?:\\b\\d{1,2}\\s*[/\\.-]\\s*\\d{1,2}\\s*[/\\.-]\\s*(?:\\d{4}|\\d{2})\\b)|\\b\\d{1,2}\\s*[/\\.-]?\\s*(?:Januar|Februar|März|April|Mai|Juni|Juli|August|September|Oktober|November|Dezember|(?:Jan|Feb|Febr|Mar|Apr|Jun|Jul|Aug|Sept|Sep|Oct|Nov|Dec).?)" 
      #extract Date
      dt <-str_extract_all(dt, pattern=p)%>%unlist
      if(length(dt)>1){
        tmp$date_start  <- dt[1]%>%as.Date(format="%d. %B")
        tmp$date_end    <- dt[2]%>%as.Date(format="%d. %B")
      }else{
        tmp$date_start  <- dt[1]%>%as.Date(format="%d. %B")
        tmp$date_end    <- dt[1]%>%as.Date(format="%d. %B")
      }
      
      tmp$time_start <- times("11:00:00")
      tmp$time_end <- times("18:00:00")
      tmp$title <- read_html(links[i])%>%html_node(".entry-title")%>%html_text()
      string <-read_html(links[i])%>%html_node(".entry-content")%>%html_nodes("p")
      tmp$description <- string[3]%>%html_text()
      tmp$price       <- NA
      tmp$city        <-"Wuerzburg"
      tmp$street      <- "Zeller Straße 1"
      tmp$zip         <- as.numeric("97082")
      tmp$organizer   <-"Spitäle"
      tmp$lat         <- as.numeric(49.79288)
      tmp$lng         <- as.numeric(9.92413)
      tmp$url         <- links[i]
    }else{
      #read part where start & end Date is hidden
      dt <- read_html(links[i])%>%html_node(".su-column-inner")%>%html_nodes("p")%>%html_text()
      #get part where Date is 
      dt <- lapply(dt, function(x) if(str_detect(x,"Konzert am")) x)%>%unlist
      #pattern for Date extraction
      p="\\s*(?:\\b\\d{4}\\b)|(?:\\b\\d{1,2}\\s*[/\\.-]\\s*\\d{1,2}\\s*[/\\.-]\\s*(?:\\d{4}|\\d{2})\\b)|\\b\\d{1,2}\\s*[/\\.-]?\\s*(?:Januar|Februar|März|April|Mai|Juni|Juli|August|September|Oktober|November|Dezember|(?:Jan|Feb|Febr|Mar|Apr|Jun|Jul|Aug|Sept|Sep|Oct|Nov|Dec).?)" 
      #extract time
      tm <- str_extract_all(dt, pattern="\\b[0-9]{2} Uhr")%>%unlist
      #extract date
      dt <-str_extract_all(dt, pattern=p)%>%unlist
      tmp$date_start  <- dt[1]%>%as.Date(format="%d. %B")
      tmp$date_end    <- dt[1]%>%as.Date(format="%d. %B")
      tmp$time_start <- times(paste0(str_extract(tm, "[0-9]{2}"),":00:00"))
      tmp$time_end <- NA
      tmp$title <- read_html(links[i])%>%html_node(".entry-title")%>%html_text()
      string <- read_html(links[i])%>%html_node(".entry-content")%>%html_nodes("p")
      tmp$description <- string[2]%>%html_text()
      price <-read_html(links[i])%>%html_node(".entry-content")%>%html_nodes("p")%>%html_text
      price <- lapply(price, function(x) if(str_detect(x,"Eintritt")) x)%>%unlist
      endpos<-regexpr(pattern = ")", text = price, ignore.case = TRUE)
      price <- substr(price, 0 , endpos)
      tmp$price       <- price
      tmp$city        <-"Wuerzburg"
      tmp$street      <- "Zeller Straße 1"
      tmp$zip         <- as.numeric("97082")
      tmp$organizer   <-"Spitäle"
      tmp$lat         <- as.numeric(49.79288)
      tmp$lng         <- as.numeric(9.92413)
      tmp$url         <- links[i]
    }#else
    events <- tmp
  }#if
  
  tmp.name <- read_html(links[i])%>%html_node(".entry-title")%>%html_text()
}#for

tmp$time_end <- tmp$time_end%>%times()

##PRICE
get_price <- function(link) {
  url <- link
  url %>%
    read_html() %>%
    html_nodes("td") %>%
    html_text %>%
    str_extract_all("([0-9]+,[0-9]+.(?=â¬))|(kostenfrei)") 
}
sapply(url_igz, get_price)-> price_igz
gsub("character\\(0\\)",NA,price_igz) -> price_igz
unlist(price_igz) -> price_igz


##DATA FRAME
df_igz <- data.frame(title=title_igz, url=url_igz, description=description_igz,date_start=date_start_igz,date_end=date_end_igz,
                     time_start=time_start_igz,price=price_igz,organizer=organizer_igz,lng=lng_igz,lat=lat_igz,
                     city=city_igz,street=street_igz,zip=zip_igz)




rownames(df_igz) <- NULL

df_igz
