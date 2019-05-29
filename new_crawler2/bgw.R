url = "http://www.bgw.uni-wuerzburg.de/fuehrungen-und-veranstaltungen/veranstaltungen/page/1/"

url %>%
  read_html() -> raw_read

raw_read %>%
  html_nodes(".news-list__item-header a") %>%
  html_text(trim = T) -> title

raw_read %>%
  html_nodes(".news-list__item-header a") %>%
  html_attr("href") -> link

raw_read %>%
  html_nodes(".icon-calendar+ .mod_events_latest_date") %>%
  html_text(trim = T) -> dates

month_convertor <- function(given_date){
  given_date = gsub("Jan ","01.",given_date)
  given_date = gsub("Feb ","02.",given_date)
  given_date = gsub("Mär ","03.",given_date)
  given_date = gsub("Apr ","04.",given_date)
  given_date = gsub("Mai ","05.",given_date)
  given_date = gsub("Jun ","06.",given_date)
  given_date = gsub("Jul ","07.",given_date)
  given_date = gsub("Aug ","08.",given_date)
  given_date = gsub("Sep ","09.",given_date)
  given_date = gsub("Okt ","10.",given_date)
  given_date = gsub("Nov ","11.",given_date)
  given_date = gsub("Dez ","12.",given_date)
  return(given_date)
}

dates = map(dates,month_convertor)
dates = unlist(dates)
dates = gsub(" ",".",dates)
date_start = as.Date(dates,"%d.%m.%Y")


raw_read %>%
  html_nodes(".icon-time+ .mod_events_latest_date") %>%
  html_text(trim = T) -> time_start
time_start = paste0(time_start,":00")
time_start <- chron(times = time_start)

raw_read %>%
  html_nodes(".mod_events_latest_date+ .mod_events_latest_date") %>%
  html_text(trim = T) -> time_end
time_end = paste0(time_end,":00")
time_end <- chron(times = time_end)


# setting up the rest data
city = "Wuerzburg"
street = "Zeller Straße 44"
zip = 97082
organizer = "Ökohaus Würzburg"
lat = 49.79471
lng = 9.91973
description = NA


#set up to write to database
crawled_df = data.frame(title, description, link, date_start, date_end, time_end, time_start, street, city, zip, lng, lat)
meta_df = data.frame(organizer, url)
names(meta_df)[names(meta_df) == 'url'] <- 'url_crawler'

#write to database
write_dataframes_to_database(crawled_df, meta_df, conn)


















##### ALTE!!!! #########

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