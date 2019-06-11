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

#add metadf idlocation
idlocation = 4868
meta_df = data.frame(organizer, url, idlocation)
names(meta_df)[names(meta_df) == 'url'] <- 'url_crawler'

#write to database
write_dataframes_to_database(crawled_df, meta_df, conn)

















