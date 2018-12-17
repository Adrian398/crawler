library(rvest)
library(tidyverse)
library(stringr)
library(chron)
library(gsubfn)

#Anzahl der Seiten
url <- "https://www.juliusspital.de/aktuelles/veranstaltungskalender/index.html?page=0"
url%>%
  read_html() %>%
  html_nodes(".page:nth-child(7)") %>%
  html_text(trim = T) -> page_number
as.numeric(page_number) -> page_number

links <- paste0("https://www.juliusspital.de/aktuelles/veranstaltungskalender/index.html?page=",0:(page_number-1))

pages <- function(link) {
url <- link
url%>%
  read_html() %>%
  html_nodes(".ev-title") %>%
  html_attr("href") -> href

#Links der einzelnen Veranstaltungen
sapply(href, function(x) paste0("https://www.juliusspital.de",x)) -> href

##DATE
get_date <- function (link) {
  url <- link
  url%>%
    read_html() %>%
    html_nodes(".evtermin") %>%
    html_text(trim = T) 
}
sapply(href,get_date) -> date
datum_bereinigt2 <- str_extract_all(date, "\\d{1,2}\\.\\d{1,2}\\.\\d{4}")
unlist(datum_bereinigt2) -> datum_bereinigt2
as.Date(datum_bereinigt2, format = "%d.%m.%Y") -> date_start
date_start -> date_end

##TIME
time <- strapply(as.character(date), "\\d{1,2}\\:\\d{1,2}")
times(paste0(time,":00")) -> time_start
time_end <- NA

##DESCRIPTION
get_description <- function (link) {
  url <- link
  url%>%
    read_html() %>%
    html_nodes("p") %>%
    html_text(trim = T) -> descr
  paste(descr, collapse = " ")
  
}
lapply(href,get_description) -> description
unlist(description) -> description

##PRICE
str_extract(description,"[0-9]+(?=,-)|(kostenfrei)") -> price
gsub("character\\(0\\)",NA,price) -> price

##TITLE
url <- link
url%>%
  read_html() %>%
  html_nodes(".ev-title") %>%
  html_text(trim=T) -> title

##Veranstaltungsort
get_Veranstaltungsort <- function (link) {
  url <- link
  url%>%
    read_html() %>%
    html_nodes(".evadress") %>%
    html_text(trim = T) 
}
lapply(href,get_Veranstaltungsort) -> veranstaltungsort
#test <- data.frame(veranstaltungsort,description)

##zusätzliche Infos
lng <- 9.9329
lat <- 49.79927
city <- "Wuerzburg"
street <- "Juliuspromenade 19"
zip <- 97070
organizer <- "Stiftung Juliusspital Würzburg"

df <- data.frame(title=title,url=href,description=description,lng=lng,lat=lat,
                 city=city,street=street,zip=zip,
                 date_start=date_start,date_end=date_end,time_start=time_start,
                 time_end = time_end,price=price,organizer=organizer)

row.names(df) <- NULL

return(df)
}

lapply(links,pages) -> df

df_juliusspital <- rbind(df[[1]],df[[2]],df[[3]],df[[4]],df[[5]],df[[6]],df[[7]],df[[8]],df[[9]],
                         df[[10]],df[[11]],df[[12]],df[[13]],df[[14]],df[[15]],df[[16]])
                                                                                           