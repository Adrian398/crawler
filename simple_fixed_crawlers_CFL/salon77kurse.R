library(tidyverse)
library(rvest)
library(gsubfn)
library(lubridate)
library(chron)
library(knitr)
library(plyr)
library(RSelenium)
library(XML)

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
