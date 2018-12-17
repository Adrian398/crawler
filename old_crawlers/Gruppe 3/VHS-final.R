library(tidyverse)
library(rvest)
library(tidyr)
library(plyr)
library(lubridate)
library(qdapRegex)
library(RSelenium)
library(chron)


VHS <- function(){

#------------------------------------------------
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
df_vhs_to$city <- "W?rzburg"
df_vhs_to$street <- "M?nzstra?e 1"
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
