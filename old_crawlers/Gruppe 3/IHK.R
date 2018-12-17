#install.packages("devtools")
#install_version("binman", version = "0.1.0", repos = "https://cran.uni-muenster.de/")
#install_version("wdman", version = "0.2.2", repos = "https://cran.uni-muenster.de/")
#install_version("RSelenium", version = "1.7.1", repos = "https://cran.uni-muenster.de/")
library(devtools)
library(RSelenium)
library(tidyverse)
library(rvest)

IHK <- function() {
  #--------------------------------ME HAUS FERTIG-------------------------------------------
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
