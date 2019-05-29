#install.packages("devtools")
# install_version("binman", version = "0.1.0", repos = "https://cran.uni-muenster.de/")
# install_version("wdman", version = "0.2.2", repos = "https://cran.uni-muenster.de/")
# install_version("RSelenium", version = "1.7.1", repos = "https://cran.uni-muenster.de/")

library(devtools)
library(RSelenium)

Thomas <- function() {
  #--------------------------------ME HAUS FERTIG-------------------------------------------
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
