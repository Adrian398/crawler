AGK <- function() {
  #--------------------------------Augustinerkirche FERTIG-------------------------------------------
  agk_monat_voll <- vector()
  agk_beschreibung_voll <- vector()
  agk_tag_voll <- vector()
  agk_zeit_voll <- vector()
  agk_titel_voll <- vector()
  agk_links_voll <- vector()
  
  agk_url <- "http://augustinerkirche-wuerzburg.de/veranstaltungen-3/"
  
  rD <- rsDriver()
  remDr <- rD[["client"]]
  remDr$navigate(agk_url)
  # Smarter: Click button until all sites are loaded
  run <- TRUE
  i <- 1
  while (run){
    tryCatch(
      remDr$findElement(using = 'css selector', ".ai1ec-pull-left .ai1ec-next-page")$clickElement(),
      error= function(c) {run <<- F},
      warning = function(w) {run <<- F},
      finally = print(paste("Pressed button", i, "times"))
    )
    raw_data_agk <- read_html(remDr$getPageSource()[[1]])
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
    
    i <- i + 1
    Sys.sleep(7)
  }
  
  # Shut down selenium
  remDr$close()
  rm(rD)
  gc()
  
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
  agk_EndUhrzeit <- rep("",nrow(df_agk))
  agk_organizer <- rep("Augustinerkirche",nrow(df_agk))
    
    #Latitude und Longitude deklarieren
  agk_links_voll <- unique(agk_links_voll)
  
  df_agk$StartDatum <- as.character(df_agk$StartDatum)
  df_agk$StartDatum <- if_else(str_length(df_agk$StartDatum) == 4, as.character(paste("0",df_agk$StartDatum, sep = "")), df_agk$StartDatum)

  df_agk_final <- data.frame(title=df_agk$Titel, url = agk_links_voll, description = df_agk$Beschreibung, lng = agk_longitude, lat = agk_latitude, city = agk_city, street = agk_street, zip = agk_zip, date_start = df_agk$StartDatum, date_end = df_agk$StartDatum, time_start = df_agk$Zeit, time_end = agk_EndUhrzeit, price = agk_EndUhrzeit, organizer = agk_organizer)
  df_agk_final <- unique(df_agk_final)
  
  
return(df_agk_final)
}



