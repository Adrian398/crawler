ZOM <- function() {
  
  #----------------ZOM R-Silenium----------------
  zom_url <- "https://www.ukw.de/patienten-besucher/veranstaltungskalender/"
  
  # Start Browser
  rD <- rsDriver()
  remDr <- rD[["client"]]
  remDr$navigate(zom_url)
  
  # Smarter: Click button until all sites are loaded
  run <- TRUE
  i <- 1
  while (run){
    tryCatch(
      remDr$findElement(using = 'css selector', ".jscroll-next")$clickElement(),
      error= function(c) {run <<- F},
      warning = function(w) {run <<- F},
      finally = print(paste("Pressed button", i, "times"))
    )
    i <- i + 1
    Sys.sleep(5)
  }
  #-------------

  raw_data <- read_html(remDr$getPageSource()[[1]])
  
  raw_data %>%
    html_nodes(".ui-helper-reset") -> node_data_zom
  
  # Get Values
  title_selector <- ".title"
  teaser_selector <- ".teaser"
  from_date_selector <- ".date1"
  to_date_selector <- ".date2"
  time_selector <- ".time"
  ort_selector <- ".organizer"
  link <- ".print a"
  
  node_data_zom %>%
    html_node(title_selector) %>%
    html_text() -> title
  node_data_zom %>%
    html_node(teaser_selector) %>%
    html_text() -> teaser
  node_data_zom %>%
    html_node(from_date_selector) %>%
    html_text() -> from_date
  node_data_zom %>%
    html_node(to_date_selector) %>%
    html_text() -> to_date
  node_data_zom %>%
    html_node(time_selector) %>%
    html_text() -> time
  node_data_zom %>%
    html_node(ort_selector) %>%
    html_text() -> ort
  node_data_zom %>%
    html_node(link) %>%
    html_attr("href") -> links
    links <- paste("https://www.ukw.de",links, sep = "")
  # Merge
  df <- data.frame(Title = title,
                   Teaser = teaser,
                   From_date = from_date, 
                   To_date = to_date,
                   Time = time,
                   Ort = ort,
                   Links = links)
  
  # Clean up
  df %>%
    filter(!is.na(Title)) -> df_clean
  
  df_clean$Links
  
  # Shut down selenium
  remDr$close()
  rm(rD)
  gc()
  
  links
  
  df_clean$To_date <- gsub(" - ", "", df_clean$To_date)
  
  df_clean <- df_clean %>%
    separate(Time, c("StartDatum", "EndDatum"), sep="-")
  
  df_clean$StartDatum <- gsub(" Uhr", "", df_clean$StartDatum)
  df_clean$EndDatum <- gsub(" Uhr", "", df_clean$EndDatum)
  
  df_clean$From_date <- as.character(df_clean$From_date)
  df_clean$From_date <- if_else(str_length(df_clean$From_date) == 6,str_sub(df_clean$From_date, 1, str_length(df_clean$From_date)-1) , df_clean$From_date)
  
  zom_latitude <- rep(49.80655,nrow(df_clean))
  zom_longitude <- rep(9.95743,nrow(df_clean)) 
  zom_city <- rep("Würzburg",nrow(df_clean))
  zom_street <- rep("Oberdürrbacher Straße 6",nrow(df_clean))
  zom_zip <- rep(97080,nrow(df_clean))
  zom_preis <- rep("",nrow(df_clean))
  zom_organizer <- rep("Universitätsklinikum Würzburg",nrow(df_clean))
  
  df_clean <- mutate(df_clean, Teaser = paste(df_clean$Teaser, "Ort: ", df_clean$Ort))
 # df_zom_final <- data.frame(Titel=df_clean$Title, Beschreibung=df_clean$Teaser, StartDatum = df_clean$From_date, EndDatum = df_clean$To_date, StartUhrzeit = df_clean$StartDatum, EndZeit = df_clean$EndDatum, BeschreibenderOrt = df_clean$Ort, Adresse = zom_adresse, Latitude = zom_latitude, Longitude = zom_longitude)
  #df_zom_final <- unique(df_zom_final)
  
  df_zom_final <- data.frame(title=df_clean$Title, url = df_clean$Links, description = df_clean$Teaser, lng = zom_longitude, lat = zom_latitude, city = zom_city, street = zom_street, zip = zom_zip, date_start = df_clean$From_date, date_end = df_clean$To_date, time_start = df_clean$StartDatum, time_end = df_clean$EndDatum, price = zom_preis, organizer = zom_organizer)
  df_zom_final <- unique(df_zom_final)
  
  
  return(df_zom_final)
}
