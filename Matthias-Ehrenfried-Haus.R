mh_url <- "http://www.me-haus.de/startseite/kalender"
mh_titel_voll <- vector()
mh_datum_voll <- vector()
mh_date_voll <- vector()

rD <- rsDriver()
remDr <- rD[["client"]]
remDr$navigate(mh_url)

run <- TRUE
i <- 1
while (i <= 60){
  tryCatch(
    remDr$findElement(using = 'css selector', ".pagingbox-navigate-right")$clickElement(),
    error= function(c) {run <<- F},
    warning = function(w) {run <<- F},
    finally = print(paste("Pressed button", i, "times"))
  )
  raw_data <- read_html(remDr$getPageSource()[[1]])
  raw_data %>%
    html_nodes(".date") %>%
    html_text(trim = T) %>%
    ifelse(. == "", NA, .) -> mh_date
  mh_datum_voll <- c(mh_datum_voll, mh_date)
  raw_data %>%
    html_nodes(".itemtitle a") %>%
    html_text(trim = T) %>%
    ifelse(. == "", NA, .) -> mh_titel
  mh_titel_voll <- c(mh_titel_voll, mh_titel)
  raw_data %>%
    html_nodes(".function") %>%
    html_text(trim = T) %>%
    ifelse(. == "", NA, .) -> mh_date
  mh_date2 <- rep(mh_date,length(mh_titel))
  mh_date_voll <- c(mh_date_voll, mh_date2)
  
  i <- i + 1
  Sys.sleep(7)
}

# Shut down selenium
remDr$close()
rm(rD)
gc()

df_mh <- data.frame(mh_titel_voll, mh_datum_voll, mh_date_voll)

df_mh <- df_mh %>%
  separate(mh_datum_voll, c("StartUhrzeit", "EndUhrzeit"), sep=" bis ")

df_mh$EndUhrzeit <- gsub(" Uhr", "", df_mh$EndUhrzeit)

df_mh <- df_mh %>%
  separate(mh_date_voll, c("A", "StartDatum"), sep=", ")

mh_latitude <- rep(49.7978,nrow(df_mh))
mh_longitude <- rep(9.93509,nrow(df_mh)) 
mh_city <- rep("Würzburg",nrow(df_mh))
mh_street <- rep("Kolpingstraße 11",nrow(df_mh))
mh_zip <- rep(97070,nrow(df_mh))
mh_Beschreibung <- rep("",nrow(df_mh))
mh_links <- rep("http://www.me-haus.de/startseite/kalender",nrow(df_mh))
mh_organizer <- rep("Matthias-Ehrenfried-Haus",nrow(df_mh))
#Latitude und Longitude deklarieren


df_mh_final <- data.frame(title=df_mh$mh_titel_voll, url = mh_links, description = mh_Beschreibung, lng = mh_longitude, lat = mh_latitude, city = mh_city, street = mh_street, zip = mh_zip, date_start = df_mh$StartDatum, date_end = df_mh$StartDatum, time_start = df_mh$StartUhrzeit, time_end = df_mh$EndUhrzeit, price = mh_Beschreibung, organizer = mh_organizer)
df_mh_final <- unique(df_mh_final)