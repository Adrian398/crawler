##### Import Packages ####
library(stringr)
library(rvest)
library(tidyverse)
library(chron)
library(lubridate)
library(plyr)
library(R.utils)
library(gridExtra)
library(grid)
library(gsubfn)
library(yaml)
library(RSelenium)
library(devtools)
library(xml)

###ukw####
# Standard approach
url <- "https://www.ukw.de/patienten-besucher/veranstaltungskalender/"

url %>%
  read_html() %>%
  html_nodes(".ui-helper-reset") %>%
  html_text()

url %>%
  read_html() %>%
  html_nodes(".date") %>%
  html_text()



# Setting up RSelenium

install_version("binman", version = "0.1.0", repos = "https://cran.uni-muenster.de/")
install_version("wdman", version = "0.2.2", repos = "https://cran.uni-muenster.de/")
install_version("RSelenium", version = "1.7.1", repos = "https://cran.uni-muenster.de/")




# Start Browser
rD <- rsDriver()
remDr <- rD[["client"]]
remDr$navigate("http://www.google.com")
remDr$navigate(url)

# Click button 20 times:
i <- 1
while (i<20){
  try(remDr$findElement(using = 'css selector', ".jscroll-next")$clickElement())
  i <- i + 1
  Sys.sleep(2)
}

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
  Sys.sleep(2)
}

# Get HTML
site <- read_html(remDr$getPageSource()[[1]])

# Get Nodes
site %>%
  html_nodes(".ui-helper-reset") -> node_data

# Get Values
title_selector <- ".title"
teaser_selector <- ".teaser"
from_date_selector <- ".date1"
to_date_selector <- ".date2"
time_selector <- ".time"

node_data %>%
  html_node(title_selector) %>%
  html_text() -> title
node_data %>%
  html_node(teaser_selector) %>%
  html_text() -> teaser
node_data %>%
  html_node(from_date_selector) %>%
  html_text() -> from_date
node_data %>%
  html_node(to_date_selector) %>%
  html_text() -> to_date
node_data %>%
  html_node(time_selector) %>%
  html_text() -> time

# Merge
ukw <- data.frame(Title = title,
                 Teaser = teaser,
                 From_date = from_date, 
                 To_date = to_date,
                 Time = time)

# Clean up
ukw %>%
  filter(!is.na(Title)) -> df_clean


# Shut down selenium
remDr$close()
rm(rD)
gc()

##### Jugendbildungsstätte Unterfranken ######

jubi_raw_month=c(month(today()), month(today())+1, month(today())+2)
jubi_year=c(year(today()), year(today()+30), year(today()+60))
jubi_month = formatC(jubi_raw_month, width = 2, format = "d", flag = "0")
jubi_url_raw=list()
jubi_url=list()
jubi_kurse=list()
jubi_links=list()
jubi_start=list()
jubi_ort=list()
jubi_ende=list()
jubi_beschreibung=list()
jubi_links_raw=list()
i=1


while(i<=length(jubi_year)){
  b = paste0("http://www.jubi-unterfranken.de/events/", jubi_year[i],"-", jubi_month[i])
  jubi_url_raw[[i]] = b
  jubi_url=unlist(jubi_url_raw)
  jubi_url[i] %>%
    read_html() -> jubi_data
  
  jubi_data %>%
    html_nodes(".tribe-events-month-event-title") %>%
    html_text() -> jubi_a
  jubi_kurse=append(jubi_kurse,jubi_a)
  
  jubi_data %>%
    html_nodes(".tribe-events-has-events > div > h3 > a") %>%
    html_attr("href") -> jubi_b
  jubi_c = unique(jubi_b)
  jubi_links_raw=append(jubi_links_raw,jubi_b)
  jubi_links=append(jubi_links,jubi_c)  
  i=i+1
}

jubi_kurse = unlist(jubi_kurse)
jubi_links = unlist(jubi_links)
jubi_links_raw=unlist(jubi_links_raw)
jubi_year2=append(jubi_year, jubi_year)

j=1
while(j<=length(jubi_links_raw)){
  
  jubi_links_raw[[j]] %>%
    read_html() -> jubi_data
  
  jubi_data %>%
    html_nodes(".tribe-events-schedule .tribe-event-date-start") %>%
    html_text() -> jubi_c
  jubi_start = append(jubi_start, jubi_c)
  
  jubi_data %>%
    html_nodes(".tribe-postal-code , .tribe-locality , .tribe-street-address") %>%
    html_text() -> jubi_o
  jubi_ort = append(jubi_ort, jubi_o)
  
  jubi_data %>%
    html_nodes(".tribe-events-schedule .tribe-event-date-end") %>%
    html_text() -> jubi_d
  jubi_ende = append(jubi_ende,jubi_d)
  
  j=j+1
}

jubi_start=unlist(jubi_start)
jubi_ende=unlist(jubi_ende)

jubi_start_tag=list()
jubi_start_monat=list()
jubi_start_jahr=list()
jubi_start_uhrzeit=list()

jubi_ende_tag=list()
jubi_ende_monat=list()
jubi_ende_jahr=list()
jubi_ende_uhrzeit=list()

k=1
while(k<=length(jubi_start)){
  
  jubi_start[k] %>%
    strsplit(" ") %>%
    unlist() -> a
  jubi_start_tag[k] = a[1]
  jubi_start_monat[k] = a[2]
  jubi_start_jahr[k] = jubi_year2[k]
  jubi_start_uhrzeit[k] = a[4]
  
  jubi_ende[k] %>%
    strsplit(" ") %>%
    unlist() -> b
  jubi_ende_tag[k] = b[1]
  jubi_ende_monat[k] = b[2]
  jubi_ende_jahr[k] = jubi_year2[k]
  jubi_ende_uhrzeit[k] = b[4]
  
  k=k+1
  
}

jubi_ende_monat= unlist(jubi_ende_monat)
jubi_ende_uhrzeit= unlist(jubi_ende_uhrzeit)
jubi_ende_tag=unlist(jubi_ende_tag)
jubi_ende_jahr= unlist(jubi_ende_jahr)
jubi_start_monat=unlist(jubi_start_monat)
jubi_start_tag=unlist(jubi_start_tag)
jubi_start_uhrzeit=unlist(jubi_start_uhrzeit)
jubi_start_jahr=unlist(jubi_start_jahr)

jubi_date_start = paste0(jubi_start_tag, jubi_start_monat, ".", jubi_start_jahr)
jubi_date_ende = paste0(jubi_ende_tag, jubi_ende_monat, ".", jubi_ende_jahr)

#jubi_start_final = paste0(jubi_start_tag, jubi_start_monat, ".", jubi_start_jahr, " ", jubi_start_uhrzeit, ":00")
#jubi_ende_final = paste0(jubi_ende_tag, jubi_ende_monat, ".", jubi_ende_jahr, " ", jubi_ende_uhrzeit, ":00")


jubi_time_start = times(paste0(jubi_start_uhrzeit, ":00"))
jubi_time_ende = times(paste0(jubi_ende_uhrzeit, ":00"))

jubi_date_start = as.Date(jubi_date_start,format = "%d.%B.%Y")
jubi_date_ende = as.Date(jubi_date_ende,format = "%d.%B.%Y")

jubi_ort=unlist(jubi_ort[1])
jubi_ort=paste0(jubi_ort[1], ", ", jubi_ort[3], " ",jubi_ort[2])


##### Bund Naturschutz #####

#Startseite der Veranstaltungen auslesen
natur_title=list()
natur_url= "https://wuerzburg.bund-naturschutz.de/veranstaltungen.html"
natur_url %>%
  read_html() -> natur_data

natur_data %>%
  html_nodes("td:nth-child(3)") %>%
  html_text() -> natur_ort

natur_data %>%
  html_nodes("td a") %>%
  html_text() -> natur_t
natur_title=append(natur_title,natur_t)

natur_data %>%
  html_nodes("td a") %>%
  html_attrs() %>%
  unlist() -> n_data

natur_start=list()
natur_beschreibung=list()
j=1

natur_besch_kombi=list()
natur4=list()
n_links=list()
#Auslesen der Beschreibungen der jeweiligen Veranstaltungen durch Öffnen jedes Links und auslesen

while(j<=length(n_data)){
  n_link=paste0("https://wuerzburg.bund-naturschutz.de", n_data[j])
  n_links=append(n_links, n_link)
  n_link %>%
    read_html() -> natur_links
  
  natur_links %>%
    html_nodes(".border p") %>%
    html_text() -> natur_besch
  
  print(paste0("Schleife: ", j, "Länge: ", length(natur_besch)))
  i=1
  k=1
  natur2=list()
  
  while(i<=length(natur_besch)){
    
    if(nchar(natur_besch[i])>20){
      natur2=append(natur2, natur_besch[i])
    }
    print(paste("Schleife: ", i, " ---> ", natur2))
    i=i+1
  }
  
  if(k==2){natur2=paste0(natur2[1], " ", natur2[2])}
  if(k==3){natur2=paste0(natur2[1], " ", natur2[2], " ", natur2[3])}
  natur4=append(natur4,natur2)
  
  natur_links %>%
    html_nodes(".border") %>%
    html_text() -> natur_c
  natur_start = append(natur_start,natur_c)
  
  j=j+1
}


# Auslesen der Anzahl der Veranstaltungsseiten (Seite 1 ausgeschlossen, da anderer Link)
natur_url%>%
  read_html() -> natur_seiten

natur_seiten %>%
  html_nodes(".f3-widget-paginator") %>%
  html_text() %>%
  str_extract_all('[0-9]') %>%
  unlist() %>%
  .[-1]-> seiten

k=0
h=1
a=list()
natur_besch_kombi_2=list()
natur4_2=list()
natur5=list()
while(h<=length(seiten)){
  natur_url2=paste0("https://wuerzburg.bund-naturschutz.de/veranstaltungen/teil/", seiten[h],".html")
  
  natur_url2 %>%
    read_html() -> natur_data2
  
  natur_data2 %>%
    html_nodes("td:nth-child(3)") %>%
    html_text() -> natur_ort2
  natur_ort = append(natur_ort, natur_ort2)
  
  natur_data2 %>%
    html_nodes("td a") %>%
    html_text() -> natur_t
  
  natur_title = append(natur_title, natur_t)
  
  
  natur_data2 %>%
    html_nodes("td a") %>%
    html_attrs() %>%
    unlist() -> n_data
  
  j=1
  
  i=1
  ## Durchsuchen der einzelnen Veranstaltungen
  while(j<=length(n_data)){
    natur_url3=paste0("https://wuerzburg.bund-naturschutz.de", n_data[j])
    n_links=unlist(append(n_links, natur_url3))
    
    natur_url3 %>%
      read_html() -> natur_links
    
    natur_links %>%
      html_nodes(".border p") %>%
      html_text() -> natur_besch
    i=1
    k=0
    natur2=list()
    
    ## Durchsuchen der einzelnen Beschreibungscontainer
    while(i<=length(natur_besch)){
      if(nchar(natur_besch[i])>20){
        natur2=append(natur2, natur_besch[i])
        k=k+1
      }
      i=i+1
    }
    
    if(k==2){natur2=paste0(natur2[1], " ", natur2[2])}
    if(k==3){natur2=paste0(natur2[1], " ", natur2[2], " ", natur2[3])}
    
    natur5=append(natur5,natur2)
    
    natur_links %>%
      html_nodes(".border") %>%
      html_text() -> natur_c
    natur_start=append(natur_start, natur_c)
    j=j+1
  }
  
  h=h+1
}

natur_beschreibung=unlist(append(natur4, natur5))
natur_start=unlist(natur_start)
natur_datum=str_extract(natur_start, '[0-3]?[0-9].[0-1][0-9].[0-9]{4}')

natur_start_uhrzeit=str_extract(natur_start, '[0-2][0-9]:[0-5][0-9]')

natur_ende=str_extract(natur_start, '- [0-2][0-9]:[0-5][0-9]')
natur_ende <- lapply(natur_ende, gsub, pattern = "- ", replacement = "", fixed = TRUE)
natur_ende = unlist(natur_ende)

natur_title=unlist(natur_title)

Natur_time_start = times(paste0(natur_start_uhrzeit, ":00"))
Natur_time_ende = times(paste0(natur_ende, ":00"))

Natur_date_start = as.Date(natur_datum,format = "%d.%m.%Y")
Natur_date_ende = as.Date(natur_datum,format = "%d.%m.%Y")


##### LaViva Danceclub #####

library(RSelenium)

viva_url = "http://www.la-viva-danceclub.de/events-laviva-danceclub"

binman::list_versions("seleniumserver")
rD <- rsDriver()
remDr <- rD[["client"]]
remDr$navigate(viva_url)

viva_data <- read_html(remDr$getPageSource()[[1]])

remDr$close()
rm(rD)
gc()

viva_data %>%
  html_nodes(".date") %>%
  html_text()-> viva_d

viva_d <- lapply(viva_d, gsub, pattern = " ", replacement = "", fixed = TRUE)
viva_date=str_extract(viva_d, '\\d{2}.\\d{2}.\\d{4}')

viva_data %>%
  html_nodes(".details") %>%
  html_text() -> viva_details

viva_data %>%
  html_nodes(".details div:nth-child(1)") %>%
  html_text() -> viva_beschreibung

viva_details <- lapply(viva_details, gsub, pattern = " ", replacement = "", fixed = TRUE)
viva_uhrzeit=str_extract(viva_details, '[0-2][0-9].[0-5][0-9]')

viva_data %>%
  html_nodes("h2") %>%
  html_text()-> viva_title

viva_time_start = times(paste0(viva_uhrzeit, ":00"))

viva_date = as.Date(viva_date,format = "%d.%m.%Y")


Jugendbildungszentrumsveranstaltungen = data.frame("title"=jubi_kurse, "url"=jubi_links_raw, "description"= NA,"lng"=9.9547, "lat"=49.73939, 
                                                   "city"="Würzburg", "street"="Berner Straße 14", "zip"=97084, "date_start"=jubi_date_start, 
                                                   "date_end"=jubi_date_ende, "time_start"=jubi_time_start, "time_end"=jubi_time_ende , "price"=NA, 
                                                   "organizer"="Jugendbildungszentrum Unterfranken")

Naturveranstaltungen = data.frame("title"=natur_title, "url"=n_links, "description"= natur_beschreibung,
                                  "lng"=9.9199, "lat"=49.79746, "city"="Würzburg", "street"=natur_ort, "zip"=97082, 
                                  "date_start"=Natur_date_start, "date_end"=Natur_date_ende, "time_start"=Natur_time_start, 
                                  "time_end"=Natur_time_ende , "price"=NA, "organizer"="Naturschutzbund Bayern")

LaVivaDanceclub = data.frame("title"=viva_title, "url"="http://www.la-viva-danceclub.de/events-laviva-danceclub", "description"= viva_beschreibung,
                             "lng"=9.970704, "lat"=49.79563, "city"="Würzburg", "street"="Nürnberger Straße 72-74", "zip"=97076, 
                             "date_start"=viva_date, "date_end"=viva_date, "time_start"=viva_time_start, "time_end"=NA , "price"=NA, "organizer"="LaViva Danceclub")


####Frankenwarte ########
frankenwarte_link <- "https://www.frankenwarte.de/unser-bildungsangebot/aktuell.html"
fw_desc_sublink <- frankenwarte_link %>% read_html() %>% html_nodes(".intern") %>% html_attr('href') %>% unique()


getdesc <- function(sublink){
  baselink <- "https://www.frankenwarte.de/"
  link <- paste0(baselink, sublink)
  desc <-link %>%read_html() %>% html_nodes('p')  %>% html_text()
  return(desc[1])
}

getprice <- function(sublink){
  baselink <- "https://www.frankenwarte.de/"
  link <- paste0(baselink, sublink)
  desc <-link %>%read_html() %>% html_nodes('p:nth-child(7)')  %>% html_text()
  return(desc[1])
}

getzipcode <- function(sublink){
  baselink <- "https://www.frankenwarte.de/"
  link <- paste0(baselink, sublink)
  desc <-link %>%read_html() %>% html_nodes('img+ p')  %>% html_text()
  desc <- gsub(desc, pattern="Akademie FrankenwarteLeutfresserweg 81 - 83", replacement="", fixed=T)
  desc <- gsub(desc, pattern=" Würzburg", replacement="", fixed=T) %>% as.numeric
  return(desc[1])
}

getstreet <- function(sublink){
  baselink <- "https://www.frankenwarte.de/"
  link <- paste0(baselink, sublink)
  desc <-link %>%read_html() %>% html_nodes('img+ p')  %>% html_text()
  desc <- gsub(desc, pattern="97082 W?rzburg", replacement="", fixed=T)
  desc <- gsub(desc, pattern="Akademie Frankenwarte", replacement="", fixed=T)
  return(desc)
}

getlink <- function(sublink){
  baselink <- "https://www.frankenwarte.de/"
  link <- paste0(baselink, sublink)
  return(link <- paste0(baselink, sublink))
}

fw <- frankenwarte_link %>% read_html() %>% html_nodes(".va-item") %>% html_text()

fw <- map(fw, function(fw) return(strsplit(fw, "\r\n\t"))) %>% unlist() 
fw <- fw[fw != ""] 
fw <- fw[fw != "\t"]
x <- as.data.frame(fw)
sel <- seq(4,nrow(x),4)
x <- x[-sel,]
fw <- as.vector(x)

fw <- matrix(fw,ncol=3, byrow=TRUE) %>% as.data.frame()

fw$V3 <- as.character(fw$V3)
y <- map(fw$V3, function(fw) if(nchar(fw) <13){return(paste(fw," - ",fw))} else{return(fw)})%>% unlist()
y <- map(y, function(fw) return(strsplit(fw, split=" - ", fixed =T)))%>% unlist() %>% matrix(., ncol=2, byrow = TRUE) %>% as.data.frame()


fw_startdate <- map(y$V1, function(fw) return(as.Date(fw, format="%d.%m."))) %>% do.call(c, .)
fw_enddate <- map(y$V2, function(fw) return(as.Date(fw, format="%d.%m."))) %>% do.call(c, .)

descriptions_fw <- map(fw_desc_sublink, getdesc) %>% unlist()
prices_fw <- map(fw_desc_sublink, getprice) %>% unlist()
zipcode_fw <- map(fw_desc_sublink, getzipcode) %>% unlist()
street_fw <- map(fw_desc_sublink, getstreet) %>% unlist()
link_fw <- map(fw_desc_sublink, getlink) %>% unlist()

frankenwarte  <- data.frame("title" = fw$V2,
                            "url" = link_fw,
                            "description" = descriptions_fw,
                            "lng" = NA,
                            "lat" = NA,
                            "city" = "Wuerzburg",
                            "street" = street_fw,
                            "zip" = zipcode_fw,
                            "date_start" = fw_startdate, 
                            "date_end" = fw_enddate, 
                            "time_start" = NA, 
                            "time_end" = NA, 
                            "price" = prices_fw,
                            "organizer" = "Frankenwarte")

####VHS ######




vhs_url1 <- "https://www.vhs-wuerzburg.info/programm/gesellschaft.html?params=%7B%22filter%22%3Atrue%2C%22noHeadline%22%3Atrue%2C%22noHeaders%22%3Atrue%7D&socketsId=405&filters%5Bparams%5D=%7B%22tophighlights%22%3Anull%2C%22sort%22%3A%5B%22startDate%2Casc%22%5D%2C%22extraFields%22%3A%5B%22courseNumber%22%2C%22title%22%2C%22startDate%22%2C%22venues.city%22%2C%22customDatesText%22%5D%2C%22hideEmptyCategories%22%3Atrue%2C%22catId%22%3A%22489-CAT-KAT5892354%22%2C%22page%22%3A1%2C%22returnType%22%3A%22default%22%7D&filters%5BpageSize%5D=99999&course_url=%2F%2Fwww.vhs-wuerzburg.info%2Fprogramm%2Fgesellschaft.html%3Faction%255B405%255D%3Dcourse&plugin_url=%2F%2Fwww.vhs-wuerzburg.info%2Fprogramm%2Fgesellschaft.html&sockets_ID=405&filters%5BweekdayExclude%5D%5B1%5D=on&filters%5BweekdayExclude%5D%5B2%5D=on&filters%5BweekdayExclude%5D%5B3%5D=on&filters%5BweekdayExclude%5D%5B4%5D=on&filters%5BweekdayExclude%5D%5B5%5D=on&filters%5BweekdayExclude%5D%5B6%5D=on&filters%5BweekdayExclude%5D%5B7%5D=on&filters%5BsearchString%5D="
vhs_url2 <- "https://www.vhs-wuerzburg.info/programm/beruf.html?params=%7B%22filter%22%3Atrue%2C%22noHeadline%22%3Atrue%7D&socketsId=401&filters%5Bparams%5D=%7B%22tophighlights%22%3Anull%2C%22sort%22%3A%5B%22startDate%2Casc%22%5D%2C%22extraFields%22%3A%5B%22courseNumber%22%2C%22title%22%2C%22startDate%22%2C%22venues.city%22%2C%22customDatesText%22%5D%2C%22hideEmptyCategories%22%3Atrue%2C%22catId%22%3A%22489-CAT-KAT5892355%22%7D&filters%5BpageSize%5D=9999999&course_url=%2F%2Fwww.vhs-wuerzburg.info%2Fprogramm%2Fberuf.html%3Faction%255B401%255D%3Dcourse&plugin_url=%2F%2Fwww.vhs-wuerzburg.info%2Fprogramm%2Fberuf.html&sockets_ID=401&filters%5BweekdayExclude%5D%5B1%5D=on&filters%5BweekdayExclude%5D%5B2%5D=on&filters%5BweekdayExclude%5D%5B3%5D=on&filters%5BweekdayExclude%5D%5B4%5D=on&filters%5BweekdayExclude%5D%5B5%5D=on&filters%5BweekdayExclude%5D%5B6%5D=on&filters%5BweekdayExclude%5D%5B7%5D=on&filters%5BsearchString%5D="
vhs_url3 <- "https://www.vhs-wuerzburg.info/programm/sprachen.html?params=%7B%22filter%22%3Atrue%2C%22noHeadline%22%3Atrue%2C%22noHeaders%22%3Atrue%7D&socketsId=402&filters%5Bparams%5D=%7B%22tophighlights%22%3Anull%2C%22sort%22%3A%5B%22startDate%2Casc%22%5D%2C%22extraFields%22%3A%5B%22courseNumber%22%2C%22title%22%2C%22startDate%22%2C%22venues.city%22%2C%22customDatesText%22%5D%2C%22hideEmptyCategories%22%3Atrue%2C%22catId%22%3A%22489-CAT-KAT5892356%22%2C%22pageSize%22%3A%2220%22%2C%22returnType%22%3A%22default%22%7D&filters%5BpageSize%5D=999999&course_url=%2F%2Fwww.vhs-wuerzburg.info%2Fprogramm%2Fsprachen.html%3Faction%255B402%255D%3Dcourse&plugin_url=%2F%2Fwww.vhs-wuerzburg.info%2Fprogramm%2Fsprachen.html&sockets_ID=402&filters%5BweekdayExclude%5D%5B1%5D=on&filters%5BweekdayExclude%5D%5B2%5D=on&filters%5BweekdayExclude%5D%5B3%5D=on&filters%5BweekdayExclude%5D%5B4%5D=on&filters%5BweekdayExclude%5D%5B5%5D=on&filters%5BweekdayExclude%5D%5B6%5D=on&filters%5BweekdayExclude%5D%5B7%5D=on&filters%5BsearchString%5D="
vhs_url5 <- "https://www.vhs-wuerzburg.info/programm/kultur.html?params=%7B%22filter%22%3Atrue%2C%22noHeadline%22%3Atrue%7D&socketsId=404&filters%5Bparams%5D=%7B%22tophighlights%22%3Anull%2C%22sort%22%3A%5B%22startDate%2Casc%22%5D%2C%22extraFields%22%3A%5B%22courseNumber%22%2C%22title%22%2C%22startDate%22%2C%22venues.city%22%2C%22customDatesText%22%5D%2C%22hideEmptyCategories%22%3Atrue%2C%22catId%22%3A%22489-CAT-KAT5892357%22%7D&filters%5BpageSize%5D=99999&course_url=%2F%2Fwww.vhs-wuerzburg.info%2Fprogramm%2Fkultur.html%3Faction%255B404%255D%3Dcourse&plugin_url=%2F%2Fwww.vhs-wuerzburg.info%2Fprogramm%2Fkultur.html&sockets_ID=404&filters%5BweekdayExclude%5D%5B1%5D=on&filters%5BweekdayExclude%5D%5B2%5D=on&filters%5BweekdayExclude%5D%5B3%5D=on&filters%5BweekdayExclude%5D%5B4%5D=on&filters%5BweekdayExclude%5D%5B5%5D=on&filters%5BweekdayExclude%5D%5B6%5D=on&filters%5BweekdayExclude%5D%5B7%5D=on&filters%5BsearchString%5D="
vhs_url6 <- "https://www.vhs-wuerzburg.info/programm/grundbildung.html?params=%7B%22filter%22%3Atrue%2C%22noHeadline%22%3Atrue%7D&socketsId=406&filters%5Bparams%5D=%7B%22tophighlights%22%3Anull%2C%22sort%22%3A%5B%22startDate%2Casc%22%5D%2C%22extraFields%22%3A%5B%22courseNumber%22%2C%22title%22%2C%22startDate%22%2C%22venues.city%22%2C%22customDatesText%22%5D%2C%22hideEmptyCategories%22%3Atrue%2C%22catId%22%3A%22489-CAT-KAT9143021%22%7D&filters%5BpageSize%5D=999999&course_url=%2F%2Fwww.vhs-wuerzburg.info%2Fprogramm%2Fgrundbildung.html%3Faction%255B406%255D%3Dcourse&plugin_url=%2F%2Fwww.vhs-wuerzburg.info%2Fprogramm%2Fgrundbildung.html&sockets_ID=406&filters%5BweekdayExclude%5D%5B1%5D=on&filters%5BweekdayExclude%5D%5B2%5D=on&filters%5BweekdayExclude%5D%5B3%5D=on&filters%5BweekdayExclude%5D%5B4%5D=on&filters%5BweekdayExclude%5D%5B5%5D=on&filters%5BweekdayExclude%5D%5B6%5D=on&filters%5BweekdayExclude%5D%5B7%5D=on&filters%5BsearchString%5D="

vhs_url1_sublinks <-vhs_url1 %>% read_html() %>% html_nodes(".title") %>% html_attr('href')
vhs_url2_sublinks <-vhs_url2 %>% read_html() %>% html_nodes(".title") %>% html_attr('href')
vhs_url3_sublinks <-vhs_url3 %>% read_html() %>% html_nodes(".title") %>% html_attr('href')
vhs_url5_sublinks <-vhs_url5 %>% read_html() %>% html_nodes(".title") %>% html_attr('href')
vhs_url6_sublinks <-vhs_url6 %>% read_html() %>% html_nodes(".title") %>% html_attr('href')

vhs_url1_sublinks <- vhs_url1_sublinks[!is.na(vhs_url1_sublinks)]
vhs_url2_sublinks <- vhs_url2_sublinks[!is.na(vhs_url2_sublinks)]
vhs_url3_sublinks <- vhs_url3_sublinks[!is.na(vhs_url3_sublinks)]
vhs_url5_sublinks <- vhs_url5_sublinks[!is.na(vhs_url5_sublinks)]
vhs_url6_sublinks <- vhs_url6_sublinks[!is.na(vhs_url6_sublinks)]
vhs_sublinks <- c(vhs_url1_sublinks,vhs_url2_sublinks,vhs_url3_sublinks,vhs_url5_sublinks,vhs_url6_sublinks) %>% paste0("https:",.)



gethtmlfiles_vhs <- function(link){
  
  tryCatch({desc <-link %>%read_html()},error = function(e) {
    return(NA)})
  
  return(desc)
  
}
getdesc <- function(link){
  
  tryCatch({desc <-link %>% html_nodes('.pull-left p')  %>% html_text()
  desc <- gsub(pattern="(\\n|\\t|\\r)", desc, replacement="")
  #print(length(desc))
  if(length(desc)>1){
    desc <- paste0(desc[1],desc[2])
  }
  
  
  return(desc)
  },error = function(e) {
    return(NA)})
  
} 
getprice <- function(link){
  
  
  tryCatch({
    price <- link %>%html_nodes("tr:nth-child(5) td+ td")  %>% html_text()
    if(nchar(price)>300){
      price <- link %>%read_html() %>% html_nodes("tr:nth-child(4) td+ td")  %>% html_text()
    }
    
    price <-  gsub(pattern="(\\n|\\t)", price, replacement="") %>% gsub(pattern="EUR", price,fixed=T, replacement="EUR, ") %>% gsub(pattern="EUR,  ", price,fixed=T, replacement="EUR")
    
    return(price[1])
  }, error=function(e){return("Nope")})
  
} 
getstartdates <- function(link){
  
  
  
  tryCatch({dates <- link  %>% html_nodes('.future-event td , th')  %>% html_text()
  
  if(length(dates)<4){
    dates <- link %>% html_nodes("tr:nth-child(3) td+ td")  %>% html_text()
    dates <- map(dates, function(x) return(gsub(pattern="(\\n|\\t)", x, replacement=""))) %>% unlist()
    dates <- str_extract(dates, "\\d{2}.\\d{2}.\\d{4}") %>% as.Date(.,format="%d.%m.%Y")
    dates <- dates[1]
  }
  else{
    
    dates <-matrix(dates,ncol=3,byrow=TRUE)
    
    
    dates[,2] <- map(dates[,2], function(x) return(str_extract(x, "\\d{2}.\\d{2}.\\d{4}") )) %>% unlist()
    dates <- dates[1] %>% as.Date(.,format="%d.%m.%Y")
    
  }
  
  
  return(dates)}, 
  error=function(e){
    
    return(c("Nope"))
  })
  
  
}
getenddates <- function(link){
  
  
  tryCatch({dates <- link  %>% html_nodes('.future-event td , th')  %>% html_text()
  
  if(length(dates)<4){
    dates <- link  %>% html_nodes("tr:nth-child(3) td+ td")  %>% html_text()
    dates <- map(dates, function(x) return(gsub(pattern="(\\n|\\t)", x, replacement=""))) %>% unlist()
    dates <- str_extract(dates, "\\d{2}.\\d{2}.\\d{4}") %>% as.Date(.,format="%d.%m.%Y")
    dates <- dates[1]
    
    occurence <- link  %>% html_nodes(".title-dates-count")  %>% html_text() %>% str_extract(., "\\d") %>% as.numeric()
    
    if (length(occurence)==1){
      dates[length(dates)] <- dates[1]+ocurrence
    }
    dates <- dates[length(dates)]
  }
  else{
    
    dates <-matrix(dates,ncol=3,byrow=TRUE)
    
    
    dates[,2] <- map(dates[,2], function(x) return(str_extract(x, "\\d{2}.\\d{2}.\\d{4}") )) %>% unlist()
    dates <- dates[2:length(dates[,2]),2] %>% as.Date(.,format="%d.%m.%Y")
    dates <- dates[length(dates)]
    
  }
  
  
  return(dates)}, 
  error=function(e){
    
    return(c("Nope"))
  })
  
  
}
getstarttimes <- function(link){
  
  
  out <- tryCatch(
    { dates <- link  %>% html_nodes("tr:nth-child(3) td+ td")  %>% html_text()
    
    dates <- str_extract(dates[1], "\\d{2}:\\d{2}") 
    
    if(is.na(dates)){
      out <- NA
    }
    else{
      out <- dates %>% paste0(., ":00") %>% times()
    }
    
    
    
    }, error=function(e){return(NA)})
  return(out)
  
} 
getendtimes <- function(link){
  
  out <- tryCatch(
    { dates <- link  %>% html_nodes("tr:nth-child(3) td+ td")  %>% html_text()
    
    dates <- str_extract(dates[1], "\\d{2}:\\d{2} Uhr") %>% str_extract(., "\\d{2}:\\d{2}") 
    
    if(is.na(dates)){
      out <- NA
    }
    else{
      out <- dates %>% paste0(., ":00") %>% times()
    }
    
    
    
    }, error=function(e){return(NA)})
  return(out)
  
} 
getadress <- function(link){
  
  #print(link)
  tryCatch(
    {
      desc <-link  %>% html_nodes('.show-venue-on-map p , .show-venue-on-map span')  %>% html_text()
      
      desc <- str_match_all(desc, "( [A-Z].{1,20}str. [0-9])|[0-9]{5}|( [A-Z].{1,20}Str. [0-9])|( [A-Z].{1,20}StraÃŸe [0-9])|( [A-Z].{1,20}straÃŸe [0-9])") %>% unlist()
      desc <- desc[!is.na(desc)]
      desc <- gsub(desc, pattern=(" {2,30}"), replacement="")
      
      if(length(desc)<2){
        return(c("Nope","Nope"))
      }
      else if(length(desc) >2){
        if(desc[1]==" Kist, Obere Dorfstr. 1"){
          desc <- desc[2:3]
          
        }
        if(desc[1]==" Wendt , Jahnstr. 1"){
          desc <- desc[2:3]
          
        }
        if(desc[1]==" Bayern, Theaterstr. 2"){
          desc <- desc[2:3]
          
        }
        if(nchar(desc)<1){
          desc <-c("Nope","Nope") 
          
        }
        
        if(desc[1]==desc[2]){
          desc  <- desc[2:3]
          
        }
        else if(desc[2]==desc[3]){
          desc <- c(desc[1],desc[3])
          
        }
        else{
          desc <- desc[1:2]
        }
        return(desc)
      }
      else{
        return(c("Nope","Nope"))
      }},
    error = function(e) {
      return(c("Nope","Nope"))
    })
  return(c("Nope","Nope"))
} 




html_files_vhs <- map(vhs_sublinks, gethtmlfiles_vhs)

vhs1 <- vhs_url1 %>% read_html() %>% html_nodes(xpath='//*[@id="resultReceiver"]/div[2]/div[1]/table') %>% html_table()
vhs2 <- vhs_url2 %>% read_html() %>% html_nodes(xpath='//*[@id="resultReceiver"]/div[2]/div[1]/table') %>% html_table()
vhs3 <- vhs_url3 %>% read_html() %>% html_nodes(xpath='//*[@id="resultReceiver"]/div[2]/div[1]/table') %>% html_table()
vhs5 <- vhs_url5 %>% read_html() %>% html_nodes(xpath='//*[@id="resultReceiver"]/div[2]/div[1]/table') %>% html_table()
vhs6 <- vhs_url6 %>% read_html() %>% html_nodes(xpath='//*[@id="resultReceiver"]/div[2]/div[1]/table') %>% html_table()

vhs1 <- vhs1[[1]]
vhs2 <- vhs2[[1]]
vhs3 <- vhs3[[1]]

vhs5 <- vhs5[[1]]
vhs6 <- vhs6[[1]]

vhs <- rbind(vhs1,vhs2,vhs3,vhs5,vhs6)


name <- map(vhs[,2], function(x) return(gsub(pattern="(\\n|\\t)", x, replacement=" ")))
name <- map(name, function(x) return(gsub(pattern="(Mo|Di|Mi|Do|Fr|Sa|So)\\..*$", x, replacement="")))
name <- map(name, function(x) return(gsub(pattern="  +", x, replacement=" - ")))
name <- map(name, function(x) return(gsub(pattern=" - $", x, replacement=""))) %>% unlist()
vhs$`Was?` <- name


adresses_vhs <- map(html_files_vhs, getadress)%>% do.call(c, .)%>%matrix(.,ncol=2,byrow=T) %>% as.data.frame()

descriptions_vhs <- map(html_files_vhs, getdesc) %>% do.call(c, .)

startdates_vhs <- map(html_files_vhs, getstartdates)%>% do.call(c, .)

enddates_vhs <- map(html_files_vhs, getenddates)%>% do.call(c, .)


price_vhs <- map(html_files_vhs, getprice) %>% do.call(c, .)%>% matrix(.,ncol=1)%>% as.data.frame()

starttimes_vhs <- map(html_files_vhs, getstarttimes) %>% do.call(c, .)

endtimes_vhs <- map(html_files_vhs, getendtimes) %>% do.call(c, .)





vhs  <- data.frame("title" = vhs$`Was?`,
                   "url" =vhs_sublinks ,
                   "description" = descriptions_vhs,
                   "lng" = 49.78884,
                   "lat" = 9.93252,
                   "city" = vhs$`Wo?`,
                   "street" = adresses_vhs$V1,
                   "zip" = adresses_vhs$V2,
                   "date_start" = startdates_vhs, 
                   "date_end" = enddates_vhs, 
                   "time_start" = starttimes_vhs, 
                   "time_end" = endtimes_vhs, 
                   "price" = price_vhs$V1,
                   "organizer" = "VHS")


vhs$price[vhs$price=="Nope"]<-NA
vhs$street[vhs$street=="Nope"]<-NA
vhs$city[vhs$city=="Nope"]<-NA
vhs$description[vhs$description=="Nope"]<-NA
vhs$zip[vhs$zip=="Nope"]<-NA


#### Mariannhill ####
url_mariannhill <-"http://www.kirchenmusik-mariannhill.de/programm.html"

url_mariannhill %>%
  read_html() %>%
  html_nodes('.texteng td+ td , .texteng .texteng , .texteng~ tr+ tr .texteng') %>%
  html_text() %>%
  str_trim() %>%
  as.list() -> Mariannhill


Mariannhill <- as.data.frame(str_split_fixed(str_subset(Mariannhill,pattern=""), ",", 3))
Mariannhill$V1 <- NULL
Mariannhill <- subset(Mariannhill, Mariannhill$V2!="")

Mariannhill <- cbind(Mariannhill, as.list(as.data.frame(gsub("(Uhr).*","\\1",Mariannhill$V3))))
colnames(Mariannhill)[1] <- "date_start"
colnames(Mariannhill)[2] <- "title"
colnames(Mariannhill)[3] <- "time_start"

Mariannhill %>%
  mutate_all(as.character) -> Mariannhill

replaceTime_MH <-list(" " = "", "Uhr" = "")
Mariannhill$time_start %>%
  gsubfn(paste(names(replaceTime_MH),collapse="|"),replaceTime_MH,.) %>%
  paste0(., ":00:00") %>%
  times(.)-> Mariannhill$time_start

Mariannhill$title %>%
  gsub("\\d|(Uhr)|\\s{2}|(\n)","",.) %>%
  gsub("([a-z])([A-Z])", "\\1 \\2", .) %>%
  gsub(" Musik",", Musik", .) %>%
  str_trim("both") -> Mariannhill$title

Mariannhill$title %>%
  gsub("^[A-zäöü0-9 ]*, (.*)", "\\1", .) %>%
  gsub(" Leitung",", Leitung", .) %>%
  gsub("Steinmeyer-Orgel",", Steinmeyer-Orgel", .) -> Mariannhill$description

Mariannhill$title <- gsub("^([A-zäöü0-9 ]*), (.*)", "\\1", Mariannhill$title)

replaceMonth_MH <-list(". Januar " = "jan", ". Februar " = "feb",". März " = "mar",". April " = "apr",
                       ". Mai " = "mai",". Juni " = "jun",". Juli " = "jul",". August " = "aug",
                       ". September " = "sep",". Oktober " = "okt",". November " = "nov",". Dezember " = "dez")
Mariannhill$date_start   %>%
  gsubfn(paste(names(replaceMonth_MH),collapse="|"),replaceMonth_MH,.) %>%
  as.Date(. ,"%d%B%Y") -> Mariannhill$date_start  

Mariannhill$time_end <- times(NA)
Mariannhill$date_end <- Mariannhill$date_start
Mariannhill$organizer <- "Mariannhill"
Mariannhill$lat <- as.numeric("49.79348")
Mariannhill$lng <- as.numeric("9.9545")
Mariannhill$price <- as.character(NA)
Mariannhill$url <- url_mariannhill
Mariannhill$city <- "Würzburg"
Mariannhill$street <- "Mariannhillstraße 1"
Mariannhill$zip <- as.numeric("97074")

Mariannhill <- Mariannhill[,c(2, 11, 4, 9, 8, 12, 13, 14, 1, 6, 3, 5, 10, 7)]

#### Salon 77 Veranstaltungen ####

url_salon77 <- "https://www.salon77.de/index.php?nav=ve&mod=va&bc1=Veranstaltungen"

url_salon77 %>%
  read_html() %>%
  html_nodes('strong , .additional') %>%
  html_text() %>%
  str_trim()  %>%
  unlist() %>%
  as.vector() -> Data_s77V

s77V <- cbind.data.frame(split(Data_s77V, rep(1:3, times=length(Data_s77V)/3)), stringsAsFactors=F)
s77V_split <- as.data.frame(s77V$`1`)
colnames(s77V_split)[1] <- "date"
s77V_split <- separate(data = s77V_split, col = date, into = c("startDate", "endDate"), sep = "\\-")

colnames(s77V)[2] <- "title"
s77V[1] <- NULL

replaceMonth_s77V <-list(".01." = "jan", ".02." = "feb",".03." = "mar",".04." = "apr",
                         ".05." = "mai",".06." = "jun",".07." = "jul",".08." = "aug",
                         ".09." = "sep",".10." = "okt",".11." = "nov",".12." = "dez")

s77V$time_start <- times(NA)
s77V$time_end <- times(NA)

s77V$date_start <- s77V_split$startDate
s77V$date_start %>%
  gsub("([a-z]*)([A-Z]*)(\\s*)","", .) %>%
  gsubfn(paste(names(replaceMonth_s77V),collapse="|"),replaceMonth_s77V, .) %>%
  as.Date(. ,"%d%B%Y") -> s77V$date_start

s77V$date_end <- s77V_split$endDate
s77V$date_end %>%
  gsub("([a-z]*)([A-Z]*)(\\s*)","",.) %>%
  gsubfn(paste(names(replaceMonth_s77V),collapse="|"),replaceMonth_s77V, .)%>%
  as.Date(. ,"%d%B%Y") -> s77V$date_end

url_salon77 %>%
  read_html() %>%
  html_nodes("#innercontent a") %>%
  html_attr("href") %>%
  paste0("https://www.salon77.de", .) %>%
  as.data.frame(.) %>%
  mutate_all(as.character) -> links_s77V

GetDataS77V <- function(url_salon77) {
  url_salon77 %>%
    read_html() %>%
    html_nodes('h2') %>%
    html_text() %>%
    str_trim()  %>%
    unlist() %>%
    as.vector() -> Data_s77V
}

for (i in 1: nrow(links_s77V)) {
  links_s77V[c(i),] <- gsub(" ", "%20", links_s77V[c(i),], fixed = TRUE)
}
s77V$url <- links_s77V$.
s77V$`3` <- NULL
s77V$organizer <- "Salon77"
s77V$lat <- as.numeric("49.79304")
s77V$lng <- as.numeric("9.95808")
s77V$price <- as.character(NA)
s77V$city <- "Würzburg"
s77V$street <- "Richard-Wagner-Str. 60"
s77V$zip <- as.numeric("97074")
s77V$description <- as.character(unlist(map(links_s77V$., GetDataS77V)))

s77V <- s77V[,c(1, 6, 14, 9, 8, 11, 12, 13, 4, 5, 2, 3, 10, 7)]

#### Salon 77 Wochenkurse ####

url_s77WK <- "https://www.salon77.de/index.php?nav=wk&mod=wk&bc1=Wochenkurse"

url_s77WK %>%
  read_html() %>%
  html_nodes("#innercontent a") %>%
  html_attr("href") %>%
  paste0("https://www.salon77.de", .) -> links_s77WK
links_s77WK <- as.data.frame(links_s77WK)
links_s77WK %>%
  mutate_all(as.character) -> links_s77WK

GetDataS77 <- function(link){
  link <- gsub(" ", "%20", link, fixed = TRUE)
  link %>%
    read_html() %>%
    html_nodes("h2+ strong , h1") %>%
    html_text(trim = T)  %>%
    as.list() %>%
    as.data.frame() %>%
    mutate_all(as.character) -> Data_s77WK
}

s77WK <- map(links_s77WK$links_s77WK, GetDataS77)
s77WK %>%
  unlist() %>%
  as.vector() -> s77WK

s77WK <- cbind.data.frame(split(s77WK, rep(1:2, times=length(s77WK)/2)), stringsAsFactors=F)

s77WK_split <- s77WK[,2, drop=FALSE]
colnames(s77WK_split)[1] <- "content"
s77WK_split <- separate(data = s77WK_split, col = content, into = c("day", "time", "loc"), sep = "\\|")
s77WK_split <- separate(data = s77WK_split, col = time, into = c("start", "end"), sep = "\\-")

s77WK_split$start %>%
  gsub("(Uhr)|\\s*|(\n)","",.) %>%
  paste0(., ":00") %>%
  times(.) -> s77WK$time_start

s77WK_split$end %>%
  gsub("(Uhr)|\\s*|(\n)","",.) %>%
  paste0(., ":00") %>%
  times(.) -> s77WK$time_end

s77WK$date_start <- as.Date(NA ,"%d%B%Y")
s77WK$date_end <- as.Date(NA ,"%d%B%Y")

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

for (i in 1: nrow(s77WK_split)) {
  s77WK_split[c(i),] <- gsub(" ", "", s77WK_split[c(i),], fixed = TRUE)
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
  }
}

dates <- map(s77WK_split$day, setDay)
dates <- as.data.frame(dates)
dates <- as.data.frame(t(dates))
s77WK$date_start <- dates$V1
s77WK$date_start %>%
  as.character(.) %>%
  as.Date(.) -> s77WK$date_start

s77WK$`2` <- NULL

for (i in 1: nrow(links_s77WK)) {
  links_s77WK[c(i),] <- gsub(" ", "%20", links_s77WK[c(i),], fixed = TRUE)
}
s77WK$url <- links_s77WK$links_s77WK

colnames(s77WK)[1] <- "title"
s77WK$description <- paste0("Immer ", sapply(s77WK_split$day, tolower), ", Ort: ", s77WK_split$loc)
s77WK$organizer <- "Salon77"
s77WK$lat <- as.numeric("49.79304")
s77WK$lng <- as.numeric("9.95808")

getPrice <- function(linkS77WK) {
  linkS77WK %>%
    read_html() %>%
    html_nodes("#innercontent") %>%
    html_text(trim = T)  %>%
    as.list() %>%
    as.data.frame() %>%
    mutate_all(as.character) %>%
    str_extract(., "(.){6}(Euro)") %>%
    str_trim() -> gebuehr
}

s77WK_prices <- map(links_s77WK$links_s77WK, getPrice)
s77WK_prices <- as.data.frame(s77WK_prices)
s77WK_prices <- t(s77WK_prices)
s77WK_prices <- as.data.frame(s77WK_prices)

s77WK$price <- s77WK_prices$V1
s77WK$city <- "Würzburg"
s77WK$street <- "Richard-Wagner-Str. 60"
s77WK$zip <- as.numeric("97074")

check_Address <- as.data.frame(grepl("(Salon77)|(salon77)", s77WK$description))

for (i in 1:nrow(s77WK)) {
  if (check_Address[i, ] == FALSE) {
    s77WK$street[i] <- NA
    s77WK$zip[i] <- NA
  }
}

s77WK <- s77WK[,c(1, 6, 7, 10, 9, 12, 13, 14, 4, 5, 2, 3, 11, 8)]





###Jonathan####
##### Import Packages ####
library(stringr)
library(rvest)
library(tidyverse)
library(lubridate)
library(rvest)
library(tidyverse)
library(yaml)
library(stringr)
library(tidyverse)
library(rvest)
library(dplyr)
library(stringr)
library(gsubfn)
library(lubridate)
library(chron)
library(knitr)
library(reshape)
library(RSelenium)
library(readxl)
library(devtools)
library(base)
library(chron)
install_version("binman", version = "0.1.0", repos = "https://cran.uni-muenster.de/")
install_version("wdman", version = "0.2.2", repos = "https://cran.uni-muenster.de/")
install_version("RSelenium", version = "1.7.1", repos = "https://cran.uni-muenster.de/")
#####UKW####
# Standard approach


url <- "https://www.ukw.de/patienten-besucher/veranstaltungskalender/"

# Start Browser
rD <- rsDriver()
remDr <- rD[["client"]]
remDr$navigate("http://www.google.com")
remDr$navigate(url)


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

# Get HTML
site <- read_html(remDr$getPageSource()[[1]])

# Get Nodes
site %>%
  html_nodes(".ui-helper-reset") -> node_data

# Get Values
title_selector <- ".title"
teaser_selector <- ".teaser"
from_date_selector <- ".date1"
to_date_selector <- ".date2"
time_selector <- ".time"

node_data %>%
  html_node(title_selector) %>%
  html_text() -> title
node_data %>%
  html_node(teaser_selector) %>%
  html_text() -> teaser
node_data %>%
  html_node(from_date_selector) %>%
  html_text() -> from_date
node_data %>%
  html_node(to_date_selector) %>%
  html_text() -> to_date
node_data %>%
  html_node(time_selector) %>%
  html_text() -> time
node_data %>%
  html_node(".links") %>%
  html_text() -> infos
test


# Merge
df <- data.frame(Title = title,
                 Teaser = teaser,
                 From_date = from_date, 
                 To_date = to_date,
                 Time = time,
                 Infos = infos)

# Clean up
df %>%
  filter(!is.na(Title)) -> df_clean


# Shut down selenium
remDr$close()
rm(rD)
gc()

df_clean$Time = lapply(df_clean$Time, gsub, pattern="\t", replacement="", fixed= TRUE)
df_clean$Time = lapply(df_clean$Time, gsub, pattern="\n", replacement="", fixed= TRUE)
df_clean$Teaser = lapply(df_clean$Teaser, gsub, pattern="\t", replacement="", fixed= TRUE)
df_clean$Teaser = lapply(df_clean$Teaser, gsub, pattern="\n", replacement="", fixed= TRUE)
df_clean$Infos = lapply(df_clean$Infos, gsub, pattern="\t", replacement="", fixed= TRUE)
df_clean$Infos = lapply(df_clean$Infos, gsub, pattern="\n", replacement="", fixed= TRUE)
jahr=str_extract(df_clean$To_date, '20[1-9][0-9]')
df_clean$From_date = paste0(df_clean$From_date, jahr)
df_clean$From_date = lapply(df_clean$From_date, gsub, pattern="NA", replacement="", fixed= TRUE)
df_clean$To_date = lapply(df_clean$To_date, gsub, pattern="- ", replacement="", fixed= TRUE)
df_clean$EndTime=str_extract(df_clean$Time, '- [0-2][0-9]:[0-5][0-9]')
df_clean$EndTime = lapply(df_clean$EndTime, gsub, pattern="- ", replacement="", fixed= TRUE)
df_clean$Time = lapply(df_clean$Time, gsub, pattern='- [0-2][0-9]:[0-5][0-9]', replacement="")
df_clean$Ort = str_extract(df_clean$Infos, "Veranstaltungsort:.*Würzburg")
df_clean$Ort = lapply(df_clean$Ort, gsub, pattern="Veranstaltungsort:", replacement="")
df_clean$Comment = str_extract(df_clean$Teaser, "Kurszeiten:.*Uhr")
df_clean$Comment = lapply(df_clean$Comment, gsub, pattern="Kurszeiten:", replacement="")
df_clean$Time = lapply(df_clean$Time, gsub, pattern="Uhr", replacement="")

df_clean %>%
  filter(!is.na(To_date)) -> df_clean3
df_clean %>%
  filter(is.na(To_date)) -> df_clean4
df_clean4$To_date = df_clean4$From_date
df_clean = rbind(df_clean3,df_clean4)
df_clean$Time[df_clean$Time==""] = NA
'
df_clean %>%
filter(!is.na(Time)) -> df_clean3
df_clean %>%
filter(is.na(Time)) -> df_clean4
df_clean3$Startzeit = as.POSIXct(paste(df_clean3$From_date, df_clean3$Time), format="%d.%m.%Y %H:%M")
df_clean4$Startzeit = as.POSIXct(paste(df_clean4$From_date), format="%d.%m.%Y")
df_clean = rbind(df_clean3,df_clean4)
df_clean %>%
filter(!is.na(EndTime)) -> df_clean3
df_clean %>%
filter(is.na(EndTime)) -> df_clean4
df_clean3$Endzeit = as.POSIXct(paste(df_clean3$To_date, df_clean3$EndTime), format="%d.%m.%Y %H:%M")
df_clean4$Endzeit = as.POSIXct(paste(df_clean4$To_date), format="%d.%m.%Y")
df_clean = rbind(df_clean3,df_clean4)'
df_clean$date_start = as.Date(unlist(df_clean$From_date), format="%d.%m.%Y")
df_clean$date_end = as.Date(unlist(df_clean$To_date), format="%d.%m.%Y")
df_clean$time_start = times(paste0(df_clean$Time, ":00"))
df_clean$time_end = times(paste0(df_clean$EndTime, ":00"))
df_clean
df_clean$zip=str_extract(df_clean$Ort, '97[0-9][0-9][0-9]')
df_clean$city = str_extract(df_clean$Ort, '97[0-9][0-9][0-9].*')
df_clean$city = lapply(df_clean$city, gsub, pattern='0', replacement="", fixed= TRUE)
df_clean$city = lapply(df_clean$city, gsub, pattern='1', replacement="", fixed= TRUE)
df_clean$city = lapply(df_clean$city, gsub, pattern='2', replacement="", fixed= TRUE)
df_clean$city = lapply(df_clean$city, gsub, pattern='3', replacement="", fixed= TRUE)
df_clean$city = lapply(df_clean$city, gsub, pattern='4', replacement="", fixed= TRUE)
df_clean$city = lapply(df_clean$city, gsub, pattern='5', replacement="", fixed= TRUE)
df_clean$city = lapply(df_clean$city, gsub, pattern='6', replacement="", fixed= TRUE)
df_clean$city = lapply(df_clean$city, gsub, pattern='7', replacement="", fixed= TRUE)
df_clean$city = lapply(df_clean$city, gsub, pattern='8', replacement="", fixed= TRUE)
df_clean$city = lapply(df_clean$city, gsub, pattern='9', replacement="", fixed= TRUE)
df_clean$city = lapply(df_clean$city, gsub, pattern=' ', replacement="", fixed= TRUE)
df_clean$street = df_clean$Ort
df_clean$price = NA
df_clean$url = NA


title = df_clean$Title
url = df_clean$url
description = df_clean$Teaser
description=unlist(description)
city = df_clean$city
city = unlist(city)
street = df_clean$street
street = unlist(street)
zip = df_clean$zip
date_start = df_clean$date_start
date_end = df_clean$date_end
time_start = df_clean$time_start
time_end = df_clean$time_end
price = df_clean$price

ukw  <- data.frame("title" = title, 
                   "url" = url, 
                   "description" = description,
                   "lat" = 49.80328,
                   "lon" = 9.95565,
                   "city" = city,
                   "street" = street,
                   "zip" = zip,
                   "date_start" = date_start,
                   "date_end" = date_end,
                   "time_start" = time_start,
                   "time_end" = time_end,
                   "price" = price,
                   "organizer" = "Uniklinik Würzburg")



####write_csv####

df=rbind(Naturveranstaltungen, LaVivaDanceclub, Jugendbildungszentrumsveranstaltungen, vhs, frankenwarte, s77WK, s77V, Mariannhill, ukw)
replaceUmlaute <-list("ä" = "ae", "ö" = "oe","ü" = "ue",
                      "Ä" = "Ae", "Ö" = "Oe", "Ü" = "Ue", "ß" = "ss")
df$title <- gsubfn(paste(names(replaceUmlaute),collapse="|"),replaceUmlaute, as.character(df$title))
df$description <- gsubfn(paste(names(replaceUmlaute),collapse="|"),replaceUmlaute, as.character(df$description))
df$city <- gsubfn(paste(names(replaceUmlaute),collapse="|"),replaceUmlaute, as.character(df$city))
df$street <- gsubfn(paste(names(replaceUmlaute),collapse="|"),replaceUmlaute, as.character(df$street))
df$price <- gsubfn(paste(names(replaceUmlaute),collapse="|"),replaceUmlaute, as.character(df$price))
df$organizer <- gsubfn(paste(names(replaceUmlaute),collapse="|"),replaceUmlaute, as.character(df$organizer))

write.csv(df, file ="C:/Users/Laurell/Dropbox/Stockdata/abgabe.csv", sep=";")
