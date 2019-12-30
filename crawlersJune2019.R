library(rvest)
library(XML)
library(RCurl)
library(calendar)
tmp <- "test.xml"
req <- "http://www.vku-kunst.de/?plugin=all-in-one-event-calendar&controller=ai1ec_exporter_controller&action=export_events&no_html=true"
curl::curl_download(req, tmp)

spital = calendar::ic_read(tmp)

names(spital) [3] = "category"

spital %>%
  #filter(category != "SpitÃ¤leFenster") %>%
  filter(!is.na(`DTSTART;VALUE=DATE`))

"http://www.theater-kasperhaus.de/spielplan.htm" %>%
  read_html() %>%
  html_nodes(".TitelSeitenleiste td") %>%
  html_text() %>%
  matrix(ncol=3, byrow = T) %>%
  as.tibble() -> kasperhaus

names(kasperhaus) = c("Datum", "Titel", "Uhrzeit")


#Akademisch-Musikalische Verbindung - inkonsistent in Strings codiert
#Theaterwerkstatt - Grafik des Spielplans

"https://www.bechtolsheimerhof.de/index.php/veranstaltungen" %>%
  read_html() %>%
  html_nodes("#g-mainbar .sprocket-strips-c-title a")%>%
  html_attr("href") -> bechtolsEventSites

bechtolsEventSites = paste0("https://www.bechtolsheimerhof.de", bechtolsEventSites)

bechtolEventList = NULL

for (x in bechtolsEventSites)
{
  print(x)
  if(!grepl("kartenvorverkauf", x)) {

  
  x %>%
    read_html() -> event
  
  event %>%
    html_nodes("h2") %>%
    html_text()  -> datumTitel
  
  event %>%
    html_nodes("#g-mainbar h3") %>%
    html_text() -> titel
  
  if(identical(titel, character(0))){titel = datumTitel}
  
  event %>%
    html_nodes("#p:nth-child(2)") %>%
    html_text() -> Uhrzeit
  
  if(identical(Uhrzeit, character(0))){Uhrzeit = "-"}
    bechtolEventList = bind_rows(bechtolEventList, data.frame(datum = datumTitel, titel = titel))
  }
}
  
#nur Ohne Uhrzeit machbar  


#hfm geht nicht sinnvoll - aber relaunch website angekündigt

# http://www.immerhin-wuerzburg.de/ keine struktur


"https://www.wuerzburg-thomaskirche.de/veranstaltungen" %>%
  read_html() %>%
  html_nodes("div .et_vera_title a") %>%
  html_attr("href") -> events

ids = map_chr(events, function(x){(str_split(x,"ID=") %>% .[[1]] %>% .[2])})

getThomasisusEventDetails = function(id) {
  url = paste0("https://www.evangelische-termine.de/detail-bt?ID=",id)
  url %>%
    read_html() -> event
  
  event %>%
    html_nodes("#et_detail_date") %>%
    html_text() -> date
  
  
  event %>%
    html_nodes("  #et_detail_title") %>%
    html_text() -> title

  data.frame(date = date, title = title)
  
}



ids %>% map_df(getThomasisusEventDetails)


"https://www.heidingsfeld-evangelisch.de/veranstaltungen" %>%
  read_html() %>%
  html_nodes("div .et_vera_title a") %>%
  html_attr("href") -> events

ids = map_chr(events, function(x){(str_split(x,"ID=") %>% .[[1]] %>% .[2])})

getHeidingsEvEventDetails = function(id) {
  url = paste0("http://www.evangelische-termine.de/veranstaltung_im_detail",id,".html")
  url %>%
    read_html() -> event
  
  event %>%
    html_nodes("#et_detail_date") %>%
    html_text() -> date
  
  
  event %>%
    html_nodes("  #et_detail_title") %>%
    html_text() -> title
  
  data.frame(date = date, title = title)
  
}

ids = map_chr(events, function(x){(str_split(x,"ID=") %>% .[[1]] %>% .[2])})

ids %>% map_df(getHeidingsEvEventDetails)


#cvjm keine eventliste, kalender nur mit google übertragbar


"https://www.buergerbraeu-wuerzburg.de/aktuelles-archive/was-kommt.html" %>%
  read_html() %>%
  html_nodes(".col-md-4") -> eventNodes

getBuergerDetail = function(eventNode) {
  eventNode %>%
    html_nodes("span") %>%
    .[1] %>%
    html_text() %>%
    str_split("\\|") -> titel
  
  eventNode %>%
    html_nodes("time") %>%
    .[1] %>%
    html_text() %>%
    stringr::str_trim() %>%
    str_split("\\|") -> time
  
  data.frame(titel = titel[[1]][1], Kategorie = ifelse(identical(titel[[1]][2], character(0)),
                                                       "none",
                                                       titel[[1]][2]),
             date = time[[1]][1] , time = time[[1]][2])
}

map_df(eventNodes, getBuergerDetail)
