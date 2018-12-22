url_bock <- 'http://www.bockshorn.de/spielplan.htm'
page_bock <- read_html(url_bock)

# INFO link
info_links <- html_nodes(page_bock,'.aspevpreis') %>%
  html_attr('href')
main_link_bock <- 'http://www.bockshorn.de/'
info_links <- paste(main_link_bock,info_links) %>%
  gsub(" ","",.)


getBockshornEventDetails = function(page)
{
  page %>%
    read_html() -> page

  page %>%
    html_node("h1") %>%
    html_text() -> title
  
  page %>%
    html_nodes("tr td tr td tr td") %>%
    html_text() -> infos
  
  page %>%
    html_nodes("p span") %>%
    html_text() -> description
  
  if(infos[3] == "AUSVERKAUFT")
  {
  data.frame(title = title,
             start_date=infos[1],
             location = infos[4],
             preis = infos[3],
             start_time = infos[2],
             description = ifelse(length(description)>0,description," ")
             )
  }
  else
  {
  data.frame(title = title,
               start_date=infos[1],
               location = infos[6],
               preis = infos[3],
               start_time = infos[2],
             description = ifelse(length(description)>0,description," ")
    )
  }
}



bockshorn = map_df(info_links, getBockshornEventDetails)

bockshorn$start_time = unlist(str_extract_all(bockshorn$uhrzeit, "[0-9]?[0-9]\\:[0-9][0-9]"))






table_bock <- html_table(page_bock, fill=TRUE)

reg_date <- "[A-z]{2}[\\,/]\\s[0-9]{2}[\\./][0-9]{2}[\\./]20[1-2]{1}[0-9]{1}"
datum_bock <- str_extract_all(table_bock[[3]]$X1,reg_date) %>%
  as.character() %>%
  gsub("character[\\(/]0[\\)/]",NA,.) 
datum_bock <- datum_bock[!is.na(datum_bock)]

# format the date
for (i in datum_bock) {
  datum_bock=gsub("Mo, ","",datum_bock)
  datum_bock=gsub("Di, ","",datum_bock)
  datum_bock=gsub("Mi, ","",datum_bock)
  datum_bock=gsub("Do, ","",datum_bock)
  datum_bock=gsub("Fr, ","",datum_bock)
  datum_bock=gsub("Sa, ","",datum_bock)
  datum_bock=gsub("So, ","",datum_bock)
}
datum_bock <- as.Date(datum_bock, format = "%d.%m.%Y")

reg_uhrzeit <- "[0-9]{2}[\\:/][0-9]{2}\\sUhr"
uhrzeit_bock <- str_extract_all(table_bock[[3]]$X2,reg_uhrzeit) %>%
  as.character() %>%
  gsub("character[\\(/]0[\\)/]",NA,.) %>%
  gsub(" Uhr","",.)
uhrzeit_bock <- uhrzeit_bock[!is.na(uhrzeit_bock)]
uhrzeit_bock <- times(paste0(uhrzeit_bock, ":00"))

time_end_bock <- rep(NA,length(uhrzeit_bock)) %>%
  as.numeric() %>%
  times()

reg_ort <- "\\bOrt[\\:/]\\s[A-z]+"
ort_bock <- str_extract_all(table_bock[[3]]$X1,reg_ort) %>%
  as.character() %>%
  gsub("character[\\(/]0[\\)/]",NA,.)
ort_bock <- ort_bock[!is.na(ort_bock)]
ort_bock <- gsub("Ort: ","",ort_bock)

veranstalter_bock <- rep("Bockshorn",length(uhrzeit_bock))

reg_preis <- '[0-9]{1,2}[\\,/][0-9]{1,2}.+'
preis_bock <- str_extract_all(table_bock[[3]]$X4,reg_preis) %>%
  as.character() %>%
  gsub("character[\\(/]0[\\)/]",NA,.)
preis_bock <- preis_bock[!is.na(preis_bock)]
preis_bock

kurz_infor_bock <- rep(NA,length(uhrzeit_bock))
city_bock <- rep("Wuerzburg",length(uhrzeit_bock))
street_bock <- rep("Oskar-Laredo-Platz 1 (vormals Veitshöchheimer Straße 5a )",length(uhrzeit_bock))
zip_bock <- rep(97080,length(uhrzeit_bock))
lat_bock <- rep(49.8158268,length(uhrzeit_bock))
lng_bock <- rep(9.9102608,length(uhrzeit_bock))

veranstaltungen_bock <- data.frame(date_start=datum_bock,
                                   date_end=datum_bock,
                                   time_start=uhrzeit_bock,
                                   time_end=time_end_bock,
                                   title=title_list_bock,
                                   description=kurz_infor_bock,
                                   price=preis_bock,
                                   city=city_bock,
                                   street=street_bock,
                                   zip=zip_bock,
                                   organizer=veranstalter_bock,
                                   lat=lat_bock,
                                   lng=lng_bock,
                                   url=info_links)

