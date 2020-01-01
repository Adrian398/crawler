library(XML)
#library(hashmap)

write_df_to_xml <- function(df,meta_df, file) {
  
  ID_Event = seq(countit,countit+nrow(df)-1)
  countit <<- countit + nrow(df) -1 

  if("title" %in% names(df)){
    Titel =as.character(df["title"][,1])
  } else {
    Titel = rep("NA",nrow(df))
  }
  if("description" %in% names(df) | all(is.na(as.character(df["description"][,1])))){
    tmp = as.character(df["description"][,1])
    Kurztext = c()
    for (i in (1:length(tmp))){
      if(!is.na(tmp[i])){
        words = strsplit(tmp[i], " ")[[1]]
        if(length(words) < 21){
          text = paste(words, collapse = ' ')
          #text = paste(text, "...")
          Kurztext = append(Kurztext, text)
        }else{
          text = paste(words[1:20], collapse = ' ')
          text = paste(text, "...")
          Kurztext = append(Kurztext, text)
        }
      }else {
        Kurztext = append(Kurztext, "NA")
      }
    }
    #for i in nrow
  } else {
    Kurztext = rep("NA",nrow(df))
  }
  if("description" %in% names(df) | all(is.na(df["description"][,1]))){
    Detailtext = as.character(df["description"][,1])
  } else {
    Detailtext = rep("NA",nrow(df))
  }
  if("link" %in% names(df)){
    Link = as.character(df["link"][,1])
  } else {
    Link = rep("NA",nrow(df))
  }
  if("date_start" %in% names(df)){
    Startdatum = as.character(df["date_start"][,1])
  } else {
    Startdatum = rep("NA",nrow(df))
  }
  if("date_end" %in% names(df)){
    Enddatum = as.character(df["date_end"][,1])
  } else {
    Enddatum = rep("NA",nrow(df))
  }
  if("time_start" %in% names(df)){
    Startzeit = as.character(df["time_start"][,1])
  } else {
    Startzeit = rep("NA",nrow(df))
  }
  if("time_end" %in% names(df)){
    Endzeit = as.character(df["time_end"][,1])
  } else {
    Endzeit = rep("NA",nrow(df))
  }
  if("street" %in% names(df)){
    Strasse = as.character(df["street"][,1])
  } else {
    Strasse = rep("NA",nrow(df))
  }
  if("city" %in% names(df)){
    Stadt = as.character(df["city"][,1])
  } else {
    Stadt = rep("NA",nrow(df))
  }
  if("zip" %in% names(df)){
    PLZ = as.character(df["zip"][,1])
  } else {
    PLZ = rep("NA",nrow(df))
  }
  if("city" %in% names(df)){
    Stadt = as.character(df["city"][,1])
  } else {
    Stadt = rep("NA",nrow(df))
  }
  if("city" %in% names(df)){
    Stadt = as.character(df["city"][,1])
  } else {
    Stadt = rep("NA",nrow(df))
  }
  if("lng" %in% names(df)){
    lng = as.character(df["lng"][,1])
  } else {
    lng = rep("NA",nrow(df))
  }
  if("lat" %in% names(df)){
    lat = as.character(df["lat"][,1])
  } else {
    lat = rep("NA",nrow(df))
  }
  if("advance_price" %in% names(df)){
    Vorverkauf = as.character(df["advance_price"][,1])
  } else {
    Vorverkauf = rep("NA",nrow(df))
  }
  if("price" %in% names(df)){
    Abendkasse = as.character(df["price"][,1])
  } else {
    Abendkasse = rep("NA",nrow(df))
  }
  if("category" %in% names(df)){
    Kategorie = as.character(df["category"][,1])
    ID_Kategorie = c()
    Keys = c("Ausstellung / Messe",
             "Bühne",
             "Fasching",
             "Fest / Fesival",
             "Film",
             "Flohmarkt",
             "Führung",
             "Kinder / Jugend",
             "Kongress / Tagung",
             "Konzert",
             "Vortrag / Seminar",
             "Literatur",
             "Nachtleben",
             "Rund um den Wein",
             "Sport",
             "Und…")
    Values = c(10144,
               10145,
               10580,
               10581,
               12111,
               12408,
               10583,
               10148,
               19718,
               10147,
               10587,
               10582,
               10146,
               10429,
               12112,
               10586
               )
      
    H <- hashmap(Keys, Values)
    for (k in Kategorie) {
      if(is.na(k)){
        ID_Kategorie = c(ID_Kategorie, k)
      }else{
        if(!is.na(H$find(k))){
          ID_Kategorie = c(ID_Kategorie,H$find(k))
        }else{
          ID_Kategorie = c(ID_Kategorie, NA)
        }
      }
      
    }
    
  } else {
    Kategorie = rep("NA",nrow(df))
    ID_Kategorie = rep("NA",nrow(df))
  }
  if("image_url" %in% names(df)){
    Bild_url = as.character(df["image_url"][,1])
  } else {
    Bild_url = rep("NA",nrow(df))
  }
  xml_df = data.frame(ID_Event,Titel,Kurztext,Detailtext, Link,Startdatum,Enddatum,Startzeit,Endzeit,Strasse,Stadt, PLZ, lng,lat,Vorverkauf,Abendkasse, Kategorie, ID_Kategorie,Bild_url)
  
  xml <- xmlTree() 
  # names(xml)
  xml$addTag("crawler", close=FALSE, attrs=c(crawler_url=as.character(meta_df["url_crawler"][1,1]), Veranstalter = as.character(meta_df["organizer"][1,1]), ID_Ort = as.character(meta_df["idlocation"][1,1])))
  for (i in 1:nrow(xml_df)) {
    xml$addTag("event", close=FALSE)
    for (j in names(xml_df)) {
      xml$addTag(j, xml_df[i, j])
    }
    xml$closeTag()
  }
  xml$closeTag()
  
  write(substring(as(saveXML(xml,encoding = "utf8"), "character"),24),file ,append=TRUE)
}


finish_xml <- function(file) {
  start_xml = paste0("<?xml version='1.0'?>\n\n<crawlers date = '",Sys.Date(),"'>")
  end_xml = "</crawlers>"
  xml_document = read_file(file)
  #xml_document = substring(xml_document,nchar(start_xml)+1)
  xml_document = gsub("\n","\n  ",xml_document)
  xml_document = paste0(start_xml, xml_document)
  xml_document = substr(xml_document,1,nchar(xml_document)-2)
  xml_document = paste0(xml_document,end_xml)
  
  write(xml_document,file,append=FALSE)
  
}
