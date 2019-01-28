write_dataframes_to_database <- function(crawled_df, meta_df, conn) {
  
  crawled_df %>% map_if(is.factor, as.character) %>% as_data_frame -> crawled_df
  
  
  #convert times 
  temp = chron(times = c(c(sapply(crawled_df["time_start"], as.character)),"00:00:59"))
  crawled_df["time_start"] = temp[-length(temp)]
  temp = chron(times = c(c(sapply(crawled_df["time_end"], as.character)),"00:00:59"))
  crawled_df["time_end"] = temp[-length(temp)]
  
  #add standard row

  
  ##columns to check:
  #title
  #link
  #date
  #time
  #price
  #advanced_price
  #street
  
  
  ##to do:
  #vergleich auf time basis verbessern
  # Probleme bei date in Datenbank schreiben 
  #Problem bei Zeit in Datenbank schreiben
  
  #improvement: order db via crawlerid then some binary seach
  
###all you can write to dataframe:
  ##crawled_df
    #title req
    #description req
    #link req 
    #date_start  req 
    #date_end req 
    #time_start req 00:00:00 if Na
    #time_end req as 00:00:00 if Na
    #price req
    #idgroup not req
    #advance_sale_price not req
    #event_site not req
    #image
    #category not req
    #house_number req
    #street req
    #city req
    #zip req 
    #lng req
    #lat req 
    #booking_office not req
  
  ##foreign keys
    #idevent_location
    #idorganizer
    #idcrawler
  
  
  ##meta dataframe
    #organizer req
    
    #url_crawler req
    
   

  
  
  ### write to crawler table
    ##get current urls of crawlers
  db_url_crawlers <- as.data.frame(tbl(conn, "crawler") %>%
                                     select(url_crawler))
  ##fix encoding error
  for (i in colnames(db_url_crawlers)){
    if(is.character(db_url_crawlers[[i]])){
      db_url_crawlers[[i]] = iconv(db_url_crawlers[[i]], "latin1", "UTF-8")
    }
  }
  
    ## if url_crawler doesnt exist write url_crawler in database
  if(!any(db_url_crawlers==as.character(meta_df["url_crawler"][1,1]))){
    dbWriteTable(conn, value = meta_df["url_crawler"], name = "crawler", append = TRUE, row.names=F )
    print(paste("New Crawler url added:", as.character(meta_df["url_crawler"][1,1])))
  }
  
  ### write to organizer table
  db_organizer_names <- as.data.frame(tbl(conn, "organizer") %>%
                                        select(organizer))
  ##fix encoding error
  for (i in colnames(db_organizer_names)){
    if(is.character(db_organizer_names[[i]])){
      db_organizer_names[[i]] = iconv(db_organizer_names[[i]], "latin1", "UTF-8")
    }
  }
  
    ## if organizer doesnt exist write him in database
  if(!any(db_organizer_names==as.character(meta_df["organizer"][1,1]))){
    dbWriteTable(conn, value = meta_df["organizer"], name = "organizer", append = TRUE, row.names=F )
    print(paste("New Organizer added:", as.character(meta_df["organizer"][1,1])))
  }
  

  ##write event_location to event table
  
  #if("booking_office" %in% colnames(meta_df)){
   # df_event_location = meta_df[c("house_number",
    #                            "street",
     #                           "city",
      #                          "zip",
       #                         "lng",
        #                        "lat",
         #                       "booking_office")]
  # }else{
  #   df_event_location = meta_df[c("house_number",
  #                                 "street",
  #                                 "city",
  #                                 "zip",
  #                                 "lng",
  #                                 "lat")]
  # }
  # db_event_location_lng_lat <- as.data.frame(tbl(conn, "event_location") %>%
  #                                              select(lng, lat))
  
    ## if event_location doesnt exist write it in database
  # if(!(any(db_event_location_lng_lat["lng"]==as.character(meta_df["lng"][1,1])) & any(db_event_location_lng_lat["lat"]==as.character(meta_df["lat"][1,1])) )){
  #   dbWriteTable(conn, value = df_event_location, name = "event_location", append = TRUE, row.names=F )
  #   print(paste("New Event_Location added:", as.character(paste(df_event_location["lng"],df_event_location["lng"]))))
  # }
  
  ### write to event table
    ## add ids for crawler, organizer
      # add crawler foreignkey
  all_id_crawler = as.data.frame(tbl(conn, "crawler"))
  all_id_crawler[[2]] = iconv(all_id_crawler[[2]], "latin1", "UTF-8")
  idcrawler = filter(all_id_crawler, url_crawler == as.character(meta_df["url_crawler"][1,1]))%>%
    select(idcrawler)
  idcrawler = rep.int(as.integer(as.data.frame(idcrawler)), nrow(crawled_df))
  crawled_df = cbind(crawled_df,idcrawler)
  
      # add organizer foreignkey
  all_id_organizer = as.data.frame(tbl(conn, "organizer"))
  all_id_organizer[[2]] = iconv(all_id_organizer[[2]], "latin1", "UTF-8")
  idorganizer = filter(all_id_organizer, organizer == as.character(meta_df["organizer"][1,1]))%>%
    select(idorganizer)
  idorganizer = rep.int(as.integer(as.data.frame(idorganizer)), nrow(crawled_df))
  crawled_df = cbind(crawled_df,idorganizer)
  
      # add event_location foreignkey
  # idevent_location = tbl(conn, "event_location") %>%
  #   filter(lng == as.character(meta_df["lng"][1,1]) & lat == as.character(meta_df["lat"][1,1]))%>%
  #   select(idevent_location)
  # 
  # idevent_location = rep.int(as.integer(as.data.frame(idevent_location)), nrow(crawled_df))
  # crawled_df = cbind(crawled_df,idevent_location)
  # 
    ## if event is not in database write it in database
      #get all events of the current crawler
  db_events_current_crawler = as.data.frame(tbl(conn, "event") %>%
                                             filter(idcrawler == idcrawler[1]))
  
  ##fix encoding error
  for (i in colnames(db_events_current_crawler)){
    if(is.character(db_events_current_crawler[[i]])){
      db_events_current_crawler[[i]] = iconv(db_events_current_crawler[[i]], "latin1", "UTF-8")
    }
  }
  
  # ##add row to fix
  # idevent = c(10000000)
  # atemp = cbind(crawled_df[nrow(crawled_df),],idevent)
  
  
  #db_events_current_crawler = rbind(db_events_current_crawler, atemp)
      #compare all the events of current crawler in the database with the new events of the current crawler
      #parse date and time
  if(nrow(db_events_current_crawler) != 0){
  
    #check if date or posix
    db_events_current_crawler["date_start"] = as.Date(c(sapply(db_events_current_crawler["date_start"], as.character)), tz =  "UTC", format = "%Y-%m-%d")
    db_events_current_crawler["date_end"] = as.Date(c(sapply(db_events_current_crawler["date_end"], as.character)), tz =  "UTC", format = "%Y-%m-%d")
    
    #print(db_events_current_crawler["time_end"])
    
    #as.character()
    
    
    # convert_to_times <- function(x){
    #   if(!is.na(x))
    #     return(as.character(x))
    #   
    #   return(x)
    # }
    
    #convert times 
    temp = chron(times = c(c(sapply(db_events_current_crawler["time_start"], as.character)),"00:00:59"))
    db_events_current_crawler["time_start"] = temp[-length(temp)]
    temp = chron(times = c(c(sapply(db_events_current_crawler["time_end"], as.character)),"00:00:59"))
    db_events_current_crawler["time_end"] = temp[-length(temp)]
    
    #print(db_events_current_crawler["time_end"])
    
    ### join differntly if price or advanced price are excluded
    if('price' %in% colnames(crawled_df) & 'advanced_price' %in% colnames(crawled_df)){
      new_events= anti_join(crawled_df, db_events_current_crawler,by=c("title", "link","date_start", "date_end", "time_start", "time_end", "price", "advanced_price", "street"))
    }else if('price' %in% colnames(crawled_df)){
      new_events= anti_join(crawled_df, db_events_current_crawler,by=c("title", "link","date_start", "date_end", "time_start", "time_end", "price", "street"))
    } else if('advanced_price' %in% colnames(crawled_df)){
      new_events= anti_join(crawled_df, db_events_current_crawler,by=c("title", "link","date_start", "date_end", "time_start", "time_end", "advanced_price", "street"))
    }else
      new_events= anti_join(crawled_df, db_events_current_crawler,by=c("title", "link","date_start", "date_end", "time_start", "time_end", "street"))
    
    
    
    #temp = c(chron(times = c(sapply(new_events["time_start"], as.character))),"00:00:99")
    #new_events["time_start"] = temp[-length(temp)]
    #temp = c(chron(times = c(sapply(new_events["time_end"], as.character))),"00:00:99")
    #new_events["time_end"] = temp[-length(temp)]
    
    
    
    if (nrow(new_events) != 0) {
      #fix 00:00:00 mysql error
      if(all(is.na(new_events["time_start"]))){
       new_events$time_start <- NULL
      }
      if(all(is.na(new_events["time_end"]))){
        new_events$time_end <- NULL
      }
      dbWriteTable(conn, value = new_events, name = "event", append = TRUE, row.names=F)
      write.xml(new_events, file="new_events.xml")
      print(paste(nrow(new_events),"new events added to database!"))
    }else{
      print("No new events found!")
    }
    ### join differntly if price or advanced price are excluded
    if('price' %in% colnames(crawled_df) & 'advanced_price' %in% colnames(crawled_df)){
      events_to_delete= anti_join(db_events_current_crawler, crawled_df,by=c("title", "link","date_start", "date_end", "time_start", "time_end", "price", "advanced_price", "street"))
    }else if('price' %in% colnames(crawled_df)){
      events_to_delete= anti_join(db_events_current_crawler, crawled_df,by=c("title", "link","date_start", "date_end", "time_start", "time_end", "price", "street"))
    } else if('advanced_price' %in% colnames(crawled_df)){
      events_to_delete= anti_join(db_events_current_crawler, crawled_df,by=c("title", "link","date_start", "date_end", "time_start", "time_end", "advanced_price", "street"))
    }else
      events_to_delete= anti_join(db_events_current_crawler, crawled_df,by=c("title", "link","date_start", "date_end", "time_start", "time_end", "street"))
    
    
    # temp = c(chron(times = c(sapply(events_to_delete["time_start"], as.character))),"00:00:99")
    # events_to_delete["time_start"] = temp[-length(temp)]
    # temp = c(chron(times = c(sapply(events_to_delete["time_end"], as.character))),"00:00:99")
    # events_to_delete["time_end"] = temp[-length(temp)]
    
    
    if (nrow(events_to_delete) != 0) {
        #create sql delete string
      sql_charaacter_ids_comma_separated_for_query = paste(as.character(events_to_delete$idevent), collapse=",")
      sql_delete_query = paste0('DELETE FROM event WHERE idevent IN (',sql_charaacter_ids_comma_separated_for_query,')')
      sql_delete_query
        #delete ids from database
      dbSendQuery(conn, sql_delete_query)
      #new_events["description"] = iconv(new_events["description"], "enc2native", "UTF-8", sub=NA)
      write.xml(events_to_delete, file="deleted_events.xml")
      print(paste(nrow(events_to_delete)," events droped from database!"))
    }else{
      print("No events deleted!")
    }
  }else{
      #fix 00:00:00 mysql error
      if(all(is.na(crawled_df["time_start"]))){
        crawled_df$time_start <- NULL
      }
      if(all(is.na(crawled_df["time_end"]))){
        crawled_df$time_end <- NULL
      }
    dbWriteTable(conn, value = crawled_df, name = "event", append = TRUE, row.names=F)
    write_df_to_xml(crawled_df)
    #write.xml(crawled_df, file="new_events.xml")
    print(paste(nrow(crawled_df),"new events added to database!"))
  }
}  

