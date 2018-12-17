source("CrawlAllSitesLukas.R")
library(openxlsx)

GesamtEvents <- data.frame(matrix(ncol = 14, nrow = 0))
                           
EventsLukas <- EventCrawlerLukas()
GesamtEvents <- rbind(EventsLukas,GesamtEvents)

write.xlsx(GesamtEvents, file = "EventsLukas.xlsx", asTable = TRUE)
