EventCrawlerLukas <- function(){
source("FHWSNeu.R")
source("Gnadenkirche Wuerzburg.R")
source("PGSanderauRSelenium.R")
source("VersuchEinzelseiteBBK_Unterfranken.R")
library(chron)

EventsLukas <- data.frame(matrix(ncol = 10, nrow = 0))

Eventtabelle <- FHWS()
EventsLukas <- rbind(EventsLukas,Eventtabelle)

Eventtabelle <- Gnadenkirche()
EventsLukas <- rbind(EventsLukas,Eventtabelle)

Eventtabelle <- BBKUnterfranken()
EventsLukas <- rbind(EventsLukas,Eventtabelle)

Eventtabelle <- pgsanderau()
EventsLukas <- rbind(EventsLukas,Eventtabelle)

colnames(EventsLukas) <- c("title", "description", "date_start","date_end", "time_start", "time_end","url","price","organizer","city","street","zip","lng","lat")

EventsLukas$time_end <- times(EventsLukas$time_end)
EventsLukas$time_start <- times(EventsLukas$time_start)


return(EventsLukas)
}