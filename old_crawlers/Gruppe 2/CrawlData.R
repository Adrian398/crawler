library(feather)
library(naniar)

# Crawl all data
source("project_KFilakovska.R")
test <-  c("title", "url", "description", "lng", "lat", "city", "street", "zip", "date_start", "date_end", "time_start", "time_end", "price","organizer") %in% names(v_all)
#if_else(sum(test) == 14, KF <- v_all, KF <- data.frame(matrix(NA, ncol = 14, nrow=0))) # (leerer df mit 14 spalten), ...))
if(sum(test) == 14) { KF <- v_all } else { KF <- data.frame(matrix(NA, ncol = 14, nrow=0)) }

source("project_KTylak.R")
test <- c("title", "url", "description", "lng", "lat", "city", "street", "zip", "date_start", "date_end", "time_start", "time_end","price", "organizer") %in% names(events_all)
# if_else(sum(test) == 14, KT <- events_all, KT <- data.frame(matrix(NA, ncol = 14, nrow=0)))
if(sum(test) == 14) { KT <- events_all } else { KT <- data.frame(matrix(NA, ncol = 14, nrow=0)) }

source("project_EGallo.R")
test <- c("title", "url", "description", "lng", "lat", "city", "street", "zip", "date_start", "date_end", "time_start", "time_end","price", "organizer") %in% names(df_complete)
#if_else(sum(test) == 14, EG <- df_complete, EG <- data.frame(data.frame(matrix(NA, ncol = 14, nrow=0))))
if(sum(test) == 14) { EG <- df_complete } else { EG <- data.frame(matrix(NA, ncol = 14, nrow=0)) }

source("project_ALipps.R")
test <- c("title", "url", "description", "lng", "lat", "city", "street", "zip", "date_start", "date_end", "time_start", "time_end", "price", "organizer") %in% names(veranstaltungengesamt)
#if_else(sum(test) == 14, AL <- veranstaltungengesamt, AL <- data.frame(data.frame(matrix(NA, ncol = 14, nrow=0))))
if(sum(test) == 14) { AL <- veranstaltungengesamt } else { AL <- data.frame(matrix(NA, ncol = 14, nrow=0)) }

# Bind all data frames
events_gruppe2 <- rbind(if(exists("KF")) KF, 
	if(exists("KT")) KT,
	if(exists("EG")) EG,
	if(exists("AL") AL)


# Save as feather

path <- "events_gruppe2.feather"
write_feather(events_gruppe2, path)
df <- read_feather(path)


