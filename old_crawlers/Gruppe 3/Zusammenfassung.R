library(xlsx)

source("Kulturspeicher.R")
source("IHK.R")
source("Thomaskirche.R")

EventsChristine <- data.frame(matrix(ncol = 14, nrow = 0))

EventtabelleKultur <- Kultur()
EventsChristine <- rbind(EventsChristine,EventtabelleKultur)

EventtabelleIHK <- IHK()
EventtabelleIHK$time_start<- NA
EventtabelleIHK$time_start<- times(EventtabelleIHK$time_start)
EventtabelleIHK$time_end<- NA
EventtabelleIHK$time_end<- times(EventtabelleIHK$time_end)


EventsChristine <- rbind(EventsChristine,EventtabelleIHK)


EventtabelleThomas <- Thomas()
EventsChristine <- rbind(EventsChristine,EventtabelleThomas)


write.xlsx(EventsChristine, file = "C:/Users/envy/Dropbox/2.Semester SS18/Data Science/Projekt/Events.xlsx")
write.csv(EventsChristine, file = "C:/Users/envy/Dropbox/2.Semester SS18/Data Science/Projekt/Events.csv")

