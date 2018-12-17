library(xlsx)

source("Lindleinsmuehle_final.R")
source("FabLab_final.R")
source("VHS-final.R")
source("Domschule.R")

EventsVerena <- data.frame(matrix(ncol = 14, nrow = 0))

EventtabelleDom <- Dom()
EventsVerena <- rbind(EventsVerena,EventtabelleDom)

EventtabelleVHS <- VHS()
EventsVerena <- rbind(EventsVerena,EventtabelleVHS)

EventtabelleFab <- Fab()
EventsVerena <- rbind(EventsVerena,EventtabelleFab)

EventtabelleLin <- Lin()
EventsVerena <- rbind(EventsVerena,EventtabelleLin)

write.xlsx(EventsVerena, file = "C:/Users/Verena/Desktop.xlsx")
write.csv(EventsVerena, file = "C:/Users/Verena/Desktop.csv")

