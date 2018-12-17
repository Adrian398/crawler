#main function
setwd("C:/Users/Alex/Documents/R/dateien")

source("AOK_crawler.R")
source("hochschule_fuer_musik_crawler.R")
source("profamilia_crawler.R")
source("sternwarte_wuerzburg_crawler.R")
source("LGS_crawler.R")
source("ClubKatze_crawler.R")
source("BayHof_crawler.R")
source("Tanzspeicher_crawler.R")
source("MineralogischesMuseum_crawler.R")
source("StAlfons_crawler.R")
source("StPaul_crawler.R")
source("GalerieKlose_crawler.R")
source("theater_crawler.R")
source("zauberberg_crawler.R")
source("immerhin_crawler.R")


# Um Error zu ignorieren, leere dataframes erstellen
df1 <- data.frame(title=NA, url=NA, description=NA, lng=NA, lat=NA, city=NA, street=NA, zip=NA, date_start=NA, date_end=NA, time_start=NA, time_end=NA, price=NA, organizer=NA)
df2 <- df1
df3 <- df1
df4 <- df1
df5 <- df1
df6 <- df1
df7 <- df1
df8 <- df1
df9 <- df1
df10 <- df1
df11 <- df1
df12 <- df1
df13 <- df1
df14 <- df1


df1 <- immerhin_crawler()
df2 <- zauberberg_crawler()
df3 <- profamilia_crawler()
df4 <- sternwarte_wuerzburg_crawler()
df5 <- LGS_crawler()
df6 <- ClubKatze_crawler()
df7 <- BayHof_crawler()
df8 <- Tanzspeicher_crawler()
df9 <- MineralogischesMuseum_crawler()
df10 <- StAlfons_crawler()
df11 <- StPaul_crawler()
df12 <- GalerieKlose_crawler()
df13 <- theater_crawler()
df14 <- AOK_crawler()


events_merged <- bind_rows(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12, df13, df14)


#leere Veranstaltungen entfernen
subset(events_merged, !(is.na(title)))-> events_merged


library(feather)

write_feather(events_merged, "Events_Group_5.feather")
write.csv(events_merged, file = "Events_Group_5.csv",row.names=FALSE)

#test
read_feather("Events_Group_5.feather") -> Events_Group_5
