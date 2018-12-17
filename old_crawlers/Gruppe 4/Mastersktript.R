# Masterskript - Gruppe 4

# Import der Skripte

# B-Hof
source("bhof.R")
# Chambinsky
source("chambinsky.R")
# Innovations- und Gründerzentrum
source("Innovations-&Gründerzentrum.R")
# Johanne-Stahl-Zentrum
source("johanna-stahl-zentrum.R")
# Museum am Dom
source("museum_dom.R")
# Residenz
source("residenz.R")
# St. Barbara
source("St. Barbara.R")
# Universität Würzburg
source("Uni Wü.R")
# Waldorf Schule
source("Waldorf Schule.R")
# Wein am Stein
source("weinstein.R")
# Juliusspital
source("Juliusspital.R")

# Zusammenfügen aller relevanten Dataframes
df_total_group4 <- rbind(df_final,df_igz,df_st,resi_df,total_df,wst_df,mus_df,df_jsz,df_juliusspital,df_wd,df_uni)

#Generieren einer CSV
write.csv(df_total_group4, file = "DatensatzGruppe4.csv")