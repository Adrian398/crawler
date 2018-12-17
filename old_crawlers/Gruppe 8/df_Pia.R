library(chron)

source("franziskanerkloster.R")
df -> df_franz
source("kirche_frauenland.R")
df -> df_k_frau
source("kirche_zellerau.R")
df -> df_k_zell
source("theater_hobbit.R")
df -> df_theater
source("zfk-wuerzburg.R")
df -> df_zfk

bind_rows(df_franz, df_k_frau, df_k_zell, df_theater, df_zfk) -> df_final

df_final$time_start <- times(df_final$time_start)
df_final$time_end <- times(df_final$time_end)

