source("./awo.R")
source("./evdhg.R")
source("./kwm_klinikum.R")
source("./oberthuerschule.R")
source("./sundermann.R")

getdfNiklas <- function()
{
  bigdf <- rbind(crawl_awo_jw(), crawl_evdgh(),crawl_kwm_klinikum(), crawl_franz_obertheur_schule(), crawl_sundermannkunst())
  #View(bigdf)
  
  return(bigdf)
}

#getdfNiklas()