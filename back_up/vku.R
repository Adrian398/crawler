require(xml2)
require(XML)
require(RCurl)
library(httr)
library(purrr)
library(tidyverse)
library(textutils)



temp = GET("http://www.vku-kunst.de/?plugin=all-in-one-event-calendar&controller=ai1ec_exporter_controller&action=export_events&xml=true")
a = HTMLdecode(temp)
a = xmlParse(a)
temp2 = getNodeSet(a, "//vevent")


getContent = function(x)
{
  v=getChildrenStrings(x)
  data.frame(as.list(v), stringsAsFactors=F)
}


df =map_df(temp2, getContent)

df %>% select(summary, dtstart, dtend, description) %>%
  mutate(description = str_extract(description,"[:print:]+")) -> df_clean
