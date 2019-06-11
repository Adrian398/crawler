require(xml2)
require(XML)
require(RCurl)

library(httr)
temp = GET("http://www.vku-kunst.de/?plugin=all-in-one-event-calendar&controller=ai1ec_exporter_controller&action=export_events&xml=true")

doc = xmlParse(temp)

temp2 = getNodeSet(doc, "//vevent")


getContent = function(x)
{
  v=getChildrenStrings(x)
  data.frame(as.list(v), stringsAsFactors=F)
}


df =map_df(temp2, getContent)

df %>% select(summary, dtstart, dtend, description) %>%
  mutate(description = str_extract(description,"[:print:]+")) -> df_clean
