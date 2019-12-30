library(jsonlite)

json_frame = fromJSON("https://cairo.wue.de/veranstaltungen.json")




library(feedeR)
library(tidyRSS)

df = tidyfeed("https://cairo.wue.de/veranstaltungen.rss")
df["item_title"]
names(json_frame)
json_frame$title
names(df)
json_frame$field_beginn
