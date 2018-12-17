require(tidyverse)
require(rvest)

readLinks = function(aLinks, fnCallback, sBase){
  oRes=data.frame()
for (sLink in aLinks){
    url=paste0(sBase,sLink)
    url %>% read_html() -> raw_data
    event=fnCallback(raw_data,url=url)
    if(!is.null(event)){
      oRes=data.frame(rbind((oRes), (event)))
    }
    
  }
  return (oRes)
}

getMonth= function(){
  
  sRegex= ("?: Jan(?:uar)?|Feb (?:ruar?) | März | April | Mai |Juni | Juli | Aug(?:ust?)|Sep(?:tember?)| Okt(?:ober?) | Nov(?:ember?)| Dez(?:ember?)")
  
}

#function definition
html_text_collapse <- function(x, trim = FALSE, collapse = " "){
  UseMethod("html_text_collapse")
}

html_text_collapse.xml_nodeset <- function(x, trim = FALSE, collapse = " "){
  vapply(x, html_text_collapse.xml_node, character(1), trim = trim, collapse = collapse)
}

html_text_collapse.xml_node <- function(x, trim = FALSE, collapse = " "){
  paste(xml2::xml_find_all(x, ".//text()"), collapse = collapse)
}