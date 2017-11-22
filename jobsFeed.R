library(XML)
library(curl)
library(RCurl)

link <- "https://stackoverflow.com/jobs/feed"

html <- getURL(link, followlocation = TRUE)
doc = htmlParse(html, asText=TRUE)

len = length(xpathSApply(doc, "//item", xmlValue))


author = c()
date = c()
title = c()
category = c()
location = c()

for(i in 1:len){
  item = paste("//item[",i,"]/",sep="")
  
  v1 = xpathSApply(doc, paste(item,"title",sep=""), xmlValue)
  v2 = xpathSApply(doc, paste(item,"category",sep=""), xmlValue)
  v3 = xpathSApply(doc, paste(item,"location",sep=""), xmlValue)
  v4 = xpathSApply(doc, paste(item,"author/name",sep=""), xmlValue)
  v5 = xpathSApply(doc, paste(item,"updated",sep=""), xmlValue)
  
  # skip tuple values if there is no location or category provided
  if (length(v2)==0 | length(v3)==0 ){
    v1 = "null"
    v2 = "null"
    v3 = "null"
    v4 = "null"
    v5 = "null"
  }
  # concatenate all category vaulues to one string
  else{
    v2 = paste(v2, collapse = ';')
  }
  author[i] = v4
  date[i] = v5
  title[i] = v1
  category[i] = v2
  location[i] = v3
}

df = data.frame(author,date,title,category,location)
write.csv(df, file = "jobsFeed.csv")

