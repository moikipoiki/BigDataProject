library(XML)
library(curl) 
library(RCurl)
library(stringr)
setwd("/Users/michaelstedler/PycharmProjects/BigDataProject")

# Dictionary for translating certain locations to English 
city_transl <- c("MUENCHEN", 
                 "KOELN", 
                 "FRANKFURT", 
                 "MALMO", 
                 "GENEVE", 
                 "KIEV", 
                 "ZUERICH", 
                 "MILAN", 
                 "WARSZAWA") 
names(city_transl) <- c("münchen", 
                        "köln", 
                        "frankfurt am Main", 
                        "malmö", 
                        "genève", 
                        "kyiv", 
                        "zürich", 
                        "milano", 
                        "warsaw") 


# get JobsFeed from Stackoverflow 
link <- "https://stackoverflow.com/jobs/feed" 
html <- getURL(link, followlocation = TRUE) 
doc = htmlParse(html, asText=TRUE) 

len = length(xpathSApply(doc, "//item", xmlValue)) 


author = c() 
date = c() 
title = c() 
category = c() 
location = c() 
city = c() 
country  = c() 

for(i in 1:len){ 
  item = paste("//item[",i,"]/",sep="") 

  tit = xpathSApply(doc, paste(item,"title",sep=""), xmlValue) 
  cat = xpathSApply(doc, paste(item,"category",sep=""), xmlValue) 
  loc = xpathSApply(doc, paste(item,"location",sep=""), xmlValue) 
  aut = xpathSApply(doc, paste(item,"author/name",sep=""), xmlValue) 
  upd = xpathSApply(doc, paste(item,"updated",sep=""), xmlValue) 
  cou = "null" 
  cit = "null"
  
  # skip tuple values if there is no location or category provided 
  if (length(loc)==0 | length(cat)==0 ){ 
    tit = "null" 
    cat = "null" 
    loc = "null" 
    aut = "null" 
    upd = "null"
    
    print("Arschloch")
  }
  # concatenate all category vaulues to one string 
  else{ 
      cat_2 = c() 
      for(x in cat){ 
        cat_2 = c(cat_2,str_replace_all(x,"[^[:alnum:]]",""))  
      } 
      cat = paste(cat_2, collapse = ';') 
      cit = toupper(strsplit(loc,split=", ")[[1]][1]) 
      cou = toupper(strsplit(loc,split=", ")[[1]][2]) 
      # print(cit) 
      # translate city if possible 
      if(!is.na(city_transl[tolower(cit)])){ 
        cit = city_transl[tolower(cit)] 
      } 
  } 
  author[i] = aut 
  date[i] = upd 
  title[i] = tit 
  category[i] = cat 
  #location[i] = loc 
  city[i] = cit 
  country[i] = cou 
}

# write to data.frame and export as csv 
df = data.frame(author,date,title,category,city,country)
write.csv(df, file = "data/jobsFeed.csv")