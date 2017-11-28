library(XML)
library(curl) 
library(RCurl)
library(stringr)
setwd("/Users/michaelstedler/PycharmProjects/BigDataProject")


updateJobs <- function(df,new_df){
  
  #new_df = crawlJobs()

  for(i in 1:nrow(new_df)) {
    if(!(new_df$id[i] %in% df$id) & !(new_df$id[i]=="null")){
      
      cat("adding new ID",fill = TRUE)
      app = c(toString(new_df$id[1]), 
              toString(new_df$author[1]), 
              toString(new_df$date[1]),
              toString(new_df$title[1]),
              toString(new_df$category[1]),
              toString(new_df$city[1]),
              toString(new_df$country[1]))
      df <- rbind(df,app)
    }
    else{
      cat("ID already exists",fill = TRUE)
    }
  }
  
  return(df)
}

# Crawls Stackoverflow for jobs
crawlJobs <- function(){
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
  id = c()
  
  for(i in 1:len){ 
    item = paste("//item[",i,"]/",sep="") 
    
    tit = xpathSApply(doc, paste(item,"title",sep=""), xmlValue) 
    cat = xpathSApply(doc, paste(item,"category",sep=""), xmlValue) 
    loc = xpathSApply(doc, paste(item,"location",sep=""), xmlValue) 
    aut = xpathSApply(doc, paste(item,"author/name",sep=""), xmlValue) 
    upd = xpathSApply(doc, paste(item,"updated",sep=""), xmlValue)
    iden = xpathSApply(doc, paste(item,"guid",sep=""), xmlValue)
    cou = "null" 
    cit = "null"
    
    # skip tuple values if there is no location or category provided 
    if (length(loc)==0 | length(cat)==0 ){ 
      tit = "null" 
      cat = "null" 
      loc = "null" 
      aut = "null" 
      upd = "null"
      iden = "null"
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
    id[i] = iden
    author[i] = aut 
    date[i] = upd 
    title[i] = tit 
    category[i] = cat 
    #location[i] = loc 
    city[i] = cit 
    country[i] = cou 
  }
  
  # write to data.frame and export as csv 
  df = data.frame(id,author,date,title,category,city,country,stringsAsFactors=FALSE)
  
  return(df)
}

df = c()

drops <- c("X.1","X")
# Main 
if(!file.exists("data/jobsFeed.csv")){
  print("No Job Database existing.")
  print("Initialising Jobs.")
  df = df[ , !(names(df) %in% drops)]
  df = crawlJobs()
}else{
  print("Job Database existing.")
  df = read.csv("data/jobsFeed.csv")
  print("Updating Jobs.")
  df = df[ , !(names(df) %in% drops)]
  df = updateJobs(df=df, new_df = df) 
}

write.csv(df, file = "data/jobsFeed.csv")
