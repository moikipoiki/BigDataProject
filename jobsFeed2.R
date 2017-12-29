library(shiny)
library(ggplot2)
library(XML)
library(curl) 
library(RCurl)
library(sparklyr) 
library(dplyr) 
library(stringr) 
library(arules)
#library(plyr)
library(base)
setwd("/Users/michaelstedler/PycharmProjects/BigDataProject")
source("weatherData.R")
source("shiny_function.R")


weather_transformation <- function(df){

    city_names = unique(df$city)
    # tg = mean temperature | 0 = under OC, 1 = under 10C , 2 = under 20C, 3 = under 30C
    # cc = cloudly | 0 = not cloudy, 1 = cloudy
    # ss = sunnshine in hours | 0 = less, 1 = middle, 3 = much
    # sd = mean snow depth in cm |  = 0cm, 1 = <10cm, 2 = <20cm, 3 = >=20cm
    temp_df_cc <- getNewData("cc")
    temp_df_tg <- getNewData("tg")
    temp_df_ss <- getNewData("ss")
    temp_df_sd <- getNewData("sd")
    
    
    
    df$cc = 0
    df$tg = 0
    df$ss = 0
    df$sd = 0
    
    for(city in city_names){
      cc = as.numeric(temp_df_cc %>%
            select(classes, STANAME) %>%
            filter(STANAME==city) %>%
            select(classes))
      
      if(!is.na(cc)){
        df$cc[which(df$city==city)] = cc
      }else{
        df$cc[which(df$city==city)] = -10
      }
    }
    
    df = df[-which(df$cc == "-10"),]
    df$cc = as.factor(df$cc)
    return(df)
}

# get rules for specific pattern
getRules <- function(arules, cc_value, ss_value, sd_value, tg_value) {
  rhs = tryCatch(
    {
      subrules = arules
      if(!cc_value==999){
        subrules = subset(subrules,lhs %in% paste0("cc=",cc_value))
      }
      if(!ss_value==999){
        subrules = subset(subrules,lhs %in% paste0("ss=",ss_value))  
      }
      if(!sd_value==999){
        subrules = subset(subrules,lhs %in% paste0("sd=",sd_value)) 
      }
      if(!tg_value==999){
        subrules = subset(subrules,lhs %in% paste0("tg=",tg_value)) 
      }
      rhs = data.frame(name = labels(rhs(subrules), setStart = "", setEnd = ""),
                       subrules@quality)
      rhs <- rhs %>%
        select_all() %>%
        group_by(name) %>%
        top_n(1,support) %>%
        top_n(1,confidence) %>%
        top_n(1,lift) %>%
        top_n(1,count) %>%
        distinct(name,support,confidence, lift,count)
      
      return(rhs)
    }, error = function(e) {
      cat("No Rules found!",fill=TRUE)
      return(NULL)
    }
  )
  return(rhs)
}

# set rules
arule_mining <- function(df,s,c,len){
  # perform Assiociation Rules
  colnam = colnames(df)
  # exclude these attributes from the right hand side of rules
  # df = df[,colnam != c("author","city","country","category","cc")]
  
  rules <- apriori(df, 
                       parameter = list(supp = s, 
                                        conf = c,
                                        minlen = len
                                    )
           )
  
  r = subset(rules, !(rhs %pin% "city"))
  r = subset(r, !(rhs %pin% "country"))
  r = subset(r, !(rhs %pin% "author"))
  r = subset(r, !(rhs %pin% "category"))
  r = subset(r, !(rhs %pin% "cc"))
  r = subset(r, !(rhs %pin% "ss"))
  r = subset(r, !(rhs %pin% "sd"))
  r = subset(r, !(rhs %pin% "tg"))
  r = subset(r, !(lhs %pin% "country"))
  r = subset(r, !(lhs %pin% "city"))
  # r = subset(r, !(lhs %pin% "mysql"))
  # r = subset(r, !(lhs %pin% "php"))
  # r = subset(r, !(lhs %pin% "javascript"))
  # r = subset(r, !(lhs %pin% "nodejs"))
  # r = subset(r, !(lhs %pin% "reactjs"))
  # r = subset(r, !(lhs %pin% "css"))
  # r = subset(r, !(lhs %pin% "html"))
  
  # delete all programming languages from lhs - not running sufficient
  # categories <- c()
  # for(i in 1:length(mydf$category)){ 
  #   ls = strsplit(as.character(mydf$category[i]),split = ";") 
  #   for(cat in ls[[1]]){ 
  #     if(!cat %in% categories){ 
  #       categories <-c(categories,str_replace_all(cat,"[^[:alpha:]]","")) 
  #     } 
  #   } 
  # } 
  # 
  # for(i in 1:length(categories)){
  #     r = subset(r, !(lhs %pin% categories[i]))
  # }
  
  return(r)
}

category_transformation <- function(jobsFeed){

  # load dataframe 
  tbl_jobsFeed <- copy_to(sc,jobsFeed,name=spark_table_name(substitute(jobsFeed)),overwrite=TRUE) 
  
  #  
  x <- tbl_jobsFeed %>% 
    select(author,category,city,country)%>% 
    collect() 
  
  # build list with all categories in filtered jobsFeed 
  categories <- c()
  for(i in 1:length(x$category)){ 
    ls = strsplit(x$category[i],split = ";") 
    for(cat in ls[[1]]){ 
      if(!cat %in% categories){ 
        categories <-c(categories,str_replace_all(cat,"[^[:alpha:]]","")) 
      } 
    } 
  } 
  
  # create columns for every possible language
  author = x$author
  city = x$city
  country = x$country
  category = x$category
  
  mydf <- data.frame(author, 
                     city,
                     country,
                     category) 
  
  #rename(mydf, c("x.author"="author", "x.city"="city","x.country"="country","x.catgory"="category"))
  
  for (i in categories) {
    eval(parse(text = paste0('mydf$',i,' <- rep(FALSE,length(x$category))'))) 
  }  
  
  # load true for every language in inserat  
  for(i in 1:length(x$category)){ 
    ls = strsplit(x$category[i],split = ";") 
    for(cat in ls[[1]]){ 
      if(cat %in% categories){ 
        eval(parse(text = paste0('mydf$',cat,'[',i,'] <- TRUE'))) 
      } 
    } 
  }
  return(mydf)
}

# Update JobsFeed
updateJobs <- function(df){
  
  new_df = crawlJobs()
  
  for(i in 1:nrow(new_df)) {
    if(!(new_df$id[i] %in% df$id) & !(new_df$id[i]=="null")){
      cat("adding new ID",fill = TRUE)
      app = c(toString(new_df$id[i]),
              toString(new_df$author[i]),
              toString(new_df$date[i]),
              toString(new_df$title[i]),
              toString(new_df$category[i]),
              toString(new_df$city[i]),
              toString(new_df$country[i]))
      df <- rbind(df,app)
    }
  }
  drops <- c("X")
  df = df[ , !(names(df) %in% drops)]
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
        cat_2 = c(cat_2,str_replace_all(x,"[^[:alpha:]]",""))  
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
  df = df[-which(df$id=="null"),]
  return(df)
}

getWeatherData <- function(df){
  ss_ = c()
  for(i in 1:length(df$id)){
    #print(df$city[i])
    ss_[i] = as.numeric(getData("ss",df$city[i]))
  }
  df$ss = ss_
  return(df)
}

# Main 

df = c()
drops <- c("X.1","X")
# Start spark
conf <- spark_config() 
sc <- spark_connect(master = "local")

if(!file.exists("data/jobsFeed.csv")){
  cat("No Job Database existing.",fill = TRUE)
  cat("Initialising Jobs.",fill = TRUE)
  df = df[ , !(names(df) %in% drops)]
  df = crawlJobs()
}else{
  cat("Job Database existing.",fill = TRUE)
  df = read.csv("data/jobsFeed.csv",stringsAsFactors = FALSE)
  cat("Updating Jobs.",fill = TRUE)
  df = df[ , !(names(df) %in% drops)]
  df = updateJobs(df=df) 
}

write.csv(df, file = "data/jobsFeed.csv")

loadData("/Users/michaelstedler/PycharmProjects/BigDataProject/data/")
setwd("/Users/michaelstedler/PycharmProjects/BigDataProject")

# transform categories
mydf = category_transformation(df)
mydf = weather_transformation(mydf)

# association rules
rules = arule_mining(mydf)
