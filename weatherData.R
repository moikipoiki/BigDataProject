library(sparklyr)
library(dplyr)
library(base)

#data
# tg = mean temperature | 0 = under OC, 1 = under 10C , 2 = under 20C, 3 = under 30C
# cc = cloudly | 0 = not cloudy, 1 = cloudy
# ss = sunnshine in hours | 0 = less, 1 = middle, 3 = much
# sd = mean snow depth in cm |  = 0cm, 1 = <10cm, 2 = <20cm, 3 = >=20cm



# sc <- spark_connect(master = "local") # connect to spark
# setwd("/home/julain/Documents/weather/ready/")
# loadData(getwd())
# sc <- spark_connect(master = "local") # connect to spark
# setwd("/home/julain/Documents/weather/ready/")
# loadData(getwd())
# #
# print(getData("ss", "HAMBURG"))
# #
# # print(getStations("ss"))

loadData <- function(path){
  savePath <- getwd()
  setwd(path) # get path where files are stored 
  files <- list.files(pattern=".csv") # grap all stored files from the folder 
  for (file in files){
    var <- print(substr(file,1,nchar(file)-4))
    spark_read_csv(sc,var,file,memory=FALSE)
  }
  setwd(savePath)
}

getData <- function(data, city){
  query <- left_join(tbl(sc,data), tbl(sc,"stations"), by="STAID") %>%
    select(STAID,STANAME,DATE,rdata = starts_with(data), quality = starts_with("Q_")) %>%
    filter(rlike(STANAME, city), quality == 0) %>%
    summarise(mean(rdata))
  query <- as.data.frame(query)
  
  if(data == "ss"){
    query <- query * 0.1
  }
  return(query)
}

getNewData <- function(data){
  query <- left_join(tbl(sc,data), tbl(sc,"stations"), by="STAID") %>%
    select(STAID,STANAME,DATE,rdata = starts_with(data), quality = starts_with("Q_")) %>%
    filter(quality == 0) %>%
    group_by(STANAME) %>%
    summarise(classes = mean(rdata))
  
  query <- as.data.frame(query)
  
  if(data == "ss" | data == "tg"){
    query <- query %>%
      select_all() %>%
      mutate(classes = round(classes*0.1))
  }
  
  # create new column 
  myClasses <- createClasses(query %>% select(classes), data)
  
  # append new columns
  query <- query %>%
    mutate(myClass = myClasses[,1])
  
  return(query)
}

# 
x <- getNewData("sd")
head(x)
x %>%
  select(classes,myClass) %>%
  distinct(myClass, classes) %>%
  arrange(classes)

x %>%
  select_all %>%
  filter(classes==204.75)


# x %>%
#   select(classes) %>%
#   distinct(classes) %>%
#   arrange(classes)

createClasses <- function(my, data){
  if(data=="cc"){
    # https://en.wikipedia.org/wiki/Okta
    for(element in 1:length(t(my))){
      if(my[element,]<=2){
        my[element,] = 0
      }
      else{
        my[element,] = 1
      }

    }  
  }
  if(data=="ss"){

    for(element in 1:length(t(my))){
      if(my[element,]<=2 ){
        my[element,] = 0
      }
      else if(my[element,] <= 6){
        my[element,] = 1
      }
      else{
        my[element,] = 2
      }
    }  
  }
  if(data=="tg"){
    
    for(element in 1:length(t(my))){
      if(my[element,] <= 0 ){
        my[element,] = 0
      }
      else if(my[element,] <=10){
        my[element,] = 1
      }
      else if(my[element,] <=20){
        my[element,] = 2
      }
      else{
        my[element,] = 3
      }
    }  
  }
  if(data=="sd"){
    
    for(element in 1:length(t(my))){
      if(my[element,] <= 0 ){
        my[element,] = 0
      }
      else if(my[element,] <=10){
        my[element,] = 1
      }
      else if(my[element,] <=20){
        my[element,] = 2
      }
      else{
        my[element,] = 3
      }
    }  
  }
  return(my)
}

getStations <- function(data){
  query <- left_join(tbl(sc,data), tbl(sc, "stations"), by="STAID") %>%
    select(STANAME,STAID) %>% 
    distinct(STANAME,STAID)
  return(query)
}

isStationLike <- function(data, city){
  query <- left_join(
    tbl(sc,data), tbl(sc,"stations"), by="STAID") %>% 
    select(STANAME, STAID) %>%
    filter(rlike(STANAME, city), quality == 0)
}
