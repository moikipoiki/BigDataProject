library(sparklyr)
library(dplyr)
library(base)

# sc <- spark_connect(master = "local") # connect to spark 
# #setwd("/home/julain/Documents/weather/ready/")
# loadData(getwd())
# 
# print(getData("ss", "HAMBURG"))
# 
# print(getStations("ss"))

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


x = getNewData("cc", "HAMBURG")

getNewData <- function(data, city){
  query <- left_join(tbl(sc,data), tbl(sc,"stations"), by="STAID") %>%
    select(STAID,STANAME,DATE,rdata = starts_with(data), quality = starts_with("Q_")) %>%
    filter(quality == 0) %>%
    group_by(STANAME) %>%
    summarise_each(funs(mean)) 
  
  query <- as.data.frame(query)
  
  #query <- apply(query[,4],2,mean)
  return(query)
}

c
calcualteCC(1)

calcualteCC <- function(value){
  if(value == 1 ){
    return(2)
  }
  else{
    return(3)
  }
}

getStations("cc")

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