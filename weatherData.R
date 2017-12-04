library(sparklyr)
library(dplyr)
library(base)

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
    summarise(classes = ROUND(mean(rdata)))
  
  query <- as.data.frame(query)
  
  
  return(query)
}


# x = getNewData("cc")
# x
# 
# for(element in 1:length(t(my))){
#   print(my[element,])
#   if(my[element,]==1 | my[element,] == 2){
#     my[element,] = 999
#   }
#   else if(my[element,]==3 | my[element,] == 4){
#     my[element,] = 888
#   }
#   else{
#     my[element,] = 111
#   }
# }
# my
# 
# length(t(my))
# my[1,]
# 
# my <- x %>%
#   select(classes)
# apply(my,2,function(x){
#   return(x/2)
#   }
#   )
# 
# apply(my,2,function(x){
#   if(x == 1 ){
#     return(2)
#   }
#   else{
#     return(3)
#   }
# }
# 
#   
#   )
# 
# my <- calcualteCC(my)
# calcualteCC(my[,1])
# 
# getData("cc", "ALFELD")
# x[,which(x[,1] == "ALFELD")]
# x[725,]
# x[,1]
# x[1,2]
# c
# calcualteCC(1)

calcualteCC <- function(value){
  if(value == 1 ){
    return(2)
  }
  else{
    return(3)
  }
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
