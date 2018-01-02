library(sparklyr)
library(dplyr)


#######################################################################
# Convert txt files to csv files 
getwd()
setwd("/home/julain/Documents/weather/data/") # select folder where the folders with data are stored 
FOLDERS <- list.files() # list all folders in the current folders
FOLDERS
for(folder in FOLDERS){
  var <- data.frame()
  var_temp <- data.frame()
  setwd(paste("/home/julain/Documents/weather/data/",folder,sep=""))
  print(getwd())
  FILES <- list.files(pattern=".txt")
  print(length(FILES))
  if(length(FILES)==0){
    print(paste("Folder",folder,"is empty."))
    next
    } 
  i <- 0 
  for(element in FILES){
    switch(element,
           "elements.txt"={
             file.remove(element)
             next
           },
           "sources.txt"={
             file.remove(element)
             next
           },
           "stations.txt"={
             file.remove(element)
             next
           },
           {
             print(paste("write",folder,element))
             
             #### Save data in variable ### 
             var_temp <- as.data.frame(read.table(file=element, skip=20, header=TRUE, fill=TRUE, sep=",", strip.white = TRUE))
           
             # Take the data from 2013 to 2017 only + take only the data with approved quality 
             var_temp <- var_temp[substr(var_temp$DATE,1,4)>=2013 & var_temp$Q_SD==0,]
             
             # bind data to array 
             var <- rbind(var, var_temp)
             
             # remove txt file in folder
             file.remove(element)
           }
    )
    print(paste(folder, "|","(",i+1,"/", length(FILES),")"))
    i <- i + 1
  }
  write.csv(x=var, file=paste("/home/julain/Documents/weather/readyBigData/",substr(folder,nchar(folder)-1,nchar(folder)), ".csv",sep=""),row.names = FALSE)
  print("finished")
  break # start next folder manually 
}



####### Station files ######## 

setwd("/home/julain/Documents/weather/readyBigData/") # select folder where the folders with data are stored 
getwd()

stations_cc <- read.table(file="stations_CC.txt",  header=TRUE, fill=TRUE, sep=",", strip.white = TRUE)
stations_ss <- read.table(file="stations_SS.txt",  header=TRUE, fill=TRUE, sep=",", strip.white = TRUE)
stations_rr <- read.table(file="stations_RR.txt",  header=TRUE, fill=TRUE, sep=",", strip.white = TRUE)
stations_sd <- read.table(file="stations_SD.txt",  header=TRUE, fill=TRUE, sep=",", strip.white = TRUE,quote = "")
stations_tg <- read.table(file="stations_TG.txt",  header=TRUE, fill=TRUE, sep=",", strip.white = TRUE)

rr <- read.table(file="rr.csv",  header=TRUE, fill=TRUE, sep=",", strip.white = TRUE)
ss <- read.csv(file="ss.csv",  header=TRUE, fill=TRUE, sep=",", strip.white = TRUE)
tg <- read.csv(file="tg.csv",  header=TRUE, fill=TRUE, sep=",", strip.white = TRUE)
sd <- read.csv(file="sd.csv",  header=TRUE, fill=TRUE, sep=",", strip.white = TRUE)
cc <- read.csv(file="cc.csv",  header=TRUE, fill=TRUE, sep=",", strip.white = TRUE)




################################## cc ###############################################

number_cc <- cc %>%
  inner_join(stations_cc, by="STAID") %>%
  select_all() %>%
  distinct(CN) %>%
  arrange(CN) %>%
  summarize(length(CN))
print(paste("The number of cities in the cloud dataset is:", number_cc))

cities_cc <- cc %>%
  inner_join(stations_cc, by="STAID") %>%
  select_all() %>%
  distinct(CN) %>%
  arrange(CN)

print("The cities in the cloud dataset are:") 
cities_cc

################################## ss ###############################################

ss %>%
  inner_join(stations_ss, by="STAID") %>%
  select_all() %>%
  distinct(CN) %>%
  arrange(CN)

################################## rr ###############################################
rr %>%
  inner_join(stations_rr) %>%
  select_all() %>%
  distinct(CN)
  
################################## sd ####################################
sd%>%
  inner_join(stations_sd, by="STAID") %>%
  select_all() %>%
  distinct(CN) %>%
  arrange(CN)

############################## tg #####################
tg %>%
  inner_join(stations_tg, by="STAID") %>%
  select_all() %>%
  distinct(CN) %>%
  arrange(CN) 

# 40 cities from france:

numberTowns_tg <- tg %>%
  inner_join(stations_tg, by="STAID") %>%
  select(STANAME,CN) %>%
  filter(CN=="FR") %>%
  distinct(STANAME) %>%
  summarize(length(STANAME))

numberTowns_tg <- as.data.frame(numberTowns_tg)
numberTowns_tg
print(paste("Number of france cities in this dataset:", numberTowns_tg))

rows_tg <- tg %>%
  inner_join(stations_tg, by="STAID")%>%
  select(STANAME,CN) %>%
  filter(CN == "FR") %>%
  summarize(length(STANAME))
print(paste("Number of rows in the dataset:", rows_tg))

towns_tg <- tg %>%
  inner_join(stations_tg, by="STAID") %>%
  select_all() %>%
  filter(CN=="FR") %>%
  distinct(STANAME) 
towns_tg

tg %>%
  inner_join(stations_tg, by="STAID") %>%
  select(STANAME,TG,DATE,CN ) %>%
  filter(CN=="FR") 

tg %>%
  select(DATE) %>%
  distinct(DATE) %>%
  arrange(DATE) %>%
  filter(DATE == min(DATE))

#### tg data before 


#write.csv(tg_new, file="tg_new.csv")
