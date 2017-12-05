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
             
             ### Read data and store in var ### 
             #var <- read.table(file=element, skip=20, header=TRUE, fill=TRUE, sep=",") # read table
             
             #### Save data in variable ### 
             var_temp <- as.data.frame(read.table(file=element, skip=20, header=TRUE, fill=TRUE, sep=",", strip.white = TRUE))
             #var_temp <- rbind(var, as.data.frame(read.table(file=element, skip=20, header=TRUE, fill=TRUE, sep=",")))
             
             # Take the data from 2013 to 2017 only + take only the data with approved quality 
             var_temp <- var_temp[substr(var_temp$DATE,1,4)>=2013 & var_temp$Q_SD==0,]
             
            
             
             var <- rbind(var, var_temp)
             
             #print(paste("Hierhin",paste(substr(element,1,nchar(element)-3) )))
             
             ### Write every txt file in a csv file ### 
             #write.csv(x=var, file=paste(substr(element,1,nchar(element)-3), "csv",sep=""),row.names = FALSE)
             
             ### remove txt file ### 
             file.remove(element)
           }
    )
    print(paste(folder, "|","(",i+1,"/", length(FILES),")"))
    i <- i + 1
    #if(i == 10){break} #temp for checking 
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


head(ss)
head(stations_ss)

################################## cc ###############################################
cc %>%
  inner_join(stations_cc, by="STAID") %>%
  select_all() %>%
  distinct(CN) %>%
  arrange(CN)

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
  select(STANAME,TG,DATE,CN ) %>%
  filter(CN=="FR") 

write.csv(tg_new, file="tg_new.csv")
