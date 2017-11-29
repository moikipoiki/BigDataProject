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
    print("true")
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
             
             # Take the data from 2017 only 
             var_temp <- var_temp[substr(var_temp$DATE,1,4)==2017 & substr(var_temp$DATE,7,8)=="01",]
          
             
             var <- rbind(var, var_temp)
             
             #print(paste("Hierhin",paste(substr(element,1,nchar(element)-3) )))
             
             ### Write every txt file in a csv file ### 
             #write.csv(x=var, file=paste(substr(element,1,nchar(element)-3), "csv",sep=""),row.names = FALSE)
             
             ### remove txt file ### 
             file.remove(element)
           }
           
           #print(read.table(file=element,header=T))
    )
    print(paste(folder, "|","(",i+1,"/", length(FILES),")"))
    i <- i + 1
    #if(i == 10){break} #temp for checking 
  }
  
  #print(paste("assign data to data.frame", substr(folder, nchar(folder)-1,nchar(folder))))
  #assign(substr(folder, nchar(folder)-1,nchar(folder)),var)
  write.csv(x=var, file=paste("/home/julain/Documents/weather/ready/",substr(folder,nchar(folder)-1,nchar(folder)), ".csv",sep=""),row.names = FALSE)
  print("finished")
  break # start next folder manually 
}

folder
substr(folder,nchar(folder)-1,nchar(folder))
nchar(folder)
