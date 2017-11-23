library(sparklyr)
library(dplyr)

# spark
setwd("/home/julain/Documents/weather/data/ECA_blend_cc")
sc <- spark_connect(master = "local")
FILES <- list.files(pattern=".csv")
FILES
a

cc <- copy_to(sc, a)
cc


sdf_bind_cols(a,cc)
Spark_

test1 <- spark_dataframe(a)


for(element in FILES){
  print(element)
}


sdf_bind_cols

getwd()

test <- spark_read_csv(sc, name = "credit_data", path = paste(getwd(),"/CC_STAID000001.csv",sep=""), header = TRUE, delimiter = ",")
test <- spark_read_csv(sc, name = "credit_data", path = paste(getwd(),"/CC_STAID000002.csv",sep=""), header = TRUE, delimiter = ",")
mydata <- copy_to()


#######################################################################
# Convert txt files to csv files 
getwd()
setwd("/home/julain/Documents/weather/data/") # select folder where the folders with data are stored 
FOLDERS <- list.files() # list all folders in the current folders
for(folder in FOLDERS){
  var <- data.frame()
  setwd(paste("/home/julain/Documents/weather/data/",folder,sep=""))
  print(getwd())
  FILES <- list.files(pattern=".txt")
  print(length(FILES))
  if(length(FILES)==3){
    print("true")
    next
    } 
  i <- 0 
  for(element in FILES){
    switch(element,
           "elements.txt"={
             next
           },
           "sources.txt"={
             next
           },
           "stations.txt"={
             next
           },
           {
             print(paste("write",folder,element))
             #var <- read.table(file=element, skip=20, header=TRUE, fill=TRUE, sep=",") # read table
             #var <- append(var, read.table(file=element, skip=20, header=TRUE, fill=TRUE, sep=","))
             var <- rbind(var, as.data.frame(read.table(file=element, skip=20, header=TRUE, fill=TRUE, sep=",")))
             #print(paste("Hierhin",paste(substr(element,1,nchar(element)-3) )))
             #write.csv(x=var, file=paste(substr(element,1,nchar(element)-3), "csv",sep=""),row.names = FALSE)
             file.remove(element)
           }
           
           #print(read.table(file=element,header=T))
    )
    print(paste(folder, "|","(",i,"/", length(FILES),")"))
    i <- i + 1
    #if(i == 10){break} #temp for checking 
  }
  
  #print(paste("assign data to data.frame", substr(folder, nchar(folder)-1,nchar(folder))))
  #assign(substr(folder, nchar(folder)-1,nchar(folder)),var)
  write.csv(x=var, file=paste(substr(element,1,nchar(element)-3), "csv",sep=""),row.names = FALSE)
  print("finished")
  break # start next folder manually 
}
#######################################################################
