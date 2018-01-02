library(dplyr)
library(measurements)
library(stringr)
data <- quakes
data

getwd()
stations <- read.csv("DataProf/liste-des-gares.csv", sep=";")

test <- as.character(stations[,"coordonnees_geographiques"])

lat <- list()
lon <- list()

for(i in 1:length(test)){
  temp <- strsplit(test[i], ",")
  temp <- unlist(temp)
  
  lat[[i]] <- as.numeric(temp[1])
  lon[[i]] <- as.numeric(temp[2])
}


lat <- unlist(lat)
lon <- unlist(lon)
names <- stations$LibellÃ..Gare
mag <- rep(4,length(names))
#mag <- norm()
#norm(3)
rep(dnorm(1),4)

#sample(4:6.4, length(lat),replace=T)
mag <- rnorm(length(lat), mean=4, sd=2)



myStations <- data.frame(lat = lat,
                         lon = lon,
                         name = names,
                         mag = mag)

#which(is.na(myStations$lat))
length(myStations)
myStations <- myStations[-which(is.na(myStations$lon)),]
which(is.na(myStations$lat))
length(myStations[,1])


myStations <- myStations[1:1500,]



test2 <- strsplit(test,",")
test3 <- unlist(test2)
as.numeric(test3[2])




myStations <- data.frame(name = c("stadt1", "stadt2", "stadt3"),
                         lat = c(51.0026249949,50.9968331082,50.9786523513),
                         lon = c(2.31305342637,2.29310840674,2.12515082734),
                         mag = c(4,5,7)
                         )


quakes[quakes$mag >= 3 & quakes$mag <= 6,]
myStations[myStations$mag >= 3 & myStations$mag <= 6,]


######### Lost and Found ###############

lostAndFound <- read.table("DataBjoern/FoundLostAmount", sep=",")
colnames(lostAndFound) <- c("id", "date", "name", "value1", "value2")


lostAndFound <- lostAndFound %>% 
  select(name, value1, value2, date)


myData <- inner_join(lostAndFound, myStations, by="name") %>%
  select(name, lat, lon, date,value1,value2)

length(unique(myData$name))


myData <- data.frame()
head(myStations)
head(lostAndFound)



