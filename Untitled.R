library(XML)
library(curl) 
library(RCurl)
library(stringr)
setwd("/Users/michaelstedler/PycharmProjects/BigDataProject")

check_new_Jobs <- function(df, new_df){
  
  for(i in 1:nrow(new_df)) {
    row <- dataFrame[i,]
    # do stuff with rowchec
  }
  
  return(df)
}