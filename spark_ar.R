library(sparklyr) 
library(dplyr) 
library(stringr) 
library(arules) 

setwd("/Users/michaelstedler/PycharmProjects/BigDataProject") 


# Spark Configurations 
conf <- spark_config() 

# build SPARK Connection 
sc <- spark_connect(master = "local") 

# load dataframe 
jobsFeed <- read.csv("data/jobsFeed.csv") 
tbl_jobsFeed <- copy_to(sc,jobsFeed,name=spark_table_name(substitute(jobsFeed))) 

#  
x <- tbl_jobsFeed %>% 
  filter(city=="BERLIN") %>% 
  select(author,category,city)%>% 
  collect() 


# build list with all categories in filtered jobsFeed 
categories <- c()
for(i in 1:length(x$category)){ 
  ls = strsplit(x$category[i],split = ";") 
  for(cat in ls[[1]]){ 
    if(!cat %in% categories){ 
      categories <-c(categories,str_replace_all(cat,"[^[:alnum:]]","")) 
    } 
  } 
} 

# create columns for every possible language 
mydf <- data.frame(x$category,x$author) 
for (i in categories) { 
  print(i) 
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

# perform Assiociation Rules
rules_all <- apriori(mydf) 
inspect(rules_all) 