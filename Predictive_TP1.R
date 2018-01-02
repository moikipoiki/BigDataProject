library(dplyr)
library(ggplot2)
library(gridExtra)
library(A3)

exerciseA1 <- function(data,label){
  
  # Investigate variables 
  print(paste("Here are", length(colnames(data)), "different variables"))
  print(colnames(data))
  
  # Number of data
  print(paste("The dataset has",length(t(data)),"rows"))
  
  # Number of data per variable
  for(i in 1:length(t(labels))){
    print(labels[i,])
    print(length(which(data[]==as.character(labels[i,]))))
    i <- i + 1 
  }
  
  vars <- colnames(data)
  
  i <- 1 # counter for names 
  my_plots <- list() # create a histogramm for each variable 
  
  for(i in 1:length(vars)){
    print(paste("variable:", as.character(vars[i])))
    if(vars[i] == label){
      next # do not make a historgram for the variable "Species"
    }
    hist <- ggplot(data=data, aes_string(x=vars[i])) + # get(vars[i])
      geom_histogram(color="black", aes_string(fill="Species")) +
      xlab(as.character(vars[i])) + 
      ylab("Frequency")
    #my_plots <- append(my_plots, assign(var_names[i], hist)) # create variable and save the histogram 
    my_plots[[i]] <- hist
    i <- i + 1 # next variable
    test <- hist
    #hist <- NULL
    if(i == length(vars)+1){

    }
  }
  n <- length(my_plots)
  do.call("grid.arrange", c(my_plots, ncol=floor(sqrt(n))))
  
}
exerciseA1(iris,"Species") # iris data
# wine data




exerciseB1 <- function(data,label){
  rel_col <- colnames(data)
  rel_col <- rel_col[-which(rel_col==label)]
  combinations <- combn(rel_col,2)
  combinations
  
  my_plots <- list()
  pairs <- length(combinations)/2
  for(i in 1:pairs){
    scatter <- ggplot(iris, aes_string(x=as.character(combinations[,i][1]), y=as.character(combinations[,i][2])))+
      geom_point(aes_string(color=as.character(label), shape=as.character(label))) +
      xlab(combinations[,i][1]) +  
      ylab(combinations[,i][2]) +
      ggtitle(paste(combinations[,i][1], "vs.", combinations[,i][2]))
    my_plots[[i]] <- scatter
  }
  do.call("grid.arrange", c(my_plots, ncol=floor(sqrt(pairs))))
}
exerciseB1(iris,"Species")


data <- iris
label <- "Species"
unique(data[label])

exerciseB2 <- function(data, label){
  groups <- unique(data[label])
  groups <- as.data.frame(groups)
  
  rel_col <- colnames(data)
  rel_col <- rel_col[-which(rel_col==label)]
  
  pairs <- combn(rel_col,2)
  pairs
  
  results <- list()
  
  for(i in 1:length(t(groups))){
    group <- groups[i,]
    print(paste("Group:", group))
    temp_data <- data[which(data[label] == as.character(group)),]
    for(j in 1:(length(pairs)/2)){
      var1 <- temp_data[pairs[,j][1]] # data from first variable, e.g. sepal.width
      var2 <- temp_data[pairs[,j][2]] # data from second variable, e.g. sepal.length
      cor_temp <- cor(var1,var2)

      results <- rbind(results, c(pairs[,j][1], pairs[,j][2],cor_temp, as.character(group)))
      #results <- rbind(results, result_temp)
      print(paste("Covariance between", pairs[,j][1], "and",pairs[,j][2], "=", cor_temp))
    }
  }
  colnames(results) <- c("var1", "var2", "cov", "group")
  return(results)

}
cor_results <- exerciseB2(iris, "Species")

cor_results <- as.data.frame(cor_results)
cor_results
rel_cor <- cor_results[which(cor_results$cov > 0.5),]
rel_cor


exerciseB3 <- function(data, rel_cor, label){
  
}
exerciseB3()

plot(lm(my_data$Sepal.Length ~ my_data$Sepal.Width))

a3.lm(my_data, my_data$Sepal.Length ~ my_data$Sepal.Width)



#my_groups <- unique(rel_cor$group)

rel_cor

for(i in 1:length(t(rel_cor[,1]))){
  group <- as.character(rel_cor[i,"group"])
  var1 <-  as.character(rel_cor[i,"var1"])
  var2 <-  as.character(rel_cor[i,"var2"])
  
  #my_data <- iris[which(iris[label] == as.character(my_groups[i])),]
}

my_data

my_data <- iris["setosa"]
my_data
a3.lm()

my_data["Sepal.Length"]

my_data

my_plots <- list()


plot(a3.lm(get(var1) ~ get(var2), my_data, "gaussian"))
test <- a3.lm(get(var1) ~ get(var2), my_data, "gaussian")
print(test)
test$slopes

plot(1.6947730, 0.6314052)

test$feature.R2[1]

do.call("grid.arrange", c(my_plots, ncol=floor(sqrt(2))))


lm(get("Sepal.Length") ~ get("Sepal.Width"), my_data)

head(iris)
