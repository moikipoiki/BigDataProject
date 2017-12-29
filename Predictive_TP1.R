library(dplyr)
library(ggplot2)
library(gridExtra)

# First overview of distribution of variables 
plot(iris)




length(t(iris))


exerciseA1(iris,"Species")
exerciseA1 <- function(data,label){
  
  # Investigate variables 
  print(paste("Here are", length(colnames(data)), "different variables"))
  colnames(data)
  
  # Number of data
  print(paste("The dataset has",length(t(data)),"rows"))
  
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






exerciseB1(iris)

exerciseB1 <- function(data){
  # 
  scatter + 
    geom_point(aes(color=Species, shape=Species)) +
    xlab("Sepal Length") +  
    ylab("Petal Width") +
    ggtitle("Sepal Length-Width")  
  
}

data1 <- iris$Sepal.Length
data2 <- iris$Sepal.Width
data <- data.frame()
data <- cbind(data1)
data <- cbind(data, data2)
head(data)
data <- as.data.frame(data)
mode(data)
scatter <- ggplot(data, aes(data1, data2))

scatter + 
  geom_point(aes(color=Species, shape=Species)) +
  xlab("Sepal Length") +  
  ylab("Petal Width") +
  ggtitle("Sepal Length-Width")  







