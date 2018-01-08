library(dplyr)
library(ggplot2)
library(gridExtra)
library(A3)
library(MASS)
library(scales)


getwd()
setwd("/home/julain/Documents/PredictiveAnalytics")

wine <- read.table("Wine/wine.txt" ,sep=",")
colnames(wine) <- c("Species","Alcohol", "MalicAcid", "Ash","AlcalinityOfAsh","Magnesium","TotalPhenols",
                    "Flavanoids","NonflavanoidPhenols", "Proanthocyanins","ColorIntensity","Hue",
                    "OD280", "Proline")
wine[which(wine$Species == 1),"Species"] <- "WineA"
wine[which(wine$Species == 2),"Species"] <- "WineB"
wine[which(wine$Species == 3),"Species"] <- "WineC"

wine2 <- wine[,1:5]

exerciseA1 <- function(data,label){
  # Investigate variables 
  print(paste("Here are", length(colnames(data)), "different variables"))
  print(colnames(data))
  
  # labels of every column
  labels <- colnames(data)
  
  # Number of data
  print(paste("The dataset has",length(t(data)),"rows"))
  
  # Number of data per variable
  for(i in 1:length(t(labels))){
    print(labels[i])
    print(length(which(data[]==as.character(labels[i]))))
    i <- i + 1 
  }
  
  vars <- colnames(data)
  
  i <- 1 # counter for names 
  my_plots <- list() # create a histogramm for each variable 
  
  for(i in 1:length(vars)){
    print(paste("variable:", as.character(vars[i])))
    if(vars[i] == label){
      print("next")
      next # do not make a historgram for the variable "Species"
    }
    hist <- ggplot(data=data, aes_string(x=as.character(vars[i]))) + # get(vars[i])
      geom_histogram(color="black", aes_string(fill=as.character(label))) +
      xlab(as.character(vars[i])) + 
      ylab("Frequency")
    my_plots[[i]] <- hist
  }
  n <- length(my_plots)
  print(paste("length of plots", n))
  #do.call("grid.arrange", c(grobs=my_plots))
  do.call("grid.arrange", c(my_plots, ncol=floor(sqrt(n))))
  
}
exerciseA1(iris,"Species") # iris data
exerciseA1(wine2,"Species") # try wine or wine2 (wine2 is just variable 1 to 5)

exerciseA2 <- function(data, label){
  for(i in 1:length(colnames(data))){
    if(colnames(data[i]) == label){
      next # do not analyse label 
    }
    print(paste("Position criteria for", colnames(data[i])))
    var <- data[,colnames(data[i])]
    var_un <- unique(var)
    print(paste("Median:", median(var)))
    print(paste("Mean:", mean(var)))
    print(paste("Mode:", var_un[which.max(tabulate(match(var, var_un)))]))
    
    #Dispersion criterial
    print(paste("Dispersion criteria for", colnames(data[i])))
    print(paste("Standard deviation:", sd(var)))
    print(paste("Range", range(var)))
    print(paste("Interquartile range", IQR(var)))
    print(paste("Median absolute deviation", mad(var)))
  }
  
  
}
exerciseA2(iris, "Species") # iris data
exerciseA2(wine, "Species")

exerciseB1 <- function(data,label){
  rel_col <- colnames(data)
  rel_col <- rel_col[-which(rel_col==label)]
  combinations <- combn(rel_col,2)
  combinations
  
  my_plots <- list()
  pairs <- length(combinations)/2
  print(paste("Pairs", pairs))
  for(i in 1:pairs){
    print(i)

    scatter <- ggplot(data, aes_string(x=as.character(combinations[,i][1]), y=as.character(combinations[,i][2])))+
      geom_point(aes_string(color=as.character(label), shape=as.character(label))) +
      xlab(combinations[,i][1]) +  
      ylab(combinations[,i][2]) +
      ggtitle(paste(combinations[,i][1], "vs.", combinations[,i][2]))
    plot(scatter)
    my_plots[[i]] <- scatter
  }
  do.call("grid.arrange", c(my_plots, ncol=floor(sqrt(pairs))))
}
exerciseB1(iris,"Species")
exerciseB1(wine2,"Species")


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
cor_results <- exerciseB2(wine, "Species")


cor_results <- as.data.frame(cor_results)
cor_results
rel_cor <- cor_results[which(cor_results$cov > 0.5),] # take 0.5 as siginficant value 
rel_cor

exerciseB3 <- function(data, rel_cor, label){
  my_plots <- list()
  for(i in 1:length(t(rel_cor[,1]))){
    group <- as.character(rel_cor[i,"group"]) # get group name from list, e.g. "virginica"
    var1 <-  as.character(rel_cor[i,"var1"]) # get var name, e.g. "Sepal.Width"
    var2 <-  as.character(rel_cor[i,"var2"]) # get var name, e.g. "Sepal.Length"
    
    my_data <- data[which(data$Species==group),] # take just the data from the relevant group, e.g. "virginicia"
    
    test <- coef(lm(get(var1) ~ get(var2), my_data)) # calculate linear regression coefficient related to vars and group
    intercept <- as.numeric(test[1]) # save intercept 
    slope <- as.numeric(test[2]) # save slope 
    
    scatter <- ggplot(my_data, aes_string(x=as.character(var1), y=as.character(var2)))+
      geom_point(aes_string(color=as.character(label), shape=as.character(label))) +
      xlab(var1) +  
      ylab(var2) + 
      geom_smooth(method = "lm", se = FALSE)
      # geom_abline(yintercept = intercept, slope = slope, col = "blue") +
      # ggtitle(paste("slope:", slope, "intercept", intercept))
    my_plots[[i]] <- scatter # save scatter on list 
  }
  
  do.call("grid.arrange", c(my_plots, ncol=floor(sqrt(length(my_plots))))) # print whole list of plots 
  
}
exerciseB3(iris, rel_cor, "Species") # ATTENTION! You have to run the correct dataset on exercise B2 to get the corret "rel_cor"
exerciseB3(wine, rel_cor, "Species") # ATTENTION! You have to run the correct dataset on exercise B2 to get the corret "rel_cor"

# code from git hub "lda_vs_pca.R" 
# https://gist.github.com/thigm85/8424654
exerciseC1 <- function(data, label){
  myForm <- as.formula(paste(label,"~."))
  lda <- lda(myForm, data, prior=c(1,1,1)/3)
  prop.lda = lda$svd^2/sum(lda$svd^2)
  plda <- predict(object=lda, newdata = data)
  dataset = data.frame(species=data[,label],
                       lda = plda$x)
  p1 <- ggplot(dataset) +
    geom_point(aes(lda.LD1, lda.LD2, colour=species, shape=species), size = 2.5)
  print(p1)
}
exerciseC1(iris, "Species")
exerciseC1(wine, "Species")


# Compare projection obtained with the PCA and
# LDA, explain why there are differences:

# the reuslts of PCA and LDA are differnet because they have another focus. While PCA maximize the 
# variance, LDA maximize the distance between the variables along the new axe (discrimination line). 
# Furthermore LDA works with existing labels. PCA on the other hand creates own classifications due 
# to what there are differents between the results of PCA and LDA
