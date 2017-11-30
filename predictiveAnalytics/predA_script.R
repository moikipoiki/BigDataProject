library(MASS)
library(psych)
library(readr)
library(nnet)

WhiteWine <- read_delim("/Users/michaelstedler/PycharmProjects/BigDataProject/predictiveAnalytics/data/winequality-white.csv", 
                        ";", 
                        escape_double = FALSE, 
                        trim_ws = TRUE)
attach(WhiteWine)
#######################################################################
#######################################################################
#######################################################################
# VISUALIZATION
par(mfrow=c(2,2), oma = c(1,1,0,0) + 0.1, mar = c(3,3,1,1) + 0.1)

# barplot for Quality because it is categorial, histogram for fixed, volatile and citric acidity
barplot((table(quality)), col=c("slateblue4", "slategray", "slategray1", "slategray2", "slategray3", "skyblue4"))
mtext("Quality", side=1, outer=F, line=2, cex=0.8)
truehist(fixed.acidity, h = 0.5, col="slategray3")
mtext("Fixed Acidity", side=1, outer=F, line=2, cex=0.8)
truehist(volatile.acidity, h = 0.05, col="slategray3")
mtext("Volatile Acidity", side=1, outer=F, line=2, cex=0.8)
truehist(citric.acid, h = 0.1, col="slategray3")
mtext("Citric Acid", side=1, outer=F, line=2, cex=0.8)

# histogram for residual.sugar,chloride,free.sulfur.dioxide and total.sulfur.dioxide
truehist(residual.sugar, h = 1.5, col="slategray3")
mtext("Residual Sugar", side=1, outer=F, line=2, cex=0.8)
truehist(chlorides, h = 0.008, col="slategray3")
mtext("Chlorides", side=1, outer=F, line=2, cex=0.8)
truehist(free.sulfur.dioxide, h =5, col="slategray3")
mtext("Free Sulfur Dioxide", side=1, outer=F, line=2, cex=0.8)
truehist(total.sulfur.dioxide, h = 6, col="slategray3")
mtext("Total Sulfur Dioxide", side=1, outer=F, line=2, cex=0.8)

# histogram for residual.sugar,chloride,free.sulfur.dioxide and total.sulfur.dioxide
truehist(density, h = 0.002, col="slategray3")
mtext("Density", side=1, outer=F, line=2, cex=0.8)
truehist(pH, h = 0.05, col="slategray3")
mtext("PH", side=1, outer=F, line=2, cex=0.8)
truehist(sulphates, h =0.05, col="slategray3")
mtext("Sulphates", side=1, outer=F, line=2, cex=0.8)
truehist(alcohol, h = 0.5, col="slategray3")
mtext("Alcohol", side=1, outer=F, line=2, cex=0.8)

# Boxplot Quality, Fixed Acidity, Volatile Acidity, Citric Acid
par(mfrow=c(1,4), oma = c(1,1,0,0) + 0.1,  mar = c(3,3,1,1) + 0.1)
boxplot(quality, col="slategray2", pch=19)
mtext("Quality", cex=0.8, side=1, line=2)
boxplot(fixed.acidity, col="slategray2", pch=19)
mtext("Fixed Acidity", cex=0.8, side=1, line=2)
boxplot(volatile.acidity, col="slategray2", pch=19)
mtext("Volatile Acidity", cex=0.8, side=1, line=2)
boxplot(citric.acid, col="slategray2", pch=19)
mtext("Citric Acid", cex=0.8, side=1, line=2)

# Boxplot Residual Sugar, Chlorides, Free Sulfur Dioside, Total Sulfur Dioxide
boxplot(residual.sugar, col="slategray2", pch=19)
mtext("Residual Sugar", cex=0.8, side=1, line=2)
boxplot(chlorides, col="slategray2", pch=19)
mtext("Chlorides", cex=0.8, side=1, line=2)
boxplot(free.sulfur.dioxide, col="slategray2", pch=19)
mtext("Free Sulfur Dioxide", cex=0.8, side=1, line=2)
boxplot(total.sulfur.dioxide, col="slategray2", pch=19)
mtext("Total Sulfur Dioxide", cex=0.8, side=1, line=2)

# Boxplot Density, PH, Sulphates and Alcohol
boxplot(density, col="slategray2", pch=19)
mtext("Density", cex=0.8, side=1, line=2)
boxplot(pH, col="slategray2", pch=19)
mtext("PH", cex=0.8, side=1, line=2)
boxplot(sulphates, col="slategray2", pch=19)
mtext("Sulphates", cex=0.8, side=1, line=2)
boxplot(alcohol, col="slategray2", pch=19)
mtext("Alcohol", cex=0.8, side=1, line=2)

#######################################################################
#######################################################################
#######################################################################
# STATISTICS
summary(WhiteWine)
describe(WhiteWine)

# covariance matrix
cor(WhiteWine[,-12])
# covariance matrix with spearman algorithm
cor(WhiteWine[,-12], method="spearman")
# scatterplot of all variables
pairs(WhiteWine[,-12], gap=0, pch=10, cex=0.4, col="darkblue")
title(sub="Scatterplot of Chemical Attributes", cex=0.8)

#######################################################################
#######################################################################
#######################################################################
# DATA PREPROCESSING

# Determine Q3+1.5IQR for determine a threshold for outliers
limout <- c()
data_quantiles = apply(WhiteWine,2,quantile,probs=0.75)
data_IQR = apply(WhiteWine,2,IQR)
for (i in 1:11){
  t1 <- data_quantiles[[i]]
  t2 <- data_IQR[[i]]
  limout[i] = t1 + 1.5*t2
}

# If value for each Colum is outside of the threshold, replace value in ZERO-Matrix with 1
WhiteWineIndex <- matrix(0, 4898, 11)
for (i in 1:4898)
  for (j in 1:11){
    if (WhiteWine[i,j] > limout[j]) WhiteWineIndex[i,j] <- 1
  }


WWInd <- apply(WhiteWineIndex, 1, sum)
WhiteWineTemp <- cbind(WWInd, WhiteWine)
Indexes <- rep(0, 208)
j <- 1

for (i in 1:4898){
  if (WWInd[i] > 0) {Indexes[j]<- i
  j <- j + 1}
  else j <- j
}

# Create testing and Test Data
WhiteWineLib <-WhiteWine[-Indexes,]   # Inside of Q3+1.5IQR, delete rest
indexes = sample(1:nrow(WhiteWineLib), size=0.8*nrow(WhiteWineLib))
WWTrain80 <- WhiteWineLib[indexes,]
WWTest20 <- WhiteWineLib[-indexes,]

# Normalize Data
data_train = as.data.frame(apply(WWTrain80[],2,function(x){(x-min(x)) / (max(x) - min(x))}))
data_test = as.data.frame(apply(WWTest20[],2,function(x){(x-min(x)) / (max(x) - min(x))}))

data_train$quality = WWtrain80$quality
data_test$quality = WWTest20$quality

data_train$quality = as.factor(data_train$quality)
data_test$quality = as.factor(data_test$quality)

#######################################################################
#######################################################################
#######################################################################
# MACHINE LEARNING MODEL
neural_net = nnet::nnet(quality~., 
                  data = data_train,
                  size = 48,
                  hidden = 20,
                  decay=5e-4, 
                  maxit=3200,
                  entropy=TRUE)
# WIP for optimzing hyperparameters
# fitControl <- trainControl(method = "repeatedcv", 
#                            number = 10, 
#                            repeats = 5, 
#                            classProbs = TRUE, 
#                            summaryFunction = twoClassSummary)
# 
# nnetGrid <-  expand.grid(size = seq(from = 1, to = 10, by = 1),
#                          decay = seq(from = 0.1, to = 0.5, by = 0.1))
# 
# nnetFit <- train(quality~., 
#                  data = data_train,
#                  method = "nnet",
#                  metric = "ROC",
#                  trControl = fitControl,
#                  tuneGrid = nnetGrid,
#                  verbose = FALSE)


#######################################################################
#######################################################################
#######################################################################
# MODEL EVALUATION
predictions = predict(neural_net, data_test,type="class")

correct = 0
for(i in 1:length(predictions)){
  if(predictions[i]==data_test$quality[i]){
    correct = correct + 1
  }
}
print("Accuracy on Test Data:")
correct/length(predictions)

predictions = predict(neural_net, data_train,type="class")

correct = 0
for(i in 1:length(predictions)){
  if(predictions[i]==data_train$quality[i]){
    correct = correct + 1
  }
}
print("Accuracy on Train Data:")
correct/length(predictions)


