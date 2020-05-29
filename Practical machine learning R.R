#Downloading the libraries
library(lattice)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
library(rattle)
library(randomForest)
library(RColorBrewer)
library(corrplot)

# Dowloading the data from the desktop
training = read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
testing = read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")
set.seed(12345)
#the training dataset is split into 2 with 70% for the training set and 30% for the test set
inTrain  <- createDataPartition(training$classe, p=0.7, list=FALSE)
TrainS <- training[inTrain, ]
TestS  <- training[-inTrain, ]
#I now ask to show the number of variables in each set
dim(TrainS)
dim(TestS)

#Removing the variables with plenty of NA in both set
NAs = sapply(TrainS, function(x) mean(is.na(x))) > 0.95
TrainS = TrainS[, NAs==FALSE]
TestS  = TestS[, NAs==FALSE]
#I now ask to show the new number of variables in each set
dim(TrainS)
dim(TestS)

#Removing the Near Zero variance variables in both set
NZV = nearZeroVar(TrainS)
TrainS = TrainS[, -NZV]
TestS  = TestS[, -NZV]
#I now ask to show the new number of variables in each set
dim(TrainS)
dim(TestS)

#Removing the first variables as their are irrelevant
TrainS = TrainS[ , -(1:5)]
TestS  = TestS[ , -(1:5)]
#I now ask to show the new number of variables in each set
dim(TrainS)
dim(TestS)

#Correlation with the correlation matrix
CorrMat = cor(TrainS[, -54])
corrplot(CorrMat, method="color", type="lower")


#Decision tree model
set.seed(12345)
modFitT = rpart(classe ~., data=TrainS, method = "class")
#the following instruction is to plot the decision tree in a clearer manner
fancyRpartPlot(modFit, sub ="Classification Tree")

predictionTree = predict(modFit, newdata=TestS, type = "class")
predictionTree

#Random forest model
library(e1071)
set.seed(12345)
#Including a 3-fold cross validation 
ControlRF = trainControl(method="cv", number=3)
modFitRF = train(classe ~ .,method="rf", data=TestS, ControlRF=ControlRF,verbose=FALSE) 
#I do the prediction on the test data set
predictRF = predict(modFitRF, newdata=TestS)
#Confusion Matrix
confMatRF = confusionMatrix(predictRF, TestSet$classe)
confMatRF

#Applying the model to the data
predictTEST = predict(modFitT, newdata=testing)
predictTEST