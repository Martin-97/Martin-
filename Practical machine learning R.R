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
training = read.csv("/Users/riehl/Downloads/pml-training.csv", sep=',')
testing = read.csv("/Users/riehl/Downloads/pml-testing.csv", sep=',')

set.seed(12345)
#the training dataset is split into 2 with 70% for the training set and 30% for the test set
inTrain  = createDataPartition(y=training$classe, p=0.7, list=FALSE)
TrainSet = data[inTrain, ]
TestSet  = data[-inTrain, ]
#I now ask to show the number of variables in each set
dim(TrainSet)
dim(TestSet)

#Removing the variables with plenty of NA in both set
NAs    = sapply(TrainSet, function(x) mean(is.na(x))) > 0.95
TrainSet = TrainSet[, NAs==FALSE]
TestSet  = TestSet[, NAs==FALSE]
#I now ask to show the new number of variables in each set
dim(TrainSet)
dim(TestSet)

#Removing the Near Zero variance variables in both set
Near_Zero_Var = nearZeroVar(TrainSet)
TrainSet = training[, -Near_Zero_Var]
TestSet  = training[, -Near_Zero_Var]
#I now ask to show the new number of variables in each set
dim(TrainSet)
dim(TestSet)

#Correlation with the correlation matrix
CorrMatrix = cor(TrainSet[, -59])
corrplot(CorrMatrix, order = "FPC", method = "color", type = "upper", 
         tl.cex = 0.8, tl.col = rgb(0, 0, 0))

#Decision tree model
set.seed(12345)
modFit = train(classe ~ ., method="rpart", data=TrainSet)
print(modFit$finalModel)

predict(modFit$finalModel, newdata=TestSet)
#I now plot the decision tree
plot(modFit$finalModel, uniform=TRUE, main = "Classification Tree")
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=.8)
#the following instruction is to plot the decision tree in a clearer manner
fancyRpartPlot(modFit$finalModel)



#Random forest model
set.seed(12345)
controlRF = trainControl(method="cv", number=3, verboseIter=FALSE)
modFitRF = train(classe ~ ., data=TrainSet, method="rf",
                          trControl=controlRF)
modFitRF$finalModel
# Prediction of the RF model on TestSet
predictRF = predict(modFitRF, newdata=TestSet)
confMatRF = confusionMatrix(predictRF, TestSet$classe)
confMatRF
# Ploting the matrix results of the RF model
plot(confMatRF$table, col = confMatRF$byClass, 
     main = paste("RF Accuracy =",
                  round(confMatRF$overall['Accuracy'], 4)))

#Applying the model to the data
predictTEST = predict(modFitRF, newdata=testing)
predictTEST