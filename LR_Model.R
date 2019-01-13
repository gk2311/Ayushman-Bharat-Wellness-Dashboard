install.packages("magic")
install.packages("caret")
install.packages("randomForest")
install.packages("e1071")
install.packages("pROC")

library("sqldf")
library("magic")
library("randomForest")
library("e1071")
library("gbm")
library("kernlab")
library("caret")

HeartRiskAssement <- read_excel("Paytm/HeartRiskAssement.xlsx")
View(HeartRiskAssement)

HeartRiskAssement$num[HeartRiskAssement$num > 0] <- 1
barplot(table(HeartRiskAssement$num),
        main="Fate", col="black")
summary(HeartRiskAssement)

HeartRiskAssement$num <- as.factor(HeartRiskAssement$num)


set.seed(10)
inTrainRows <- createDataPartition(HeartRiskAssement$num,p=0.7,list=FALSE)
trainData <- HeartRiskAssement[inTrainRows,]
testData <-  HeartRiskAssement[-inTrainRows,]
nrow(trainData)/(nrow(testData)+nrow(trainData))

AUC = list()
Accuracy = list()

set.seed(1230)
logRegModel <- train(num ~ ., data=trainData, method = 'glm', family = 'binomial')
logRegPrediction <- predict(logRegModel, testData)
logRegPredictionprob <- predict(logRegModel, testData, type='prob')[2]
logRegConfMat <- confusionMatrix(table(logRegPrediction, testData$num))
logRegPrediction <- as.data.frame(logRegPrediction)
HeartRiskAssementfinal <- cbind(testData, logRegPredictionprob, logRegPrediction)


#ROC Curve
library(pROC)
AUC$logReg <- roc(as.numeric(testData$num),as.numeric(as.matrix((logRegPredictionprob))))$auc
Accuracy$logReg <- logRegConfMat$overall['Accuracy']

#writng results to csv
x <- write.csv(HeartRiskAssementfinal, file = "HeartRiskAssementfinal.csv")
