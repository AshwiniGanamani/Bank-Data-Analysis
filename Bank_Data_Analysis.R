## Bank TeleMarketing Analysis


BankData <- read.csv("//Users//ashwinichowdhary//Documents//Fall2016//BA//DataSets//bank//BankDataCleaned.csv")
BankAdditional <- read.csv("//Users//ashwinichowdhary//Documents//Fall2016//BA//DataSets//bank//bank-additional.csv")

summary(BankData)
summary(BankAdditional)
str(BankAdditional)
str(BankData)


## Naive Bayes Analysis 

##Devloped a Test and Train data. 
##Confusion Matrix to show the predicted vs actual values. 
##Accuracy of the prediction is calculated.

library(caret)
set.seed(500)

fortrain <- createDataPartition(BankData$y, p=0.6,list = FALSE)
TrainData <- BankData[fortrain,]
TestData <- BankData[-fortrain,]
nrow(TrainData)
nrow(TestData)
prop.table(table(TrainData$y))
prop.table(table(TestData$y))

table(BankData$y)
library(e1071)
model <- naiveBayes(y ~ ., data = TrainData)

predict <- predict(model, TestData)
head(predict, n=5)
print(confusionMatrix(predict, TestData$y, positive = "yes", dnn = c("Prediction", "True")))

##Decision Tree Analysis

##Developed a decision tree for the Subscription, to see the clasification of the subscription. Included a confusion matrix to see the difference in predicted and actual values.

library(rpart)
library(rpart.plot)
Decision <- rpart(y ~.,data=BankAdditional,method="class")
rpart.plot(Decision, type = 1, extra=4, box.col = "cadetblue2", shadow.col = "darkgray")

MainDecision <- rpart(y ~.,data=BankData,method="class")
rpart.plot(MainDecision, type = 1, extra=4, box.col = "yellow", shadow.col = "darkgray")

conf.matrix <- table(BankData$y, predict(MainDecision, type = "class"))
conf.matrix

##Logistic Regression Analysis 

##Since the outcome here has only two values possible, hence used the logistic regression instead of a linear regression.
##Develope a Regression model to see the list of significant variables that are dependent on 'Subscription'. 

Logisticmodel <- glm(BankData$y ~ .,data=BankData,family= binomial(link= "logit"))
summary(Logisticmodel )
anova(Logisticmodel, test ="Chisq")
plot(BankData$y,fitted.values(Logisticmodel))

VariableImportance<-varImp(Logisticmodel)
VariableImportance


