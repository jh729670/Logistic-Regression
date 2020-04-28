library(dplyr)
library(tidyverse)
library(caret)
library(purrr)
install.packages('e1071')
install.packages('ROCR')
library(ROCR)
library(ggplot2)
install.packages('caTools')
library(caTools)
#Reads Logisitc Data
data<-read_csv('LogisticData_1(1).csv')
#Basic Summary statistics
summary(data)
str(data)
plot(data$Y)

#Set up Cross Validation settings
crossValsettings<-trainControl(method='repeatedcv',number=10,savePredictions = TRUE)

#Cross validates data 10 times, taking different samples to train and test 
#Prints summary of results, including sample sizes, and accuracy
crossval=train(as.factor(Y)~., data=data, family='binomial', method='glm', trControl=crossValsettings)
#Makes predictions based off of cross validated model and original data
predict<-predict(crossval, new_data=data)
#Converts Y into a factor to produce confusion table results
data$Y<-as.factor(data$Y)
#Creates confusion matrix of TP, TN, FP,FN. Prints accuracy, sensitivity,and specificity
cf_table<-confusionMatrix(predict,data$Y)

#Create ROC Curve

#Creates a glm model
glm_model<-glm(Y~., data=data,family='binomial')

#Creates predictions, gives numerical predictions. 
roc_predict<-predict(glm_model,data=data, type='response')

prediction<-prediction(roc_predict,data$Y)

roc_performance<-performance(prediction, measure='tpr',x.measure='fpr')


#Plots ROC curve
plot(roc_performance, col='blue')

#check accuracy using table
table(data$Y,roc_predict>=0.5)
