setwd("C:/Users/aditya/Desktop/Shikha Desktop/Eduvancer data science/HR Project/")
getwd()

#--------------Libraries-----------------------------

library(dplyr)
library(car)
library(gbm)
library(randomForest)
library(ggplot2)
library(cvTools)
library(xgboost)

#--------------Assignment of data--------------------
hr_train=read.csv("hr_train.csv",stringsAsFactors = FALSE)
hr_train=hr_train[,c(1,2,3,4,5,6,8,9,10,7)]
hr_test= read.csv("hr_test.csv",stringsAsFactors = F)


#-------------Creating a Combine data from Test and Train-------

hr_test$left= NA
hr_train$data='train'
hr_test$data='test'
hr_all=rbind(hr_train,hr_test)

#----------Data Preparation-------------------------------------## ----
glimpse(hr_test)
glimpse(hr_train)
#Check for NA and the unique values of each field
sapply(hr_all,  function(x) sum(is.na(x)))
sapply(hr_all,  function(x) length(unique(x)))
sapply(hr_train,  function(x) length(unique(x)))

#Checking for balanced and imbalanced data#
table(hr_train$left)


#Dummy creation funtion#
CreateDummies=function(data,var,freq_cutoff=100){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    name=gsub("/","_",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  data[,var]=NULL
  return(data)
}

glimpse(hr_all)
table(hr_train$salary)
hr_all=CreateDummies(hr_all,'sales',20)
hr_all=CreateDummies(hr_all,'salary',20)


glimpse(hr_all)

#Separating test and train#
hr_train=hr_all %>% filter(data=='train') %>% select(-data)
hr_test=hr_all %>% filter(data=='test') %>% select(-data,-left)

## ------------------------------------------------------------------------
set.seed(2)

s=sample(1:nrow(hr_train),0.7*nrow(hr_train))

train=hr_train[s,]

test=hr_train[-s,]


glimpse(train)

## ----Start of logistics regression--

library(car)
for_vif=lm(left~.,data=train)

sort(vif(for_vif),decreasing = T)[1:3]


fit=glm(left~.,data=train,family='binomial')

## ----
fit=step(fit)

## ------------------------------------------------------------------------
formula(fit)

## ----
fit1=glm(left ~ satisfaction_level + last_evaluation + number_project + 
           average_montly_hours + time_spend_company + Work_accident + 
           promotion_last_5years + sales_hr + sales_accounting + 
           salary_medium + salary_low, data=train,family = "binomial")
summary(fit1)

formula(fit1)
## -Testing-----------------------------------------------------------------------
hr_test$score=predict(fit1,newdata = hr_test,type = "response")
train$score=predict(fit1,newdata = train,type = "response")
test$score=predict(fit1,newdata = test,type = "response")

#check for ROC values
library('ROCR')

pred = prediction(train$score, train$left)
roc = performance(pred, "tpr", "fpr")

plot(roc, lwd=2, colorize=TRUE)
lines(x=c(0, 1), y=c(0, 1), col="black", lwd=1)

auc = performance(pred, "auc")
auc = unlist(auc@y.values)
auc

pred = prediction(test$score, test$left)
roc = performance(pred, "tpr", "fpr")

plot(roc, lwd=2, colorize=TRUE)
lines(x=c(0, 1), y=c(0, 1), col="black", lwd=1)

auc = performance(pred, "auc")
auc = unlist(auc@y.values)
auc

##End of logistics regression##

write.csv(hr_test$score,'shikha_agarwal_P2_part2(5).csv',row.names = F)
## ------------------------------------------------------------------------


##Random Forest##
library(tree)
library(ISLR)
library(dplyr)
library(randomForest)

train=train %>%
  mutate(left=as.factor(left))
test=test %>%
  mutate(left=as.factor(left))


fit_rf=randomForest(left~.,data=train,na.action=na.exclude)
#fir_rf=randomForest(y~.,data=bd_train,ntree=500,mtry=5)

fit_rf
train$score=predict(fit_rf,newdata =train,type = "prob")[,1]
test$score=predict(fit_rf,newdata =test,type = "prob")[,1]
forest.pred.train=predict(fit_rf,newdata=train,type='prob')[,1]
pROC::roc(train$left,forest.pred.train)
table(train$left,forest.pred.train)

library('ROCR')
pred = prediction(train$score, train$left)
roc = performance(pred, "tpr", "fpr")

plot(roc, lwd=2, colorize=TRUE)
lines(x=c(0, 1), y=c(0, 1), col="black", lwd=1)

auc = performance(pred, "auc")
auc = unlist(auc@y.values)
auc


forest.pred.test=predict(fit_rf,newdata=test,type='prob')[,1]
pROC::roc(test$left,forest.pred.test)
table(test$left,forest.pred.test)


pred = prediction(test$score, test$left)
roc = performance(pred, "tpr", "fpr")

plot(roc, lwd=2, colorize=TRUE)
lines(x=c(0, 1), y=c(0, 1), col="black", lwd=1)

auc = performance(pred, "auc")
auc = unlist(auc@y.values)
auc

importance(fit_rf)

varImpPlot(fit_rf)


# xgboost
x_train=train %>% select(-left)
y_train=train$left
x_test=test %>% select(-left)

xgb.fit=xgboost(data=data.matrix(x_train),
                label = y_train,
                objective='binary:logistic',
                verbose=1,
                nrounds = 10)
glimpse(train)

train.predicted=predict(xgb.fit,data.matrix(x_train),type="response")
test.predicted=predict(xgb.fit,data.matrix(x_test),type="response")
library('ROCR')
pred = prediction(train.predicted, train$left)
roc = performance(pred, "tpr", "fpr")

plot(roc, lwd=2, colorize=TRUE)
lines(x=c(0, 1), y=c(0, 1), col="black", lwd=1)

auc = performance(pred, "auc")
auc = unlist(auc@y.values)
auc

hr_test.predicted=predict(xgb.fit,data.matrix(hr_test),type="response")
write.csv(hr_test.predicted,'shikha_agarwal_P4_part2.csv',row.names = F)
