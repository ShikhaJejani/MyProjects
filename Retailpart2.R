setwd("C:/Users/aditya/Desktop/Shikha Desktop/Eduvancer data science/Retail Project/")
getwd()

#--------------Libraries-----------------------------

library(dplyr)
library(car)

#--------------Assignment of data--------------------
store_train=read.csv("store_train.csv",stringsAsFactors = FALSE)
store_test= read.csv("store_test.csv",stringsAsFactors = F)


#-------------Creating a Combine data from Test and Train-------

store_test$store = NA
store_train$data='train'
store_test$data='test'
store_all=rbind(store_train,store_test)

#----------Data Preparation-------------------------------------## ----
glimpse(store_all)

#Check for NA and the unique values of each field
sapply(store_all,  function(x) sum(is.na(x)))
sapply(store_all,  function(x) length(unique(x)))
sapply(store_train,  function(x) length(unique(x)))

#extracting feature from storecode#
store_all$storecode=substr(store_all$storecode,0,4)

#Imputing the NA and blanks with some values before creating the dummies#
store_all$country[is.na(store_all$country)] <- mean(store_all$country, na.rm = T)
store_all$population[is.na(store_all$population)] <- mean(store_all$population, na.rm = T)

table(store_train$store)
round(prop.table(table(store_train$country,store_train$store),1),1)

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

char_logical=sapply(store_all,is.character)
cat_cols=names(store_all)[char_logical]
cat_cols
cat_cols=cat_cols[!(cat_cols %in% c('data','Areaname'))]
cat_cols

for(col in cat_cols){
store_all=CreateDummies(store_all,col,50)
}
glimpse(store_all)

store_all=CreateDummies(store_all,'country',50)
store_all=CreateDummies(store_all,'CouSub',50)
#Separating test and train#
store_train=store_all %>% filter(data=='train') %>% select(-data,-Id,-Areaname,-State)
store_test=store_all %>% filter(data=='test') %>% select(-data,-store,-Id,-Areaname,-State)

## ------------------------------------------------------------------------
set.seed(2)

s=sample(1:nrow(store_train),0.7*nrow(store_train))

train=store_train[s,]

test=store_train[-s,]


glimpse(train)

## ----Start of logistics regression--

library(car)

for_vif=lm(store~.,data=train)
summary(for_vif)
sort(vif(for_vif),decreasing = T)[1:3]

for_vif=lm(store~.-sales0-sales2-sales3,data=train)
sort(vif(for_vif),decreasing = T)[1:3]

fit=glm(store~.-sales0-sales2-sales3,data=train,family='binomial')

## ----
fit=step(fit)

## ------------------------------------------------------------------------
formula(fit)

## ----
#This code gave .83 score
fit1=glm(store ~ sales1 + population + countyname_WorcesterCounty + storecode_NCNT + 
           +             state_alpha_MS + state_alpha_IN + state_alpha_TN + 
           +            state_alpha_NC + state_alpha_IL +  state_alpha_VT + 
           +            country_13 + country_27 + country_5  , data=train, family = "binomial")
summary(fit1)
  
formula(fit1)

#the model submitted and the result is 76 percent
fit_final=glm(store ~ sales4 + population + countyname_PenobscotCounty + state_alpha_WV + 
                state_alpha_CA + state_alpha_CO + state_alpha_PR + state_alpha_IN + 
                state_alpha_TN + state_alpha_VT + state_alpha_NH + state_alpha_MA + 
                country_11, data=train, family = "binomial")
summary(fit_final)

## -Testing-----------------------------------------------------------------------
store_test$score=predict(fit1,newdata = store_test,type = "response")
train$score=predict(fit1,newdata = train,type = "response")
test$score=predict(fit1,newdata = test,type = "response")

write.csv(store_test$score,'shikha_agarwal_P2_part2(4).csv',row.names = F)
## ------------------------------------------------------------------------

library('ROCR')
install.packages("ROCR")
pred = prediction(train$score, train$store)
roc = performance(pred, "tpr", "fpr")

plot(roc, lwd=2, colorize=TRUE)
lines(x=c(0, 1), y=c(0, 1), col="black", lwd=1)

auc = performance(pred, "auc")
auc = unlist(auc@y.values)
auc

pred = prediction(test$score, test$store)
roc = performance(pred, "tpr", "fpr")

plot(roc, lwd=2, colorize=TRUE)
lines(x=c(0, 1), y=c(0, 1), col="black", lwd=1)

auc = performance(pred, "auc")
auc = unlist(auc@y.values)
auc

##End of logistics regression##

## ----

##Random Forest##
library(tree)
library(ISLR)
library(dplyr)
library(randomForest)

train=train %>%
  mutate(store=as.factor(store))
test=test %>%
  mutate(store=as.factor(store))

fit_rf=randomForest(store~.,data=train,na.action=na.exclude,ntree=1000,mtry=5)
#fir_rf=randomForest(y~.,data=bd_train,ntree=500,mtry=5)

fit_rf
forest.pred.train=predict(fit_rf,newdata=train,type='prob')[,1]
pROC::roc(train$store,forest.pred.train)


forest.pred.test=predict(fit_rf,newdata=test,type='prob')[,1]
pROC::roc(test$store,forest.pred.test)
table(test$store,forest.pred.test)

importance(fit_rf)

varImpPlot(fit_rf)

store_test$store=predict(fit_rf,newdata=store_test,type='prob')[,1]

write.csv(store_test$store,'shikha_agarwal_P2_part2(2).csv',row.names = F)

