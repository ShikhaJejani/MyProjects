setwd("C:/Users/aditya/Desktop/Shikha Desktop/Eduvancer data science/manufacturing")
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
manf_train=read.csv("product_train.csv",stringsAsFactors = F)
manf_test= read.csv("product_test.csv",stringsAsFactors = F)

head(manf_train)
class(manf_train$potential_issue)
table(manf_train$potential_issue)
#-------------Creating a Combine data from Test and Train-------

manf_test$went_on_backorder= NA
manf_train$data='train'
manf_test$data='test'
manf_all=rbind(manf_train,manf_test)

#----------Data Preparation-------------------------------------## ----
glimpse(manf_all)

#Check for NA and the unique values of each field
sapply(manf_all,  function(x) sum(is.na(x)))
sapply(manf_all,  function(x) length(unique(x)))
sapply(manf_train,  function(x) length(unique(x)))

#Converting YES/NO to numeric 1/0#

manf_all=manf_all %>% mutate(potential_issue=as.numeric(potential_issue=="Yes"),
                              deck_risk=as.numeric(deck_risk=="Yes"),
                              oe_constraint=as.numeric(oe_constraint=="Yes"),
                              ppap_risk=as.numeric(ppap_risk=="Yes"),
                              stop_auto_buy=as.numeric(stop_auto_buy=="Yes"),
                              rev_stop=as.numeric(rev_stop=="Yes"),
                              went_on_backorder=as.numeric(went_on_backorder=="Yes"))



#Checking for balanced and imbalanced data#
table(manf_train$went_on_backorder)

#See the plot if required
#library(ggplot2)
#ggplot(manf_all, aes(x = `Areaname`)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

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

glimpse(manf_all)

#Separating test and train#
manf_train=manf_all %>% filter(data=='train') %>% select(-data,-sku)
manf_test=manf_all %>% filter(data=='test') %>% select(-data,-went_on_backorder,-sku)

## ------------------------------------------------------------------------
set.seed(123)

s=sample(1:nrow(manf_train),0.7*nrow(manf_train))

train=manf_train[s,]
test=manf_train[-s,]

table(train$went_on_backorder)
table(test$went_on_backorder)


## ----Start of logistics regression--

library(car)
for_vif=lm(went_on_backorder~.,data=train)
summary(for_vif)
sort(vif(for_vif),decreasing = T)[1:3]

for_vif=lm(went_on_backorder~.-forecast_9_month-sales_6_month-forecast_6_month-sales_9_month-perf_12_month_avg-sales_1_month,data=train)
sort(vif(for_vif),decreasing = T)[1:3]


fit=glm(went_on_backorder~.-forecast_9_month-sales_6_month-forecast_6_month-sales_9_month-perf_12_month_avg-sales_1_month,data=train,family='binomial')

## ----
fit=step(for_vif)

formula(fit)
fit1=glm(went_on_backorder ~ national_inv + lead_time + in_transit_qty + 
            perf_6_month_avg + local_bo_qty + deck_risk + 
           ppap_risk + rev_stop , data=train, family = "binomial")
summary(fit1)

formula(fit1)

#check for max KS values
train$score=predict(fit1,newdata=train,type = "response")
glimpse(train)
min(train$score)
max(train$score)
## ----
library(ggplot2)
ggplot(train,aes(y=went_on_backorder,x=score,color=factor(went_on_backorder)))+
  geom_point()+geom_jitter()

## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
cutoff_data=data.frame(cutoff=0,TP=0,FP=0,FN=0,TN=0)
cutoffs=seq(0,1,length=100)

for (cutoff in cutoffs){
  predicted=as.numeric(train$score>cutoff)
  TP=sum(predicted==1 & train$went_on_backorder==1)
  FP=sum(predicted==1 & train$went_on_backorder==0)
  FN=sum(predicted==0 & train$went_on_backorder==1)
  TN=sum(predicted==0 & train$went_on_backorder==0)
  cutoff_data=rbind(cutoff_data,c(cutoff,TP,FP,FN,TN))
}
# lets remove the dummy data cotaining top row
cutoff_data=cutoff_data[-1,]

## ------------------------------------------------------------------------
cutoff_data=cutoff_data %>%
  mutate(P=FN+TP,N=TN+FP,Sn=TP/P, Sp=TN/N,dist=sqrt((1-Sn)**2+(1-Sp)**2)) %>%
  mutate(KS=abs((TP/P)-(FP/N))) %>%
  mutate(Accuracy=(TP+TN)/(P+N)) %>%
  select(-P,-N)

View(cutoff_data)
## ----warning=F,message=F-------------------------------------------------
## ------------------------------------------------------------------------
#Cutoff with max KS:
KS_cutoff=cutoff_data$cutoff[which(cutoff_data$KS==max(cutoff_data$KS))][1]
KS_cutoff
table(train$went_on_backorder,as.numeric(train$score>KS_cutoff))

(900+13224)/175054

#For test data
test$score=predict(fit1,newdata =test,type = "response")

test$score=as.numeric(test$score>KS_cutoff)
pred= test$score
real=test$went_on_backorder
TP=sum(pred== 1 & real==1)
TN=sum(pred==0 & real==0)
FP=sum(pred==1 & real==0)
FN=sum(pred==0 & real==1)
P=TP+FN
N=TN+FP
KS=abs((TP/P)-(FP/N))
KS
Total= P+N
Acc=((TP+TN)/(P+N))
Acc
table(test$score,test$went_on_backorder)

(5599+412)/75024
Predicted_test=predict(fit1,manf_test,type = "response")
manf_test$score=as.numeric(Predicted_test>KS_cutoff)

manf_test$went_on_backorder=ifelse(manf_test$score==1,'yes','no')

write.csv(manf_test$went_on_backorder,'shikha_agarwal_P3_part2.csv',row.names = F)

table(manf_test$went_on_backorder)

glimpse(bank_test)

##End of logistics regression##

# GBM 
gbm.fit=gbm(went_on_backorder~.,
            data=train,
            distribution="bernoulli",
            n.trees = 500 , interaction.depth = 3)

gbm.predicted=predict(gbm.fit,newdata=test,n.trees=500,type="response")
min(gbm.predicted)
max(gbm.predicted)
table(bd_test$y,gbm.predicted)
table(bd_test$y)
gbm.perf(gbm.fit)
summary()
