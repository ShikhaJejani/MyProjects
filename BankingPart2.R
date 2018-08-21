

setwd("C:/Users/aditya/Desktop/Shikha Desktop/Eduvancer data science/Banking project/")

getwd()
## ----
ld_train=read.csv("bank-full_train.csv",stringsAsFactors = FALSE)
ld_test= read.csv("bank-full_test.csv",stringsAsFactors = F)

##--table(ld_train$age)
#table(ld_train$job)
#table(ld_train$marital)
#table(ld_train$education)
#table(ld_train$default)
#sum(is.na(ld_train$balance))
#table(ld_train$housing)
#table(ld_train$loan)
#table(ld_train$contact)
#table(ld_train$day)
#table(ld_train$month)
#sum(is.na(ld_train$duration))
#table(ld_train$campaign)
#sum(is.na(ld_train$pdays))
#table(ld_train$previous)
#table(ld_train$poutcome)--##


ld_test$y=NA
ld_train$data='train'
ld_test$data='test'
ld_all=rbind(ld_train,ld_test)


library(dplyr)
glimpse(ld_all)

ld_all$loan=as.numeric(ld_all$loan=="yes")
ld_all$default=as.numeric(ld_all$default=="yes")
ld_all$housing=as.numeric(ld_all$housing=="yes")
## ----

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

char_logical=sapply(ld_all,is.character)
cat_cols=names(ld_all)[char_logical]
cat_cols
cat_cols=cat_cols[!(cat_cols %in% c('data','y'))]
cat_cols

for(col in cat_cols){
  ld_all=CreateDummies(ld_all,col,50)
}
glimpse(ld_all)

ld_all=ld_all[!((is.na(ld_all$y)) & ld_all$data=='train'), ]

#Removing NA records if any#
for(col in names(ld_all)){
  if(sum(is.na(ld_all[,col]))>0 & !(col %in% c("data","y"))){
    ld_all[is.na(ld_all[,col]),col]=mean(ld_all[ld_all$data=='train',col],na.rm=T)
  }
}
table(ld_all$y)
# to be run for regression 
ld_all=ld_all %>% mutate(y=as.numeric(y=="yes"))

#Removing outliers#
#qnt <- quantile(ld_all$balance, probs=c(.25, .75), na.rm = T)
#caps <- quantile(ld_all$balance, probs=c(.15,.85), na.rm = T)
#H <- 1.5 * IQR(ld_all$balance, na.rm = T)
#ld_all$balance[ld_all$balance < (qnt[1] - H)] <- caps[1]
#ld_all$balance[ld_all$balance > (qnt[2] + H)] <- caps[2]
#boxplot(ld_all$balance)

#boxplot(ld_all$age)
#qnt1 <- quantile(ld_all$age, probs=c(.25, .75), na.rm = T)
#caps1 <- quantile(ld_all$age, probs=c(.05,.95), na.rm = T)
#H <- 1.5 * IQR(ld_all$age, na.rm = T)
#ld_all$age[ld_all$age < (qnt1[1] - H)] <- caps1[1]
#ld_all$age[ld_all$age > (qnt1[2] + H)] <- caps1[2]
#boxplot(ld_all$age)

glimpse(ld_all)

#Separating test and train#
bank_train=ld_all %>% filter(data=='train') %>% select(-data,-ID)
bank_test=ld_all %>% filter(data=='test') %>% select(-data,-ID,-y)

## Logistics regression------------------------------------------------------------------------
set.seed(2)
s=sample(1:nrow(bank_train),0.7*nrow(bank_train))
train=bank_train[s,]
test=bank_train[-s,]

## ----Start of logistics regression--

library(car)
for_vif=lm(y~.,data=train)
sort(vif(for_vif),decreasing = T)[1:3]

for_vif=lm(y~.-month_may,data=train)
sort(vif(for_vif),decreasing = T)[1:3]

for_vif=lm(y~.-month_may-job_blue_collar-poutcome_unknown-education_secondary-contact_unknown,data=train)
sort(vif(for_vif),decreasing = T)[1:3]

summary(for_vif)
fit=glm(y~.-month_may-job_blue_collar-poutcome_unknown-education_secondary-contact_unknown,data=train,family='binomial')

## ----
fit=step(fit)

## ------------------------------------------------------------------------
formula(fit)

## ----
fit1=glm(y ~ age + balance + housing + loan  + duration + campaign + 
           pdays + previous + job_student + job_retired + 
           job_admin. + job_technician + job_management + marital_single  
            + education_primary + education_tertiary + 
           contact_cellular + month_mar + month_sep + month_oct + month_jan + 
           month_feb + month_apr + month_jun + month_jul + poutcome_other + 
           poutcome_failure, data=train, family = "binomial")
summary(fit1)
## ----

formula(fit1)

fit_final=glm(y ~ age + balance + housing + loan + duration + campaign + pdays + 
                previous + job_student + job_retired + job_admin. + job_technician + 
                job_management + marital_single + education_primary + education_tertiary + 
                contact_cellular + month_mar + month_sep + month_oct + month_jan + 
                month_feb + month_apr + month_jun + month_jul + poutcome_other + 
                poutcome_failure,family = "binomial",data=train)
summary(fit_final)

## ------------------------------------------------------------------------
train$score=predict(fit_final,newdata=train,type = "response")
glimpse(train)
## ----
library(ggplot2)
ggplot(train,aes(y=y,x=score,color=factor(y)))+
  geom_point()+geom_jitter()

## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
cutoff_data=data.frame(cutoff=0,TP=0,FP=0,FN=0,TN=0)
cutoffs=seq(0,1,length=100)

for (cutoff in cutoffs){
  predicted=as.numeric(train$score>cutoff)
  TP=sum(predicted==1 & train$y==1)
  FP=sum(predicted==1 & train$y==0)
  FN=sum(predicted==0 & train$y==1)
  TN=sum(predicted==0 & train$y==0)
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
table(train$y,as.numeric(train$score>KS_cutoff))

(417+3699)/22152

#For test data
test$score=predict(fit_final,newdata =test,type = "response")

test$score=as.numeric(test$score>KS_cutoff)
pred= test$score
real=test$y
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
table(test$score,test$y)


Predicted_test=predict(fit_final,bank_test,type = "response")
bank_test$score=as.numeric(Predicted_test>KS_cutoff)

bank_test$y=ifelse(bank_test$score==1,'yes','no')

write.csv(bank_test$y,'shikha_agarwal_P5_part2(9).csv',row.names = F)


glimpse(bank_test)

##End of logistics regression##

##Random Forest##
library(tree)
library(ISLR)
library(dplyr)
library(randomForest)

ld_train=ld_train %>%
  mutate(y=as.factor(y))

glimpse(bd_train)
## ------------------------------------------------------------------------
set.seed(2)
s=sample(1:nrow(ld_train),0.8*nrow(ld_train))
bd_train=ld_train[s,]
bd_test=ld_train[-s,]
glimpse(bd_train)



fit_rf=randomForest(y~.,data=bd_train,na.action=na.exclude)
#fir_rf=randomForest(y~.,data=bd_train,ntree=500,mtry=5)

fit_rf

forest.pred.train=predict(fit_rf,newdata=bd_train)

table(bd_train$y,forest.pred.train)

(308)/31647
165/22152
238/25317


forest.pred.test=predict(fit_rf,newdata=bd_test)

table(bd_test$y,forest.pred.test)

(754+183)/(9495)
(476+127)/6330
importance(fit_rf)

varImpPlot(fit_rf)

ld_test$y=predict(fit_rf,newdata=ld_test)
table(ld_test$y)
write.csv(ld_test$y,'shikha_agarwal_P5_part2(8).csv',row.names = F)

#Boosting#
library(dplyr)
library(gbm)
library(randomForest)
library(ggplot2)
library(cvTools)
library(xgboost)
ld_train$y=as.numeric(ld_train$y=='yes')

table(ld_train$y)

glimpse(ld_train)
## ------------------------------------------------------------------------
set.seed(2)
s=sample(1:nrow(ld_train),0.7*nrow(ld_train))
bd_train=ld_train[s,]
bd_test=ld_train[-s,]
glimpse(bd_train)

# GBM 
gbm.fit=gbm(y~.,
            data=bd_train,
            distribution="bernoulli",
            n.trees = 500 , interaction.depth = 3)

gbm.predicted=predict(gbm.fit,newdata=bd_test,n.trees=500,type="response")
min(gbm.predicted)
max(gbm.predicted)
table(bd_test$y,gbm.predicted)
table(bd_test$y)
gbm.perf(gbm.fit)
summary()


# xgboost
x_train=ld_train %>% select(-y)
y_train=ld_train$y
x_test=ld_test %>% select(-y)
xgb.fit=xgboost(data=data.matrix(x_train),
                label = y_train,
                objective='binary:logistic',
                verbose=1,
                nrounds = 10)
glimpse(ld_train)

test.predicted=predict(xgb.fit,data.matrix(x_test),type="response")
test.predicted=as.numeric(test.predicted>0.32)
table(gbm_test$y,test.predicted)

