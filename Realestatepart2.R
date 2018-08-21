setwd("C:/Users/aditya/Desktop/Shikha Desktop/Eduvancer data science/Real estate project/")
ld_train=read.csv("C:/Users/aditya/Desktop/Shikha Desktop/Eduvancer data science/Real estate project/housing_train.csv",stringsAsFactors = FALSE)
ld_test=read.csv("C:/Users/aditya/Desktop/Shikha Desktop/Eduvancer data science/Real estate project/housing_test.csv",stringsAsFactors = FALSE)

ld_train=ld_train[,c(1,2,3,4,6,7,8,9,10,11,12,13,14,15,16,5)]

library(dplyr)
#View(ld_train)
#View(ld_test)
ld_test$Price=NA
ld_train$data='train'
ld_test$data='test'
ld_all=rbind(ld_train,ld_test)

#View(ld_all)

## ----
##--sort(table(ld_all$Suburb))
#sort(table(ld_all$Address))
#sort(table(ld_all$SellerG))
#sort(table(ld_all$Type))
#sort(table(ld_all$Method))
#sort(table(ld_all$CouncilArea))
#sort(table(ld_all$YearBuilt))
#sort(table(ld_all$Bedroom2))
#(table(ld_all$Landsize))
#sapply(ld_train,  function(x) sum(is.na(x)))
#sapply(ld_train,  function(x) length(unique(x)))
##summary(ld_all$Bedroom2)--##

glimpse(ld_all)
#Imputing the NA and blanks with some values before creating the dummies#
ld_all$Car[is.na(ld_all$Car)] <- mean(ld_all$Car, na.rm = T)
ld_all$Bedroom2[is.na(ld_all$Bedroom2)] <- mean(ld_all$Bedroom2, na.rm = T)
ld_all$Bathroom[is.na(ld_all$Bathroom)] <- mean(ld_all$Bathroom, na.rm = T)
ld_all$Landsize[is.na(ld_all$Landsize)] <- mean(ld_all$Landsize, na.rm = T)
ld_all$CouncilArea[ld_all$CouncilArea==""]<- "Boroondara"

CreateDummies=function(data,var,freq_cutoff=50){
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

cat_cols=cat_cols[!(cat_cols %in% c('data','Address',"SellerG","Suburb"))]
cat_cols

for(col in cat_cols){
  ld_all=CreateDummies(ld_all,col,50)
}
glimpse(ld_all)             

ld_train=ld_all %>% filter(data=='train') %>% select(-data,-Address,-BuildingArea,
                                                     -YearBuilt,-Suburb,-SellerG)
ld_test=ld_all %>% filter(data=='test') %>% select(-data,-Price,-Address,-BuildingArea,
                                                   -YearBuilt,-Suburb,-SellerG)
## ------------------------------------------------------------------------

set.seed(2)
s=sample(1:nrow(ld_train),0.7*nrow(ld_train))
bd_train=ld_train[s,]
bd_test=ld_train[-s,]

## ------------------------------------------------------------------------

fit=lm(Price~.,data=bd_train)

library(car)
sort(vif(fit),decreasing = T)[1:3]
fit=lm(Price~.-CouncilArea_Boroondara,data=bd_train)
sort(vif(fit),decreasing = T)[1:3]
## ------------------------------------------------------------------------
summary(fit)

## ------------------------------------------------------------------------
fit=lm(Price~.-CouncilArea_Boroondara-CouncilArea_Stonnington,data=bd_train)
summary(fit)
fit=lm(Price~.-CouncilArea_Boroondara-CouncilArea_Stonnington-CouncilArea_HobsonsBay,data=bd_train)
summary(fit)
fit=lm(Price~.-CouncilArea_Boroondara-CouncilArea_Stonnington-CouncilArea_HobsonsBay-Method_SP,data=bd_train)
summary(fit)
fit=lm(Price~.-CouncilArea_Boroondara
        -CouncilArea_Stonnington-CouncilArea_HobsonsBay-Method_SP-CouncilArea_Manningham,data=bd_train)
summary(fit)
fit=lm(Price~.-CouncilArea_Boroondara
       -CouncilArea_Stonnington-CouncilArea_HobsonsBay-Method_SP-CouncilArea_Manningham -CouncilArea_Whitehorse ,data=bd_train)
summary(fit)
## ------------------------------------------------------------------------
library(ggplot2)
ld_train %>%
  mutate(pred_Price=predict(fit,newdata=ld_train)) %>%
  ggplot(aes(x=Price,y=pred_Price))+geom_point(alpha=0.6)

## ------------------------------------------------------------------------
model_string=paste(fit$coefficients,names(fit$coefficients),sep="*",collapse = " + ")
strwrap(sub("*(Intercept)","",gsub("+ -","- ",model_string,fixed=TRUE)))

## ------------------------------------------------------------------------
plot(fit,which=1)

## ------------------------------------------------------------------------
plot(fit,which=2)

## ------------------------------------------------------------------------
df=data.frame(res=fit$residual)
ggplot(df,aes(x=res))+geom_density(color="red")+
  stat_function(fun=dnorm ,aes(x=res),color="green")
shapiro.test(fit$residuals)
library(nortest)
ad.test(aes(x=res))
## ------------------------------------------------------------------------
plot(fit,which=3)

## ------------------------------------------------------------------------
plot(fit,which=4)

## ------------------------------------------------------------------------
#rmse for train
mean((bd_train$Price-predict(fit,newdata=bd_train))**2) %>%
  sqrt()
#rmse for test
mean((bd_test$Price-predict(fit,newdata=bd_test))**2) %>%
  sqrt()

## ----echo=FALSE----------------------------------------------------------
# normal scatter plot
ld_temp=ld_train[ld_train$Monthly.Income<25000,]
ggplot(ld_temp,aes(x=fico,y=Interest.Rate))+geom_point()+ggtitle("Normal Scatter Plot")

## ------------------------------------------------------------------------
# Scatter Plot with density mapped
ggplot(ld_temp,aes(x=fico,y=Interest.Rate))+geom_point(alpha=0.4,size=5)

## ------------------------------------------------------------------------
#scatter plot with hex binning or 2d binning , you'll need to install package "hexbin" for the same
ggplot(ld_temp,aes(x=fico,y=Interest.Rate))+stat_binhex()
ggplot(ld_temp,aes(x=fico,y=Interest.Rate))+stat_bin2d()
## ------------------------------------------------------------------------
predicted=predict(fit,newdata=md_train)
RMSE=(predicted-md_train$shares)**2 %>% 
  mean() %>% 
  sqrt()
RMSE


##DECISION TREE##
library(tree)
library(ISLR)
library(dplyr)
## ------------------------------------------------------------------------
tree.housing=tree(Price~.,data=bd_train)
plot(tree.housing)
text(tree.housing,pretty=0)

price_pred_test=predict(tree.housing,newdata=bd_test)
price_pred_train=predict(tree.housing,newdata=bd_train)

rmse_train =sqrt(mean((price_pred_train-bd_train$Price)^2))
rmse_test =sqrt(mean((price_pred_test-bd_test$Price)^2))

rmse_train
rmse_test

## ------------------------------------------------------------------------
cv.tree.housing=cv.tree(tree.housing)
plot(cv.tree.housing$size,cv.tree.housing$dev,type='b')

## ------------------------------------------------------------------------
prune.rt.housing=prune.tree(tree.housing,best=12)

plot(prune.rt.housing)
text(prune.rt.housing,pretty=0)
prune.rt.housing

## ------------------------------------------------------------------------
price_pred_train=predict(prune.rt.housing,newdata =bd_train )
price_pred_test=predict(prune.rt.housing,newdata=bd_test)

rmse_train =sqrt(mean((price_pred_train-bd_train$Price)^2))
rmse_test =sqrt(mean((price_pred_test-bd_test$Price)^2))

rmse_train
rmse_test


##Random Forest##
library(randomForest)
rf_ld=randomForest(Price~.,data=bd_train)
View((bd_train))
rf_ld

## ------------------------------------------------------------------------
price_pred_train_rf=predict(rf_ld,newdata =bd_train )
price_pred_test_rf=predict(rf_ld,newdata=bd_test)

rmse_train_rf =sqrt(mean((price_pred_train_rf-bd_train$Price)^2))
rmse_test_rf =sqrt(mean((price_pred_test_rf-bd_test$Price)^2))

rmse_train_rf
212467/rmse_train_rf
rmse_test_rf
212467/rmse_test_rf
## ------------------------------------------------------------------------
importance(rf_ld)
varImpPlot(rf_ld)


ld_test$Price=predict(rf_ld,newdata=ld_test)

table(ld_test$Price)
write.csv(ld_test$Price,'shikha_agarwal_P1_part2(1).csv',row.names = F)


# GBM 
library(dplyr)
library(gbm)
library(randomForest)
library(ggplot2)
library(cvTools)
library(xgboost)
gbm.fit=gbm(Price~.,
            data=bd_train,
            distribution = "gaussian",
            n.trees = 500,interaction.depth = 3)


price_pred_train_gbm=predict.gbm(gbm.fit,newdata =bd_train ,n.trees=500)
price_pred_test_gbm=predict.gbm(gbm.fit,newdata=bd_test,n.trees=500)

rmse_train_gbm =sqrt(mean((price_pred_train_gbm-bd_train$Price)^2))
rmse_test_gbm =sqrt(mean((price_pred_test_gbm-bd_test$Price)^2))

212467/rmse_test_gbm
212467/rmse_train_gbm

# xgboost
x_train=bd_train %>% select(-Price)
y_train=bd_train$Price
x_test=bd_test %>% select(-Price)
xgb.fit=xgboost(data=data.matrix(x_train),
                label = y_train,
                objective='reg:linear',
                verbose=1,
                nrounds = 10)


price_pred_train_xg=predict(xgb.fit,data.matrix(x_test))
price_pred_test_xg=predict(xgb.fit,data.matrix(x_test))

rmse_train_xg =sqrt(mean((price_pred_train_xg-bd_train$Price)^2))
rmse_test_xg =sqrt(mean((price_pred_test_xg-bd_test$Price)^2))

212467/rmse_test_gbm
212467/rmse_train_gbm

