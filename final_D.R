#data understanding and preprocessing----
rm(list=ls())
setwd("D:/RStudio_Project/R_S3")
data <- read.csv("heart_failure_clinical_records_dataset.csv",encoding = "UTF-8")

#correlation
library(corrplot)
a <- cor(data)
a
par(mfrow=c(1,1)) 
corrplot(a,method = 'color',ti.col = 9,tl.cex = 0.7,order = 'AOE')
str(data)
summary(data)
data$DEATH_EVENT <- as.numeric(data$DEATH_EVENT)

#Feature selection(statistical method)
#chi-square
chisq.test(data$anaemia,data$DEATH_EVENT)
chisq.test(data$diabetes,data$DEATH_EVENT)
chisq.test(data$high_blood_pressure,data$DEATH_EVENT)
chisq.test(data$sex,data$DEATH_EVENT)
chisq.test(data$smoking,data$DEATH_EVENT)
#ks test
shapiro.test(data$age)
shapiro.test(data$creatinine_phosphokinase)
shapiro.test(data$ejection_fraction)
shapiro.test(data$platelets)
shapiro.test(data$serum_creatinine[data$DEATH_EVENT == 0])
shapiro.test(data$serum_sodium[data$DEATH_EVENT == 0])
shapiro.test(data$time[data$DEATH_EVENT == 0])
#qqplot
library(ggpubr)
par(mfrow=c(2,2)) 
layout(matrix(c(1,3,2,3,3,1), 3, 3, byrow = TRUE))
ggqqplot(data$age)
ggqqplot(data$creatinine_phosphokinase)
ggqqplot(data$ejection_fraction)
ggqqplot(data$platelets)
ggqqplot(data$serum_creatinine)
ggqqplot(data$serum_sodium)
ggqqplot(data$time)
# Mann–Whitney U test and t test
wilcox.test(age~DEATH_EVENT, data = data)
wilcox.test(ejection_fraction~DEATH_EVENT, data = data)
wilcox.test(serum_creatinine~DEATH_EVENT, data = data)
wilcox.test(platelets~DEATH_EVENT, data = data)
wilcox.test(serum_sodium~DEATH_EVENT, data = data)
wilcox.test(creatinine_phosphokinase~DEATH_EVENT,data = data)
wilcox.test(time~DEATH_EVENT, data = data)

str(data)
summary(data)
dim(data)
sum(data$DEATH_EVENT== 0)

library(ggplot2)
library(caret)
library(tidyverse)
#numerical distribution
install.packages("ggpubr")
library("ggpubr")
par(mfrow=c(2,2)) 
d_age<-ggplot(data=data,aes(x = age,fill = DEATH_EVENT))+
  geom_histogram(bins = 12)+theme(panel.grid = element_blank(),
                                  axis.title.x = element_text(size = 13),axis.title.y = element_text(size = 13))+
  geom_freqpoly(bins = 15, boundary = 0.0025)
d_age
#creatinine_phosphokinase
d_creatinine_phosphokinase<-ggplot(data=data,aes(x = creatinine_phosphokinase, fill = DEATH_EVENT))+
  geom_histogram(bins = 12)+theme(panel.grid = element_blank(),
                                  axis.title.x = element_text(size = 15),axis.title.y = element_text(size = 15)) 
d_creatinine_phosphokinase

#ejection_fraction
d_ejection_fraction<-ggplot(data=data,aes(x = ejection_fraction,y = stat(density)))+
  geom_histogram(bins = 12)+theme(panel.grid = element_blank(),
                                  axis.title.x = element_text(size = 15),axis.title.y = element_text(size = 15))+
  geom_density(color = "blue", size = 1)
d_ejection_fraction
#platelets
d_platelets<-ggplot(data=data,aes(x = platelets,y = stat(density)))+
  geom_histogram(bins = 12)+theme(panel.grid = element_blank(),
                                  axis.title.x = element_text(size = 15),axis.title.y = element_text(size = 15))+
  geom_density(color = "blue", size = 1)
d_platelets

#serum_creatinine
d_serum_creatinine<-ggplot(data=data,aes(x = serum_creatinine))+
  geom_histogram(bins = 12)+theme(panel.grid = element_blank(),
                                  axis.title.x = element_text(size = 15),axis.title.y = element_text(size =15))
d_serum_creatinine
#serum_sodium
d_serum_sodium<-ggplot(data=data,aes(x = serum_sodium))+
  geom_histogram(bins = 12)+theme(panel.grid = element_blank(),
                                  axis.title.x = element_text(size = 15),axis.title.y = element_text(size = 15))
d_serum_sodium
ggarrange(d_creatinine_phosphokinase,d_ejection_fraction,d_platelets,ncol=3,nrow=2,labels=c("A","B","C","D","E"))
#time
d_time<-ggplot(data=data,aes(x = time,fill = DEATH_EVENT))+
  geom_histogram(bins = 12)+theme(panel.grid = element_blank(),
                                  axis.title.x = element_text(size = 13),axis.title.y = element_text(size = 13))
d_time
#EVENT
d_EVENT <- tibble(
  DEATH_EVENT = c("0", "1"),
  freq = c(170, 129)
)
p <- ggplot(data = d_EVENT, mapping = aes(
  x = DEATH_EVENT, y = freq ))
p + geom_col()+ylim(0,250)+theme(panel.grid = element_blank(),
                                       axis.title.x = element_text(size =10),axis.title.y = element_text(size = 12))+
  geom_text(aes(label= freq,y = freq+6))


#boxplot
par(mfrow=c(3,3)) 
boxplot(data$age, xlab="age")
boxplot(data$creatinine_phosphokinase,xlab="creatinine_phosphokinase")
boxplot(data$ejection_fraction,xlab="ejection_fraction")
boxplot(data$platelets,xlab="platelets")
boxplot(data$serum_creatinine,xlab="serum_creatinine")
boxplot(data$serum_sodium,xlab="serum_sodium")
boxplot(data$time, xlab="time")

#out.vals = boxplot(data$creatinine_phosphokinase)$out
#out.vals
#which(data$creatinine_phosphokinase %in% out.vals)
#data$creatinine_phosphokinase[which(data$creatinine_phosphokinase %in% out.vals)] <- median(data$creatinine_phosphokinase)
summary(data)

par(mfrow=c(1,1)) 

#split dataset ----
data$DEATH_EVENT <- as.factor(data$DEATH_EVENT)
data <- data[,-12]
library(caret)

library(ranger)
library(splitTools)
#partition
data_01 <- partition(data$DEATH_EVENT,p = c(train = 0.8,test = 0.2))
train <- data[data_01$train, ]
test <- data[data_01$test, ]
train
test
prop.table(table(train$DEATH_EVENT))
prop.table(table(test$DEATH_EVENT))


train_a <- as.data.frame(lapply(train[1:12], normalize))
train_b <- train[ ,12]
test_a <- as.data.frame(lapply(test[1:12], normalize))
test_b <- test[ , 12]
test_b

str(data)

par(mfrow=c(1,1)) 

#standard machine learning----
library(class)
library(gmodels)

#knn
set.seed(123)
fitControl = trainControl(method = "cv",number = 5)#5 fold

knnfit <- train(DEATH_EVENT~.,data = train,method = "knn",metric ="Kappa",
      preProcess = c("scale"), trControl = fitControl,tuneLength = 12) 
knnfit
library(pROC)
#accuracy etc
pred <- predict(knnfit,test)
pred
test_b <- as.numeric(test_b)
pred <- as.numeric(pred)
knnroc <- roc(test$DEATH_EVENT,pred)
plot(knnroc,print.auc = TRUE, auc.polygon = TRUE,max.auc.polygon = TRUE,print.thres= TRUE)
CrossTable(x= test_b, y = pred, prop.chisq = FALSE)


#2 svm
set.seed(123)
fitControl = trainControl(method = "cv",number = 5)
svmfit <- train(DEATH_EVENT~.,data = train,method = "svmLinear3",metric ="Kappa",
                preProcess = "scale", trControl = fitControl,tuneLength = 20) 
svmfit
pred <- predict(svmfit,test)
pred

CrossTable(x= test_b, y = pred, prop.chisq = FALSE)

test_b <- as.numeric(test_b)
pred <- as.numeric(pred)
svmroc <- roc(test$DEATH_EVENT,pred)
plot(svmroc,print.auc = TRUE, auc.polygon = TRUE,max.auc.polygon = TRUE,print.thres= TRUE)


#3 rf
library(randomForest)
set.seed(123)
trControl = trainControl(method = "cv",number = 5, search = "grid")

tuneGrid <- expand.grid(.mtry = c(1:20))
rf_mtry <- train(DEATH_EVENT~.,data = train,method = "rf",metric ="Accuracy",
                preProcess = "scale", tuneGrid =tuneGrid,
                trControl = trControl,importance = TRUE,ntree = 200) 
rf_mtry
pred <- predict(rf_mtry,test )
pred
#feature selection using random forest
importance1= varImp(rf_mtry,scale = TRUE,type = 1)
importance1
plot(importance1)
importance2= varImp(rf_mtry,scale = FALSE,type = 2)
importance2
plot(importance2)

test_b <- as.numeric(test_b)
pred <- as.numeric(pred)
rfroc <- roc(test$DEATH_EVENT,pred)
plot(rfroc,print.auc = TRUE, auc.polygon = TRUE,max.auc.polygon = TRUE,print.thres= TRUE)


#4 boosting
library(gbm)
set.seed(123)
fitControl = trainControl(method = "cv",number = 5)
boostfit <- train(DEATH_EVENT~.,data = train,method = "gbm",metric ="Kappa",
                preProcess = c("scale"), trControl = fitControl,tuneLength = 12) 
boostfit
pred_boost <- predict(boostfit,test )
pred_boost
CrossTable(x= test_b, y = pred_boost, prop.chisq = FALSE)

test_b <- as.numeric(test_b)
pred_boost <- as.numeric(pred_boost)
Broc <- roc(test$DEATH_EVENT,pred_boost)
plot(Broc,print.auc = TRUE, auc.polygon = TRUE,max.auc.polygon = TRUE,print.thres= TRUE)


# machine learning after feature selection----
#delete unimportant features
data0 <- data[,c(1,5,8,9,12)]
test_02 <- test[,c(1,5,8,9,12)]
test_02
#knn
set.seed(123)
fitControl = trainControl(method = "cv",number = 5)
knnfit_02 <- train(DEATH_EVENT~.,data = data0,method = "knn",metric ="Kappa",
                preProcess = c("center","scale"), trControl = fitControl,tuneLength = 12) 
knnfit_02
pred <- predict(knnfit_02,test )
pred
test_b <- as.numeric(test_b)
pred <- as.numeric(pred)
knnroc <- roc(data0$DEATH_EVENT,pred)
plot(knnroc,print.auc = TRUE, auc.polygon = TRUE,max.auc.polygon = TRUE,print.thres= TRUE)
knncm <- table(actual= test_b, pred= pred)
knncm
confusionMatrix(knncm, positive = "1")
CrossTable(x= test_b, y = pred, prop.chisq = FALSE)

#svm
installed.packages("LiblineaR")
library("LiblineaR")
set.seed(123)
fitControl = trainControl(method = "cv",number = 5)
svmfit_02 <- train(DEATH_EVENT~.,data = data0,method = "svmLinear3",metric ="Kappa",
                preProcess = "center", trControl = fitControl,tuneLength = 20) 
svmfit_02
pred <- predict(svmfit_02,test )
pred
table(data0$DEATH_EVENT,pred )
pred <- as.numeric(pred)
svmroc <- roc(test$DEATH_EVENT,pred)
plot(svmroc,print.auc = TRUE, auc.polygon = TRUE,max.auc.polygon = TRUE,print.thres= TRUE)

svmcm <- table(actual= test_b, pred= pred)
svmcm
confusionMatrix(svmcm, positive = "1")
CrossTable(x= test_b, y = pred, prop.chisq = FALSE)

#rf
set.seed(123)
trControl = trainControl(method = "cv",number = 5, search = "grid")
tuneGrid <- expand.grid(.mtry = c(1:20))
rf_mtry <- train(DEATH_EVENT~.,data = data0,method = "rf",metric ="Accuracy",
                 preProcess = "center", tuneGrid =tuneGrid,
                 trControl = trControl,importance = TRUE,ntree = 100) 
rf_mtry
pred <- predict(rf_mtry,test )
pred

test_b <- as.numeric(test_b)
pred <- as.numeric(pred)
rfroc <- roc(test$DEATH_EVENT,pred)
plot(rfroc,print.auc = TRUE, auc.polygon = TRUE,max.auc.polygon = TRUE,print.thres= TRUE)
CrossTable(x= test_b, y = pred, prop.chisq = FALSE)

#boosting
set.seed(123)
fitControl = trainControl(method = "cv",number = 5)
boostfit_02 <- train(DEATH_EVENT~.,data = data0,method = "gbm",metric ="Kappa",
                  preProcess = c("center","scale"), trControl = fitControl,tuneLength = 12) 
boostfit_02
pred_boost <- predict(boostfit_02,test )
pred_boost

CrossTable(x= test_b, y =pred_boost, prop.chisq = FALSE)
pred_boost<- as.numeric(pred_boost)
Broc <- roc(test$DEATH_EVENT,pred_boost)
plot(Broc,print.auc = TRUE, auc.polygon = TRUE,max.auc.polygon = TRUE,print.thres= TRUE)
(184+49)/299

