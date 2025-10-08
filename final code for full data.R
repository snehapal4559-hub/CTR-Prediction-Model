rm(list=ls())
setwd("C:/Users/sneha/OneDrive/Desktop/CTR DIS/CTR FINAL DATA/DATASET")
data=read.csv("Final_Ad_Topics_categorical.csv")
head(data)

#daily time spent on site
x1=data[,2];x1
summary(x1)
#age
x2=data[,5];x2
summary(x2)
#area income
x3=data[,6];x3
summary(x3)
#daily internet usage
x4=data[,7];x4
summary(x4)
#gender
x5=data[,8];x5
table(x5)
#time
x7=data[,4]
table(x7)
#ad category
x8=data[,3]
table(x8)
cont=data[,11]
table(cont)
#ad click
y=data[,9]


#After spliting of dataset - 80% train and 20% testing
set.seed(445)
index=sample(1:10000,size=0.8*10000,replace=F);index
head(index)
length(index)
df_train=data[index,]
head(df_train)
df_test=data[-index,]
head(df_test)

#declaring variables of train dataset:
x1.tr=df_train[,2];x1.tr  #daily time spent on site
x2.tr=df_train[,5];x2.tr  #age
x3.tr=df_train[,6];x3.tr  #area income
x4.tr=df_train[,7];x4.tr  #daily internet usage
x5.tr=df_train[,3];x5.tr  #ad category
x6.tr=df_train[,4];x6.tr  #time category
x7.tr=df_train[,8];x7.tr  #gender
y.tr=df_train[,9];y.tr  #clicked on ad
train_data=data.frame(x1.tr,x2.tr,x3.tr,x4.tr,as.factor(x5.tr),as.factor(x6.tr),as.factor(x7.tr))
head(train_data)

fit.logit.tr=glm(y.tr~.,data=train_data,family=binomial("logit"))
summary(fit.logit.tr)
library("MASS")
model2=stepAIC(fit.logit.tr)
summary(model2)

#determining accuracy of the model
#MEAN
m=mean(fit.tr);m #cutpoint
y.p=ifelse(fit.tr>m,1,0)
table(y.tr,y.p)
TPR=3121/(3121+1300);TPR
FPR=1157/(1157+3422);FPR
ERROR=1-TPR+FPR;ERROR
TPR*(1-FPR)

#MEDIAN
m2=median(fit.tr);m2 #cutpoint
y.p1=ifelse(fit.tr>m2,1,0)
table(y.tr,y.p1)
TPR1=3217/(1204+3217);TPR1
FPR1=1283/(1283+3296);FPR1
ERROR1=1-TPR+FPR;ERROR1
TPR1*(1-FPR1)

#test
x1.tr=df_test[,2];x1.tr #daily time spent on site
x2.tr=df_test[,5];x2.tr #age
x3.tr=df_test[,6];x3.tr  #area income
x4.tr=df_test[,7];x4.tr  #daily internet usage
x5.tr=df_test[,3];x5.tr  #ad category
x6.tr=df_test[,4];x6.tr  #time category
x7.tr=df_test[,8];x7.tr
test_data=data.frame(x1.tr,x2.tr,x3.tr,x4.tr,as.factor(x5.tr),as.factor(x6.tr),as.factor(x7.tr))
head(test_data)
fit.tr=predict(fit.logit.tr,newdata=test_data,type="response") #predict ctr using model ( to obtain probs)
head(fit.tr)
length(fit.tr)
m_pred=mean(fit.tr)
pred_test=ifelse(fit.tr>m_pred,1,0)
table(df_test[,9],pred_test)
tpr=697/(697+281);tpr
fpr=262/(760+262);fpr
tpr*(1-fpr)
install.packages("ROCit")
library(ROCit)
#??rocit
roc.info=rocit(pred_test,df_test[,9]);roc.info  #logit
summary(roc.info)
plot(roc.info)
locator(1)
plot(roc.info, col = "blue", main ="ROC Curve", print.auc=TRUE)
accuracy=(760+697)/(760+281+262+697);accuracy

