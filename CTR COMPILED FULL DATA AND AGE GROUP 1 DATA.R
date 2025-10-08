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


#After spliting of dataset - 90% train and 10% testing
set.seed(445)
index=sample(1:10000,size=0.9*10000,replace=F);index
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

#for mean trp*(1-fpr) is slightly higher, indicating a better predicted model
#using mean as cutpoint.
#Comment:Mean of predicted probabilties acts as a better cut-point
#to obtain the ad click status.

install.packages("ROCit")
library(ROCit)
#??rocit
#check for y1.tr and y2.tr
roc.info=rocit(y.tr,y1.tr);roc.info  #logit
summary(roc.info)
plot(roc.info)
locator(1)

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
tpr=355/(141+355);tpr
fpr=125/(379+125);fpr
tpr*(1-fpr)

#AGE GROUP 1

data$age_group=age_group
head(data)
df_age1=data[which(age_group==1),]
head(df_age1)
#age group 1 variables
#daily time spent 
x1.a1=df_age1[,1];x1.a1
summary(x1.a1)
#age
x2.a1=df_age1[,2];x2.a1
summary(x2.a1)
#area income
x3.a1=df_age1[,3];x3.a1
summary(x3.a1)
#daily internet usage
x4.a1=df_age1[,4];x4.a1
summary(x4.a1)
#gender
x5.a1=df_age1[,7];x5.a1
table(x5.a1)
#ad click
y.a1=df_age1[,10]
table(y.a1)

#GRAPHICAL:
#graphical method:
#x1 
hist(x1.a1,probability=T,xlab="Average daily time spent on site (in minutes)",main="Histogram of Daily Time spent on site(in minutes)-Age Group 1")
boxplot(x1.a1~y.a1,col=2:3,xlab="Clicked on Ad",ylab="Average daily time spent on site (in minutes)",main="Boxplot of Average daily time spent on site (in minutes)Vs Ad Clicked - Age Group 1" )
#x2
hist(x2.a1,probability=F,xlab="Age(in years) Age Group 1",breaks=19:31,main="Histogram of Age of Customer (in years) - Age Group 1")
boxplot(x2.a1~y.a1,col=2:3,xlab="Clicked on Ad",ylab="Age(in years)",main="Boxplot of Age of Customers(in years)Vs Ad Clicked- Age Group 1" )
#x3
hist(x3.a1,probability=T,xlab="Area Income",main="Histogram of Area Income - Age Group 1")
boxplot(x3.a1~y.a1,col=2:3,xlab="Clicked on Ad",ylab="Area Income",main="Boxplot of Area Income Vs Ad Clicked - Age Group 1" )
#x4
hist(x4.a1,probability=T,xlab="Average daily time spent on internet (in minutes)",main="Histogram of Daily Time spent on internet(in minutes) - Age Group 1")
boxplot(x4.a1~y.a1,col=2:3,xlab="Clicked on Ad",ylab="Average daily time spent on internet (in minutes)",main="Boxplot of Average daily time spent on internet(in minutes)Vs Ad Clicked-Age Group 1" )
#x5
d1.a1=table(x5.a1,y.a1);d1.a1
barplot(d1.a1, beside = T, col =2:3,xlab="Ad Clicked",main="Grouped Bar Plot of Ad Clicked vs Gender - Age Group 1",ylab = "Count", legend=c("female","male"))
#age group
d2=table(age_group,y)
barplot(d2, beside = T,col=c(2,3,4),xlab="Ad Clicked",main="Grouped Bar Plot of Ad Clicked vs Age Group",ylab = "Count")
legend("topleft",legend=c("age group 1","age group 2","age group 3"),col=c(2,3,4),lty="solid",lwd=4)
#x1 & x4
plot(x1.a1,x4.a1,xlab="Average daily time spent on site (in minutes)",ylab="Average daily time spent on internet (in minutes)",main="Histogram of Daily time spent on site vs Daily time spent on internet - Age Group 1",col="blue")
#x2 vs x4
plot(x2.a1,x4.a1,xlab="Age",ylab="Average daily time spent on internet (in minutes)",col="red",main="Histogram of Average Daily Time Spent on internet Vs Age (in years) - Age group 1")
#x1 vs x2
plot(x2.a1,x1.a1,xlab="Age",ylab="Average daily time spent on site (in minutes)",col="green",main="Histogram of Average Daily Time Spent on Site (In minutes) Vs Age (in years)- Age group 1")

#After spliting of dataset - 90% train and 10% testing
set.seed(445)
index.a1=sample(1:2995,size=0.9*2995,replace=F);index
head(index.a1)
length(index.a1)
df_train.a1=df_age1[index.a1,]
head(df_train.a1)
df_test.a1=df_age1[-index.a1,]
head(df_test.a1)

#declaring variables of train dataset:
x1.tr.a1=df_train.a1[,1];x1.tr.a1
x2.tr.a1=df_train.a1[,2];x2.tr.a1
x3.tr.a1=df_train.a1[,3];x3.tr.a1
x4.tr.a1=df_train.a1[,4];x4.tr.a1
x5.tr.a1=df_train.a1[,7];x5.tr.a1
x5.1.tr.a1=ifelse(x5.tr.a1=="Male",1,0);x5.1.tr.a1
y.tr.a1=df_train.a1[,10];y.tr.a1
length(y.tr.a1)
