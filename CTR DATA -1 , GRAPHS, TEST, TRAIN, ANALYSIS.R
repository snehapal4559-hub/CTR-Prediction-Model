rm(list=ls())
setwd("C:/Users/sneha/OneDrive/Desktop/CTR DIS/ctr3")
data=read.csv("ad_10000records.csv");data
head(data)

#daily time spent 
x1=data[,1];x1
summary(x1)
#age
x2=data[,2];x2
summary(x2)
#area income
x3=data[,3];x3
summary(x3)
#daily internet usage
x4=data[,4];x4
summary(x4)
#gender
x5=data[,7];x5
table(x5)
#timestamp
x6=data[,9]
#ad click
y=data[,10]
table(y)

#Age group
age_group=ifelse(x2>=18 & x2<31,1,ifelse(x2>30 & x2<51,2,3))
head(age_group)
tail(age_group)
table(age_group)

#graphical method:
#x1 
hist(x1,probability=T,xlab="Average daily time spent on site (in minutes)",main="Histogram of Daily Time spent on site(in minutes)")
boxplot(x1~y,col=2:3,xlab="Clicked on Ad",ylab="Average daily time spent on site (in minutes)",main="Boxplot of Average daily time spent on site (in minutes)Vs Ad Clicked" )
#x2
hist(x2,probability=F,xlab="Age(in years)",breaks=19:61,main="Histogram of Age of Customer (in years)")
boxplot(x2~y,col=2:3,xlab="Clicked on Ad",ylab="Age(in years)",main="Boxplot of Age of Customers(in years)Vs Ad Clicked" )
#x3
hist(x3,probability=T,xlab="Area Income",main="Histogram of Area Income")
boxplot(x3~y,col=2:3,xlab="Clicked on Ad",ylab="Area Income",main="Boxplot of Area Income Vs Ad Clicked" )
#x4
hist(x4,probability=T,xlab="Average daily time spent on internet (in minutes)",main="Histogram of Daily Time spent on internet(in minutes)")
boxplot(x4~y,col=2:3,xlab="Clicked on Ad",ylab="Average daily time spent on internet (in minutes)",main="Boxplot of Average daily time spent on internet(in minutes)Vs Ad Clicked" )
#x5
d1=table(x5,y)
barplot(d1, beside = T, col =2:3,xlab="Ad Clicked",main="Grouped Bar Plot of Ad Clicked vs Gender",ylab = "Count", legend=c("female","male"))
#age group
d2=table(age_group,y)
barplot(d2, beside = T,col=c(2,3,4),xlab="Ad Clicked",main="Grouped Bar Plot of Ad Clicked vs Age Group",ylab = "Count")
legend("topleft",legend=c("age group 1","age group 2","age group 3"),col=c(2,3,4),lty="solid",lwd=4)
#x1 & x4
plot(x1,x4,xlab="Average daily time spent on site (in minutes)",ylab="Average daily time spent on internet (in minutes)",main="Histogram of Daily time spent on site vs Daily time spent on internet",col="blue")
#x2 vs x4
plot(x2,x4,xlab="Age",ylab="Average daily time spent on internet (in minutes)",col="red")
#x1 vs x2
plot(x2,x1,xlab="Age",ylab="Average daily time spent on site (in minutes)",col="green")

#converting variable gender into binary variable taking value 0 if female and 1 if male.
x5.1=ifelse(x5=="Male",1,0);x5.1
head(x5.1)
head(x5)

#before spliting of dataset - 90% train and 10% testing
fit.logit=glm(y~(x1+x2+x3+x4+x5.1),family=binomial("logit"))
summary(fit.logit)
fit=as.vector(fitted(fit.logit));fit
length(fit)

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
x1.tr=df_train[,1];x1.tr
x2.tr=df_train[,2];x2.tr
x3.tr=df_train[,3];x3.tr
x4.tr=df_train[,4];x4.tr
x5.tr=df_train[,7];x5.tr
x5.1.tr=ifelse(x5.tr=="Male",1,0);x5.1.tr
y.tr=df_train[,10];y.tr
length(y.tr)

fit.logit.tr=glm(y.tr~(x1.tr+x2.tr+x3.tr+x4.tr+x5.1.tr),family=binomial("logit"))
summary(fit.logit.tr)
fit.tr=as.vector(fitted(fit.logit.tr));fit.tr
length(fit.tr)
nrow(fit.logit.tr$model)

#MEAN
y1.tr=c()
m=mean(fit.tr);m #cutpoint
for(i in 1:length(fit.tr))
{
 if(fit.tr[i]>m){
  y1.tr[i]=1
 }
 else{
 y1.tr[i]=0
 }
}
y1.tr
table(y.tr,y1.tr)
TPR=3086/(1335+3086);TPR
FPR=1195/(1195+3384);FPR
ERROR=1-TPR+FPR;ERROR
TPR*(1-FPR)

#MEDIAN
y2.tr=c()
m2=median(fit.tr);m2 #cutpoint
for(i in 1:length(fit.tr))
{
 if(fit.tr[i]>m2){
  y2.tr[i]=1
 }
 else{
 y2.tr[i]=0
 }
}
y2.tr
table(y.tr,y2.tr)
TPR1=3175/(1246+3175);TPR1
FPR1=1325/(1325+3254);FPR1
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
roc.info=rocit(y.tr,y2.tr);roc.info  #logit
summary(roc.info)
plot(roc.info)
locator(1)

#test
y.ts=df_test[,10]
x1.ts=df_test[,1]
x2.ts=df_test[,2]
x3.ts=df_test[,3]
x4.ts=df_test[,4]
x5.ts=df_test[,7]
x5.1.ts=ifelse(x5.ts=="Male",1,0)
b1=fit.logit.tr$coefficients[1];b1
b2=fit.logit.tr$coefficients[2];b2
b3=fit.logit.tr$coefficients[3];b3
b4=fit.logit.tr$coefficients[4];b4
b5=fit.logit.tr$coefficients[5];b5
b6=fit.logit.tr$coefficients[6];b6
m=b1+(b2*x1.ts)+(b3*x2.ts)+(b4*x3.ts)+(b5*x4.ts)+(b6*x5.1.ts);m
trained_model=exp(m)/(1+exp(m));trained_model
y1.ts=ifelse(trained_model>m2,1,0);y1.ts

#confusion matrix
cf.tr=table(y.ts,y1.ts);cf.tr
TPR.ts=373/(373+123);TPR.ts
FPR.ts=159/(159+345);FPR.ts
TPR.ts*(1-FPR.ts)

