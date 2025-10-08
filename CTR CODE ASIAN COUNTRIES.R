rm(list=ls())
setwd("C:/Users/sneha/OneDrive/Desktop/CTR DIS/CTR FINAL DATA/DATASET")
data=read.csv("Final_Ad_Topics_categorical.csv")


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

#ad click
y=data[,9]
table(y)

#graphical analysis
#graphical method:
#x1 
hist(x1,probability=T,xlab="Average daily time spent on site (in minutes)",main="Histogram of Daily Time spent on site(in minutes)")
boxplot(x1~y,col=2:3,xlab="Clicked on Ad",ylab="Average daily time spent on site (in minutes)",main="Boxplot of Average daily time spent on site (in minutes)Vs Ad Clicked" )
#x2
hist(x2,probability=F,xlab="Age(in years)",breaks=19:30,main="Histogram of Age of Customer (in years)")
boxplot(x2~y,col=2:3,xlab="Clicked on Ad",ylab="Age(in years)",main="Boxplot of Age of Customers(in years)Vs Ad Clicked" )
#x3
hist(x3,probability=T,xlab="Area Income (in Rupees)",main="Histogram of Area Income")
boxplot(x3~y,col=2:3,xlab="Clicked on Ad",ylab="Area Income",main="Boxplot of Area Income Vs Ad Clicked" )
#x4
hist(x4,probability=T,xlab="Average daily time spent on internet (in minutes)",main="Histogram of Daily Time spent on internet(in minutes)")
boxplot(x4~y,col=2:3,xlab="Clicked on Ad",ylab="Average daily time spent on internet (in minutes)",main="Boxplot of Average daily time spent on internet(in minutes)Vs Ad Clicked" )
#x5
d1=table(x5,y)
barplot(d1, beside = T, col =2:3,xlab="Ad Clicked",main="Grouped Bar Plot of Ad Clicked vs Gender",ylab = "Count", legend=c("female","male"))
hist(x4,probability=T,xlab="Average daily time spent on internet (in minutes)",main="Histogram of Daily Time spent on internet(in minutes)")
boxplot(x4~y,col=2:3,xlab="Clicked on Ad",ylab="Average daily time spent on internet (in minutes)",main="Boxplot of Average daily time spent on internet(in minutes)Vs Ad Clicked" )
#AD category
d2=table(x8,y)
category_colors <- c("0" ="grey", "1" = "blue", "2" = "red", "3" = "green", "4" = "purple",
"5"="yellow", "6"="pink", "7"="orange","8"="black","10"="seagreen")
barplot(d2, beside = T,col=category_colors,xlab="Ad Clicked",main="Grouped Bar Plot of Ad Clicked vs Ad Category",ylab = "Count")
legend("topright",legend=c("0","1","2","3","4","5","6","7","8","10"),col=category_colors,lty="solid",lwd=4)
hist(x4,probability=T,xlab="Average daily time spent on internet (in minutes)",main="Histogram of Daily Time spent on internet(in minutes)")
boxplot(x4~y,col=2:3,xlab="Clicked on Ad",ylab="Average daily time spent on internet (in minutes)",main="Boxplot of Average daily time spent on internet(in minutes)Vs Ad Clicked" )
#Time
d3=table(x7,y)
category_colors1 <- c("0" ="pink", "1" = "blue", "2" = "red", "3" = "green")
barplot(d3, beside = T,col=category_colors1,xlab="Ad Clicked",main="Grouped Bar Plot of Ad Clicked vs Time",ylab = "Count")
legend("topright",legend=c("0","1","2","3"),col=category_colors,lty="solid",lwd=4)
#x1 & x4
plot(x1,x4,xlab="Average daily time spent on site (in minutes)",ylab="Average daily time spent on internet (in minutes)",main="Histogram of Daily time spent on site vs Daily time spent on internet",col="blue")
#x2 vs x4
plot(x2,x4,xlab="Age",ylab="Average daily time spent on internet (in minutes)",col="red")
#x1 vs x2
plot(x2,x1,xlab="Age",ylab="Average daily time spent on site (in minutes)",col="green")




#Logistic Regression model:
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
x1.tr=df_train[,2];x1.tr   #daily time on site
x2.tr=df_train[,5];x2.tr   #age
x3.tr=df_train[,6];x3.tr   #area income
x4.tr=df_train[,7];x4.tr   #daily time on internet
x5.tr=df_train[,8];x5.tr   #gender
x6.tr=df_train[,3];x6.tr   #ad category
x7.tr=df_train[,4];x7.tr   #time category

y.tr=df_train[,9];y.tr
length(y.tr)
head(df_train)
fit.logit.tr=glm(y.tr~(x1.tr+x2.tr+x3.tr+x4.tr+x5.tr+as.factor(x6.tr)+as.factor(x7.tr)),family=binomial("logit"))
summary(fit.logit.tr)
fit.tr=as.vector(fitted(fit.logit.tr));fit.tr
length(fit.tr)
nrow(fit.logit.tr$model)
coef_array=coef(fit.logit.tr)  # Extract coefficients
se_array=sqrt(diag(vcov(fit.logit.tr)))  # Extract standard errors
z_val=coef_array/se_array;z_val
z_abs=abs(z_val)
crit=qnorm(0.995,0,1)
sgnf=ifelse(z_abs>crit,1,0)
library(lmtest)


# Likelihood Ratio Test
fit.logit.red.tr=glm(y.tr~(x3.tr+x2.tr+x1.tr+x4.tr+as.factor(x6.tr)++as.factor(x7.tr)),family=binomial("logit"))
aov1=anova(fit.logit.red.tr,fit.logit.tr,test="Chisq");aov1

full_model=glm(y.tr~(x1.tr+x2.tr+x3.tr+x4.tr+x5.tr+as.factor(x6.tr)+as.factor(x7.tr)),data=df_train,family=binomial("logit"))
null_model=glm(y.tr~1,data=df_train,family=binomial("logit"))
aov2=anova(null_model,full_model,test="Chisq");aov2
