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
table_data=table(x5,y)
click_data=matrix(c(2609,2767,2474,2150),nrow = 2, byrow = TRUE)
# Add row and column names
rownames(click_data) <- c("Female", "Male")
colnames(click_data) <- c("Clicked_No", "Clicked_Yes")
click_data
# Perform Chi-square test
chi_result <- chisq.test(click_data)
#p-value=7.832*(10^-7)<0.05
chi_result$expected
#reject the null hypothesis, meaning significant association between X and Y
install.packages("vcd")
library("vcd")
mosaic(table_data, shade = TRUE, main = "Mosaic Plot: Gender vs Clicked on Ad")

#graphical analysis
#graphical method:
#x1 
hist(x1,probability=T,col="yellow",xlab="Average daily time spent on site (in minutes)",main="Histogram of Daily Time spent on site(in minutes)")
boxplot(x1~y,col=2:3,xlab="Clicked on Ad",ylab="Average daily time spent on site (in minutes)",main="Boxplot of Average daily time spent on site (in minutes)Vs Ad Clicked" )
#x2
hist(x2,probability=T,xlab="Age(in years)",main="Histogram of Age of Customer (in years)",col="pink")
boxplot(x2~y,col=2:3,xlab="Clicked on Ad",ylab="Age(in years)",main="Boxplot of Age of Customers(in years)Vs Ad Clicked" )
#x3
hist(x3,probability=T,xlab="Area Income (in Rupees)",main="Histogram of Area Income",col="red")
boxplot(x3~y,col=2:3,xlab="Clicked on Ad",ylab="Area Income",main="Boxplot of Area Income Vs Ad Clicked" )
#x4
hist(x4,probability=T,xlab="Average daily time spent on internet (in minutes)",col="blue",main="Histogram of Daily Time spent on internet(in minutes)")
boxplot(x4~y,col=2:3,xlab="Clicked on Ad",ylab="Average daily time spent on internet (in minutes)",main="Boxplot of Average daily time spent on internet(in minutes)Vs Ad Clicked" )
#x5
d1=table(x5,y)
barplot(d1, beside = T, col =11:12,xlab="Ad Clicked",main="Grouped Bar Plot of Ad Clicked vs Gender",ylab = "Count", legend=c("female","male"))
#AD category
d2=table(x8,y)
category_colors <- c("0" ="grey", "1" = "blue", "2" = "red", "3" = "green", "4" = "purple","5"="yellow", "6"="pink", "7"="orange","8"="black","9"="skyblue","10"="seagreen")
barplot(d2, beside = T,col=category_colors,xlab="Ad Clicked",main="Grouped Bar Plot of Ad Clicked vs Ad Category",ylab = "Count")
legend("topright",legend=c("0","1","2","3","4","5","6","7","8","9","10"),col=category_colors,lty="solid",lwd=4)
#Time
d3=table(x7,y)
category_colors1 <- c("0" ="pink", "1" = "blue", "2" = "red", "3" = "skyblue")
barplot(d3, beside = T,col=category_colors1,xlab="Ad Clicked",main="Grouped Bar Plot of Ad Clicked vs Time",ylab = "Count")
legend("topright",legend=c("0","1","2","3"),col=category_colors1,lty="solid",lwd=4)
#x1 & x4
plot(x1,x4,xlab="Average daily time spent on site (in minutes)",ylab="Average daily time spent on internet (in minutes)",main="Histogram of Daily time spent on site vs Daily time spent on internet",col="blue")
#x2 vs x4
plot(x2,x4,xlab="Age",ylab="Average daily time spent on internet (in minutes)",col="red")
#x1 vs x2
plot(x2,x1,xlab="Age",ylab="Average daily time spent on site (in minutes)",col="green")

pairs(data[,c(2,5,6,7)],labels=c("Daily time spent on site","Age","Area Income","Daily internet usage"),col=10:14)

age_group=ifelse(x2>=18 & x2<26,1,ifelse(x2>25 & x2<36,2,ifelse(x2>35 & x2<51,3,4)))
head(age_group)
tail(age_group)
table(age_group)
d4=table(age_group,y);d4
category_colors <- c("0" ="yellow", "1" = "skyblue", "2" = "red", "3" = "pink")
barplot(d4, beside = T,col=category_colors,xlab="Ad Clicked",main="Grouped Bar Plot of Ad Clicked vs Age Group",ylab = "Count")
legend("topright",legend=c("0","1","2","3"),col=category_colors,lty="solid",lwd=4)

