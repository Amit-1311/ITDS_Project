mydata<- read.csv("D:/Dataset.csv",header = TRUE,sep = ",")
mydata

names(mydata)

colSums(is.na(mydata))

which(is.na(mydata$Age))
mydata <- mydata[-4,]
which(is.na(mydata$Age))
mydata <- mydata[-23,]
which(is.na(mydata$Age))
mydata <- mydata[-31,]
which(is.na(mydata$Age))

mean(mydata$Age)
median(mydata$Age)

mydata$Sex <- factor(mydata$Sex, levels = c("M","F"), labels = c("1","2"))
mydata

which(is.na(mydata$Sex))
mydata <- mydata[-8,]
which(is.na(mydata$Sex))
mydata <- mydata[-23,]
which(is.na(mydata$Sex))
mydata <- mydata[-35,]
which(is.na(mydata$Sex))

str(mydata)

summary(mydata)

install.packages("dplyr")
library("dplyr")
mydata%>%summarise_if(is.numeric,sd)

s <- mydata$Age
sd(s)
a <- mydata$Cholesterol
sd(a)

hist(mydata$Age, col = "Green")

hist(mydata$Cholesterol, col = "Red")

var(mydata$Age)
sd(mydata$Age)