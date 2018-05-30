install.packages("magick")
df<-iris
par(mfrow=c(2,2))
hist(df$Sepal.Length, breaks = 20,col="Red")
hist(df$Sepal.Width, breaks = 20,col="blue")
hist(df$Petal.Length, breaks = 20,col="green")
hist(df$Petal.Width, breaks = 20, col="black")

df$Sepal.Length<-(df$Sepal.Length-max(df$Sepal.Length))/(max(df$Sepal.Length-min(df$Sepal.Length)))
df$Sepal.Length<-(df$Sepal.Width-max(df$Sepal.Width))/(max(df$Sepal.Width-min(df$Sepal.Width)))
df$Sepal.Length<-(df$Petal.Length-max(df$Petal.Length))/(max(df$Petal.Length-min(df$Petal.Length)))
df$Sepal.Length<-(df$Petal.Width-max(df$Petal.Width))/(max(df$Petal.Width-min(df$Petal.Width)))

summary(df)
library(caret)
library(psych)
pairs.panels(df[-5])

#removing the skewness
pcadf<-df[-5]
predf<-preProcess(x=pcadf,method = c("BoxCox"))
pcadf<-predict(predf,newdata = pcadf)

library(moments)
sapply(pcadf,function(x) skewness(x))
par(mfrow=c(2,2))
hist(df$Sepal.Length, breaks = 20,col="Red")
hist(df$Sepal.Width, breaks = 20,col="blue")
hist(df$Petal.Length, breaks = 20,col="green")
hist(df$Petal.Width, breaks = 20, col="black")
t<-na.omit(df)

#PCA
model<-prcomp(pcadf,center=  F)
summary(model)
screeplot(model)
