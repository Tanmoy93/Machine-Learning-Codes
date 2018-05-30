data("iris")
str(iris)
data<-iris
library(MASS)
library(psych)
pairs.panels(iris[1:4],
             gap=0,
             bg=c("red","blue","green")[iris$Species],
             pch = 21)
#data partition
set.seed(123)
ind<-sample(2,nrow(data),replace = T,prob = c(0.6,0.4))
train<-data[ind==1,]
test<-data[ind==2,]

#LDA
library(MASS)
linear<-lda(Species~.,data=train)
linear
pred<-predict(linear,train)
print(pred)
ldahist(pred$x[,1],g=train$Species)
install.packages("devtools")
library(devtools)
install.packages("klaR")
library(klaR)
#partition plot
partimat(Species~.,data = train,method="lda")
partimat(Species~.,data = train,method="qda")
p1<-predict(linear,train)$class
tab<-table(predicted=p1,Actual=train$Species)
print(tab)
p2<-predict(linear,test)$class
tab2<-table(predicted=p2,Actual=test$Species)
print(tab2)
