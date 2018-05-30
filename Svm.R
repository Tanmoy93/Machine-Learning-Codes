data(iris)
library(ggplot2)
str(iris)
qplot(iris$Petal.Length,iris$Petal.Width,color=iris$Species)
library(e1071)
mymodel<-svm(iris$Species~.,data = iris)
summary(mymodel)
plot(mymodel,data = iris,
     Petal.Length~Petal.Width,
     slice = list(Sepal.Length=3,Sepal.Width=4))
#confusion matrix
pred<-predict(mymodel,iris)
tab<-table(predicted=pred,Actual=iris$Species)
print(tab)
1-sum(diag(tab))/sum(tab)

#different Kernel
mymodel<-svm(iris$Species~.,data = iris,kernel="linear")
summary(mymodel)

mymodel<-svm(iris$Species~.,data = iris,kernel="polynomial")
summary(mymodel)

mymodel<-svm(iris$Species~.,data = iris,kernel="sigmoid")
summary(mymodel)

#Tunning
set.seed(123)
tmodel<-tune(svm,Species~.,data = iris,
     ranges = list(epsilon=seq(0,1,0.1),cost=2^(2:7)))
plot(tmodel)
summary(tmodel)
#best model
mymodel<-tmodel$best.model
summary(mymodel)
pred<-predict(mymodel,iris)
tab<-table(predicted=pred,Actual=iris$Species)
print(tab)
1-sum(diag(tab))/sum(tab)

