library(readr)
data <- read_csv("C:/Users/ACER/Downloads/Cardiotocographic.csv")
str(data)
data$NSP<-as.ordered(data$NSP)
summary(data)
xtabs(~NSP+Tendency,data = data)
fix(data)
set.seed(123)
ind<-sample(2,nrow(data),replace = T,prob = c(0.8,0.2))
train<-data[ind==1,]
test<-data[ind==2,]
library(MASS)
model<-polr(NSP~LB+AC+FM, data = train,Hess = T)
summary(model)
#prediction
pred<-predict(model,train)
tab<-table(predicted=pred,Actual=train$NSP)
1-sum(diag(tab))/sum(tab)

#test data
pred<-predict(model,test)
tab1<-table(predicted=pred,Actual=test$NSP)
1-sum(diag(tab1))/sum(tab1)

#taking all variable
model1<-polr(NSP~., data = train,Hess = T)
summary(model)
 #train data classification
pred1<-predict(model1,train)
tab3<-table(predicted=pred1,Actual=train$NSP)
1-sum(diag(tab3))/sum(tab3)