library(readr)
data <- read_csv("C:/Users/ACER/Downloads/binary.csv")
data$admit<-as.factor(data$admit)
x<-prop.table(table(data$admit))
barplot(x,col = c("Red","blue"),ylim = c(0,0.8))
set.seed(123)
pd<-sample(2,nrow(data),replace = T,prob = c(0.6,0.4))
train<-data[pd==1,]
test<-data[pd==2,]

#Predictive model, Random Forest
library(randomForest)
rftrain<-randomForest(admit~.,data=train)
library(caret)
library(e1071)
confusionMatrix(predict(rftrain,test),test$admit,positive = "1")
#oversampling for increasing sensitivity
install.packages("ROSE")
library(ROSE)
over<-ovun.sample(admit~.,data = train,method = "over",N=320)$data
table(over$admit)

#checking
rftrain<-randomForest(admit~.,data=train)
rfover<-randomForest(admit~.,data=over)
confusionMatrix(predict(rfover,test),test$admit,positive = "1")
