library(readr)
data <- read_csv("C:/Users/ACER/Downloads/CTG.csv")
str(data)
data$NSP<-as.factor(data$NSP)
levels(data$NSP)<-c("Normal","Suspect","Pathelogic")
table(data$NSP)
set.seed(1234)
ind<-sample(2,nrow(data),replace = T, prob = c(0.8,0.2))
train<-data[ind==1,]
train<-na.omit(train)
test<-data[ind==2,]
test<-na.omit(test)
library(randomForest)
rf<-randomForest(NSP~.,data=train)
print(rf)
plot(rf)
library(caret)
p1<-predict(rf,train)
tab1<-table(p1,train$NSP)
print(tab1)
#Error rate
1-sum(diag(tab1))/sum(tab1)
confusionMatrix(p1,train$NSP)
p2<-predict(rf,test)
tab2<-table(p2,test$NSP)
print(tab2)
confusionMatrix(p2,test$NSP)
1-sum(diag(tab2))/sum(tab2)
#tunning the model
t<-tuneRF(train[,-22],train[,22],stepFactor = 0.5,
       plot = T,ntreeTry = 300,trace = T,improve = 0.05)
#After tunning
rf<-randomForest(NSP~.,data=train,
                 ntree=300,
                 mtry=8,importance=T,
                 proximity=T)
#No of nodes of the tree
hist(treesize(rf),
              main="No of Nodes",
              col="Red")
#variable importance
varImpPlot(rf,sort = T,
           n.var = 10,main = "Top-10 nodes")
importance(rf)
#extract a single tree
getTree(rf,1,labelVar = T)
#Multidimensional matrix of proximity
MDSplot(rf,train$NSP)