library(MASS)
library(caret)
data<-iris
library(caTools)
pd<-createDataPartition(data$Species,p=2/3,list = F)
train<-data[pd,]
test<-data[-pd,]
controlParameters<-trainControl(method = "cv",
                                number = 5,savePredictions = T,
                                classProbs = T)
parameterGrid<-expand.grid(mtry=c(2,3,4))
modelRandom<-train(Species~., data=train,
                   method="rf",
                   trControl=controlParameters,
                   tuneGrid=parameterGrid)
names(getModelInfo())
pred<-predict(modelRandom,train)
tab<-table(Predicted=pred,Actual=train$Species)

pred1<-predict(modelRandom,test)
tab1<-table(Predicted=pred1,Actual=test$Species)
