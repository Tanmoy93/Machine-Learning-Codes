library(mlbench)
library(caret)
data("Sonar")
DataFrame <- Sonar
library(caTools)
library(caret)
ind = createDataPartition(DataFrame$Class, p = 2/3, list = FALSE)
trainDF<-DataFrame[ind,]
testDF<-DataFrame[-ind,]
ControlParamteres <- trainControl(method = "cv",
                                  number = 5,
                                  savePredictions = TRUE,
                                  classProbs = TRUE
)
parametersGrid <-  expand.grid(eta = 0.1, 
                               colsample_bytree=c(0.5,0.7),
                               max_depth=c(3,6),
                               nrounds=100,
                               gamma=1,
                               min_child_weight=2,subsample=1
)
modelxgboost <- train(Class~., 
                      data = trainDF,
                      method = "xgbTree",
                      trControl = ControlParamteres,
                      tuneGrid=parametersGrid)
predictions<-predict(modelxgboost,testDF)
t<-table(predictions=predictions,actual=testDF$Class)
