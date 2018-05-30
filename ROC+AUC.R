library(readr)
binary <- read_csv("C:/Users/ACER/Downloads/binary.csv")
View(binary)
library(nnet)
p<-multinom(admit~.,data = binary)
pred<-predict(p,binary)
tab<-table(pred,binary$admit)
install.packages("ROCR")
library(ROCR)
pred1<-predict(p,binary,type = "prob")
head(pred1)
pred2<-prediction(pred1,binary$admit)
eval<-performance(pred2,"acc")
plot(eval)
#identifying best values
max<-which.max(slot(eval,"y.values")[[1]])
acc<-slot(eval,"y.values")[[1]][max]
acc
cut<-slot(eval,"x.values")[[1]][max]
cut
print(c(Accuracy=acc,Cut_off=cut))

#Reciever operating Curve(ROC)
pred3<-prediction(pred1,binary$admit)
roc<-performance(pred3,"tpr","fpr")
plot(roc,colorize=T)
abline(a=0,b=1)
#Area Under curve
auc<-performance(pred3,"auc")
auc<-unlist(slot(auc,"y.values"))
auc
