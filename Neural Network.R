library(readr)
data <- read_csv("C:/Users/ACER/Downloads/binary.csv")
#min max normalisation

data$gre<-(data$gre-min(data$gre))/(max(data$gre)-min(data$gre))
hist(data$gre)
data$gpa<-(data$gpa-min(data$gpa))/(max(data$gpa)-min(data$gpa))
hist(data$gpa)
data$rank<-(data$rank-min(data$rank))/(max(data$rank)-min(data$rank))
hist(data$rank)
set.seed(1234)
pd<-sample(2,nrow(data),replace = T,prob = c(0.7,0.3))
train<-data[pd==1,]
test<-data[pd==2,]
library(neuralnet)
n<-neuralnet(admit~gre+gpa+rank,data = train,hidden = 1,
             err.fct = "ce",
             linear.output = F)
plot(n)
output<-compute(n,train[,-1])
p1<-output$net.result
head(data$admit)
#Mis classification
pred1<-ifelse(p1>0.5,1,0)
tab1<-table(pred1,train$admit)
1-sum(diag(tab1)/sum(tab1))

output1<-compute(n,test[,-1])
p2<-output1$net.result
pred2<-ifelse(p2>0.5,1,0)
tab2<-table(pred2,test$admit)
1-sum(diag(tab2)/sum(tab2))


#increasing no of neurons
n2<-neuralnet(admit~gre+gpa+rank,data = train,hidden = 5,
             err.fct = "ce",
             linear.output = F,lifesign = "full",rep = 5)
             

plot(n2,rep=5)
output3<-compute(n2,train[,-1],rep = 2)
p3<-output3$net.result

pred3<-ifelse(p3>0.5,1,0)
tab3<-table(pred3,train$admit)
1-sum(diag(tab3)/sum(tab3))

output4<-compute(n2,test[,-1],rep = 2)
p4<-output4$net.result
pred4<-ifelse(p2>0.5,1,0)
tab4<-table(pred4,test$admit)
1-sum(diag(tab4)/sum(tab4))
