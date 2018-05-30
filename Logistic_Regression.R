library(readr)
data <- read_csv("C:/Users/ACER/Downloads/binary.csv")
str(data)
data$admit<-as.factor(data$admit)
data$rank<-as.factor(data$rank)
str(data)
xtabs(~admit+rank,data = data)
levels(data$admit)<-c("0", "1")
table(data$admit)
set.seed(1234)
ind<-sample(2,nrow(data),replace = T,prob = c(0.8,0.2))
train<-data[ind==1,]
test<-data[ind==2,]
mymodel<-glm(admit~gre+gpa+rank,data = data,family = "binomial")
summary(mymodel)
#removing gre
mymodel<-glm(admit~gpa+rank,data = data,family = "binomial")
summary(mymodel)
#prediction of training
p<-predict(mymodel,train)
tab<-table(p,train$admit)
#prediction of test
q<-predict(mymodel,test)
tab1<-table(q,test$admit)
#Mis classification error
pred1<-ifelse(p>0.5,"Got_Admit","Don't get Admit")
tab3<-table(Predicted=pred1,Actual=train$admit)
print(tab3)


