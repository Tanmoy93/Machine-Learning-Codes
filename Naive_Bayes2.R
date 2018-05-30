library(naivebayes)
library(ggplot2)
library(psych)
library(dplyr)
library(readr)
data <- read_csv("C:/Users/ACER/Downloads/binary.csv")
fix(data)
str(data)
xtabs(~admit+rank,data = data)
data$admit<-as.factor(data$admit)
data$rank<-as.factor(data$rank)
pairs.panels(data[,-1])
data %>%
        ggplot(aes(x=admit,y=gre,fill=admit))+
        geom_boxplot()+
        ggtitle("Box Plot")
data %>%
  ggplot(aes(x=gre,fill=admit))+
  geom_density(alpha=0.8,color="black")+
  ggtitle("Density_Plot")
set.seed(1234)
ind<-sample(2,nrow(data),replace = T, prob = c(0.8,0.2))
train<-data[ind==1,]
test<-data[ind==2,]
model<-naive_bayes(admit~.,data = train,usekernel = T)
summary(model)
model
train %>%
       filter(admit=="0") %>%
       summarise(mean(gre),sd(gre))
plot(model)
p<-predict(model,train)
head(cbind(p,train))
p1<-predict(model,train)
tab1<-table(p1,train$admit)
1-sum(diag(tab1))/sum(tab1)
p2<-predict(model,test)
tab2<-table(p2,test$admit)
1-sum(diag(tab2))/sum(tab2)