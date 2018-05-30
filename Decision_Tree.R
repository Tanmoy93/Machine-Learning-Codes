library(readr)
data <- read_csv("C:/Users/ACER/Downloads/Cardiotocographic.csv")
View(data)
str(data)
data$NSPF<-factor(data$NSP)
levels(data$NSPF)<-c("Normal","pathelogic","unknown")
View(data)
#data partioning 
set.seed(1234)
pd<-sample(2,nrow(data),replace = TRUE,prob = c(0.8,0.2))
train<-data[pd==1,]
validate<-data[pd==2,]
# decision tree with party
install.packages("party")
library(party)
tree<-ctree(NSPF~LB+AC+FM,data = train,controls = ctree_control(mincriterion = 0.99,minsplit = 500))
plot(tree)
p<-predict(tree,validate)
table(p,validate$NSPF)

#Decision tree with rpart
library(rpart)
tree1<-rpart(NSPF~LB+AC+FM, data = train)
library(rpart.plot)
rpart.plot(tree1)
t<-predict(tree1,validate,type = "class")
table(t,validate$NSPF)
