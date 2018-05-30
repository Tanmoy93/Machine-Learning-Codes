library(readr)
mydata <- read_csv("C:/Users/ACER/Downloads/Cardiotocographic.csv")
str(mydata)
mydata$NSPF<-factor(mydata$NSP)
mydata$out<-relevel(mydata$NSPF,ref = "1")\
#Developing Multinomial Model
install.packages("nnet")
library(nnet)
mymodel<-multinom(out~LB+AC+FM,data = mydata)
summary(mymodel)
p<-predict(mymodel,mydata)
cm<-table(p,mydata$NSPF)
print(cm)
1-sum(diag(cm))/sum(cm)

