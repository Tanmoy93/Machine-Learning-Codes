library(keras)
install_keras()
library(readr)
data <- read_csv("C:/Users/ACER/Downloads/Cardiotocographic.csv")
str(data)
data<-as.matrix(data)
dimnames(data)<-NULL

#NORMALISE
data[,1:21]<-normalize(data[,1:21])
data[,22]<-as.numeric(data[,22])-1
summary(data)

#data partition
set.seed(1000)
ind<-sample(2,nrow(data),replace = T, prob = c(0.7,0.3))
train<-data[ind==1,1:21]
test<-data[ind==2,1:21]
traintarget<-data[ind==1,22]
testtarget<-data[ind==2,22]

#one hot coding

tarinLabels<-to_categorical(traintarget)
testLabels<-to_categorical(testtarget)
print(tarinLabels)
print(testLabels)

#create sequential model

model<-keras_model_sequential()
model %>% 
        layer_dense(units = 8,activation = "relu",input_shape = c(21)) %>%
        layer_dense(units = 3, activation = "softmax")
summary(model)
#complie
model  %>%
          compile(loss = "categorical_crossentropy",
                  optimizer = "adam",
                  matrics = "accuracy")
# fit
graph<-model %>%
                  fit(train,tarinLabels,epoch=200,
                      batch_size = 32,
                      validation_split = 0.2)

