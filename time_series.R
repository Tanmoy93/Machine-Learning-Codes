data("AirPassengers")
plot(AirPassengers)
abline(lm(AirPassengers~time(AirPassengers)))
cycle(AirPassengers)
plot(aggregate(AirPassengers,FUN = mean))
plot(diff(log(AirPassengers)))
library(tseries)
#AR I MA model
# p  q  d

acf(diff(log(AirPassengers))) #determine the value of q=1
pacf(diff(log(AirPassengers))) #determine the vale of p=0
 #ARIMA model
fit<-arima(log(AirPassengers),c(0,1,1),seasonal=list(order=c(0,1,1),period=12))
pred=predict(fit,n.ahead = 10*12)
pred1<-exp(pred$pred)
ts.plot(AirPassengers,pred1,log="y",lty=c(1,3))
datawise<-ts(AirPassengers,frequency = 12,start = c(1949,1),end = c(1959,12))
fit2<-arima(log(datawise),c(0,1,1),seasonal = list(order=c(0,1,1),period=12))
pred2<-predict(fit2,n.ahead = 10*12)
pred3<-exp(pred2$pred)
data1<-head(pred3,12)            
predicted_1960<-round(data1,digits = 0)
original_1960<-tail(AirPassengers,12)

