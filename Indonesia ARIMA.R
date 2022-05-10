library(lmtest)
library(tseries)
library(forecast)
library(readxl)

indo=read.csv("D:/indocovid.csv")
indo
View(indo)
y1t <- as.ts(indo$Total.case)
#yt <- log(yt)

#READ THE DATA 7 DAYS FORECAST
Ytrain=as.ts(y1t[1:260])                             #define training data
Ytest=as.ts(y1t[261:267])                             #define testing data

#TIME SERIES PLOT#
par(mfrow=c(1,1))
ts.plot(Ytrain)

#CHECKING FOR STATIONARY USING ACF PLOT#
#ACF
acf(Ytrain,lag.max=48,ylim=c(-1,1))
#PACF
pacf(Ytrain,lag.max=48,ylim=c(-1,1))
#datanya masih blm stasioner dlm mean krn ACF dies down

#CHECKING FOR STATIONARY USING ADF TEST#
#trunc((length(Ytrain)-1)^(1/3))
adf.test(Ytrain)

#DIFFERENCING ORDER FOR YTRAIN
Wtrain=diff(Ytrain,lag=1)
plot(Wtrain)
adf.test(Wtrain)
#Masih blm stasioner, maka diff lagi

#DIFFERENCING ORDER FOR WTRAIN
W2train=diff(Wtrain,lag=1)
plot(W2train)
adf.test(W2train)
#Sudah stasioner

#ORDER IDENTIFICATION USING ACF AND PACF FROM STATIONARY DATA
#ACF
par(mfrow=(c(1,2)))
acf(W2train,lag.max=28,ylim=c(-1,1))
#PACF
pacf(W2train,lag.max=28,ylim=c(-1,1))

ARIMA([1,2,3,4,5,19],2,0)(1,0,0)^7
#ARIMA model ARIMA([3,5,6,8,12,13,15],2,[2,4,5,6,7,12])
modelARIMA=arima(Ytrain, order = c(19,2,0),seasonal = list(order = c(1, 0, 0), period = 7),
                 fixed = c(rep(NA,5),rep(0,13),NA,NA), #NA was the estimated lag, that is : 23
                 include.mean=TRUE, method = c("CSS"),transform.pars = FALSE)
summary(modelARIMA)                                        
coeftest(modelARIMA)                                       #significance test for parameter
resi.ARIMA=as.ts(modelARIMA$residuals)                     #define the residual value
fits.ARIMA=as.ts(fitted(modelARIMA))                       #define forecast value for training data

par(mfrow=c(1,1))
plot(Ytrain)
lines(fits.ARIMA, col="red")

fore.ARIMA=predict(modelARIMA,365)$pred                 #define forecast value for testing data
plot(fore.ARIMA)
se.fore.ARIMA=predict(modelARIMA, 365)$se  

#PLOTTING FOR TRAINING DATA#
par(mfrow=c(1,1))
plot(as.ts(Ytrain),ylab="Yt",xlab="t",lwd=2,ylim=c(a*0.9,b*1.1))
title("Training",line=0.3,cex.main=0.9)
lines(as.ts(fits.ARIMA),col="red",lwd=2)

plot(as.ts(Ytest))
title("Testing",line=0.3,cex.main=0.9)
lines(as.vector(fore.ARIMA),col="red",lwd=2)
lines(as.vector(lower),col="blue2",lty="dotdash",lwd=2)
lines(as.vector(upper),col="blue2",lty="dotdash",lwd=2)


#DIAGNOSTIC CHECKING FOR ARIMA MODEL
#Independency test by using Ljung-Box test
lags <- c(6,12,18,24,30,36,42,48)                     #lag we used
p=7                                                   #the number of ar parameter
q=0                                                   #the number of ma parameter
LB.result<-matrix(0,length(lags),2)
for(i in seq_along(lags))
{
  LB.test=Box.test (resi.ARIMA, lag = lags[i],type = c("Ljung-Box"),fitdf=p+q)
  LB.result[i,1]=LB.test$statistic
  LB.result[i,2]=LB.test$p.value
}
rownames(LB.result)<-lags
colnames(LB.result)<-c("statistics","p.value")
LB.result

acf(resi.ARIMA)
pacf(resi.ARIMA)

ACF = 2,4,5,7,11,12,14,18,19,20,21,24,25,27,28
PACF = 2,4,5,6,7,12,19,22

ORDER MUSIMAN: AR(7,14,21,28) 
PACF hanya signifikan pada lag 7 -> maka PACF cut off setelah lag 7
ACF signifikan pada lag 7, 14, 21, 28
Maka ACF bisa dikatakan dies down musiman dengan periode 7
Dugaan model musiman: AR(1)^7

ORDER NON MUSIMAN: 
PACF yang signifikan: lag 2,4,5,6
ACF yang signifikan: lag 2,4,5
Dugaan model:
1. AR([2,4,5,6])
2. MA([2,4,5])

