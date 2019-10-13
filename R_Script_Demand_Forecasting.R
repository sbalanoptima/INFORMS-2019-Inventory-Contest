# INFORMS 2019
# A Contest fORged by Amazon Web Services and Computational OR Exchange
# R script for demand forecasting for hardware device

rm(list=ls()) # Clear Global environment


library(forecast)

# Copy data from excel: select 120 data point
x<-scan("clipboard") #or
Demand <- ts(x,frequency=12,start=c(1996,1))
plot.ts(Demand)

# Train.csv file consists of Jan 1995 to Dec 2004 demand data
x <- read.table("C:\\Users\\HP\\Desktop\\INFORMS\\Competition\\train.csv",sep=",",header=T)
y <- ts(x,frequency=12,start=c(1996,1))
plot.ts(y)

# ACF and PACF plots
Acf(y,lag.max=NULL,type=c("correlation","covariance","partial"),plot=TRUE,na.action=na.fail,demean=TRUE,main="Auto Correlation function")$acf
Pacf(y,lag.max=NULL,plot=TRUE,na.action=na.fail,main="Partial Auto Correlation function")$acf

Acf(y,lag=24)$acf
Pacf(y,lag=24)$acf

# To check the differencing plot
z <- diff(y,1)
plot(z)

# how to convert to time series object
#class(y)

#start(y)
#end(y)

#frequency(y)

#summary(y)
#sd(y)

#y
##############################################

tsdata<- ts(y,frequency=12)
tsdata
ddata <- decompose(tsdata,"multiplicative")
ddata1 <- decompose(tsdata,"additive")

nsdiffs(y) # seasonal difference
z1 <- diff(y,12)
plot(z1)

ndiffs(z1)

# Data plots for multiplicative
plot(ddata)

plot(ddata$trend)

plot(ddata$seasonal)

plot(ddata$random)

# Data plots for additive
plot(ddata1)

plot(ddata1$trend)

plot(ddata1$seasonal)

plot(ddata1$random)

################################################

plot(y)
abline(reg=lm(y~time(y)))
cycle(y)

###############################################
# Get a box plot

boxplot(y~cycle(y),xlab="Months", ylab="Demand")

################################################
# Stationary
plot(y)

#################################################
# ARIMA to calc p,d,q
model1 <- auto.arima(y)
summary(model1)
model1.fcast <- forecast(model1,level=c(95),h=12)
model1.fcast
plot(model1.fcast,xlab="year",ylab="demand")
Box.test(model1$residuals, lag = 8, type = c("Ljung-Box"), fitdf = 7)

############REview######################

model2 <- Arima(y,order=c(1,1,2),seasonal=c(2,1,1))
summary(model2)
model2.fcast <- forecast(model2,level=c(95),h=12)
model2.fcast
plot(model2.fcast,xlab="year",ylab="demand")
Box.test(model2$residuals, lag = 7, type = c("Ljung-Box"), fitdf = 6)

model3 <- Arima(y,order=c(2,0,1),seasonal=c(1,0,1))
summary(model3)
model3.fcast <- forecast(model3,level=c(95),h=12)
model3.fcast
plot(model3.fcast)
Box.test(model3$residuals, lag = 6, type = c("Ljung-Box"), fitdf = 5)

model4 <- Arima(y,order=c(11,1,1))
summary(model4)
model4.fcast <- forecast(model4,level=c(95),h=12)
model4.fcast
plot(model4.fcast)
Box.test(model4$residuals, lag = 13, type = c("Ljung-Box"), fitdf = 12)

model5  <- auto.arima(y, xreg=fourier(y,K=5), seasonal=FALSE)
summary(model5)
model5.fcast <- forecast(model5, xreg=fourier(y, K=5, h=12))
model5.fcast
Box.test(model5$residuals, lag = 3, type = c("Ljung-Box"), fitdf = 2)
autoplot(model5.fcast,main="Forecast from ARIMA(0,1,2)")

model6  <- auto.arima(y, xreg=fourier(y,K=4), seasonal=FALSE)
summary(model6)
model6.fcast <- forecast(model6, xreg=fourier(y, K=4, h=12))
model6.fcast
Box.test(model6$residuals, lag = 4, type = c("Ljung-Box"), fitdf = 3)
autoplot(model6.fcast,main="Forecast from regression with ARIMA(1,1,2)")

model7  <- auto.arima(y, xreg=fourier(y,K=3), seasonal=FALSE)
summary(model7)
model7.fcast <- forecast(model7, xreg=fourier(y, K=3, h=12))
model7.fcast
Box.test(model7$residuals, lag = 6, type = c("Ljung-Box"), fitdf = 5)
autoplot(model7.fcast,main="Forecast from regression with ARIMA(2,1,3)")

model8  <- auto.arima(y, xreg=fourier(y,K=6), seasonal=FALSE)
summary(model8)
model8.fcast <- forecast(model8, xreg=fourier(y, K=6, h=12))
model8.fcast
Box.test(model8$residuals, lag = 4, type = c("Ljung-Box"), fitdf = 3)
autoplot(model8.fcast,main="Forecast from regression with ARIMA(3,1,0)")

Acf(model8$residuals,lag=36,main="Acf plot")$acf # MA terms
Pacf(model8$residuals,lag=36,main="Pacf plot")$acf # AR terms

#Box.test(mymodel$residuals, lag = 6, type = c("Ljung-Box"), fitdf = 5)

#Acf(z1,lag=36)$acf
#Acf(z,lag=36)$acf
#Pacf(z,lag=36)$acf

# LEts run with trace to compare the information criteria
#auto.arima(y,ic="aic", trace=TRUE)

library(tseries)

# adf.test(mymodel)
plot.ts(model8$residuals)
model8$residuals

# residual plot and distribution
library(fitdistrplus)
library(actuar)

# Upload from excel file 
# Copy paste the residuals
x <- scan("clipboard")

# normal plots and QQ plots: check the normality for residuals
qqnorm(x)
qqline(x)

## To give you a list of competing distributions
descdist(x, discrete = FALSE) # Continuous distribution 
#descdist(x, discrete = TRUE) # Discrete distribution

## Find parameters and check AIC values
fit<- fitdist(x, "norm") ## normal distribution taken as an example
summary(fit)


fn<-fitdist(x,"norm")


gofstat(fn,fitnames=c("norm"))

par(mfrow=c(2,2))

plot.legend<-c("normal")

denscomp(fn, legendtext = plot.legend)
qqcomp(fn, legendtext = plot.legend)
cdfcomp(fn, legendtext = plot.legend)
ppcomp(fn, legendtext = plot.legend)

dev.off()

# AIC lower the better
#acf(ts(mymodel$residuals),main='ACF Residual')
#pacf(ts(mymodel$residuals),main='PACF Residual')

######################################################
# Using the best fit model to predict forecast

# Copy data from excel: select all 120 data points from 1995 Jan to 2005 Dec
x<-scan("clipboard")
y <- ts(x,frequency=12,start=c(1996,1))
plot.ts(y)

tsdata<- ts(y,frequency=12)
tsdata
ddata <- decompose(tsdata,"multiplicative")

# Data plots for multiplicative
plot.legend<-c("Given demand data")
plot(ddata)

plot(ddata$trend)

plot(ddata$seasonal)

plot(ddata$random)

modelf  <- auto.arima(y, xreg=fourier(y,K=6), seasonal=FALSE)
summary(modelf)
write.table(modelf$residuals,"C:\\Users\\HP\\Desktop\\INFORMS\\Competition\\residual.csv",sep=",",row.names=T,col.names=T)
mean(modelf$residuals)
sd(modelf$residuals)

# Use the model to forecast for next 2 years at 95% confidence
modelf.fcast <- forecast(modelf, xreg=fourier(y, K=6, h=2*12))
modelf.fcast
write.table(modelf.fcast,"C:\\Users\\HP\\Desktop\\INFORMS\\Competition\\forecast.csv",sep=",",row.names=T,col.names=T)
Box.test(modelf$residuals, lag = 4, type = c("Ljung-Box"), fitdf = 3)
autoplot(modelf.fcast,main="Forecast with ARIMA(3,1,0)")


