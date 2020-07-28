library(tseries)
library(forecast)
data<-forecast::gas
summary(data)
str(data)
start(data)
end(data)
plot(data)

#trim data
datanew<-window(data,start=c(1970,1))
str(datanew)
start(datanew)
end(datanew)
plot(datanew)

#transform data and  decompose data 
datanew1<-log(datanew)
decomp1<-stl(datanew1,s.window = 'p')
plot(datanew1)
plot(decomp1)
plot(decompose(datanew,type = "multiplicative"))

#seasonalty of data 
monthplot(datanew)
library(fpp2)
ggseasonplot(datanew, year.labels=TRUE, year.labels.left=TRUE) + ylab("degree") + ggtitle("Seasonal plot")
ggseasonplot(datanew, polar=TRUE) + ylab("degree") + ggtitle("Polar seasonal plot")
monthplot(datanew1)
seasonalty<-exp(decomp1$time.series[1:12,1])
seasonalty
plot(seasonalty,type = 'l')

#de- seasonalized data 
deseason<-decomp1$time.series[,2]+decomp1$time.series[,3]
deseason2<-exp(deseason)
decomp1$time.series
ts.plot(deseason)
ts.plot(deseason,datanew1,col=c("red","blue"))
ts.plot(deseason2)
ts.plot(deseason2,datanew,col=c("red","blue"))


#stationarization of data 
#first order difference 
datadiff<-diff(datanew1)
plot(datadiff)
#first order seasonal differencing 
datadiff<-diff(datadiff,12)
plot(datadiff)
adf.test(datadiff)

#train and test 
train<-window(datanew1,start=c(1970,1),end=c(1993,12))
start(train)
end(train)
plot(train)
frequency(train)
test<-window(datanew1,start=c(1994,1))
start(test)
end(test)
plot(test)
frequency(test)
ts.plot(train,test,col=c("blue","red"))
ts.plot(exp(train),exp(test),col=c("blue","red"))


train2<-window(deseason,start=c(1970,1),end=c(1993,12))
test2<-window(deseason,start=c(1994,1))




##########
#using deseasonal data 
adf.test(train2)
datadiff2<-diff(train2)
adf.test(datadiff2)
plot(datadiff2)
acf(datadiff2,lag=50)#q=1
pacf(datadiff2,lag=50)#p=3
modelx1<-arima(train2,c(11,1,1))
modelx1
hist(modelx1$residuals,col="beige")
fitmodel<-fitted(modelx1)
ts.plot(train2,fitmodel,col=c("red","blue"))
ts.plot(test2,forecast(modelx1,h=20)$mean,col=c("red","blue"))
#if model if good then white noice or resedual will not have any pattern in them 
Box.test(modelx1$residuals,lag = 30,type = "Ljung-Box")
acf(modelx1$residuals)
testforcastx1<-forecast(modelx1,h=20)$mean+decomp1$time.series[,1][c(1:20)]
ts.plot(test,testforcastx1,col=c("red","blue"))
vecx1<-cbind(test,testforcastx1)
vecx1<-exp(vecx1)
MAPEx1<-mean(abs(vecx1[,1]-vecx1[,2])/vecx1[,1])
MAPEx1*100
modelfinalx1<-arima(deseason,c(11,1,1))
end(deseason)
finalforcastx1<-forecast(modelfinalx1,h=12)$mean+decomp1$time.series[,1][c(9:12,1:8)]
xx<-forecast(modelfinalx1,h=12)
lowxx<-xx$lower+decomp1$time.series[,1][c(9:12,1:8)]
highxx<-xx$upper+decomp1$time.series[,1][c(9:12,1:8)]
ts.plot(datanew,exp(finalforcastx1),exp(lowxx[,1]),exp(highxx[,1]),col=c("black","blue","green","green"))
finalforcastx1
exp(finalforcastx1)







############
#SARIMA (p,d,q)(P,D,Q)m
adf.test(datanew1)#p-value >0.05
acf(datanew1)
pacf(datanew1)
datadiffx3<-diff(datanew1)
acf(datadiffx3,50)
pacf(datadiffx3,50)
datadiffx3<-diff(datadiffx3,12)
acf(datadiffx3,50)
pacf(datadiffx3,50)
armmodl<-Arima(train,order = c(4,1,1),seasonal = c(3,1,1),method = "CSS")
armmodl
hist(armmodl$residuals)
fitmodelx3<-fitted(armmodl)
ts.plot(train,fitmodelx3,col=c("red","blue"))
Box.test(armmodl$residuals,lag=30,type = "Ljung-Box")
acf(armmodl$residuals)
testforcastx3<-forecast(armmodl,h=20)
round(accuracy(armmodl),2)
vecx3<-cbind(test,testforcastx3$mean)
vecx3<-exp(vecx3)
#plot of fitted values vs orignal values 
ts.plot(vecx3[,1],vecx3[,2],col=c("red","green"))
MAPEx3<-mean(abs(vecx3[,1]-vecx3[,2])/vecx3[,1])
MAPEx3*100
plot(testforcastx3)
#final model
modelfinalx3<-Arima(datanew1,order = c(4,1,1),seasonal = c(3,1,1),method = "CSS")
finalforcastx3<-forecast(modelfinalx3,h=12)
exp(finalforcastx3$mean)
plot(finalforcastx3)




######autoarima model
#####
modelx2<-auto.arima(train,seasonal = TRUE)
modelx2
hist(modelx2$residuals)
fitmodel2<-fitted(modelx2)
ts.plot(train,fitmodel2,col=c("red","blue"))
Box.test(modelx2$residuals,lag=30,type = "Ljung-Box")
acf(modelx2$residuals)
testforcast<-forecast(modelx2,h=20)
round(accuracy(modelx2),2)
vecx2<-cbind(test,testforcast$mean)
vecx2<-exp(vecx2)
#plot of fitted values vs orignal values 
ts.plot(vecx2[,1],vecx2[,2],col=c("red","green"))
MAPEx2<-mean(abs(vecx2[,1]-vecx2[,2])/vecx2[,1])
MAPEx2*100
plot(testforcast)
#final model
modelfinalx2<-auto.arima(datanew1,seasonal = TRUE)
finalforcastx2<-forecast(modelfinalx2,h=12)
plot(finalforcastx2)
exp( finalforcastx2$mean)







################
#error

summary(modelx1)
summary(armmodl)
summary(modelx2)

summary(modelfinalx1)
summary(modelfinalx3)
summary(modelfinalx2)


vecx1<-cbind(test,testforcastx1)
vecx1<-exp(vecx1)
MAPEx1<-mean(abs(vecx1[,1]-vecx1[,2])/vecx1[,1])
MAPEx1*100

vecx11<-cbind(train,fitted(modelx1)+decomp1$time.series[,1][c(1:288)])
vecx11<-exp(vecx11)
MAPEx11<-mean(abs(vecx11[,1]-vecx11[,2])/vecx11[,1])
MAPEx11*100

vecx12<-cbind(datanew1,fitted(modelfinalx1)+decomp1$time.series[,1][c(1:308)])
vecx12<-exp(vecx12)
MAPEx12<-mean(abs(vecx12[,1]-vecx12[,2])/vecx12[,1])
MAPEx12*100



vecx1<-cbind(test,testforcastx3$mean)
vecx1<-exp(vecx1)
MAPEx1<-mean(abs(vecx1[,1]-vecx1[,2])/vecx1[,1])
MAPEx1*100

vecx11<-cbind(train,fitted(armmodl))
vecx11<-exp(vecx11)
MAPEx11<-mean(abs(vecx11[,1]-vecx11[,2])/vecx11[,1])
MAPEx11*100

vecx12<-cbind(datanew1,fitted(modelfinalx3))
vecx12<-exp(vecx12)
MAPEx12<-mean(abs(vecx12[,1]-vecx12[,2])/vecx12[,1])
MAPEx12*100



vecx1<-cbind(test,testforcast$mean)
vecx1<-exp(vecx1)
MAPEx1<-mean(abs(vecx1[,1]-vecx1[,2])/vecx1[,1])
MAPEx1*100

vecx11<-cbind(train,fitted(modelx2))
vecx11<-exp(vecx11)
MAPEx11<-mean(abs(vecx11[,1]-vecx11[,2])/vecx11[,1])
MAPEx11*100

vecx12<-cbind(datanew1,fitted(modelfinalx2))
vecx12<-exp(vecx12)
MAPEx12<-mean(abs(vecx12[,1]-vecx12[,2])/vecx12[,1])
MAPEx12*100
