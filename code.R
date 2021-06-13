library(quantmod)
library(astsa)
library(aTSA)
library(ggplot2)
library(forecast)
library(rugarch)
library(car)
library(fGarch)
library(tseries)
library(WeightedPortTest)
library(ggpubr)

getSymbols('NDX', from='2010-01-01', to='2020-01-01', auto.assign=TRUE)
NASDAQ<-to.daily(NDX)
#chartSeries(NASDAQ,theme='white')
chartSeries(NASDAQ$NDX.Close)
adf.test(na.omit(NASDAQ$NDX.Close))

returns<-diff(log(NDX$NDX.Close))[-1]
chartSeries(returns,theme='white')

#ggAcf(returns)+ggtitle('Log-returns')
#ggPacf(returns)+ggtitle('Log-returns')

ggarrange(ggAcf(returns)+ggtitle('Log-returns'),
          ggPacf(returns)+ggtitle('Log-returns'))
#model1<-auto.arima(returns)
adf.test(na.omit(returns))
#View(diff(returns)[-1])
auto.arima(returns)
arma_aic<-data.frame()
for (i in 0:10) {
  for (j in 0:10) {
    #model<-Arima(returns,order = c(i,0,j),lambda = 'auto')
    model<-Arima(returns,order = c(i,0,j))
    
    arma_aic<-rbind(arma_aic,c(model$aic,i,j))
    print(c(model$aic,i,j))
  }
  
}
#model_arma<-Arima(returns,order = c(4,0,4),lambda = 'auto')
model_arma<-Arima(returns,order = c(3,0,5))

ggAcf((model_arma$residuals))+ggtitle('ARMA(3,5) Residuals')
#ggPacf((model_arma$residuals))
Box.test(x=residuals(model_arma),lag=30,type = c("Box-Pierce", "Ljung-Box")[2])

#ggAcf(abs(model_arma$residuals))+ggtitle('Absolute Value of Residuals')
#ggPacf(abs(model_arma$residuals))+ggtitle('Absolute Value of Residuals')

#ggAcf(model_arma$residuals^2)+ggtitle('Squared Residuals')
#ggPacf(model_arma$residuals^2)+ggtitle('Squared Residuals')
ggarrange(ggAcf(abs(model_arma$residuals))+ggtitle('Absolute Value of Residuals'),
          ggPacf(abs(model_arma$residuals))+ggtitle('Absolute Value of Residuals'),
          ggAcf(model_arma$residuals^2)+ggtitle('Squared Residuals'),
          ggPacf(model_arma$residuals^2)+ggtitle('Squared Residuals'))

arch.test(arima(returns,order = c(3,0,5)))
garch_aic<-data.frame()
for (i in 0:5) {
  for (j in 1:5) {
    #NDX_garch <-ugarchspec(variance.model = list(model="sGARCH",
    #                                              garchOrder=c(i,j)),
    #                        distribution.model = "norm")
    NDX_garch<-garch(x=model_arma$residuals,order=c(i,j))
    #NDX_Garch2 <- ugarchfit(spec=NDX_garch, 
    #                         data=as.vector(residuals(model_arma)))
  
    garch_aic<-rbind(garch_aic,c(AIC(NDX_garch),i,j))
    }
  
}


NDX_garch <-ugarchspec(variance.model = list(model="sGARCH",
                                             garchOrder=c(1,1)),
                       mean.model = list(armaOrder=c(0,0)),
                       
                       distribution.model = "norm")
NDX_Garch2 <-  ugarchfit(spec=NDX_garch, 
                         data=model_arma$residuals)

T<-residuals(NDX_Garch2)
ggAcf(T)+ggtitle('ARMA(3,5)+GARCH(1,1) Residuals')
Box.test(x=T,lag=30,type = c("Box-Pierce", "Ljung-Box")[2])

#dim(T)
#colnames(T)<-'return'
#View(T)
#typeof(T)
T<-as.data.frame(T)
qqPlot(T)
qqnorm(T$V1, pch = 1)
qqline(T$V1, col = "steelblue", lwd = 2)
shapiro.test(T$V1)
plot(NDX_Garch2, which = "ask")
#BerkowitzTest(NDX_Garch2, lags = 1, alpha = 0.05)







egarch_aic<-data.frame()
for (i in 1:5) {
  for (j in 1:5) {
    #NDX_egarch <-ugarchspec(variance.model = list(model="sGARCH",
    #                                              garchOrder=c(i,j)),
    #                        distribution.model = "norm")
    NDX_egarch <-ugarchspec(variance.model = list(model="eGARCH",
                                                  garchOrder=c(i,j)),
                            mean.model = list(armaOrder=c(0,0)),
                            
                            distribution.model = "norm")
    NDX_egarch2 <-  ugarchfit(spec=NDX_egarch, 
                              data=model_arma$residuals)    #NDX_egarch2 <- ugarchfit(spec=NDX_egarch, 
    #                         data=as.vector(residuals(model_arma)))
    
    egarch_aic<-rbind(egarch_aic,c(infocriteria(NDX_egarch2)[1],i,j))
  }
  
}


NDX_egarch <-ugarchspec(variance.model = list(model="eGARCH",
                                              garchOrder=c(4,4)),
                        mean.model = list(armaOrder=c(0,0)),
                        
                        distribution.model = "norm")
NDX_egarch2 <-  ugarchfit(spec=NDX_egarch, 
                          data=model_arma$residuals)

T<-residuals(NDX_egarch2)
ggAcf(T)+ggtitle('ARMA(3,5)+GARCH(1,1) Residuals')
Box.test(x=T,lag=30,type = c("Box-Pierce", "Ljung-Box")[2])

#dim(T)
#colnames(T)<-'return'
#View(T)
#typeof(T)
T<-as.data.frame(T)
qqPlot(T)
qqnorm(T$V1, pch = 1)
qqline(T$V1, col = "steelblue", lwd = 2)
shapiro.test(T$V1)
plot(NDX_egarch2, which = "ask")
#BerkowitzTest(NDX_egarch2, lags = 1, alpha = 0.05)
