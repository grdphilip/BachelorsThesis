# Engle granger cointegration test
library(dynlm)
library(urca)
library(tseries)
library(xtable)
library(lmtest)
library(car)


LongRunB <- lm(log(deltadata$DeltaLogBit) ~ (deltadata$GoTrend.B.) + log(deltadata$Gold) + log(deltadata$S.P500) + deltadata$Inflation)
EQB <-  dynlm(log(data$Bitcoin) ~ log(data$S.P500)+ log(data$Ethereum) + log(data$Close) + data$GoTrend.B.)
summary(ur.df(EQE$residuals))
EQE <-  dynlm(log(data$Ethereum) + dynlm(log(data$Bitcoin) +log(data$S.P500) + log(data$Ethereum) + log(data$Close) + data$GoTrend.B.))
summary(EQB)
summary(EQE)
plot(EQB)
stargazer::stargazer(EQB, type= "latex")
stargazer::stargazer(EQE, type= "latex")

dwtest(EQB)
EQBRes <- EQB$residuals
LongRunResB  <- LongRunB$residuals

summary(EQB$residuals)
adf.test(EQBRes, k=0)
adf.test(EQE$residuals)

adf.test(deltadata$DeltaLogBit, k = 3, alternative = "stationary")
adf.test(data$Bitcoin, k = 1, alternative = "stationary")
adf.test(data$Ethereum, k = 1)
adf.test(deltadata$DeltaLogEth)
adf.test(data$Close, k=1)
adf.test(deltadata$Close, k=1)
adf.test(data$Gold, k=1)
adf.test(deltadata$Gold, k=1)
adf.test(data$GoTrend.E., k=1)
adf.test(deltadata$GoTrend.E., k=1)
adf.test(data$GoTrend.B., k=1)
adf.test(data$S.P500 , k=3)
adf.test(data$Inflation, k=1)

adf.test(EQB$residuals,k=2)
adf.test(delta$VolBitcoin, k=0)
adf.test(delta$VolEthereum, k=0)
adf.test(delta$VolS.P500 , k=0)
adf.test(delta$VolGold , k=0)
adf.test(delta$VolGoTrend.E. , k=0)
adf.test(delta$VolGoTrend.B. , k=0)
adf.test(delta$VolInflation, k=0)
adf.test(delta$VolOil, k=0)

summary(adf.test(deltadata$GoTrend.B., k=1))
bptest(EQB)
stargazer::stargazer(LongRunB, type = "")

summary(LongRunResB)
adf.test(EQBRes, alternative = "stationary", k=1)
summary(ur.df(EQB$residuals))
xtable.summary(stardf)

#Vi genomförde Engler-Granger two step cointegration för att se 
#H0 = No cointegration between Bitcoin and GoTrend(B)
# I och med att våran värde är nära två förkastar vi nollhypotesen

indien <- ur.df(EQB$residuals, type = "none", selectlags = "AIC")
indeienE <- ur.df(EQE$residuals,type = "none", selectlags = "AIC")
indien@lags
indien@teststat
indien@cval

summary(ur.df(delta$VolOil))
summary(ur.df(delta$VolBitcoin, type = "none", selectlags = "AIC"))

qadf(0.01, N=292, trend = "nc")
dwtest(EQB$residuals)
durbinWatsonTest(EQB)
