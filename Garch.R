library(rmgarch)
library(rugarch)
library(ks)
library(tseries)
library(e1071)
library(zoo)
library(FinTS)
library(fGarch)


deltadata <- read.csv("/Users/philiprettig/Desktop/Universitet/Ekonomi/Ekonometri/Kandidat/BTC-USD-deltalog.csv", sep = ";", dec = ",")
delta <- read.csv("/Users/philiprettig/Desktop/Universitet/Ekonomi/Ekonometri/Kandidat/deltadata.csv", sep = ";", dec = ".")

adf.test(deltadata$DeltaLogBit)

stargazer::stargazer(data, type = "latex")
stargazer::stargazer(delta, type = "latex")

lines(deltadata$DeltaLogBit, color = "red")

volModel <- deltadata$DeltaLogBit ~ deltadata$S.P500 + deltadata$GoTrend.B. + deltadata$Gold + deltadata$Inflation + deltadata$VIX + deltadata$M2 
olsVol <- lm(volModel)
stargazer::stargazer(olsVol, type= "text") #Agerar assymetrisk med marknaden

GarchLog <- garchFit(data = deltadata$DeltaLogBit)
GarchLogEth <- garchFit(data=deltadata$DeltaLogEth)
summary(GarchLogEth)
plot(GarchLog)
plot(GarchLogEth)

#Steg 1 - specificiera model 
uspec = ugarchspec(mean.model = list(armaOrder = c(0,0)), variance.model = list(garchOrder =  c(1,1), model = "sGARCH"), distribution.model = "norm")
uspec
spec1 <- dccspec(uspec = multispec(replicate(2,uspec)),dccOrder = c(1,1), distribution = "mvnorm")

#Fit våra olika modeller
fitE1 <- dccfit(spec1,data = data.frame(delta$VolEthereum,delta$VolGold))
fitE2 <- dccfit(spec1,data = data.frame(delta$VolEthereum,delta$VolBitcoin))
fitE3 <- dccfit(spec1,data = data.frame(delta$VolEthereum,delta$VolS.P500))
fitE4 <- dccfit(spec1,data = data.frame(delta$VolEthereum,delta$VolGoTrend.B.))
fitE5 <- dccfit(spec1,data = data.frame(delta$VolEthereum,delta$VolGoTrend.E.))
fitE6 <- dccfit(spec1,data = data.frame(delta$VolEthereum,delta$VolInflation))
fitE7 <- dccfit(spec1,data = data.frame(delta$VolEthereum,delta$VolOil)) 

fitB1 <- dccfit(spec1,data = data.frame(delta$VolBitcoin,delta$VolGold))
fitB2 <- dccfit(spec1,data = data.frame(delta$VolBitcoin,delta$VolEthereum))
fitB3 <- dccfit(spec1,data = data.frame(delta$VolBitcoin,delta$VolS.P500))
fitB4 <- dccfit(spec1,data = data.frame(delta$VolBitcoin,delta$VolGoTrend.B.))
fitB5 <- dccfit(spec1,data = data.frame(delta$VolBitcoin,delta$VolGoTrend.E.))
fitB6 <- dccfit(spec1,data = data.frame(delta$VolBitcoin,delta$VolInflation))
fitB7 <- dccfit(spec1,data = data.frame(delta$VolBitcoin,delta$VolOil)) 



#BitGULD
cor1 <- rcor(fitB7)
dim(cor1)
cor1[,,dim(cor1)[3]]
corBit1 <- cor1[2,1,]
plot.ts(corBit1, ylab = "Correlation",col="darkblue")

#BitGULD
cov1 <- rcov(fitB1)
dim(cor1)
cov1[,,dim(cov1)[3]]
covBit1 <- cov1[2,1,]
plot.ts(covBit1, col="green")


#BitEth
cor2 <- rcor(fitB2)
dim(cor2)
cor2[,,dim(cor2)[3]]
corBit2 <- cor2[2,1,]
plot.ts(corBit2)

#BitSP5
cor3 <- rcor(fitB3)
dim(cor3)
cor3[,,dim(cor3)[3]]
corBit3 <- cor3[2,1,]
plot.ts(corBit3)

#BitSP5
cor4 <- rcor(fitB4)
dim(cor4)
cor4[,,dim(cor4)[3]]
corBit4 <- cor4[2,1,]
plot.ts(corBit4)

#Bit
cor5 <- rcor(fitB5)
dim(cor5)
cor5[,,dim(cor5)[3]]
corBit5 <- cor5[2,1,]
plot.ts(corBit5)

#BitSP5
cor6 <- rcor(fitB6)
dim(cor6)
cor6[,,dim(cor6)[3]]
corBit6 <- cor6[2,1,]
plot.ts(corBit6)

#BitSP5
cor7 <- rcor(fitB7)
dim(cor7)
cor7[,,dim(cor7)[3]]
corBit7 <- cor7[2,1,]
plot.ts(corBit7)

cor8 <- rcor(fitB8)
dim(cor8)
cor8[,,dim(cor8)[3]]
corBit8 <- cor8[2,1,]
plot.ts(corBit8)

cor9 <- rcor(fitB9)
dim(cor9)
cor9[,,dim(cor9)[3]]
corBit9 <- cor9[2,1,]
plot.ts(corBit9)





#Negativ covarians


#S&P
cor2 <- rcor(fit2)
dim(cor2)
cor2[,,dim(cor2)[3]]
corBitS <- cor2[2,1,]
plot.ts(corBitS)


#S&P
cor3 <- rcor(fit3)
dim(cor3)
cor3[,,dim(cor3)[3]]
corEth1 <- cor3[2,1,]
plot.ts(corEth1)

cor4 <- rcor(fit4)
dim(cor4)
cor4[,,dim(cor4)[3]]
corEth2 <- cor4[2,1,]
plot.ts(corEth2)




#Låg covarians därav följer de ej varandra

deltaBlm <- lm(delta$VolBitcoin ~ delta$VolS.P500 + delta$VolGoTrend.B. + delta$VolGoTrend.E. + delta$VolGold + delta$VolInflation + delta$VolOil) 
deltaElm <- lm(delta$VolEthereum ~ delta$VolS.P500 + delta$VolGoTrend.E. + delta$VolGoTrend.B. + delta$VolGold + delta$VolInflation + delta$VolOil) 

stargazer::stargazer(deltaElm, type = "text")

stargazer::stargazer(deltaBlm, type = "text")


# Plots
plot.ts(deltadata$DeltaLogBit, ylab = "Returns", col = "darkblue") + 
  abline(h = 0, col = "darkgray") +
  lines(delta$VolS.P500, col = "green")

plot.ts(delta$VolEthereum, ylab = "Returns", col = "darkblue") + 
  abline(h = 0, col = "darkgray") +
  lines(deltadata$S.P500, col = "green")


plot.ts(deltadata$DeltaLogBit^2, ylab = "Returns Bitcoin", col = "darkblue") + 
  lines(delta$VolS.P500^2, col = "red")
