install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
library(lmtest)

KandiData <- read.csv(file = "/Users/philiprettig/Desktop/BTC-USD.csv", header = TRUE, skip = 0, sep = ";")
dataframe <- as.data.frame(KandiData)
dataKand <- dataframe[nrow(dataframe):1,] #Sorterar efter datum

dataframe

timeseries <- ts(dataframe, frequency = 52, start = c(2015-08-03,))

week = as.Date("2017-06-14") - 0:291

dataframe$week <- week


#Checking if values are N/A

for(i in 1:length(dataKand$GoTrend.E.)) {
  if((dataKand$GoTrend.E.[i] == 0)) {
    print("Data is N/A")
    dataKand$GoTrend.E.[i] = 0.5
  }
}

dataKand$Bitcoin <- as.numeric(gsub("\\.", "", alle$demo))


#Våra olika modeller 

modelBitcoin <- dataKand$Bitcoin ~ dataKand$GoTrend.B. + (dataKand$S.P500) + (dataKand$M2) + (dataKand$Gold) + dataKand$Inflation + dataKand$VIX
olsBitcoin <- lm(modelBitcoin)


modelEther <- log(dataframe$Ethereum) ~ dataframe$GoTrend.E. + log(dataframe$S.P500) + log(dataframe$MoneySupply) + log(dataframe$Price.Gold.) + dataframe$Inflation + dataframe$VIX
olsEthereum <- lm(modelEther)
wlsEthereum <- lm(modelEther, weights = wtsEthereum)

wtsBitcoin <- 1/fitted(lm(abs(residuals(olsBitcoin)) ~ dataframe$GoTrend.B. + log(dataframe$S.P500) + log(dataframe$MoneySupply) + log(dataframe$Price.Gold.) + dataframe$Inflation + dataframe$VIX))^2
wtsEthereum <- 1/fitted(lm(abs(residuals(olsEthereum)) ~ dataframe$GoTrend.E. + log(dataframe$S.P500) + log(dataframe$MoneySupply) + log(dataframe$Price.Gold.) + dataframe$Inflation + dataframe$VIX))^2


## Deskriptiv statistik
stargazer::stargazer(olsBitcoin, type = "text")
summary(olsBitcoin)
stargazer::stargazer(wlsEthereum, type = "text")


## Plots 

hist(olsBitcoin$residuals, col = "Lightgreen") #Lite skev åt höger
hist(olsEthereum$residuals, col = "Lightblue") #Knappt skev
plot(olsBitcoin)
plot(olsEthereum)
plot(wlsEthereum)


#Etherum
dataframe %>%
  #filter( wage < 60) %>%
  ggplot(aes(x = as.Date(DATE), y = Ethereum, size = GoTrend.E.)) +
 # labs(title = "Ethereum pris över tid\nObservationers storlek baseras på hype vid tidpunkten") +
  geom_line(alpha=0.3, color="#6ED8FF")+
  geom_point(alpha=0.75, color="#0B779F")+
  xlab("Tid")+
  ylab("Ethereum Pris ($)")+
  scale_x_date(limit=c(as.Date("2015-08-13"),as.Date("2021-03-01")))+
  theme_minimal()

#Bitcoin
dataframe %>%
  #filter( wage < 60) %>%
  ggplot(aes(x = as.Date(DATE), y = Bitcoin, size = GoTrend.B.)) +
  #labs(title = "Bitcoin pris över tid\nObservationers storlek baseras på hype vid tidpunkten") +
  geom_line(alpha=0.5, color="#97E6B5")+
  geom_point(alpha=1.0, color="#59B77D")+
  xlab("Tid")+
  ylab("Bitcoin Pris ($)")+
  scale_x_date(limit=c(as.Date("2015-08-13"),as.Date("2021-03-01")))+
  theme_minimal()



#geom_smooth(method = lm)

#geom_smooth(method=lm , color="Darkblue", fill="#69b3a2", se=TRUE) + LINJÄR TREND
#plot(olsBitcoin)
#hist(olsBitcoin$residuals) #Indikerar att fördelningen är skev åt höger
#plot(modelBitcoin)
#hist(olsEthereum$residuals)

# Testa för heteroskedasticitet
bptest(olsBitcoin) 

#Väldigt högt P-värde innebär att vi med säkerhet kan förkaste våran nollhypotes https://www.statology.org/breusch-pagan-test-r/

#stargazer::stargazer(olsBitcoin, type = "text")

