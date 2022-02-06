library(dplyr)
library(quantmod)
library(PerformanceAnalytics)
library(imputeTS)
library(PortfolioAnalytics)

tickers <- c("BTC-USD", "ETH-USD", "GC=F", "^GSPC")
weightsBIT <- c(.1, 0, 0, .9)
weightsETH <- c(0, 0.1, 0, .9)
weightsGOLD <- c(0, 0, 0.1, .9)
WeightsCrypto <- c(.05, .05, 0, .9)
weightsALL <- c(.05, .05, .05, .85)


portfolioPrices <- NULL
for (Ticker in tickers) {
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols.yahoo(Ticker, from="2015-08-07", periodicity = "weekly", auto.assign=FALSE)[,4])}

benchmarkPrices <- getSymbols.yahoo("^GSPC", from="2015-08-07", periodicity = "weekly", auto.assign=FALSE)[,4]

benchmarkReturns <- na.omit(ROC(benchmarkPrices, type="discrete"))


colnames(portfolioPrices) <- tickers
colSums(is.na(portfolioPrices))

plot(portfolioPrices, legend = tickers)


weeklyReturns <- na.omit(ROC(portfolioPrices, type="discrete"))


portfolioReturn <- Return.portfolio(weeklyReturns, weights=weightsBIT)


chart.CumReturns(portfolioReturn)
charts.PerformanceSummary(portfolioReturn)


CAPM.beta(portfolioReturn, benchmarkReturns, .035/52)
CAPM.beta.bull(portfolioReturn, benchmarkReturns, .035/52)
CAPM.beta.bear(portfolioReturn, benchmarkReturns, .035/52)


#Räkna ratios
CAPM.jensenAlpha(portfolioReturn, benchmarkReturns, .035/52)
SharpeRatio(portfolioReturn, Rf = .035/52)
table.AnnualizedReturns(portfolioReturn, Rf=.035/52, geometric=TRUE)

