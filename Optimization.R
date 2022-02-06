
library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
portfolioPrices = NULL
portfolioPrices

tickers
tickers <- c("BTC-USD","ETH-USD", "^GSPC", "GC=F", "BZ=F")

for (Ticker in tickers) {
portfolioPrices <- cbind(portfolioPrices,
                         getSymbols.yahoo(Ticker, from="2015-08-07", to="2021-05-30" ,periodicity = "weekly", auto.assign=FALSE)[,4])
}
portfolioReturns <- na.omit(ROC(portfolioPrices))

portf <- portfolio.spec(colnames(portfolioPrices))
portf <- add.constraint(portf, type="weight_sum", min_sum=0.99, max_sum=1.01)
portf <- add.constraint(portf, type="transaction_cost", ptc = 0.001)
portf <- add.constraint(portf, type="box", min=0.02, max=.40)
portf <- add.objective(portf, type="return", name="mean")
portf <- add.objective(portf, type="risk", name="StdDev", target=0.005)
rp <- random_portfolios(portf, 1000, "sample")
opt <- optimize.portfolio.rebalancing(portfolioReturns,
                                            portf,
                                            optimize_method="random",
                                            maxSR=TRUE,
                                            rp=rp,
                                            rebalance_on="weeks",
                                            training_period=1,
                                            rolling_window=10)

rebal_weights_test <- NULL
rebal_weights_test <- extractWeights(opt)
rebal_returns_test <- NULL
rebal_returns_test <- Return.portfolio(portfolioReturns, weights=rebal_weights_test)



rets_df <- cbind(rebal_returns_test, rebal_returns_normal, sp500Rets, rebal_returns_BIT, rebal_returns_ETH)
charts.PerformanceSummary(rets_df, main="P/L Over Time")
chart.Weights(opt, main="Rebalanced Weights Over Time")
rebal_weights_All <- NULL
rebal_weights_All <- extractWeights(opt_rebal)
rebal_returns_All <- NULL
rebal_returns_All <- Return.portfolio(portfolioReturns, weights=rebal_weights_All)
rets_df <- cbind(rebal_returns_All, rebal_returns_normal, sp500Rets, rebal_returns_BIT, rebal_returns_ETH)


SharpeRatio(portfolioReturns, Rf = .035/252, p = 0.95, FUN = "StdDev",
            weights = NULL, annualize = FALSE)
charts.PerformanceSummary(rebal_returns_All)

table.AnnualizedReturns(portfolioReturns, Rf=.035/252, geometric=TRUE)
table.AnnualizedReturns(portfolioReturns, Rf=.035/252, geometric=TRUE)
