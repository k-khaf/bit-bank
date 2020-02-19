library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)
library(BatchGetSymbols)

tickers <- c("CPG.L","PSN.L","EXPN.L","BA.L","SMT.L","CCH.L","RMV.L","AHT.L","MNG.L","TSCO.L","SSE.L","BATS.L","SMIN.L","RB.L","CCL.L","SPX,L","SDR.L","PRU.L","STJ.L","EZJ.L","RDSA.L","RTO.L","CNA.L","RR.L","JET.L","VOD.L","ANTO.L","TUI.L","AUTO.L","NMC.L")
  
FTSE100info <- GetFTSE100Stocks()
FTSE100tickers <- FTSE100info[,2]

portfolioPrices1 <- NULL
for(ticker in FTSE100tickers) {
  try(
    portfolioPrices1 <- cbind(portfolioPrices1,
                              getSymbols.yahoo(ticker, from='2015-01-01', periodicity = 'daily', auto.assign=FALSE)[,4])
    )
} 

getSymbols("ANTO.L")


# WoW <- new.env()
# ##
# sapply(FTSE100tickers, function(x){
#   try(
#     getSymbols(
#       x,
#       from=as.Date("2010-01-01"),
#       env=WoW),
#     silent=TRUE)
# })
# 
# tickers <- ls(WoW)
# 
# portfolioPrices <- NULL
# for(ticker in tickers) {
#   portfolioPrices <- cbind(portfolioPrices,
#                            getSymbols.yahoo(ticker, from='2019-01-01', periodicity = 'daily', auto.assign=FALSE)[,4])
# }  
# 
# FTSE100prices <- getSymbols.yahoo("^FTSE", from='2010-01-01', periodicity = 'daily', auto.assign=FALSE)[,4]
# FTSE100Rets <- na.omit(ROC(sp500prices))
# FTSE100Rets <- as.xts(sp500Rets)

tickers <- c("FB", "AAPL", "AMZN", "NFLX", "GOOGL", "SQ", "NVDA")

portfolioPrices <- NULL
for(ticker in tickers) {
  portfolioPrices <- na.omit(cbind(portfolioPrices,
                           getSymbols.yahoo(ticker, from='2015-01-01', periodicity = 'daily', auto.assign=FALSE)[,4]))
} 

portfolioReturns <- na.omit(ROC(portfolioPrices))

FTSE100info <- GetFTSE100Stocks()
FTSE100tickers <- FTSE100info[,2]
##################################################################


tickers <- c("CPG.L","PSN.L","EXPN.L","BA.L","CCH.L","RMV.L","AHT.L","TSCO.L","SSE.L","BATS.L","SMIN.L","RB.L","CCL.L","SPX.L","SDR.L","PRU.L","STJ.L","EZJ.L","RTO.L","CNA.L","RR.L","VOD.L","ANTO.L","TUI.L","NMC.L")

portfolioPrices <- NULL
for(ticker in tickers) {
    portfolioPrices <- cbind(portfolioPrices,
                              getSymbols.yahoo(ticker, from='2015-01-01', periodicity = 'daily', auto.assign=FALSE)[,4])
} 

portfolioReturns <- na.omit(ROC(portfolioPrices))


portf <- portfolio.spec(colnames(portfolioReturns))

portf <- add.constraint(portf, type="weight_sum", min_sum=0.99, max_sum=1.01)
#portf <- add.constraint(portf, type="transaction_cost", ptc = 0.001)
portf <- add.constraint(portf, type="box", min=0, max=0.5)
portf <- add.objective(portf, type="return", name="mean")
portf <- add.objective(portf, type="risk", name="StdDev", targert = 0.005)



optPort <- optimize.portfolio(portfolioReturns, portf, optimize_method = "ROI",trace = TRUE)

chart.Weights(optPort)

ef <- extractEfficientFrontier(optPort, match.col = "StdDev", n.portfolios = 25,
                               risk_aversion = NULL)

chart.EfficientFrontier(ef,
                        match.col = "StdDev", n.portfolios = 25, xlim = NULL, ylim = NULL,
                        cex.axis = 0.8, element.color = "darkgray", main = "Efficient Frontier",
                        RAR.text = "SR", rf = 0, tangent.line = TRUE, cex.legend = 0.8,
                        chart.assets = TRUE, labels.assets = TRUE, pch.assets = 21,
                        cex.assets = 0.8)



# Backtesting
rp <- random_portfolios(portf, 10000, "sample")

opt_rebal <- optimize.portfolio.rebalancing(portfolioReturns,
                                            portf,
                                            search_size = 5000,
                                            optimize_method="random",
                                            rp=rp,
                                            rebalance_on="quarters",
                                            training_period=1,
                                            rolling_window=3)

equal_weight <- rep(1 / ncol(portfolioReturns), ncol(portfolioReturns))
benchmark <- Return.portfolio(portfolioReturns, weights = equal_weight)
colnames(benchmark) <- "Benchmark Portfolio"

FTSEprices <- getSymbols.yahoo("^FTSE", from='2015-01-01', periodicity = 'daily', auto.assign=FALSE)[,4]
FTSERets <- na.omit(ROC(FTSEprices))
FTSERets <- as.xts(sp500Rets)

chart.Weights(opt_rebal, main="Rebalanced Weights Over Time")

rebal_weights <-extractWeights(opt_rebal)
rebal_returns <- Return.portfolio(portfolioReturns, weights=rebal_weights)

rets_df <- cbind(rebal_returns, benchmark, FTSERets)

charts.PerformanceSummary(rets_df, main="P/L Over Time")



