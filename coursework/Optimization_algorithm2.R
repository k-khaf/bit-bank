library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(DEoptim)
library(ROI)
require(ROI.plugin.glpk)
require(ROI.plugin.quadprog)
library(ROI.plugin.symphony)
library(corpcor)
library(BatchGetSymbols)


tickers <- c("FB", "AAPL", "AMZN", "NFLX", "GOOGL", "SQ", "NVDA")
portfolioPrices <- NULL
for(ticker in tickers) {
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols.yahoo(ticker, from='2015-01-01', periodicity = 'daily', auto.assign=FALSE)[,4])
}

portfolioReturns <- na.omit(ROC(portfolioPrices))


port1 <- portfolio.spec(colnames(portfolioReturns))
port1 <- add.constraint(port1, type = "box", min = 0, max = 0.7) # Box
port1 <- add.constraint(port1, type="transaction_cost", ptc = 0.001)  #transaction cost
port1 <- add.constraint(portfolio=port1, type="long_only")  #long only
port1 <- add.constraint(port1, type="weight_sum", min_sum=0.99, max_sum=1.01)# Leverage
port1 <- add.objective(port1, type = "risk", name = "StdDev")# Get minimum variance portfolio

#rportfolios1 <- random_portfolios(port1, permutations = 10000, rp_method = "sample")


# Optimize with min var only
port.opt1 <- optimize.portfolio(portfolioReturns, port1, optimize_method = "ROI", trace = TRUE)

chart.Weights(port.opt1)

ef1 <- extractEfficientFrontier(port.opt1, match.col = "StdDev", n.portfolios = 25,
                               risk_aversion = NULL)

chart.EfficientFrontier(ef1,
                        match.col = "StdDev", n.portfolios = 25, xlim = NULL, ylim = NULL,
                        cex.axis = 0.8, element.color = "darkgray", main = "Efficient Frontier 1",
                        RAR.text = "SR", rf = 0, tangent.line = TRUE, cex.legend = 0.8,
                        chart.assets = TRUE, labels.assets = TRUE, pch.assets = 21,
                        cex.assets = 0.8)

# Generate vector of returns
meanReturns1 <- colMeans(portfolioReturns)
covMat1 <- cov(portfolioReturns)

minret1 <- 0.06/100
maxret1 <- port.opt1$weights %*% meanReturns1

vec1 <- seq(minret1, maxret1, length.out = 100)


######################################################################################################

tickers <- c("FB", "AAPL", "AMZN", "NFLX", "GOOGL", "SQ", "NVDA")
portfolioPrices <- NULL
for(ticker in tickers) {
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols.yahoo(ticker, from='2015-01-01', periodicity = 'daily', auto.assign=FALSE)[,4])
}

portfolioReturns <- na.omit(ROC(portfolioPrices))


port2 <- portfolio.spec(colnames(portfolioReturns))
port2 <- add.constraint(port2, type = "box", min = 0, max = 0.9) # Box
port2 <- add.constraint(port2, type="transaction_cost", ptc = 0.001)  #transaction cost
port2 <- add.constraint(portfolio=port2, type="long_only")  #long only
port2 <- add.constraint(port2, type="weight_sum", min_sum=0.99, max_sum=1.01)# Leverage
port2 <- add.objective(port2, type = "return", name = "mean")# Get max mean portfolio

rportfolios2 <- random_portfolios(port2, permutations = 10000, rp_method = "sample")

# Optimize with max mean only
port.opt2 <- optimize.portfolio(portfolioReturns, port2, optimize_method = "random", trace = TRUE,
                               rp = rportfolios2)

chart.Weights(port.opt2)

ef2 <- extractEfficientFrontier(port.opt2, match.col = "StdDev", n.portfolios = 25,
                               risk_aversion = NULL)

chart.EfficientFrontier(ef2,
                        match.col = "StdDev", n.portfolios = 25, xlim = NULL, ylim = NULL,
                        cex.axis = 0.8, element.color = "darkgray", main = "Efficient Frontier",
                        RAR.text = "SR", rf = 0, tangent.line = TRUE, cex.legend = 0.8,
                        chart.assets = TRUE, labels.assets = TRUE, pch.assets = 21,
                        cex.assets = 0.8)

# Generate vector of returns
meanReturns2 <- colMeans(portfolioReturns)
covMat2 <- cov(portfolioReturns)

minret2 <- 0.06/100
maxret2 <- port.opt2$weights %*% meanReturns2

vec2 <- seq(minret2, maxret2, length.out = 100)


###################################### USE THIS CODE ###################################################


#tickers <- c("FB", "AAPL", "AMZN", "NFLX", "GOOGL", "SQ", "NVDA")
#tickers <- c("CPG.L","PSN.L","EXPN.L","BA.L","CCH.L","RMV.L","AHT.L","TSCO.L","SSE.L","BATS.L","SMIN.L","RB.L","CCL.L","SPX.L","SDR.L","PRU.L","STJ.L","EZJ.L","RTO.L","CNA.L","RR.L","VOD.L","ANTO.L","TUI.L","NMC.L")
#tickers <- c("CPG.L","PSN.L","EXPN.L","BA.L","CCH.L","RMV.L","AHT.L")

df.SP500 <- GetSP500Stocks()
tickers <- df.SP500$Tickers

#write.csv(tickers,"sp500ticker.csv")

portfolioPrices <- NULL
for(ticker in tickers) {
  portfolioPrices <- cbind(portfolioPrices,
                           try(
                           getSymbols.yahoo(ticker, from='2015-01-01', periodicity = 'daily', auto.assign=FALSE)[,4])
                           )
}

# Removing Tickers with more than 100 NAs to clean up the dataset
store <- c()
for (i in 1:(ncol(portfolioPrices))){
  store[i] <- sum(is.na(portfolioPrices[,i]))
  
}

rem <- 0
for (j in 1:length(store)){
  if (store[j] >= 100){
    portfolioPrices <- portfolioPrices[,-(j-rem)]
    rem <- rem + 1
  }
}

# Portfolio Returns
portfolioReturns <- na.omit(ROC(portfolioPrices))

# Clean up Ticker names
col <- colnames(portfolioReturns)
col <- gsub(".Close","",col)
colnames(portfolioReturns) <- col




# # check....
# store3 <- c()
# for (k in 1:(ncol(portfolioPrices))){
#   store3[k] <- sum(is.na(portfolioPrices[,k]))
#   
# }
# store3






# Portfolio Setup
port3 <- portfolio.spec(colnames(portfolioReturns))
port3 <- add.constraint(port3, type = "box", min = 0, max = 0.4) # Box
#port3 <- add.constraint(port3, type="transaction_cost", ptc = 0.001)  #transaction cost
port3 <- add.constraintstraint(portfolio=port3, type="long_only")  #long only
port3 <- add.constraint(port3, type="weight_sum", min_sum=1, max_sum=1)# Leverage
port3 <- add.objective(port3, type = "risk", name = "StdDev")# Get minimum variance portfolio
port3 <- add.objective(port3, type = "return", name = "mean")

# Optimize with max mean & mean variance
port.opt3 <- optimize.portfolio(portfolioReturns, port3, optimize_method = "ROI", maxSR = TRUE, trace = TRUE)

chart.Weights(port.opt3)

ef3 <- extractEfficientFrontier(port.opt3, match.col = "StdDev", n.portfolios = 30,
                               risk_aversion = NULL)

chart.EfficientFrontier(ef3,
                        match.col = "StdDev", n.portfolios = 30, xlim = NULL, ylim = NULL,
                        cex.axis = 0.8, element.color = "darkgray", main = "Efficient Frontier 3",
                        RAR.text = "SR", rf = 0, tangent.line = TRUE, cex.legend = 0.8,
                        chart.assets = FALSE, labels.assets = TRUE, pch.assets = 21,
                        cex.assets = 0.8)

# Backtesting
rportfolios3 <- random_portfolios(port3, permutations = 10000, rp_method = "sample")

opt_rebal3 <- optimize.portfolio.rebalancing(portfolioReturns,
                                             port3,
                                             search_size = 5000,
                                             optimize_method="random",
                                             rp=rportfolios3,
                                             rebalance_on="months",
                                             training_period=1,
                                             rolling_window=10)

equal_weight3 <- rep(1 / ncol(portfolioReturns), ncol(portfolioReturns))
benchmark3 <- Return.portfolio(portfolioReturns, weights = equal_weight3)
colnames(benchmark3) <- "Benchmark Portfolio"

# FTSEprices <- getSymbols.yahoo("^FTSE", from='2015-01-01', periodicity = 'daily', auto.assign=FALSE)[,4]
# FTSERets <- na.omit(ROC(FTSEprices))
# FTSERets <- as.xts(FTSERets)

sp500prices <- getSymbols.yahoo("SPY", from='2015-01-01', periodicity = 'daily', auto.assign=FALSE)[,4]
sp500Rets <- na.omit(ROC(sp500prices))
sp500Rets <- as.xts(sp500Rets)

chart.Weights(opt_rebal3, main="Rebalanced Weights Over Time")

rebal_weights3 <-extractWeights(opt_rebal3)
rebal_returns3 <- Return.portfolio(portfolioReturns, weights=rebal_weights3)

rets_df3 <- cbind(rebal_returns3, benchmark3, sp500Rets)

charts.PerformanceSummary(rets_df3, main="P/L Over Time")

###########################################################################################################################



sp500 <- GetSP500Stocks()
tickers <- sp500$tickers


df.SP500 <- GetSP500Stocks()
tickers <- df.SP500$Tickers
############################################################################################################################











































# Backtesting

opt_rebal <- optimize.portfolio.rebalancing(portfolioReturns,
                                            port,
                                            search_size = 5000,
                                            optimize_method="random",
                                            rp=rportfolios,
                                            rebalance_on="months",
                                            training_period=1,
                                            rolling_window=10)

equal_weight <- rep(1 / ncol(portfolioReturns), ncol(portfolioReturns))
benchmark <- Return.portfolio(portfolioReturns, weights = equal_weight)
colnames(benchmark) <- "Benchmark Portfolio"

FTSEprices <- getSymbols.yahoo("^FTSE", from='2015-01-01', periodicity = 'daily', auto.assign=FALSE)[,4]
FTSERets <- na.omit(ROC(FTSEprices))
FTSERets <- as.xts(FTSERets)

chart.Weights(opt_rebal, main="Rebalanced Weights Over Time")

rebal_weights <-extractWeights(opt_rebal)
rebal_returns <- Return.portfolio(portfolioReturns, weights=rebal_weights)

rets_df <- cbind(rebal_returns, benchmark, FTSERets)

charts.PerformanceSummary(rets_df, main="P/L Over Time")