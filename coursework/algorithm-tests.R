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
library(fPortfolio)

######################################################################################################################
# Returns index tickers and returns

optimizeIndex <- function(index){
  
  # If statement to determine tickers for respective index
  if (index == "SPY"){
    
    pr <- readRDS("data/portfolioReturns.rds")
    
    SPYtickers <- colnames(pr)
    SPYtickers <- SPYtickers[-11:-length(SPYtickers)]
  }
  
  
  # Ticker info:
  
  # Asset Prices
  portfolioPrices <- NULL
  for(ticker in SPYtickers) {
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
  
  # Portfolio Setup
  port3 <- portfolio.spec(colnames(portfolioReturns))
  port3 <- add.constraint(port3, type = "box", min = 0.02, max = 0.5) # Box
  port3 <- add.constraint(portfolio=port3, type="long_only")  #long only
  port3 <- add.constraint(port3, type="weight_sum", min_sum=0.99, max_sum=1.01)# Leverage
  #port3 <- add.constraint(port3, type = "diversification", div_tagret =0.7)
  port3 <- add.constraint(port3, type = "position_limit", max_pos = 0) # no. non-0 variables = x
  # port3 <- add.constraint(port3, type="transaction_cost", ptc = 0.01)  #transaction cost
  
  port3 <- add.objective(port3, type = "risk", name = "StdDev")# Get minimum variance portfolio
  #port3 <- add.objective(port3, type = "return", name = "mean")
  
  
  #rportfolios3 <- random_portfolios(port3, permutations = 10000, rp_method = "sample")
  
  
  # Optimize with max mean & mean variance
  port.opt3 <- optimize.portfolio(portfolioReturns, port3, optimize_method = "ROI", trace = TRUE)
  
  
  
  # Backtesing
  rportfolios3 <- random_portfolios(port3, permutations = 1000, rp_method = "sample")
  
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
  
  
  
  sp500prices <- getSymbols.yahoo("SPY", from='2015-01-01', periodicity = 'daily', auto.assign=FALSE)[,4]
  sp500Rets <- na.omit(ROC(sp500prices))
  sp500Rets <- as.xts(sp500Rets)
  
  # chart.Weights(opt_rebal3, main="Rebalanced Weights Over Time")
  
  rebal_weights3 <-extractWeights(opt_rebal3)
  rebal_returns3 <- Return.portfolio(portfolioReturns, weights=rebal_weights3)
  
  rets_df3 <- cbind(rebal_returns3, benchmark3, sp500Rets)
  
  #charts.PerformanceSummary(rets_df3, main="P/L Over Time")

  return(list(optPort = port.opt3, optReabl = opt_rebal3, rets_df = rets_df3))
  
}
  
#################################################################################################

plotWeights <- function(opt.port){

  chart.Weights(opt.port)
}

#################################################################################################

plotEF <- function(opt.port){

  ef3 <- extractEfficientFrontier(opt.port, match.col = "StdDev", n.portfolios = 30,
                                  risk_aversion = NULL)
  chart.EfficientFrontier(ef3,
                          match.col = "StdDev", n.portfolios = 30, xlim = NULL, ylim = NULL,
                          cex.axis = 0.8, element.color = "darkgray", main = "Efficient Frontier 3",
                          RAR.text = "SR", rf = 0, tangent.line = TRUE, cex.legend = 0.8,
                          chart.assets = FALSE, labels.assets = TRUE, pch.assets = 21,
                          cex.assets = 0.8)
}

#################################################################################################

plotDrawdowns <- function(rets){
  
  chart.Drawdown(rets)
  
  table.Drawdowns(rets)
}

#################################################################################################



#################################################################################################