library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(DEoptim)
library(BatchGetSymbols)

library(caTools)
library(timeSeries)
library(fPortfolio)

######################################################################################################################
# Returns index tickers and returns

optimizeIndex <- function(index){
  
  # If statement to determine tickers for respective index
  if (index == "SPY"){
    
    # tickers <- readRDS("C:/Users/kian/Desktop/R/Portfolio-optimization/data/tickerNames.rds")
    # tickers <- tickers[-11:-length(tickers)]
    
    tickers<-c("MMM"  , "ABT" ,  "ABBV" , "ABMD" , "ACN"   ,"ATVI"  ,"ADBE" , "AMD"   ,"AAP"  , "AES",   "AFL"  , "A"  ,  
               "APD" ,  "AKAM" , "ALK"   ,"ALB" ,  "ARE"  , "ALXN"  ,"ALGN" , "ALLE",  "AGN" ,  "ADS" ,  "LNT" ,  "ALL"  ,
               "GOOGL" ,"GOOG" , "MO",    "AMZN"  ,"AMCR" , "AEE" ,  "AAL"  , "AEP"   ,"AXP"  , "AIG",   "T"    ,"AMT"  ,
               "AWK"  , "AMP" ,  "ABC"  , "AME",   "AMGN" , "APH" ,  "ADI",   "ANSS" , "ANTM" , "AON" ,  "AOS"  , "APA" , 
               "AIV"   ,"AAPL"  ,"AMAT",  "APTV"  ,"ADM"   ,"ARNC" , "ANET",  "AJG" ,  "AIZ"  , "ATO"  , "ADSK",  "ADP"  ,
               "AZO",   "AVB",   "AVY" ,  "BKR" ,  "BLL"   ,"BAC" ,  "BK"   , "BAX",   "BDX"   ,"BBY" ,  "BIIB"  ,"BLK" , 
               "BA" ,   "BKNG" , "BWA" ,  "BXP",   "BSX"   ,"BMY"   ,"AVGO",  "BR",    "CHRW")
  }
  
  
  # Ticker info:
  
  # Asset Prices
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
  
  # Clean up column names
  col <- colnames(portfolioReturns)
  col <- gsub(".Close","",col)
  colnames(portfolioReturns) <- col
  

  # Portfolio Setup
  portf <- portfolio.spec(colnames(portfolioReturns))
  
  minvar.port<- portfolio.spec(colnames(portfolioReturns))
  
  #Leverage
  portf <- add.constraint(portf, type="weight_sum", min_sum=0.99, max_sum=1.01)
  #transaction cost
  portf <- add.constraint(portf, type="transaction_cost", ptc = 0.001)
  #Box
  portf <- add.constraint(portf, type = "box", min = 0, max = 0.5)
  #Max Pos
  portf <- add.constraint(portf, type = "position_limit", max_pos = 10)
  
  rportfolios <- random_portfolios(portf, permutations = 5000, rp_method = "sample")
  
  minvar.port <- add.objective(portf, type = "risk", name = "var")
  
  minvar.opt <- optimize.portfolio_v2(portfolioReturns, minvar.port, optimize_method = "DEoptim", trace = TRUE, traceDE = 5)
  
  
  # Backtesting
  opt_rebal <- optimize.portfolio.rebalancing(portfolioReturns,
                                              minvar.port,
                                              search_size =2000,
                                              optimize_method="DEoptim",
                                              rp=rportfolios,
                                              rebalance_on="quarters",
                                              training_period=2,
                                              rolling_window=1)
  
  equal_weight <- rep(1 / ncol(portfolioReturns), ncol(portfolioReturns))
  benchmark <- Return.portfolio(portfolioReturns, weights = equal_weight)
  colnames(benchmark) <- "Benchmark Portfolio"
  
  sp500prices <- getSymbols.yahoo("SPY", from='2015-01-01', periodicity = 'daily', auto.assign=FALSE)[,4]
  sp500Rets <- na.omit(ROC(sp500prices))
  sp500Rets <- as.xts(sp500Rets)

  #chart.Weights(opt_rebal, main="Rebalanced Weights Over Time")
  
  rebal_weights <-extractWeights(opt_rebal)
  rebal_returns <- Return.portfolio(portfolioReturns, weights=rebal_weights)
  
  rets_df <- cbind(rebal_returns, benchmark, sp500Rets)
  
  return(list(optPort = minvar.opt, optReabl = opt_rebal, rets_df = rets_df, tickers = tickers, portRets = portfolioReturns))
  
}

#################################################################################################

optimizeTickers <- function(start,benchmark,index,tickers){
  
  tickers <- c(tickers)
  
  startDate <- paste0(as.character(start),"-01-01")
  
  portfolioPrices <- NULL
  for(ticker in tickers) {
    portfolioPrices <- cbind(portfolioPrices,
                             try(
                               getSymbols.yahoo(ticker, from = startDate, periodicity = 'daily', auto.assign=FALSE)[,4])
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
  
  # Clean up column names
  col <- colnames(portfolioReturns)
  col <- gsub(".Close","",col)
  colnames(portfolioReturns) <- col
  
  ##### DEoptim
  portf2 <- portfolio.spec(colnames(portfolioReturns))
  
  minvar.port2<- portfolio.spec(colnames(portfolioReturns))
  
  #Leverage
  portf2 <- add.constraint(portf2, type="weight_sum", min_sum=0.99, max_sum=1.01)
  #transaction cost
  portf2 <- add.constraint(portf2, type="transaction_cost", ptc = 0.001)
  #Box
  portf2 <- add.constraint(portf2, type = "box", min = 0, max = 0.65)
  
  rportfolios2 <- random_portfolios(portf2, permutations = 50000, rp_method = "sample")
  
  minvar.port2 <- add.objective(portf2, type = "risk", name = "var")
  
  minvar.opt2 <- optimize.portfolio(portfolioReturns, minvar.port2, optimize_method = "DEoptim", trace = TRUE, traceDE=5)
  
  
  # Backtesting
  opt_rebal2 <- optimize.portfolio.rebalancing(portfolioReturns,
                                               minvar.port2,
                                               search_size =5000,
                                               optimize_method="DEoptim",
                                               rp=rportfolios2,
                                               rebalance_on="months",
                                               training_period=2,
                                               rolling_window=1)
  
  
  if (benchmark == "Equal Weights"){
    equal_weight3 <- rep(1 / ncol(portfolioReturns), ncol(portfolioReturns))
    benchmark3 <- Return.portfolio(portfolioReturns, weights = equal_weight3)
    colnames(benchmark3) <- "Benchmark Portfolio"
  }
  
  if (index == "SPY"){
    sp500prices <- getSymbols.yahoo("SPY", from = startDate, periodicity = 'daily', auto.assign=FALSE)[,4]
    sp500Rets <- na.omit(ROC(sp500prices))
    sp500Rets <- as.xts(sp500Rets)
  }
  
  rebal_weights3 <- extractWeights(opt_rebal2)
  rebal_returns3 <- Return.portfolio(portfolioReturns, weights=rebal_weights3)
  
  if (index == "SPY"){
    rets_df3 <- cbind(rebal_returns3, benchmark3, sp500Rets)
  }
  else{
    rets_df3 <- cbind(rebal_returns3, benchmark3)
  }
  
  return(list(optPort1 = minvar.opt2, optReabl1 = opt_rebal2, rets_df1 = rets_df3, tickers1 = tickers, portRets1 = portfolioReturns))
}

#################################################################################################

plotWeights <- function(opt.port){
  chart.Weights(opt.port)
}

#################################################################################################

plotEF <- function(portfolioReturns){
  
  returns <- as.timeSeries(portfolioReturns)
  
  ef <- portfolioFrontier(returns)
  plot(ef,c(1,2,3,5))
  
  legend( x="bottomright", 
          legend=c("Equal Weights Portfolio ","Tangent Portfolio", "Min Variance Portfolio"),
          col=c("blue","gray", "red"), lty=c(0,1),   
          pch=c(15,17) )
}

#################################################################################################

plotDrawdowns <- function(rets){
  chart.Drawdown(rets,main = "Drawdowns", legend.loc="bottomleft")
}

#################################################################################################

tableDrawdowns <- function(rets){
  table.Drawdowns(rets)
}

#################################################################################################

plotGrowth <- function(rets){
  chart.CumReturns(rets, main = "Portfolios' Growth", wealth.index = TRUE, legend.loc="topleft")
}

#################################################################################################

tableSummary <- function(rets){
  t <- table.AnnualizedReturns(rets)
  
  if (length(colnames(rets)) == 3){
    colnames(t) <-  c("Portfolio", "Equal-Weighted", "SPY")
  }else{
    colnames(t) <-  c("Portfolio", "Equal-Weighted")
  }
  t
}

#################################################################################################

plotReturns <- function(rets){
  chart.Bar(rets$portfolio.returns, main = "Portfolio Returns", legend.loc = "topright")
}

#################################################################################################

tableWeights <- function(port,tickers){
  c <- t(c(port$weights))
  c <- sapply(c,function(x) x*100)
  c <- formatC(c,digits = 4)
  
  t <- as.matrix(cbind(tickers,c))
  
  colnames(t) <- c("Tickers", "Allocation (%)")
  
  t
}

#################################################################################################