# PLEASE REMOVING THE COMMENTING AN INSTALL THE FOLLOWING PACKAGES BEFORE RUNNING THE CODE

#install.packages("shiny","shinycssloaders","shinyjs","shinyalert",
#                 "quantmod","PerformanceAnalytics","DEoptim","BatchGetSymbols","caTools","timeSeries","fPortfolio")

library(shiny)
library(shinycssloaders)
library(shinyjs)
library(shinyalert)
library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(DEoptim)
library(BatchGetSymbols)
library(caTools)
library(timeSeries)
library(fPortfolio)


source("optimization_algorithms.R")

tickerNames <- readRDS("data/tickerNames.rds")
opt.port <<- readRDS("data/general_optimization_optPort1.RData")


ui <- fluidPage(
  
  titlePanel("Portfolio Optimisation"),
  
  helpText("Note: V1.0 of our portfolio optimisation app currently supports a minimized standard deviation optimisation method."),
  helpText("      To start a new optimisation please click the refresh icon located in the top left of the page."),  
  sidebarLayout(
    
    sidebarPanel("Inputs",
      
      selectInput(inputId = "selectTickers", label = "Do you want to choose tickers?", 
                  choices = c("No","Yes"),
                  selected = NULL, multiple = FALSE,
                  selectize = TRUE, width = NULL, size = NULL),
      
      conditionalPanel(
        condition = "input.selectTickers == 'No'",
        
        selectInput(inputId = "whichIndex", label = "Which index would you like to optimise over?", 
                    choices = c("SPY"),
                    selected = NULL, multiple = FALSE,
                    selectize = TRUE, width = NULL, size = NULL),
      ),
      
      conditionalPanel(
        condition = "input.selectTickers == 'Yes'",
        
        useShinyjs(),
        disabled(
          selectInput(inputId = "portType", label = "Portfolio Type", 
                      choices = c("Tickers","Asset Classes"),
                      selected = NULL, multiple = FALSE,
                      selectize = TRUE, width = NULL, size = NULL)
        ),
          
        selectInput(inputId = "startYR", label = "Start Year", 
                    choices = c(2015L:(as.integer(format(Sys.Date(), "%Y"))-1)),
                    selected = NULL, multiple = FALSE,
                    selectize = TRUE, width = NULL, size = NULL),
        
        disabled(
          selectInput(inputId = "optType", label = "Optimisation Method", 
                      choices = c("Maximize Sharpe Ratio", "Minimize Risk", "Minimize Variance subject to..."),
                      selected = NULL, multiple = FALSE,
                      selectize = TRUE, width = NULL, size = NULL)
        ),
        
        selectInput(inputId = "compPort", label = "Comparitive Portfolio", 
                    choices = c("Equal Weights"),
                    selected = NULL, multiple = FALSE,
                    selectize = TRUE, width = NULL, size = NULL),
        
        selectInput(inputId = "benchmark", label = "Benchmark", 
                    choices = c("None", "Specify Ticker"),
                    selected = "None", multiple = FALSE,
                    selectize = TRUE, width = NULL, size = NULL),
        
        conditionalPanel(
          condition = "input.benchmark == 'Specify Ticker'",
          
          selectInput(inputId = "benchmarkTicker", label = "Benchmark ticker", 
                      choices = c("SPY"),
                      selected = NULL, multiple = FALSE,
                      selectize = TRUE, width = NULL, size = NULL)
        )
      )
    ),
    
    
    mainPanel(
      
      fluidRow(
        
        conditionalPanel(
          condition = "input.selectTickers == 'No'",
          
          titlePanel("General Optimisation"),
          helpText("This version of the app doesn't optimise over the whole index - only 80 random assest within the S&P500"),
          helpText("The results have been stored locally so you don't have to wait for it to load"),
          
          actionButton(inputId = "optButIndex", label = "Optimise", icon = NULL, width = '300px'),
        ),
        
        conditionalPanel(
          condition = "input.selectTickers == 'Yes'",
          
          titlePanel("Asset Selection"),
          helpText("The optimisations can take up to 15 minutes to compute depending on the number of assets."),
          helpText("Please meet the following criteria then hit the check button to enable the optimise button:"),
          helpText("1. Select more than once assests"),
          helpText("2. Select an asset only once"),
      
          column(3,
            selectInput(inputId = "t1", "Asset 1", tickerNames, selected = NULL, multiple = F,
                           selectize = TRUE, width = '200px', size = NULL),
            
            selectInput(inputId = "t2", "Asset 2", tickerNames, selected = NULL, multiple = F,
                           selectize = TRUE, width = '200px', size = NULL),
            
            selectInput(inputId = "t3", "Asset 3", tickerNames, selected = NULL, multiple = F,
                           selectize = TRUE, width = '200px', size = NULL),
            
            selectInput(inputId = "t4", "Asset 4", tickerNames, selected = NULL, multiple = F,
                           selectize = TRUE, width = '200px', size = NULL),
            
            selectInput(inputId = "t5", "Asset 5", tickerNames, selected = NULL, multiple = F,
                           selectize = TRUE, width = '200px', size = NULL),
            
            selectInput(inputId = "t6", "Asset 6", tickerNames, selected = NULL, multiple = F,
                            selectize = TRUE, width = '200px', size = NULL),
            
            actionButton(inputId = "checkBut", label = "Check", icon = NULL, width = '200px')

          ),
        
          column(3,
            selectInput(inputId = "t7", "Asset 7", tickerNames, selected = NULL, multiple = F,
                           selectize = TRUE, width = '200px', size = NULL),
      
            selectInput(inputId = "t8", "Asset 8", tickerNames, selected = NULL, multiple = F,
                           selectize = TRUE, width = '200px', size = NULL),
      
            selectInput(inputId = "t9", "Asset 9", tickerNames, selected = NULL, multiple = F,
                           selectize = TRUE, width = '200px', size = NULL),
      
            selectInput(inputId = "t10", "Asset 10", tickerNames, selected = NULL, multiple = F,
                           selectize = TRUE, width = '200px', size = NULL),
            
            selectInput(inputId = "t11", "Asset 11", tickerNames, selected = NULL, multiple = F,
                           selectize = TRUE, width = '200px', size = NULL),
            
            selectInput(inputId = "t12", "Asset 12", tickerNames, selected = NULL, multiple = F,
                           selectize = TRUE, width = '200px', size = NULL),
            
            #useShinyalert(),
            disabled(
              actionButton(inputId = "optBut", label = "Optimise", icon = NULL, width = '200px')
            )
            
          ),
        )
      )
    )
  ),
  
  conditionalPanel(
    
    condition = "input.optButIndex == 1 || input.optBut == 1",
    
    tabsetPanel(
      tabPanel("Summary",
               fluidRow(
                 
                 column(3,
                    "Portfolio Weights:",
                    helpText("NOTE: Assets with allocation % = 0 have been ommitted for General Selection"),
                    withSpinner(tableOutput("table_weights"))
                  ),
                 
                 column(9,
                   withSpinner(plotOutput("weights", width = "500px", height = "400px"))
                 ),
                 
                 "Portfolio Summary:",
                 withSpinner(tableOutput("summary")),
                 
                 withSpinner(plotOutput("growth", width = "1000px", height = "350px")),
                 
                 withSpinner(plotOutput("returns", width = "1000px", height = "350px"))
                )
               ),
      
      tabPanel("Efficient Frontier",
               fluidRow(
                 withSpinner(plotOutput("efficientFrontier", width = "800px", height = "700px"))
               )
              ),
      
      tabPanel("Drawdowns",
               fluidRow(
                 withSpinner(plotOutput("plot_dd", width = "1000px", height = "350px")),
                 
                 textOutput("textPort"),
                 withSpinner(tableOutput("table_dd_port")),
                 
                 textOutput("textBench"),
                 withSpinner(tableOutput("table_dd_benchmark")),
                 
                 textOutput("textIndex"),
                 withSpinner(tableOutput("table_dd_index"))
               )
              )
    )
  )
)
  




server <- function(input, output, session) {
  
  
  # Check error conditions are valid 
  observeEvent(input$checkBut, {
    store <- NULL
    for (i in 1:12){
      if (input[[paste0("t", as.character(i))]] != ""){
        store <- cbind(store,as.character(input[[paste0("t", as.character(i))]]))
      }
    }
    
    if (all(duplicated(as.character(store)) == FALSE) && length(store) >= 2){
      enable("optBut")
    }
  })

  
  # Deals with general optimization
  v <- reactiveValues(optButIndex=FALSE)
  
  observeEvent(input$optButIndex, {
    v$optButIndex = TRUE
      })
    
  # observe({
  #   if (v$optButIndex){
  #     opt.port <<- optimizeIndex(input$whichIndex)
  #   }
  # })
  
  
 
  # Deals with asset selection optimization 
  v1 <- reactiveValues(optBut=FALSE)
  
  observeEvent(input$optBut, {
    v1$optBut = TRUE
  })
  
  observe({
    if (v1$optBut){
      # Populate a vector of tickers to pass through the optimizeIndex function
      tickers <- NULL
      for (i in 1:12){
        if (input[[paste0("t", as.character(i))]] != ""){
          tickers <- cbind(tickers,as.character(input[[paste0("t", as.character(i))]]))
        }
      }
      
      if (input$benchmark == "Specify Ticker"){
        opt.port1 <<- optimizeTickers(input$startYR, input$compPort, input$benchmarkTicker, tickers)
      }
      else{
        opt.port1 <<- optimizeTickers(input$startYR, input$compPort, 0, tickers)
      }
    }
  })
  
  
  output$table_weights <- renderTable(width = "520px", striped = TRUE, {
    if (v$optButIndex){
      tableWeights(opt.port$optPort, opt.port$tickers)
    }
    else if (v1$optBut){
      tableWeights(opt.port1$optPort1, opt.port1$tickers1)
    }
  })
 
  output$weights <- renderPlot({
    if (v$optButIndex){
      plotWeights(opt.port$optPort)
    }
    else if (v1$optBut){
      plotWeights(opt.port1$optPort1)
    }
  })
  
  output$summary <- renderTable(rownames = TRUE, striped = TRUE, {
    if (v$optButIndex){
      tableSummary(opt.port$rets_df)
    }
    else if (v1$optBut){
      tableSummary(opt.port1$rets_df1)
    }
  })
  
  output$growth <- renderPlot({
    if (v$optButIndex){
      plotGrowth(opt.port$rets_df)
    }
    else if (v1$optBut){
      plotGrowth(opt.port1$rets_df1)
    }
  })
  
  output$returns <- renderPlot({
    if (v$optButIndex){
      plotReturns(opt.port$rets_df)
    }
    else if (v1$optBut){
      plotReturns(opt.port1$rets_df1)
    }
  })
  
  output$efficientFrontier <- renderPlot({
    if (v$optButIndex){
      plotEF(opt.port$portRets)
    }
    else if (v1$optBut){
      plotEF(opt.port1$portRets1)
    }
  })
  
  output$plot_dd <- renderPlot({
    if (v$optButIndex){
      plotDrawdowns(opt.port$rets_df)
    }
    else if (v1$optBut){
      plotDrawdowns(opt.port1$rets_df1)
    }
  })
  
  output$textPort <- renderText("Drawdowns for Portfolio:")
  
  output$table_dd_port <- renderTable(rownames = TRUE, striped = TRUE, {
    if (v$optButIndex){
      t1 <- tableDrawdowns(opt.port$rets_df$portfolio.returns)
      t1[,1] <- as.character(t1[,1])
      t1[,2] <- as.character(t1[,2])
      t1[,3] <- as.character(t1[,3])
      t1
    }
    else if (v1$optBut){
      t1 <- tableDrawdowns(opt.port1$rets_df1$portfolio.returns)
      t1[,1] <- as.character(t1[,1])
      t1[,2] <- as.character(t1[,2])
      t1[,3] <- as.character(t1[,3])
      t1
    }
  })
  
  output$textBench <- renderText("Drawdowns for Benchmark:")
  
  output$table_dd_benchmark <- renderTable(rownames = TRUE, striped = TRUE, {
    if (v$optButIndex){
      t2 <- tableDrawdowns(opt.port$rets_df$Benchmark.Portfolio)
      t2[,1] <- as.character(t2[,1])
      t2[,2] <- as.character(t2[,2])
      t2[,3] <- as.character(t2[,3])
      t2
    }
    else if (v1$optBut){
      t2 <- tableDrawdowns(opt.port1$rets_df1$Benchmark.Portfolio)
      t2[,1] <- as.character(t2[,1])
      t2[,2] <- as.character(t2[,2])
      t2[,3] <- as.character(t2[,3])
      t2
    }
  })
  
  output$textIndex <- renderText({
    if (v1$optBut){
      if (length(colnames(opt.port1$rets_df1)) == 3){
        "Drawdowns for Index:"
      }
    }
    else if (v$optButIndex){
      "Drawdowns for Index:"
    }
  })
    
  output$table_dd_index <- renderTable(rownames = TRUE, striped = TRUE, {
    if (v$optButIndex){
      t3 <- tableDrawdowns(opt.port$rets_df$SPY.Close)
      t3[,1] <- as.character(t3[,1])
      t3[,2] <- as.character(t3[,2])
      t3[,3] <- as.character(t3[,3])
      t3
    }
    else if (v1$optBut && length(colnames(opt.port1$rets_df1)) == 3){
      t3 <- tableDrawdowns(opt.port1$rets_df1$SPY.Close)
      t3[,1] <- as.character(t3[,1])
      t3[,2] <- as.character(t3[,2])
      t3[,3] <- as.character(t3[,3])
      t3
    }
  })
}

shinyApp(ui = ui, server = server)
