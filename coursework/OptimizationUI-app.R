library(shiny)
library(shinycssloaders)
library(shinyjs)
library(shinyalert)

library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(DEoptim)
library(ROI)
require(ROI.plugin.glpk)
#require(ROI.plugin.quadtickerNamesog)
library(ROI.plugin.symphony)
library(corpcor)
library(BatchGetSymbols)
library(fPortfolio)


source("algorithm-tests.R")

#tickerNames <- readRDS("data/portfolioReturns.rds")

tickerNames <- readRDS("C:/Users/kian/Desktop/R/Portfolio-optimization/data/tickerNames.rds")

ui <- fluidPage(
  
  titlePanel("Portfolio Optimization"),
  
  helpText("Note: Blah...."),
  
  # Input Selection
  sidebarLayout(
    
    sidebarPanel("Inputs",
      
      selectInput(inputId = "selectTickers", label = "Do you want to choose tickers?", 
                  choices = c("No","Yes"),
                  selected = NULL, multiple = FALSE,
                  selectize = TRUE, width = NULL, size = NULL),
      
      conditionalPanel(
        condition = "input.selectTickers == 'No'",
        
        selectInput(inputId = "whichIndex", label = "Which index would you like to optimize over?", 
                    choices = c("SPY","^FTSE"),
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
          selectInput(inputId = "optType", label = "Optimization Method", 
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
    
    #selectInput(inputId = "ticker1", label = "Asset 1", choices, selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL)
    mainPanel(
      
      fluidRow(
        
        conditionalPanel(
          condition = "input.selectTickers == 'No'",
          
          helpText("General Optimization"),
          
          actionButton(inputId = "optButIndex", label = "Optimize", icon = NULL, width = '300px'),
        
        
          #actionButton(inputId = "clearBut", label = "Clear", icon = NULL, width = '150px')
        ),
        
        conditionalPanel(
          condition = "input.selectTickers == 'Yes'",
          
          helpText("Asset Selection"),
      
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
              actionButton(inputId = "optBut", label = "Optimize", icon = NULL, width = '200px')
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
                 
                 "Drawdowns for Portfolio:",
                 withSpinner(tableOutput("table_dd_port")),
                 
                 "Drawdowns for Benchmark:",
                 withSpinner(tableOutput("table_dd_benchmark")),
                 
                 "Drawdowns for Index:",
                 withSpinner(tableOutput("table_dd_index"))
               )
              )
    )
  )
)
  




server <- function(input, output, session) {
  
  # Check error conditions are valid 
  observeEvent(input$checkBut, {
    store <<- NULL
    for (i in 1:12){
      if (input[[paste0("t", as.character(i))]] != ""){
        store <<- cbind(store,as.character(input[[paste0("t", as.character(i))]]))
      }
    }
    
    if (all(duplicated(as.character(store)) == FALSE) && length(store) >= 2){
      enable("optBut")
    }
    
  })

  
  
  v <- reactiveValues(optButIndex=FALSE)
  
  observeEvent(input$optButIndex, {
    v$optButIndex = TRUE
      })
    
  observe({
    if (v$optButIndex){
      opt.port <<- optimizeIndex(input$whichIndex)
    }
  })
  
  
 
   
  v1 <- reactiveValues(optBut=FALSE)
  
  observeEvent(input$optBut, {
    v1$optBut = TRUE
  })
  
  observe({
    if (v1$optBut){
      # Populate a vector of tickers to pass through the optimizeIndex function
      tickers <<- NULL
      for (i in 1:12){
        if (input[[paste0("t", as.character(i))]] != ""){
          tickers <<- cbind(tickers,as.character(input[[paste0("t", as.character(i))]]))
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
  
  
  
  
  output$table_weights <- renderTable(width = "570px", striped = TRUE, {
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
      plotEF(opt.port$optPort)
    }
    else if (v1$optBut){
      plotEF(opt.port1$optPort1)
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
  
  output$table_dd_index <- renderTable(rownames = TRUE, striped = TRUE, {
    if (v$optButIndex){
      t3 <- tableDrawdowns(opt.port$rets_df$SPY.Close)
      t3[,1] <- as.character(t3[,1])
      t3[,2] <- as.character(t3[,2])
      t3[,3] <- as.character(t3[,3])
      t3
    }
    else if (v1$optBut){
      t3 <- tableDrawdowns(opt.port1$rets_df1$SPY.Close)
      t3[,1] <- as.character(t3[,1])
      t3[,2] <- as.character(t3[,2])
      t3[,3] <- as.character(t3[,3])
      t3
    }
  })
  
  observeEvent(input$clearBut, {
    reset("optBut")
  })
}

shinyApp(ui = ui, server = server)
