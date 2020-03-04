library(shiny)
library(shinycssloaders)

library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(DEoptim)
library(ROI)
require(ROI.plugin.glpk)
#require(ROI.plugin.quadportRetog)
library(ROI.plugin.symphony)
library(corpcor)
library(BatchGetSymbols)
library(fPortfolio)


source("algorithm-tests.R")

portRet <- readRDS("data/portfolioReturns.rds")


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
        
        selectInput(inputId = "portType", label = "Portfolio Type", 
                    choices = c("Tickers","Asset Classes"),
                    selected = NULL, multiple = FALSE,
                    selectize = TRUE, width = NULL, size = NULL),
        
        selectInput(inputId = "startYR", label = "Start Year", 
                    choices = c(2015L:(as.integer(format(Sys.Date(), "%Y"))-1)),
                    selected = NULL, multiple = FALSE,
                    selectize = TRUE, width = NULL, size = NULL),
        
        selectInput(inputId = "endYr", label = "End Year", 
                    choices = c(2015L:as.integer(format(Sys.Date(), "%Y"))),
                    selected = 2020, multiple = FALSE,
                    selectize = TRUE, width = NULL, size = NULL),
        
        selectInput(inputId = "optType", label = "Optimization Method", 
                    choices = c("Maximize Sharpe Ratio", "Minimize Risk", "Minimize Variance subject to..."),
                    selected = NULL, multiple = FALSE,
                    selectize = TRUE, width = NULL, size = NULL),
        
        selectInput(inputId = "compPort", label = "Comparitive Portfolio", 
                    choices = c("Equal Weights", "Maximum Sharpe Ratio Weights"),
                    selected = NULL, multiple = FALSE,
                    selectize = TRUE, width = NULL, size = NULL),
        
        selectInput(inputId = "benchmark", label = "Benchmark", 
                    choices = c("None", "Specify Ticker"),
                    selected = NULL, multiple = FALSE,
                    selectize = TRUE, width = NULL, size = NULL),
        
        conditionalPanel(
          condition = "input.benchmark == 'Specify Ticker'",
          
          selectInput(inputId = "benchmarkTicker", label = "Benchmark ticker", 
                      choices = c("SPY","^FTSE"),
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
            varSelectInput(inputId = "t1", "Asset 1", portRet, selected = NULL, multiple = TRUE,
                           selectize = TRUE, width = '200px', size = NULL),
            
            varSelectInput(inputId = "t2", "Asset 2", portRet, selected = NULL, multiple = TRUE,
                           selectize = TRUE, width = '200px', size = NULL),
            
            varSelectInput(inputId = "t3", "Asset 3", portRet, selected = NULL, multiple = TRUE,
                           selectize = TRUE, width = '200px', size = NULL),
            
            varSelectInput(inputId = "t4", "Asset 4", portRet, selected = NULL, multiple = TRUE,
                           selectize = TRUE, width = '200px', size = NULL),
            
            varSelectInput(inputId = "t5", "Asset 5", portRet, selected = NULL, multiple = TRUE,
                           selectize = TRUE, width = '200px', size = NULL),
            
             varSelectInput(inputId = "t6", "Asset 6", portRet, selected = NULL, multiple = TRUE,
                            selectize = TRUE, width = '200px', size = NULL),
            
            actionButton(inputId = "optBut", label = "Optimize", icon = NULL, width = '200px')
            
          ),
        
          column(3,
            varSelectInput(inputId = "t7", "Asset 7", portRet, selected = NULL, multiple = TRUE,
                           selectize = TRUE, width = '200px', size = NULL),
      
            varSelectInput(inputId = "t8", "Asset 8", portRet, selected = NULL, multiple = TRUE,
                           selectize = TRUE, width = '200px', size = NULL),
      
            varSelectInput(inputId = "t9", "Asset 9", portRet, selected = NULL, multiple = TRUE,
                           selectize = TRUE, width = '200px', size = NULL),
      
            varSelectInput(inputId = "t10", "Asset 10", portRet, selected = NULL, multiple = TRUE,
                           selectize = TRUE, width = '200px', size = NULL),
            
            varSelectInput(inputId = "t11", "Asset 11", portRet, selected = NULL, multiple = TRUE,
                           selectize = TRUE, width = '200px', size = NULL),
            
            varSelectInput(inputId = "t12", "Asset 12", portRet, selected = NULL, multiple = TRUE,
                           selectize = TRUE, width = '200px', size = NULL),
          
            actionButton(inputId = "clearBut", label = "Clear", icon = NULL, width = '200px')
            
            ),
          
          #plotOutput("hist")
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
  
  v <- reactiveValues(optButIndex=FALSE)
  
  observeEvent(input$optButIndex, {
    v$optButIndex = TRUE
      })
    
  observe({
    if (v$optButIndex){
      opt.port <<- optimizeIndex(input$whichIndex)
    }
  })
  
  output$table_weights <- renderTable(width = "570px", striped = TRUE, {
    if (v$optButIndex){
      tableWeights(opt.port$optPort,opt.port$tickers)
    }
  })
 
  output$weights <- renderPlot({
      if (v$optButIndex){
        plotWeights(opt.port$optPort)
      }
    })
  
  output$summary <- renderTable(rownames = TRUE, striped = TRUE, {
    if (v$optButIndex){
      tableSummary(opt.port$rets_df)
    }
  })
  
  output$growth <- renderPlot({
    if (v$optButIndex){
      plotGrowth(opt.port$rets_df)
    }
  })
  
  output$returns <- renderPlot({
    if (v$optButIndex){
      plotReturns(opt.port$rets_df)
    }
  })
  
  output$efficientFrontier <- renderPlot({
    if (v$optButIndex){
      plotEF(opt.port$optPort)
    }
  })
  
  output$plot_dd <- renderPlot({
    if (v$optButIndex){
      plotDrawdowns(opt.port$rets_df)
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
  })
  
  
  output$table_dd_benchmark <- renderTable(rownames = TRUE, striped = TRUE, {
    if (v$optButIndex){
      t2 <- tableDrawdowns(opt.port$rets_df$Benchmark.Portfolio)
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
  })
  
  
  
  #Opt ticker
  
  v1 <- reactiveValues(optBut=FALSE)
  
  observeEvent(input$optBut, {
    v1$optBut = TRUE
  })
  
  observe({
    if (v1$optBut){
      
      tickers <<- NULL
      for (i in 1:12){
        tickers <<- cbind(tickers,as.character(input[[paste0("t", as.character(i))]]))
      }
      
      opt.port1 <<- optimizeTickers(input$startYR, input$endYr, input$compPort, input$benchmarkTicker, tickers)
    }
  })
  
  output$weights <- renderPlot({
    if (v1$optBut){
      plotWeights(opt.port1$optPort1)
    }
  })

}

shinyApp(ui = ui, server = server)
