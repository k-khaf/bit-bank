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
                    selected = NULL, multiple = FALSE,
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
            varSelectInput(inputId = "ticker1", "Asset 1", portRet, selected = NULL, multiple = TRUE,
                           selectize = TRUE, width = '200px', size = NULL),
            
            varSelectInput(inputId = "ticker2", "Asset 2", portRet, selected = NULL, multiple = TRUE,
                           selectize = TRUE, width = '200px', size = NULL),
            
            varSelectInput(inputId = "ticker3", "Asset 3", portRet, selected = NULL, multiple = TRUE,
                           selectize = TRUE, width = '200px', size = NULL),
            
            varSelectInput(inputId = "ticker4", "Asset 4", portRet, selected = NULL, multiple = TRUE,
                           selectize = TRUE, width = '200px', size = NULL),
            
            varSelectInput(inputId = "ticker5", "Asset 5", portRet, selected = NULL, multiple = TRUE,
                           selectize = TRUE, width = '200px', size = NULL),
            
             varSelectInput(inputId = "ticker6", "Asset 6", portRet, selected = NULL, multiple = TRUE,
                            selectize = TRUE, width = '200px', size = NULL),
            
            actionButton(inputId = "optBut", label = "Optimize", icon = NULL, width = '200px')
            
          ),
        
          column(3,
            varSelectInput(inputId = "ticker7", "Asset 7", portRet, selected = NULL, multiple = TRUE,
                           selectize = TRUE, width = '200px', size = NULL),
      
            varSelectInput(inputId = "ticker8", "Asset 8", portRet, selected = NULL, multiple = TRUE,
                           selectize = TRUE, width = '200px', size = NULL),
      
            varSelectInput(inputId = "ticker9", "Asset 9", portRet, selected = NULL, multiple = TRUE,
                           selectize = TRUE, width = '200px', size = NULL),
      
            varSelectInput(inputId = "ticker10", "Asset 10", portRet, selected = NULL, multiple = TRUE,
                           selectize = TRUE, width = '200px', size = NULL),
            
            varSelectInput(inputId = "ticker11", "Asset 11", portRet, selected = NULL, multiple = TRUE,
                           selectize = TRUE, width = '200px', size = NULL),
            
            varSelectInput(inputId = "ticker12", "Asset 12", portRet, selected = NULL, multiple = TRUE,
                           selectize = TRUE, width = '200px', size = NULL),
          
            actionButton(inputId = "clearBut", label = "Clear", icon = NULL, width = '200px')
            
            ),
          
          #plotOutput("hist")
        )
      )
    )
  ),
  
  conditionalPanel(
    
    condition = "input.optButIndex == 1",
    
    tabsetPanel(
      tabPanel("Summay",
               fluidRow(
                 withSpinner(plotOutput("weights", width = "800px", height = "700px"))
               )),
      
      tabPanel("Efficient Frontier",
               fluidRow(
                 withSpinner(plotOutput("efficientFrontier"))
               )),
      
      tabPanel("Drawdowns",
               fluidRow(
                 withSpinner(plotOutput("drawdowns"))
               ))
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
  
  output$weights <- renderPlot({
    if (v$optButIndex){
      plotWeights(opt.port$optPort)
    }
  })
  
  output$efficientFrontier <- renderPlot({
    if (v$optButIndex){
      plotEF(opt.port$optPort)
    }
  })
  
  output$drawdowns <- renderPlot({
    plotDrawdowns(opt.port$rets_df)
  })
}

shinyApp(ui = ui, server = server)
