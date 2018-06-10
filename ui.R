library(shiny)
library(dygraphs)

shinyUI(fluidPage(
  titlePanel("Stock Market Prediction"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Input a stock to examine.
               Information will be collected from yahoo finance."),
      
      textInput("symb","StockSymbol", "DJIA"),
      dateRangeInput("dates", 
                     "Date range",
                     start = "2016-01-01", 
                     end = as.character(Sys.Date())),
      br(),
      #a(href = "https://gist.github.com", "Source code"),
      br(),
      br(),
      h3("Rules, Rules, Rules!"),
      helpText("Important Note: 1. Search the 'Ticker' of the stock along with date range and be patient."),
      br(),
      helpText("Important Note: 2. Generating results takes time because this is live streaming so, 
               if results do not appear for 1-2 mins its fine")
      ),
    
    mainPanel(
      tabsetPanel(
        # Tab for Summary of the Stock
        tabPanel("Summary of Stock", 
                 br(),
                 h2("Imp: Read Rules on the Left!"),
                 br(),
                 verbatimTextOutput("summary"),br(),br(),
                 plotOutput("plot"),br(),br()),
        
        
        # Tab for General Evaluation of the Stock
        tabPanel("Interactive Stock Grpah",
                 br(),
                 h2("Imp: Read Rules on the Left!"),
                 br(),
                 dygraphOutput("interactive")),
        
        
        # Intro To Stock
        tabPanel("Data and Plots",
                 br(),
                 h2("Imp: Read Rules on the Left!"),
                 br(),
                 dygraphOutput("logplot"),br(),br(),
                 plotOutput("decomposed"), br(), br(), 
                 plotOutput("predictplot")),
        
        
        # Tab for Predictions
        tabPanel("Predictions For Next 5 days",
                 br(),
                 h2("Imp: Read Rules on the Left!"),
                 br(),
                 h4("Neural Network Prediction"),
                 tableOutput("predicttable"), br(),
                 tableOutput("Neural"), br(),
                 h4("Prediction by Arima Model  [No differencing]"), br(),
                 h5("Should give error if the Time Series is not stationary"), br(),
                 tableOutput("ArimaForcast"), br(), 
                 h4("Arima Models Predicion Graph [No differencing]"), br(), 
                 plotOutput("resultArima"),br(),
                 h4("Auto Arima Model [Best Model]"),
                 tableOutput("ArimaForcastAuto"),br(),
                 plotOutput("resultArima2"),br(),br(),
                 h4("After Extraction of information for TS Analysis ACF and PACF Graph"), br(),br(), 
                 plotOutput("resultArima3"))
      )
    )
      )
)
)