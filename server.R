library(quantmod)
library(AppliedPredictiveModeling)
library(TTR)
library(DMwR)
library(neuralnet)
library(dygraphs)
library(tseries)
library(forecast)

#source("neuralModel.R")
### Shiny Server
shinyServer(function(input, output) {
  
  
  ## Tab 1
  output$summary <- renderPrint({
    TSD <- getSymbols(input$symb, 
                      from = input$dates[1],
                      to = input$dates[2],
                      auto.assign = FALSE)
    summary(TSD)
  })
  
  output$plot <- renderPlot({
    TSData <- getSymbols(input$symb, 
                         from = input$dates[1],
                         to = input$dates[2],
                         auto.assign = FALSE)
    chartSeries(TSData, theme = chartTheme("black"), 
                type = "line")
  })
  
  
  ## Tab 2
  output$interactive <- renderDygraph({
    DDf <- as.data.frame(getSymbols(Symbols = input$symb, env = NULL), 
                         from = input$dates[1],
                         to = input$dates[2],
                         auto.assign = FALSE)
    DDf$Date <- as.Date(rownames(DDf))
    stocks <- xts(DDf[-7], order.by=as.Date(DDf[,7], "%m/%d/%Y"), frequency = 12)
    dygraph(stocks, main = "Interactive Stock Graph") %>%
      dyOptions(colors = RColorBrewer::brewer.pal(8, "Set2"))%>%
      dyHighlight(highlightCircleSize = 5, 
                  highlightSeriesBackgroundAlpha = 0.7,
                  hideOnMouseOut = FALSE)%>%
      dyAxis("y", label = "Stock Price") %>%
      dyRangeSelector()
    
  })
  
  
  
  # Tab 3  
  output$logplot <- renderDygraph({
    TSData <- getSymbols(input$symb, 
                         from = input$dates[1],
                         to = input$dates[2],
                         auto.assign = FALSE)
    dygraph(TSData) %>%
      dyOptions(colors = RColorBrewer::brewer.pal(8, "Set2"))%>%
      dyHighlight(highlightCircleSize = 5, 
                  highlightSeriesBackgroundAlpha = 2,
                  hideOnMouseOut = T)%>%
      dyAxis("y") %>%
      dyCandlestick()
  })  
  #label = "Stock Price"
  
  
  
  ##
  # ##  
  #   output$predictplot <- renderPlot({
  #     plot(predicts)
  #   }) 
  
  
  
  
  output$predictplot <- renderPlot({
    dfdj <- as.data.frame(getSymbols(Symbols = input$symb, env = NULL), 
                          from = input$dates[1],
                          to = input$dates[2],
                          auto.assign = FALSE)
    dfdj<-dfdj[-1:-5]
    dfdj<- ts(dfdj, frequency = 12)
    dec<-decompose(dfdj)
    lay= par(no.readonly = T)
    layout(matrix(c(1,1,2,3),2,2,byrow = T))
    plot.ts(dec$trend, main = "Stationary Series");acf(dfdj, main = "Auto-Correlation",lag.max = 50);pacf(dfdj,main = "Partial Correalation")
    par(lay)
  })
  
  
  
  
  output$decomposed <- renderPlot({
    dfdj <- as.data.frame(getSymbols(Symbols = input$symb, env = NULL), 
                          from = input$dates[1],
                          to = input$dates[2],
                          auto.assign = FALSE)
    dfdj<-dfdj[-1:-5]
    dfdj<- ts(dfdj, frequency = 12)
    plot(decompose(dfdj))
  }) 
  
  
  
  
  #  output$predicttable <- renderTable({
  #   dddf
  # })
  
  
  # output$Neural <- renderPlot({
  #   plot(dddf, type = 'l')
  # })
  
  
  output$ArimaForcast <- renderTable({
    Df <- as.data.frame(getSymbols(Symbols = input$symb, env = NULL), 
                        from = input$dates[1],
                        to = input$dates[2],
                        auto.assign = FALSE)
    Df$Date <- as.Date(rownames(Df))
    Df<-Df[-1:-5]
    Df<- ts(Df[-2], frequency = 12)
    boxtest<-Box.test(Df,lag = 200,type = "Ljung-Box")
    adftest<-adf.test(Df,alternative = "stationary")
    kpsstest<-kpss.test(Df)
    #fit2<-auto.arima(Df)
    fit = arima(Df, order = c(1,0,0))
    fit_resi = residuals(fit)
    boxtestFit<-Box.test(fit_resi,lag = 10,type = "Ljung-Box")
    forcast_Fit <- forecast(fit,h=5)
    Accuracy<-accuracy(forcast_Fit)
    Result<- plot(forecast(fit,h=40),include = 40)
    Result<- as.data.frame(Result)
    print(forcast_Fit)
  })
  
  
  
  
  output$resultArima <- renderPlot({
    Df <- as.data.frame(getSymbols(Symbols = input$symb, env = NULL), 
                        from = input$dates[1],
                        to = input$dates[2],
                        auto.assign = FALSE)
    Df$Date <- as.Date(rownames(Df))
    Df<-Df[-1:-5]
    Df<- ts(Df[-2], frequency = 12)
    fit = arima(Df, order = c(1,0,0))
    plot(forecast(fit,h=40),include = 40)
  })
  
  
  
  output$ArimaForcastAuto <- renderTable({
    Df <- as.data.frame(getSymbols(Symbols = input$symb, env = NULL), 
                        from = input$dates[1],
                        to = input$dates[2],
                        auto.assign = FALSE)
    Df$Date <- as.Date(rownames(Df))
    Df<-Df[-1:-5]
    Df<- ts(Df[-2], frequency = 12)
    fit2<-auto.arima(Df)
    fit_resi2 = residuals(fit2)
    forcast_Fit2 <- forecast(fit2,h=4)
    print(forcast_Fit2)
  })
  
  
  
  
  output$resultArima2 <- renderPlot({
    Df <- as.data.frame(getSymbols(Symbols = input$symb, env = NULL), 
                        from = input$dates[1],
                        to = input$dates[2],
                        auto.assign = FALSE)
    Df$Date <- as.Date(rownames(Df))
    Df<-Df[-1:-5]
    Df<- ts(Df[-2], frequency = 12)
    fit3<-auto.arima(Df)
    fit_resi3 = residuals(fit3)
    plot(forecast(fit3,h=40),include = 40)
  })
  
  
  
  output$resultArima3 <- renderPlot({
    Df <- as.data.frame(getSymbols(Symbols = input$symb, env = NULL), 
                        from = input$dates[1],
                        to = input$dates[2],
                        auto.assign = FALSE)
    Df$Date <- as.Date(rownames(Df))
    Df<-Df[-1:-5]
    Df<- ts(Df[-2], frequency = 12)
    fit3<-auto.arima(Df)
    fit_resi3 = residuals(fit3)
    par(mfrow=c(1,2))
    acf(ts(fit3$residuals),main="ACF Residual")
    pacf(ts(fit3$residuals),main="PACF Residual")
  })
  
  
  
  ##
  ##
  ## Neural Thing Below
  
  output$predicttable <- renderTable({
    dfDJIA <- as.data.frame(getSymbols(Symbols = input$symb, env = NULL), 
                            from = input$dates[1],
                            to = input$dates[2],
                            auto.assign = FALSE)
    dfDJIA$Date <- rownames(dfDJIA)
    names(dfDJIA) <- c("Open","High","Low","Close","Volume","Adjusted","Date")
    dfDJIA$Date<-as.Date(dfDJIA$Date)
    
    # data changed for 360 days from todays date and than in accending order
    today<-Sys.Date()
    dfDJIA <- dfDJIA[dfDJIA$Date >= today-360,]
    dfDJIA <- dfDJIA[order(dfDJIA$Date),]
    prevrows <- function(data,n) {sapply(1:n,function(x) c(rep(NA,x),head(data,-x)))}
    
    # gets the day number of the week
    dfDJIA$Day <- as.numeric(as.POSIXlt(dfDJIA$Date)$wday)
    
    # calulating previous Closing
    dfDJIA$PrevClose <- prevrows(dfDJIA$Close,1)
    
    # Calculating the change
    dfDJIA$Change <- dfDJIA$Close - dfDJIA$PrevClose
    
    # making range of movement from (-1 to 1)
    dfDJIA$Movement<-ifelse(dfDJIA$Change > 0 ,1 , -1)
    
    # Calculating various Technical Indicators: 
    # SMA:  Simple Moving Average MA -> SMA
    # EMA:  Exponential Moving Average
    # RSI:  Relative Strength Index
    # VOLEMA:  Volume (Exponential)
    # OBV:  On Balance Volume:- is a measure of the money flowing into or out of a security
    
    dfDJIA$SMA50<-SMA(dfDJIA$Close, n=50)
    dfDJIA$SMA10<-SMA(dfDJIA$Close, n=10)
    dfDJIA$SMA5<-SMA(dfDJIA$Close, n=5)
    dfDJIA$SMA3<-SMA(dfDJIA$Close, n=3)
    dfDJIA$EMA50<-EMA(dfDJIA$Close, n=50)
    dfDJIA$EMA10<-EMA(dfDJIA$Close, n=10)
    dfDJIA$EMA5<-EMA(dfDJIA$Close, n=5)
    dfDJIA$EMA3<-EMA(dfDJIA$Close, n=3)
    dfDJIA$RSI50<-RSI(dfDJIA$Close, n=50)
    dfDJIA$RSI10<-RSI(dfDJIA$Close, n=10)
    dfDJIA$RSI5<-RSI(dfDJIA$Close, n=5)
    dfDJIA$RSI3<-RSI(dfDJIA$Close, n=3)
    dfDJIA$VOLEMA50<-EMA(dfDJIA$Volume, n=50)
    dfDJIA$VOLEMA10<-EMA(dfDJIA$Volume, n=10)
    dfDJIA$VOLEMA5<-EMA(dfDJIA$Volume, n=5)
    dfDJIA$VOLEMA3<-EMA(dfDJIA$Volume, n=3)
    dfDJIA$OBV<-OBV(dfDJIA$Close, dfDJIA$Volume)
    
    HiLoCl<-as.matrix(cbind(dfDJIA$High, dfDJIA$Low, dfDJIA$Close))
    
    # Calculating Average true Range
    dfDJIA$ATR<-ATR(HiLoCl, n = 14)[,1]
    
    dfDJIA$MACD<-as.data.frame((MACD(dfDJIA$Close)))$macd
    
    dfDJIA<-dfDJIA[ which(complete.cases(dfDJIA)==TRUE),]
    
    today<-Sys.Date()
    training = dfDJIA[dfDJIA$Date < as.Date(today-10),]
    
    trainNN<-as.data.frame(cbind(training$Close,training$PrevClose,training$Day,training$SMA50,training$SMA10,training$SMA5,training$SMA3,training$EMA50,training$EMA10,training$EMA5,training$EMA3,training$RSI50,training$MACD,training$OBV,training$Change
    ))
    
    
    trainNN<-trainNN[ which(complete.cases(trainNN)==TRUE),]
    trainNN<-cbind(trainNN$V1, trainNN$V2
                   , prevrows(trainNN$V3,1)
                   , prevrows(trainNN$V4,1)
                   , prevrows(trainNN$V5,1)
                   , prevrows(trainNN$V6,1)
                   , prevrows(trainNN$V7,1)
                   , prevrows(trainNN$V8,1)
                   , prevrows(trainNN$V9,1)
                   , prevrows(trainNN$V10,1)
                   , prevrows(trainNN$V11,1)
                   , prevrows(trainNN$V12,1)
                   , prevrows(trainNN$V13,1)
                   , prevrows(trainNN$V14,1)
                   , prevrows(trainNN$V15,1)
    )
    
    
    trainNN<-trainNN[ which(complete.cases(trainNN)==TRUE),]
    trainNNScaled<-as.data.frame(scale(lag(as.matrix(trainNN),1)))
    
    n <- names(trainNNScaled)
    f <- as.formula(paste("V1 ~", paste(n[!n %in% "V1"], collapse = " + ")))
    set.seed(12345)
    
    # Main Algo
    net <- neuralnet(f,data=trainNNScaled, hidden=10, threshold=.006, rep=8)
    
    # Testing 
    testing = dfDJIA[dfDJIA$Date >= as.Date(today-10),]
    lastTrainingDay<-as.data.frame(tail(trainNN,1))
    testNN<-rbind(lastTrainingDay,as.data.frame(cbind(testing$Close,testing$PrevClose,testing$Day,testing$SMA50,testing$SMA10,testing$SMA5,testing$SMA3,testing$EMA50,testing$EMA10,testing$EMA5,testing$EMA3,testing$RSI50,testing$MACD,testing$OBV,testing$Change
    )))
    
    nextwday <- function(dy) {
      ifelse(dy==5, 1, dy<-dy+1)
    }
    
    testNN<-cbind(testNN$V1, testNN$V2
                  , prevrows(testNN$V3,1)
                  , prevrows(testNN$V4,1)
                  , prevrows(testNN$V5,1)
                  , prevrows(testNN$V6,1)
                  , prevrows(testNN$V7,1)
                  , prevrows(testNN$V8,1)
                  , prevrows(testNN$V9,1)
                  , prevrows(testNN$V10,1)
                  , prevrows(testNN$V11,1)
                  , prevrows(testNN$V12,1)
                  , prevrows(testNN$V13,1)
                  , prevrows(testNN$V14,1)
                  , prevrows(testNN$V15,1)
    )
    
    testNN<-testNN[ which(complete.cases(testNN)==TRUE),]
    lastDay<-as.data.frame(tail(testing,1))
    nextDay<-as.data.frame(cbind(0,lastDay$Close,nextwday(as.data.frame(tail(testNN,1))$V3),lastDay$SMA50,lastDay$SMA10,lastDay$SMA5,lastDay$SMA3,lastDay$EMA50,lastDay$EMA10,lastDay$EMA5,lastDay$EMA3,lastDay$RSI50,lastDay$MACD,lastDay$OBV,lastDay$Change
    ))
    testNN<-rbind(testNN,nextDay)
    
    for (i in 1:5){
      testNNScale <- scale(testNN, center = attr(scale(trainNN), 'scaled:center'), scale = attr(scale(trainNN), 'scaled:scale'))
      
      net.results <- compute(net, as.data.frame(testNNScale)[-1])
      results<-unscale(as.matrix(net.results$net.result)[,1], testNNScale)
      
      if(i==1){
        cleanoutput <- cbind(testNN$V1,as.data.frame(results), abs(testNN$V1-as.data.frame(results))/testNN$V1*100)
        colnames(cleanoutput) <- c("Expected Output","Neural Net Output", "Error %")
        print(cleanoutput)
        
        print(sum(cleanoutput[which(cleanoutput$"Error %" < 100),]$"Error %")/(nrow(cleanoutput)-1))
      }
      
      testNN[nrow(testNN),1]<-tail(as.data.frame(results),1)
      
      p<-rbind(trainNN,testNN)
      SMA50<-tail(SMA(p$V1, n=50),1)
      SMA10<-tail(SMA(p$V1, n=10),1)
      SMA5<-tail(SMA(p$V1, n=5),1)
      SMA3<-tail(SMA(p$V1, n=3),1)
      EMA50<-tail(EMA(p$V1, n=50),1)
      EMA10<-tail(EMA(p$V1, n=10),1)
      EMA5<-tail(EMA(p$V1, n=5),1)
      EMA3<-tail(EMA(p$V1, n=3),1)
      RSI50<-tail(RSI(p$V1, n=50),1)
      MACD<-tail(as.data.frame((MACD(p$V1)))$macd,1)
      OBV<-tail(p$V14,1)
      Change<-tail(as.data.frame(results),1)-head(tail(as.data.frame(results),2),1)
      
      nextDay<-as.data.frame(cbind(0,tail(as.data.frame(results),1),nextwday(as.data.frame(tail(testNN,1))$V3),SMA50,SMA10,SMA5,SMA3,EMA50,EMA10,EMA5,EMA3,RSI50,MACD,OBV,Change
      ))
      
      names(nextDay) <- names(testNN)
      testNN<-rbind(testNN,nextDay)
    }
    
    testNN<-testNN[-nrow(testNN),] 
    
    predicts <- as.data.frame(cbind(as.Date(as.data.frame(rbind(as.Date(tail(dfDJIA,1)$Date)+1, 
                                                                as.Date(tail(dfDJIA,1)$Date)+2,
                                                                as.Date(tail(dfDJIA,1)$Date)+3,
                                                                as.Date(tail(dfDJIA,1)$Date)+4,
                                                                as.Date(tail(dfDJIA,1)$Date)+5))$V1),                                as.data.frame(tail(testNN,5)$V1)
    ))
    
    colnames(predicts) <- c("Date", "Close")
    actuals <- dfDJIA[, c("Date", "Close")]
    dfDJIA<-rbind(actuals,predicts)
    #pred <- c("Tue [12/20/2016]","Wed [12/21/2016]","Thur [12/22/2016]",
    #          "Fri [12/23/2016]", "Mon [12/26/2016]")
    # dddf<- data.frame(pred[1:4],predicts$Close[1:4])
    pred <- c("1","2","3","4","5")
    data.frame(pred,predicts$Close)
  })
  
})