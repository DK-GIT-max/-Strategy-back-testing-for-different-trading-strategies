library(shiny)
library(quantmod)
library(PerformanceAnalytics)
library(dplyr)

# Define the strategies

# EMA Crossover Strategy
ema_strategy <- function(stock, n1, n2) {
  ema_n1 <- EMA(Cl(stock), n = n1)
  ema_n2 <- EMA(Cl(stock), n = n2)
  signal <- lag(ifelse((ema_n1 > ema_n2) & lag(ema_n1) < lag(ema_n2), 1, 
                       ifelse((ema_n1 < ema_n2) & lag(ema_n1) > lag(ema_n2), -1, 0)))
  signal[is.na(signal)] <- 0
  return(signal)
}

# RSI Strategy
rsi_strategy <- function(stock, n) {
  rsi_values <- RSI(Cl(stock), n = n)
  signal <- lag(ifelse(rsi_values > 70, -1, ifelse(rsi_values < 30, 1, 0)))
  signal[is.na(signal)] <- 0
  return(signal)
}

# MACD Strategy
macd_strategy <- function(stock, n_fast, n_slow, n_signal) {
  macd_values <- MACD(Cl(stock), nFast = n_fast, nSlow = n_slow, nSig = n_signal, wilder = FALSE)
  signal <- lag(ifelse(macd_values$macd > macd_values$signal, 1, 
                       ifelse(macd_values$macd < macd_values$signal, -1, 0)))
  signal[is.na(signal)] <- 0
  return(signal)
}

# SMA Strategy
sma_strategy <- function(stock, n) {
  sma_values <- SMA(Cl(stock), n = n)
  signal <- lag(ifelse(Cl(stock) > sma_values, 1, -1))
  signal[is.na(signal)] <- 0
  return(signal)
}

# Function to compute trading positions
compute_positions <- function(signal) {
  positions <- rep(0, length(signal))
  changeover <- 0
  for (i in 1:length(signal)) {
    if (signal[i] == 1) {
      positions[i] <- 1
      changeover <- 1
    } else if (signal[i] == -1) {
      positions[i] <- 0
      changeover <- 0
    } else {
      positions[i] <- changeover
    }
  }
  return(positions)
}

# Shiny app UI
ui <- fluidPage(
  titlePanel("Trading Strategy Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("ticker", "Enter Stock Ticker (e.g., 'AAPL'):", value = "AAPL"),
      
      # Drop-down menu for strategy selection
      selectInput("strategy", "Select Strategy:",
                  choices = c("Select Strategy", "EMA Crossover", "RSI", "MACD", "SMA")),
      
      # EMA Strategy inputs
      conditionalPanel(
        condition = "input.strategy == 'EMA Crossover'",
        numericInput("n1", "Enter Short Period for EMA1:", value = 15),
        numericInput("n2", "Enter Long Period for EMA2:", value = 50)
      ),
      
      # RSI Strategy input
      conditionalPanel(
        condition = "input.strategy == 'RSI'",
        numericInput("n_rsi", "Enter Period for RSI:", value = 14)
      ),
      
      # MACD Strategy inputs
      conditionalPanel(
        condition = "input.strategy == 'MACD'",
        numericInput("n_fast", "Enter Fast Period for MACD:", value = 12),
        numericInput("n_slow", "Enter Slow Period for MACD:", value = 26),
        numericInput("n_signal", "Enter Signal Period for MACD:", value = 9)
      ),
      
      # SMA Strategy input
      conditionalPanel(
        condition = "input.strategy == 'SMA'",
        numericInput("n_sma", "Enter Period for SMA:", value = 50)
      ),
      
      actionButton("run", "Run Strategy")
    ),
    
    mainPanel(
      h4("Strategy Summary"),
      verbatimTextOutput("strategySummary")
    )
  )
)

# Shiny app server
server <- function(input, output) {
  
  # Reactive function to fetch stock data and calculate strategy results
  strategy_data <- eventReactive(input$run, {
    # Load stock data
    stock <- tryCatch({
      getSymbols(input$ticker, from = "2022-01-01", auto.assign = FALSE)
    }, error = function(e) {
      return(NULL)
    })
    
    # Handle invalid stock data
    if (is.null(stock)) {
      return("Invalid stock ticker or no data available.")
    }
    
    # Choose strategy based on user input
    strategy_signal <- NULL
    if (input$strategy == "EMA Crossover") {
      strategy_signal <- ema_strategy(stock, input$n1, input$n2)
    } else if (input$strategy == "RSI") {
      strategy_signal <- rsi_strategy(stock, input$n_rsi)
    } else if (input$strategy == "MACD") {
      strategy_signal <- macd_strategy(stock, input$n_fast, input$n_slow, input$n_signal)
    } else if (input$strategy == "SMA") {
      strategy_signal <- sma_strategy(stock, input$n_sma)
    }
    
    # Compute trading positions
    strategy_positions <- compute_positions(strategy_signal)
    
    # Calculate daily returns
    daily_returns <- Return.calculate(Cl(stock), method = "discrete")
    daily_returns[is.na(daily_returns)] <- 0
    strategy_returns <- daily_returns * strategy_positions
    
    # Package data and summary for output
    list(
      summary_text = paste0(
        "Strategy Summary\n",
        "Selected Strategy: ", input$strategy, "\n",
        "Strategy Annualized Return: ", round(Return.annualized(strategy_returns) * 100, 2), "%\n",
        "Buy & Hold Annualized Return: ", round(Return.annualized(daily_returns) * 100, 2), "%\n",
        "Sharpe Ratio (Strategy): ", round(SharpeRatio.annualized(strategy_returns), 2), "\n",
        "Sharpe Ratio (Buy & Hold): ", round(SharpeRatio.annualized(daily_returns), 2)
      )
    )
  })
  
  # Render strategy summary
  output$strategySummary <- renderText({
    data <- strategy_data()
    if (is.character(data)) {
      data  # If data is an error message, display it
    } else {
      data$summary_text
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
