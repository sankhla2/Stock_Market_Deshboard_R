library(shiny)
library(shinyWidgets)
library(shinythemes)
library(plotly)
library(tidyverse)
library(tidyquant)
library(quantmod)
library(rvest)
library(xml2)

# Define tickers and benchmarks
tickers <- c("GRVY", "SE", "PLTR", "U", "NET", "SNOW", "MDB")
benchmarks <- c("^NDX", "^GSPC")

# Fetch stock prices and benchmark data
prices <- tq_get(tickers,
                 get = "stock.prices",
                 from = today() - years(1),
                 to = today(),
                 complete_cases = FALSE) %>%
  select(symbol, date, close)

bench <- tq_get(benchmarks,
                get = "stock.prices",
                from = today() - years(1),
                to = today()) %>%
  select(symbol, date, close)

# UI
ui <- fluidPage(
  theme = shinytheme("cyborg"),
  titlePanel("Stock Market Dashboard"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      pickerInput(
        inputId = "stocks",
        label = h4("Stocks"),
        choices = setNames(tickers, tickers),
        selected = tickers,
        options = list(`actions-box` = TRUE),
        multiple = TRUE
      ),
      radioButtons(
        "period", label = h4("Period"),
        choices = list("1 month" = 1, "3 months" = 2, "6 months" = 3, "12 months" = 4, "YTD" = 5),
        selected = 4
      ),
      radioButtons(
        "benchmark", label = h4("Benchmark"),
        choices = list("SP500" = 1, "Nasdaq100" = 2, "None" = 3),
        selected = 3
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Overview", plotlyOutput("overview_plot")),
        tabPanel("Individual Stocks", tableOutput("individual_stocks")),
        tabPanel("Portfolio", tableOutput("portfolio")),
        tabPanel("Watchlist", tableOutput("watchlist")),
        tabPanel("News Feed", uiOutput("news_feed")),
        tabPanel("Financial Calendar", uiOutput("financial_calendar")),
        tabPanel("Technical Analysis", 
                 plotOutput("rsi_plot"),
                 plotOutput("macd_plot")),
        tabPanel("Fundamental Data", uiOutput("fundamental_data"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Overview plot
  output$overview_plot <- renderPlotly({
    req(input$period, input$stocks, input$benchmark)
    selected_prices <- prices %>%
      filter(symbol %in% input$stocks)
    
    if (input$period == 1) {
      selected_prices <- selected_prices %>%
        filter(date >= today() - months(1))
    }
    if (input$period == 2) {
      selected_prices <- selected_prices %>%
        filter(date >= today() - months(3))
    }
    if (input$period == 3) {
      selected_prices <- selected_prices %>%
        filter(date >= today() - months(6))
    }
    if (input$period == 5) {
      selected_prices <- selected_prices %>%
        filter(year(date) == year(today()))
    }
    
    if (input$benchmark == 1) {
      benchmark_data <- bench %>%
        filter(symbol == "^GSPC",
               date >= min(selected_prices$date))
      selected_prices <- rbind(selected_prices, benchmark_data)
    }
    
    if (input$benchmark == 2) {
      benchmark_data <- bench %>%
        filter(symbol == "^NDX",
               date >= min(selected_prices$date))
      selected_prices <- rbind(selected_prices, benchmark_data)
    }
    
    # Create plot
    p <- ggplot(selected_prices, aes(date, close, colour = symbol)) +
      geom_line(size = 1) +
      labs(title = "Market Overview",
           x = "Date",
           y = "Price") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p)
  })
  
  # Individual stocks table
  output$individual_stocks <- renderTable({
    prices %>%
      filter(symbol %in% input$stocks) %>%
      arrange(desc(date)) %>%
      head()
  })
  
  # Portfolio table
  output$portfolio <- renderTable({
    # Placeholder output (Can replace with actual portfolio data)
    portfolio_data <- data.frame(
      Asset_Class = c("Stocks", "Bonds", "Cash"),
      Allocation = c(60, 30, 10) # Example allocation percentages
    )
    portfolio_data
  })
  
  # Watchlist table
  output$watchlist <- renderTable({
    # Placeholder output (Can replace with actual watchlist data)
    watchlist_data <- data.frame(
      Stock = c("AAPL", "GOOG", "AMZN"), # Example stocks
      Last_Price = c(1500, 2500, 3500) # Example last prices
    )
    watchlist_data
  })
  
  # News feed
  output$news_feed <- renderUI({
    url <- "https://finance.yahoo.com/topic/stock-market-news/"
    webpage <- read_html(url)
    news_items <- webpage %>%
      html_nodes(".js-stream-content") %>%
      html_text()
    
    tagList(
      h4("News Feed"),
      lapply(news_items, tags$p)
    )
  })
  
  # Financial calendar
  output$financial_calendar <- renderUI({
    url <- "https://www.earningswhispers.com/calendar"
    webpage <- read_html(url)
    calendar_items <- webpage %>%
      html_nodes(".view-content") %>%
      html_text()
    
    tagList(
      h4("Financial Calendar"),
      lapply(calendar_items, tags$p)
    )
  })
  
  # Technical analysis
  output$rsi_plot <- renderPlot({
    req(input$stocks)
    stock <- input$stocks[1]
    data <- getSymbols(stock, auto.assign = FALSE)
    
    chartSeries(data, TA = "addRSI()")
  })
  
  output$macd_plot <- renderPlot({
    req(input$stocks)
    stock <- input$stocks[1]
    data <- getSymbols(stock, auto.assign = FALSE)
    
    chartSeries(data, TA = "addMACD()")
  })
  
  # Fundamental data
  output$fundamental_data <- renderUI({
    stock <- input$stocks[1]
    data <- getSymbols(stock, auto.assign = FALSE)
    fundamentals <- quantmod::getQuote(stock)
    
    tagList(
      h4("Fundamental Data"),
      p(paste("Company:", fundamentals$Name)),
      p(paste("Sector:", fundamentals$Sector)),
      p(paste("Price/Earnings Ratio:", fundamentals$`P/E`)),
      p(paste("Dividend Yield:", fundamentals$`Dividend Yield`))
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)