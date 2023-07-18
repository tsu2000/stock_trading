library(shiny)
library(shinythemes)
library(quantmod)
library(shinyWidgets)
library(lubridate)

df <- read.csv("stocktickers.csv")

# Define UI
ui <- fluidPage(theme = shinytheme("cosmo"),
                titlePanel("Basic Stock Tracking App"),
                sidebarLayout(
                  sidebarPanel(
                    selectizeInput("symbols", 
                                   label = "Enter Stock Symbols (Max 30):", 
                                   choices = NULL,
                                   options = list(maxOptions = 5, maxItems = 30, create = TRUE),
                                   selected = c("AAPL", "^STI", "GOOGL", "MSFT")
                    ),
                    sliderTextInput("timePeriod", "Select period of time to start charting the data from:", 
                                    choices = c("All", "5yrs", "2yrs", "1yr", "6mths", "3mths","1mth"),
                                    selected = "1yr", 
                                    grid = TRUE),
                    actionButton("getStockData", "Get Stock Data")
                  ),
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Stock Chart Plots", 
                               uiOutput("stockPlots")),
                      tabPanel("All stock symbols (Year: 2020)",
                               dataTableOutput("csvTable")
                      )
                    )
                  )
                )
)

# Define server
server <- function(input, output, session) {
  
  updateSelectizeInput(session, 'symbols', choices = df$Ticker.Name, selected = c("AAPL", "GOOGL", "^STI", "MSFT"), server = TRUE)
  
  observeEvent(input$getStockData, {
    
    stockSymbols <- input$symbols
    time_period <- input$timePeriod
    stockData <- vector("list", length(stockSymbols))
    
    # Calculate the start date based on the selected time period
    current_date <- Sys.Date()
    start_date <- switch(
      time_period,
      "All" = as.character(current_date - years(100)),
      "5yrs" = as.character(current_date - years(5)),
      "2yrs" = as.character(current_date - years(2)),
      "1yr" = as.character(current_date - years(1)),
      "6mths" = as.character(current_date - months(6)),
      "3mths" = as.character(current_date - months(3)),
      "1mth" = as.character(current_date - months(1))
    )
    
    for (i in seq_along(stockSymbols)) {
      symbol <- stockSymbols[i]
      dataFetched <- FALSE
      
      tryCatch({
        data <- getSymbols(symbol, from = start_date, auto.assign = FALSE, warnings = FALSE, src = "yahoo")
        stockData[[i]] <- Cl(data)
        dataFetched <- TRUE
      }, error = function(e) {
        message(paste("Data not found for symbol:", symbol))
      })
      
    }
    
    output$stockPlots <- renderUI({
      stockPlots <- lapply(seq_along(stockData), function(i) {
        if (!is.null(stockData[[i]])) {
          plotOutput(paste0("stockPlot", i), height = "300px", width = "100%")
        }
      })
      do.call(tagList, stockPlots)
    })
    
    for (i in seq_along(stockData)) {
      if (!is.null(stockData[[i]])) {
        local({
          idx <- i
          output[[paste0("stockPlot", idx)]] <- renderPlot({
            chartSeries(stockData[[idx]], 
                        theme = chartTheme("white", up.col = "blue"), 
                        name = stockSymbols[idx],
                        type = "line"
                        )
          })
        })
      }
    }
    
  })
  
  output$csvTable <- renderDataTable(df)
}

# Run the application
shinyApp(ui = ui, server = server)
