devtools::load_all(".")
library(scales)

server <- function(input, output) {

  df <-  reactive({
    data.frame(time = 1:input$time,
               housing = plotHousingReturns(input$time, input$initialInvestment,
                                            input$propertyTaxes, input$housingAppreciation, input$capitalGains,
                                            input$annualRentPayment, input$annualRentIncrease, input$marketReturn,
                                            input$incomeTax, input$propertyManagement, input$principalOnLoan,
                                            input$loanDuration, input$loanInterest),
               stocks = plotStockMarketReturns(input$time, input$initialInvestment,
                                               input$marketReturn, input$capitalGains, input$assetManagementFees))
  })

  output$distPlot <- renderPlot({
    ggplot(df(), aes(time)) +
      geom_line(aes(y = housing, colour = "housing")) +
      geom_line(aes(y = stocks, colour = "stocks")) +
      scale_y_continuous(label=dollar_format()) +
      xlab("Time Horizon") +
      ylab("Future Value in Dollars")
  })
}
