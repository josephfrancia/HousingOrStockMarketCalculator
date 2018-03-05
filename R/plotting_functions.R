library(ggplot2)
library(dplyr)
library(purrr)

plotStockMarketReturns <- function(timeHorizon, initialInvestment, marketReturn, capitalGains, managementFees) {

  data <- data.frame(years = 1:timeHorizon,
                     assetValue =
                       map_dbl(1:timeHorizon, computeStockMarketReturns, initialInvestment = initialInvestment,
                               marketReturn = marketReturn, capitalGains = capitalGains, managementFees = managementFees))
  plot <- data %>%
    ggplot(aes(x = years, y = assetValue)) + geom_line()

  return(data$assetValue)
}
plotHousingReturns <- function(time, initialHousingValue, propertyTaxes, housingReturn,
                               capGains, annualRentPayment, rentIncreasePercent, marketRate, incomeTax, management,
                               principalOnLoan, loanDuration, loanInterest) {

  data <- data.frame(years = 1:time,
                     assetValue =
                       map_dbl(1:time, computeHousingReturns, initialHousingValue = initialHousingValue,
                               propertyTaxes = propertyTaxes, housingReturn = housingReturn,
                               capGains = capGains, annualRentPayment = annualRentPayment, rentIncreasePercent = rentIncreasePercent,
                               marketRate = marketRate, incomeTax = incomeTax, management = management, principalOnLoan = principalOnLoan,
                               loanDuration = loanDuration, loanInterest = loanInterest))

  return(data$assetValue)
}

df <- data.frame(time = 1:30,
                 housing = plotHousingReturns(30, 100000 ,.01, .02, .15,
                                              800 * 12, .005, .06, .3, .07, 80000, 30, .05),
                 stocks = plotStockMarketReturns(30, 100000, .06, .15, .0015))


ggplot(df, aes(time)) +
  geom_line(aes(y = housing, colour = "housing")) +
  geom_line(aes(y = stocks, colour = "stocks"))
