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
