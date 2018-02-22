computeStockMarketReturns <- function(initialInvestment,
                                      marketReturn = .05,
                                      timeHorizon = 30,
                                      capitalGains = .15,
                                      managementFees = .004) {

  return((initialInvestment * (1 + marketReturn - managementFees)^timeHorizon) - 
        (((initialInvestment * (1 + marketReturn - managementFees)^timeHorizon) - initialInvestment) * 
          capitalGains))
}


computeHousingReturns <- function(homeValue,
                                  downPayment,
                                  mortgageValue,
                                  mortgageInterest = .0392,
                                  initialAnnualRentPayment,
                                  rentIncreaseRate = 0,
                                  marketReturn = .05,
                                  homeValueAppreciation = .02,
                                  timeHorizonInYears = 30,
                                  loanDurationInYears = 30,
                                  topIncomeTaxRate = .3,
                                  capitalGains = .15) {

  accumulatedValueofHome <- homeValue * (1 + homeValueAppreciation)^timeHorizonInYears -
    ((capitalGains) *  (homeValue * (1 + homeValueAppreciation)^timeHorizonInYears - homeValue))

  annualMortgagePayment <- computeAnnualMortgagePayments(mortgageValue,
                                                         loanDurationInYears,
                                                         mortgageInterest)

  accumulatedValueofRent <- computeAccumulatedValueOfRent(initialAnnualRentPayment,
                                                          rentIncreaseRate,
                                                          marketReturn,
                                                          timeHorizonInYears,
                                                          topIncomeTaxRate,
                                                          annualMortgagePayment)

  return(accumulatedValueofHome + accumulatedValueofRent)
}

computeAccumulatedValueOfHome <- function(initialValue, 
                                          propertyTax, 
                                          timeHorizon,
                                          capitalGains,
                                          propertyAppreciation){
  
  futureValueOfHome <- initialValue * (1 + propertyAppreciation - propertyTax)^timeHorizon - 
    capitalGains * (initialValue * (1 + propertyAppreciation - propertyTax)^timeHorizon - initialValue)
  
  return(futureValueOfHome)
  
}


### Using growth annuity payment formula from http://financeformulas.net/Growing-Annuity-Payment-from-Future-Value.html
computeAccumulatedValueOfRent <- function(initialAnnualRentPayment,
                                          rentIncreaseRate,
                                          marketReturn,
                                          timeHorizon,
                                          topIncomeTaxRate,
                                          annualMortgagePayment, 
                                          managementFees) {

  accumulatedValueOfRentBeforeMortgagePaidOff <- (initialAnnualRentPayment - annualMortgagePayment) * (1 - topIncomeTaxRate) * (1 - managementFees) * 
    (((1 + marketReturn)^(min(timeHorizon, 30)) -
    (1 + rentIncreaseRate)^(min(timeHorizon, 30))) / (marketReturn - rentIncreaseRate))

  accumulatedValueOfRentAfterMortgagePaidOff <- 0

  if(timeHorizon > 30) {
    annualRentPaymentAfter30Years <-  ((1 - topIncomeTaxRate) * (1 - managementFees) * 
                                         initialAnnualRentPayment * (1 + rentIncreaseRate)^30)
    
    accumulatedValueOfRentAfterMortgagePaidOff <- annualRentPaymentAfter30Years * (((1 + marketReturn)^(timeHorizon - 30) -
    (1 + rentIncreaseRate)^(timeHorizon - 30)) / (marketReturn - rentIncreaseRate))
    
    accumulatedValueOfRentBeforeMortgagePaidOff <- accumulatedValueOfRentBeforeMortgagePaidOff * (1 + marketReturn)^(timeHorizon - 30)
  }

  accumulatedValueOfRent <- accumulatedValueOfRentAfterMortgagePaidOff + accumulatedValueOfRentBeforeMortgagePaidOff
  return(accumulatedValueOfRent)
}

#Using formula found here: https://www.nerdwallet.com/mortgages/mortgage-calculator/calculate-mortgage-payment
computeAnnualMortgagePayments <- function (principalRemaining, loanDurationInYears, annualInterestRate) {
    monthlyPayment <- principalRemaining * (annualInterestRate /  12) *
      (((1 + (annualInterestRate / 12)))^(loanDurationInYears * 12)) /
      ((1 + (annualInterestRate / 12))^(loanDurationInYears * 12) - 1)
    return(monthlyPayment * 12)
}
