computeStockMarketReturns <- function(initialInvestment,
                                      marketReturn = .05,
                                      timeHorizon = 30,
                                      capitalGains = .15,
                                      managementFees = .004) {

  return((initialInvestment * (1 + marketReturn - managementFees)^timeHorizon) * (1 - capitalGains))
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


computeAccumulatedValueOfRent <- function(initialAnnualRentPayment,
                                          rentIncreaseRate,
                                          marketReturn,
                                          timeHorizon,
                                          topIncomeTaxRate,
                                          annualMortgagePayment) {

  accumulatedValueOfRentBeforeMortgagePaidOff <- (initialAnnualRentPayment - annualMortgagePayment) * (1 - topIncomeTaxRate) * (((1 + marketReturn)^timeHorizon -
    (1 + rentIncreaseRate)^timeHorizon) / (marketReturn - rentIncreaseRate))

  accumulatedValueOfRentAfterMortgagePaidOff <- 0

  if(timeHorizon > 30) {
    annualRentPaymentAfter30Years <-  initialAnnualRentPayment * (rentIncreaseRate)^30
    accumulatedValueOfRentAfterMortgagePaidOff <- annualRentPaymentAfter30Years * (1 - topIncomeTaxRate) * (((1 + marketReturn)^(timeHorizon - 30) -
    (1 + rentIncreaseRate)^(timeHorizon - 30)) / (marketReturn - rentIncreaseRate))
  }

  accumulatedValueOfRent <- accumulatedValueOfRentAfterMortgagePaidOff + accumulatedValueOfRentBeforeMortgagePaidOff
  return(accumulatedValueOfRent)
}

computeAnnualMortgagePayments <- function (principalRemaining, loanDurationInYears, annualInterestRate) {
    monthlyPayment <- principalRemaining * (annualInterestRate /  12) *
      (((1 + (annualInterestRate / 12)))^(loanDurationInYears * 12)) /
      ((1 + (annualInterestRate / 12))^(loanDurationInYears * 12) - 1)
    return(monthlyPayment * 12)
}
