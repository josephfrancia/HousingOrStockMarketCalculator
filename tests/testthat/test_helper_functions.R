test_that("computeStockMarketReturns works as expected", {
  investment = 100
  return = .05
  time = 4
  fees = .006
  taxes = .15

  actual <- computeStockMarketReturns(initialInvestment = investment,
                                      marketReturn = return,
                                      timeHorizon = time,
                                      capitalGains = taxes,
                                      managementFees = fees
                                      )

  expected <- investment * (1 + return - fees)^time - ((investment * (1 + return - fees)^time) - investment) * taxes

  expect_equal(actual, expected)
})

test_that("computeAnnualMortgagePayments works as expected", {
  principal = 100
  annualRate = .05
  loanDuration = 30

  actual <- computeAnnualMortgagePayments(principalRemaining = principal,
                                          loanDurationInYears = loanDuration,
                                          annualInterestRate = annualRate)

  expected <- 12 * principal * ((annualRate / 12) * (1 + annualRate / 12)^360) / ((1 + (annualRate / 12))^360 - 1)
  expect_equal(actual, expected)
})

test_that("computeAccumulatedValueOfRent works as expected for time horizon under 30 years", {
  initialAnnualRent = 100
  rentIncrease = .01
  marketRate = .05
  time = 20
  incomeTax = .3
  annualMortgage = 40
  fees = .07

  actual <- computeAccumulatedValueOfRent(initialAnnualRentPayment = initialAnnualRent,
                                          rentIncreaseRate = rentIncrease,
                                          marketReturn = marketRate, 
                                          timeHorizon = time, 
                                          topIncomeTaxRate = incomeTax, 
                                          annualMortgagePayment = annualMortgage, 
                                          managementFees = fees)
  
  expected <- (initialAnnualRent - annualMortgage) * (1 - incomeTax) * (1 - fees) * 
    ((1 + marketRate)^time - (1 + rentIncrease)^time) / (marketRate - rentIncrease)
  expect_equal(actual, expected)
})

test_that("computeAccumulatedValueOfRent works as expected for time horizon over 30 years", {
  initialAnnualRent = 100
  rentIncrease = .01
  marketRate = .05
  time = 35
  incomeTax = .3
  annualMortgage = 40
  management = .07

   actual <- computeAccumulatedValueOfRent(initialAnnualRentPayment = initialAnnualRent,
                                          rentIncreaseRate = rentIncrease,
                                          marketReturn = marketRate, 
                                          timeHorizon = time, 
                                          topIncomeTaxRate = incomeTax, 
                                          annualMortgagePayment = annualMortgage, 
                                          managementFees = management)
  
  expected <- 
    
    (((initialAnnualRent - annualMortgage) * (1 - incomeTax) * (1 - management) *
    ((1 + marketRate)^30 - (1 + rentIncrease)^30) / (marketRate - rentIncrease))) * (1 + marketRate)^(time - 30) + 
    
    
    (initialAnnualRent * (1 + rentIncrease)^30 * (1 - incomeTax) * (1 - management)) *
    (((1 + marketRate)^(time - 30) - (1 + rentIncrease)^(time - 30)) / (marketRate - rentIncrease))
  expect_equal(actual, expected)
})
