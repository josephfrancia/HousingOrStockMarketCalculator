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

  expected <- (1 - taxes) * investment * (1 + return - fees)^time

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
