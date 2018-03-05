ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h3("Shared Assumptions"),
      numericInput("time", label = h3("Time Horizon"), value = 30),
      numericInput("initialInvestment", label = h3("Time Horizon"), value = 100000),
      numericInput("capitalGains", label = h3("Capital Gains"), value = .15),
      numericInput("marketReturn", label = h3("Market Return"), value = .05),
      h3("Stock Market Assumptions"),
      numericInput("time", label = h3("Time Horizon"), value = 30),
      numericInput("marketReturn", label = h3("Market Return"), value = .05),
      numericInput("assetManagementFees", label = h3("Asset Management Fees"), value = .004),
      h3("Housing Rental Market Assumptions"),
      numericInput("propertyTaxes", label = h3("Property Taxes"), value = .05),
      numericInput("housingAppreciation", label = h3("Housing Appreciation"), value = .01),
      numericInput("annualRentPayment", label = h3("Annual Rent Payment"), value = 10000),
      numericInput("annualRentIncrease", label = h3("Annual Rent Increase"), value = .005),
      numericInput("incomeTax", label = h3("Top Income Tax Rate"), value = .30),
      numericInput("propertyManagement", label = h3("Property Management Fees"), value = .07),
      numericInput("principalOnLoan", label = h3("Mortgage"), value = .07),
      numericInput("loanDuration", label = h3("Mortgage"), value = 30),
      numericInput("loanInterest", label = h3("Mortgage"), value = .05)
    ),
    mainPanel(plotOutput("distPlot"))
  )
)
