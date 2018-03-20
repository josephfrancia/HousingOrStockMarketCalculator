ui <- shinyUI(fluidPage(
  titlePanel("Investing in a Rental Property or in the Stock Market?"),
  plotOutput("distPlot"),
  fluidRow(
    column(3, h4("Shared Assumptions"),
           numericInput("time", label = h5("Time Horizon"), value = 30),
           numericInput("initialInvestment", label = h5("Initial Investment"), value = 100000),
           numericInput("capitalGains", label = h5("Capital Gains"), value = .15),
           numericInput("marketReturn", label = h5("Market Return"), value = .05)),
    column(3, h4("Housing Rental Market Assumptions"),
             numericInput("propertyTaxes", label = h5("Property Taxes"), value = .01),
             numericInput("housingAppreciation", label = h5("Housing Appreciation"), value = .02),
             numericInput("annualRentPayment", label = h5("Annual Rent Payment"), value = 10000),
             numericInput("annualRentIncrease", label = h5("Annual Rent Increase"), value = .005),
             numericInput("incomeTax", label = h5("Top Income Tax Rate"), value = .30)),
    column(3, h4("Housing Rental Market Assumptions (Continued)"),
             numericInput("propertyManagement", label = h5("Property Management Fees"), value = .07),
             numericInput("principalOnLoan", label = h5("Mortgage"), value = 80000),
             numericInput("loanDuration", label = h5("Mortgage Duration"), value = 30),
             numericInput("loanInterest", label = h5("Mortgage Interest"), value = .05)),
    column(3, h4("Stock Market Assumptions"),
           numericInput("assetManagementFees", label = h5("Asset Management Fees"), value = .004)
    )
  )
))


