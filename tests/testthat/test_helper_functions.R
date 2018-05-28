test_that("countingHandsWithAces works as expected", {
  sampleHand1 <- c("A", "9")
  sampleHand2 <- c("A", "A", "A")
  sampleHand3 <- c("A", "10", "A", "A", "A", "A", "A", "10")

  actual <- c(countingHandsWithAces(sampleHand1), 
              countingHandsWithAces(sampleHand2), 
              countingHandsWithAces(sampleHand3))

  expected <- c(20, 13, 26)

  expect_equal(actual, expected)
})

test_that("computeHandValue works as expected", {
  sampleHand1 <- c("A", "9")
  sampleHand2 <- c("A", "A", "A")
  sampleHand3 <- c("A", "10", "A", "A", "A", "A", "A", "10")
  sampleHand4 <- c("10", "9")
  sampleHand5 <- c("10", "9", "5")
  
  actual <- c(computeHandValue(sampleHand1), 
              computeHandValue(sampleHand2), 
              computeHandValue(sampleHand3),
              computeHandValue(sampleHand4),
              computeHandValue(sampleHand5))
  
  expected <- c(20, 13, 26, 19, 24)
  
  expect_equal(actual, expected)
})

test_that("dealerStrategy works as expected", {
  sampleHand1 <- c("A", "9")
  sampleHand2 <- c("A", "A", "A")
  sampleHand3 <- c("10", "9")
  sampleHand4 <- c("10", "6", "1")
  sampleHand5 <- c("10", "3", "3", "A")
  
  
  actual <- c(dealerStrategy(sampleHand1), 
              dealerStrategy(sampleHand2), 
              dealerStrategy(sampleHand3),
              dealerStrategy(sampleHand4),
              dealerStrategy(sampleHand5))
  
  expected <- c("Stay", "Hit", "Stay", "Stay", "Hit")
  
  expect_equal(actual, expected)
})

test_that("simulateDealerHand works as expected", {
  sampleHand1 <- c("A", "9")
  sampleHand2 <- c("A", "A", "A")
  sampleHand3 <- c("10", "9")
  sampleHand4 <- c("10", "6", "1")
  sampleHand5 <- c("10", "3", "3", "A")
  
  simulateDealerHand(dealerCards, shuffledDeck[3:4])
  
  
  actual <- c(dealerStrategy(sampleHand1), 
              dealerStrategy(sampleHand2), 
              dealerStrategy(sampleHand3),
              dealerStrategy(sampleHand4),
              dealerStrategy(sampleHand5))
  
  expected <- c("Stay", "Hit", "Stay", "Stay", "Hit")
  
  expect_equal(actual, expected)
})
