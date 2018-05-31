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
  set.seed(1)
  dealerCards1 <- c("A", "9")
  dealerCards2 <- c("A", "A")
  dealerCards3 <- c("10", "9")
  dealerCards4 <- c("10", "6")
  dealerCards5 <- c("10", "A")
  
  shuffledDeck <- shuffleDecks(createDecks(1))
  
  
  actualCount <- c(simulateDealerHand(dealerCards1, shuffledDeck)[[1]], 
                   simulateDealerHand(dealerCards2, shuffledDeck)[[1]], 
                   simulateDealerHand(dealerCards3, shuffledDeck)[[1]],
                   simulateDealerHand(dealerCards4, shuffledDeck)[[1]],
                   simulateDealerHand(dealerCards5, shuffledDeck)[[1]])
  
  expectedCount <- c(20, 21, 19, 18, 21)
  
  actualNumberOfCardsDealt <- c(simulateDealerHand(dealerCards1, shuffledDeck)[[2]], 
                                simulateDealerHand(dealerCards2, shuffledDeck)[[2]], 
                                simulateDealerHand(dealerCards3, shuffledDeck)[[2]],
                                simulateDealerHand(dealerCards4, shuffledDeck)[[2]],
                                simulateDealerHand(dealerCards5, shuffledDeck)[[2]])
    
  expectedNumberOfCardsDealt <- c(2, 4, 2, 3, 2)
    
  expect_equal(actualCount, expectedCount)
  expect_equal(actualNumberOfCardsDealt, expectedNumberOfCardsDealt)
})
