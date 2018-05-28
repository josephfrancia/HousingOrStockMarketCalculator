createDecks <- function(numDecks) {
  return(rep(rep(c(2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10, "A"), 4), numDecks))
}

shuffleDecks <- function(deck) {
  return(sample(deck, length(deck)))
}

dealCards <- function(shuffledDeck, numberOfCardsToDeal) {
  return(shuffledDeck[1:numberOfCardsToDeal])
}

countingHandsWithAces <- function(hand) {
  hand <- hand[order(hand, decreasing = TRUE)]
  if(sum(as.numeric(replace(hand, hand == "A", 1))) > 21) {
    return(sum(as.numeric(replace(hand, hand == "A", 1))))
  }
  if(sum(as.numeric(replace(hand, hand == "A", 11))) < 22) {
    return(sum(as.numeric(replace(hand, hand == "A", 11))))
  }
  countingHandsWithAces(hand[2:length(hand)]) + 1
}

computeHandValue <- function(hand) {
  if("A" %in% hand == FALSE) {
    return(sum(as.numeric(hand)))
  }
  else {
    return(countingHandsWithAces(hand))
  }
}

dealerStrategy <- function(nonBustedHand) {
  if(computeHandValue(nonBustedHand) <= 17 & "A" %in% nonBustedHand) {
    return("Hit")
  }
  if(computeHandValue(nonBustedHand) >= 17) {
    return("Stay")
  }
  else {
    return("Hit")
  }
}

simulateDealerHand <- function(dealerCards, deck) {
  if(dealerStrategy(dealerCards) == "Stay") {
    return(list(computeHandValue(dealerCards), length(dealerCards))) #first element in list is total value of dealer's hand and the second element is the number of cards dealt to dealer
  }
  else {
    return(simulateDealerHand(c(dealerCards, dealCards(deck, 1)), deck[-1]))
  }
}


shuffledDeck <- shuffleDecks(createDecks(1))
print(shuffledDeck)
dealerCards <- shuffledDeck[1:2]
print(dealerCards)
simulateDealerHand(dealerCards, shuffledDeck)






