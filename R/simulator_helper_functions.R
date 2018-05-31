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

playerStrategy <- function(nonBustedHand, dealerFaceCard) {
  if ("A" %in% nonBustedHand == FALSE) {
    if(computeHandValue(nonBustedHand) %in% c(21, 20, 19, 18, 17) | 
       computeHandValue(nonBustedHand) %in% c(13, 14, 15, 16) & dealerFaceCard < 7) | 
       computeHandValue(nonBustedHand) == 12 & dealerFaceCard %in% c(4, 5, 6)) {
         return("Stay")
       }
  } 
}
doubleDown <- function(deck, hand) {
  return(computeHandValue(c(dealCards(deck, 1), hand)))
}



