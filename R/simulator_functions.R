simulateDealerHand <- function(dealerCards, deck) {
  if(dealerStrategy(dealerCards) == "Stay") {
    return(list(computeHandValue(dealerCards), length(dealerCards))) #first element in list is total value of dealer's hand and the second element is the number of cards dealt to dealer
  }
  else {
    return(simulateDealerHand(c(dealerCards, dealCards(deck, 1)), deck[-1]))
  }
  