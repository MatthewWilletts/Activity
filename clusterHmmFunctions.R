#Here begin the HMM functions

computeEmissionProbs_mod = function(rf,behavior_data) {
  # get the list of unque states
  states = sort(rf$classes)
  symbols = sort(rf$classes)
  S = length(states)
  # set up the transition Probability matrix
  emissionProbs = matrix(0, nrow=S, ncol=S)
  rownames(emissionProbs) = states
  colnames(emissionProbs) = states
  # loop through state sequence and count transitions
  for (k in 1:length(states)) {
    emissionProbs[k, ] = colSums(rf$votes[behavior_data == states[k], ])
    emissionProbs[k, ] = emissionProbs[k, ]/sum(emissionProbs[k, ])
  }
  return(emissionProbs)
}

trainHMM_mod = function(labels, rf,data,trans,prior) {
  
  # load ground truth labels
  labels = labels$behavior
  
  cat("training HMM\n")
  # get the unique states
  states = sort(unique(labels))
  states = states[!grepl("NULL", states)]
  symbols = sort(rf$classes)
  # compute the transition probabilities
  transProbs = trans
  # compute the emission probabilities
  emissionProbs = computeEmissionProbs_mod(rf,data)
  # compute the prior probabilities
  startProbs = prior
  # make the HMM
  hmm = initHMM(states, symbols, startProbs, transProbs, emissionProbs)
  return(hmm)
}


