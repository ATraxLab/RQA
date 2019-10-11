## Functions to do steps of Angus et al. (2012) conceptual recurrence analysis

# Calculate frequency of unique words across all windows
# Input: A list of text windows; each list entry is a vector and each entry in the vector
#  is a single word.
# Output: Vector of unique words, how many windows each word appeared in (ucount), and 
#  co-occurrence matrix Cij of how often words i and j appear in the same window
calcOccur <- function(words, code = NULL) {
  if (!is.null(code)) print(paste0("Calculating occurrence statistics for code ", code, "..."))
  
  uniqWords <- unique(unlist(words))  # list of unique words
  Nuniq <- length(uniqWords)
  ucount <- rep(0,Nuniq)
  
  # ucount is occurrence vector (O) in Angus et al. 2012
  for (i in 1:length(winwords)) {
    ucount <- ucount + as.integer(uniqWords %in% winwords[[i]])
  }
  #print(rbind(uniqWords[1:10],ucount[1:10]))
  
  # Count unique word co-occurrence (C_ij matrix in Angus et al. 2012)
  cocur <- matrix(data = 0, nrow = Nuniq, ncol = Nuniq)
  
  ptm <- proc.time() # current CPU time
  for (i in 1:length(winwords)) {
    inwindow <- as.integer(uniqWords %in% winwords[[i]])
    cocur <- cocur + (inwindow %*% t(inwindow))
  }
  (time1 <- proc.time() - ptm)
  return(list(uniqWords = uniqWords, ucount = ucount, cocur = cocur))
}


# Calculate the conceptual similarity using eqn (1) in Angus et al. (2012).
# Input: Occurrence count of words (ucount), co-occurrence matrix of terms, and number of windows
# Output: Sij matrix, which is supposed to be Nkey x Nterms (currently Nterms x Nterms)
calcSim <- function(ucount,cocur,Nwind)  { #This is a function with 3 arguments
  Nuniq <- length(ucount)
  print(paste0("Calculating similarity of ",as.character(Nuniq)," words over ",
               as.character(Nwind)," windows..."))
  P11 <- matrix(nrow = Nuniq, ncol = Nuniq) # create blank matrices
  P00 <- matrix(nrow = Nuniq, ncol = Nuniq)
  P10 <- matrix(nrow = Nuniq, ncol = Nuniq)
  P01 <- matrix(nrow = Nuniq, ncol = Nuniq)
  for (i in 1:Nuniq) {
    for (j in 1:Nuniq) {
      Oi <- ucount[i]
      Oj <- ucount[j]
      Cij <- cocur[i,j]
      P11[i,j] <- Cij/Nwind # percent of occurence of a unique word through all the windows
      P00[i,j] <- (Nwind - Oi - Oj + Cij)/Nwind
      P10[i,j] <- (Oi - Cij)/Nwind
      P01[i,j] <- (Oj - Cij)/Nwind
      if ((Oi + Oj)==(Cij + Nwind)) P00[i,j] <- 1
      if (Oi == Cij) P10[i,j] <- 1
      if (Oj == Cij) P01[i,j] <- 1
    }
  }
  Sim <- P11*P00/(P10*P01)
  return(Sim)
  #return(list(Sim,P11,P00,P10,P01))
}


# Calculate the Boolean matrix (Nterm x Nutterance) as in Angus et al. (2012)
# Input: A character vector of posts (one per vector entry) and vector of uniqe words
# Output: Matrix Bij with 0/1 values for whether term i occurs in utterance j
calcB <- function(posts, uniqWords) {
  utterances <- lapply(posts, lineToWords)
  Nuniq <- length(uniqWords)
  Nutt <- length(utterances)
  B <- matrix(0, nrow = Nuniq, ncol = Nutt)
  for(j in 1:Nutt) {
    B[,j] <- as.numeric(uniqWords %in% utterances[[j]])
  }
  return(B)
}

# Calculate the Boolean matrix (Nterm x Nutterance) by windows instead of utterances
#  (old way, may delete this)
# Input: List of text windows (each entry is a vector of words), vector of unique words
# Output: Matrix Bij with 0/1 values for whether term i occurs in window j
calcBwind <- function(winwords, uniqWords) {
  Nuniq <- length(uniqWords)
  Nwind <- length(winwords)
  B <- matrix(0, nrow = Nuniq, ncol = Nwind)
  for(j in 1:length(winwords)){
    B[,j] <- as.numeric(uniqWords %in% winwords[[j]]) 
  }
  return(B)
}


# Calculate the matrix of conceptual similarity values between two utterances(/windows)
# Input: Matrix B tracking word occurrence in utterances (or windows) and the corresponding 
#  feature matrix V
# Output: Matrix of similarity between utterances(/windows) i and j
recurMat <- function(B, vnorm) {
  Nrow <- dim(B)[2]  # number of utterances or windows from Boolean matrix
  rmat <- matrix(0, Nrow, Nrow)
  for (i in 1:Nrow) {
    for (j in 1:i) {
      rmat[i,j] <- vnorm[,i] %*% vnorm[,j]
    }
  }
  return(rmat)
}