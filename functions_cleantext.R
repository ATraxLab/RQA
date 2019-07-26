## Break text into sentences, then chunk sentences into windows

# Break posts into sentences using ?, !, or period+space: First line breaks entry at any "!"
# Second line repeats for question marks, third line for periods
# Input: Posts is character vector, one post per entry
# Output: Character vector, one sentence per entry
makeSentences <- function(posts) {
  sentences <- unlist(strsplit(posts,"!",fixed=TRUE))
  sentences <- unlist(strsplit(sentences,"?",fixed=TRUE))
  sentences <- unlist(strsplit(sentences,". ",fixed=TRUE))
  return(sentences)
}

# Break sentences vector into windows of length wsize
# Input: sentences is a character vector where each entry is one sentence, wsize is number
#  of sentences to use per window
# Output: windows is a list of character vectors, each vector has wsize entries and each 
#  entry is a sentence
makeWindows <- function(sentences, wsize = 3) {
  wcount <- ceiling(length(sentences)/wsize)  # number of windows this will make
  windows <- vector("list",wcount)   # initialize empty list 
  for (i in 1:wcount) {
    # figure out where this window begins and ends in the sentences vector
    first <- wsize*(i-1)+1
    last <- min(i*wsize,length(sentences))
    # make a list using those entries from the sentences vector
    windows[i] <- list(sentences[first:last])
  }
  return(windows)
}

# Input: A character variable of words with punctuation, capitalization, etc.
# Output: A character vector, lowercase, no punctuation, one word per entry.
lineToWords <- function(dat) {
  dat <- gsub(pattern = "[[:punct:]]{2,}", replacement = " ", dat)
  dat <- unlist(strsplit(as.character(dat), " "))
  dat <- tolower(dat)
  dat <- gsub(pattern = "[[:punct:]]", replacement = "", dat)
  dat <- dat[!(dat == "")]
}
