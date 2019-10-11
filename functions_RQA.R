# Calculate RQA statistics: recurrence rate, determinism, max diagonal, average diagonal.
# Input: Recurrence matrix, optional similiarty threshold to count recurrence, optional 
#  length threshold to count a diagonal line
# Output: A list with the similarity and diagonal length thresholds, number and fraction 
#  of points counted as recurring, recurrence rate, determinism, mean and max diagonals
calcRQA <- function(rmat, s_thres = 0.5, l_thres = 2) {
######################   STATISTICS   #######################

#UPDATED ---- 11OCT2019

#############################################################

require('crqa')

## NOTE ON THRESHOLDS:
# s_thres: Similarity threshold = .50 means similarity > .5 passes as a point
# Ideally, choose s_thres such that the number of kept points isn't too high or low.
# l_thres: Distance threshold = 2 means that 2 points form a diagonal

  
# Calculate Nint from dimension of recurrence matrix (works for utterances or windows)
Nint <- dim(rmat)[1]

# Number of recurrent points, excluding the main diagonal. To avoid floating-point 
#  error, it's best to count all points at/above threshold, then subtract off the length 
#  of the diagonal.
nrec <- length(which(rmat >= s_thres)) - Nint

# Fraction of points with nonzero values that were kept: nrec / (nrec + nexclude)
# Tells us how much we are excluding, which helps us debate the threshold
nexclude <- length(which(rmat < s_thres & rmat > 0))
rec.p <- (nrec/(nrec + nexclude))*100


#############################################################

#RECURRENCE  RATE (RR)

# See difference between Webber and Zbilut (2005) eqn. (2.8) and RR 
#  formula at http://www.recurrence-plot.tk/rqa.php. It should be 
#  RR = sum(rmat)/N^2 if rmat is a full matrix, or the uncommented
#  version below if rmat is lower- or upper-triangular.
#RR <- 2/(Nint^2)*nrec  # older version, not quite accurate
RR <- 2/(Nint * (Nint - 1))*nrec

# PERCENT Recurrence (%REC)
RR.p <- RR* 100


#############################################################

# Rate of Determinism (DET)
l <- 0
ndiags <- 0

# Translate thresholded rmat into 0/1 values
rmat.cut <- rmat
rmat.cut[which(rmat.cut < s_thres)] <- 0
rmat.cut <- 1*(rmat.cut >= s_thres)

# Plot thresholded recurrence matrix
#ggplot(data = melt(rmat.cut), aes(x = Var1, y = Var2, fill = value)) + geom_tile()

diags <- spdiags(rmat.cut)

# Loop through all but the last (all 1's) column of diags
for(j in 1:(dim(diags$B)[2] - 1)) {
  # Loop through all rows of diags
  for(i in 1:Nint){
    if(diags$B[i, j] == 0) {
      l <- 0
    }
    if(diags$B[i, j] > 0) {
      l <- l + 1
    }
    if(l == l_thres) {
      # ndiags is number of points that are in diagonals
      ndiags <- ndiags + l  # just hit threshold, l new points are recognized as in a diagonal
    }
    if(l > l_thres) {
      ndiags <- ndiags + 1
    }
  }   
}     

DET <- ndiags/nrec
print(paste0("Number of points in diagonals: ", ndiags))

# PERCENT Determinism (%DET)
DET.p <- DET*100


#############################################################


# MAXLINE: Length of the longest diagonal line 
l <- 0
maxl <- 0 

# Loop through all but the last (all 1's) column of diags
# I think this loop could be combined with above (just move the l > maxl clause up)
for(j in 1:(dim(diags$B)[2]-1)) {
  for(i in 1:Nint){
    if(diags$B[i, j] > 0) {
      l <- l + 1
    }
    if(l > maxl) {
      maxl <- l
    }
    if(diags$B[i, j] == 0) {
      l <- 0
    }
  } 
}      

# MAXLINE = max
MAXLINE <- maxl


#############################################################

# Nline: Total number of diagonal lines 
### AND ###
# MEANLINE: Average length of the diagonal lines

# Add a row of zeros to the bottom of diags$B to make sure loop below recognizes
#  when a diagonal is ending (loop checks value in row i+1)
diags.pad <- rbind(diags$B, rep(0, length(Nint)))

l <- 0
ds <- c()  # vector of diagonal lengths

for(j in 1:(dim(diags$B)[2] - 1)) {
  for(i in 1:Nint) {
    if(diags.pad[i, j] == 0) {
      l <- 0
    }
    if(diags.pad[i, j] > 0) {
      l <- l + 1
    }
    # If a current diagonal is ending, record its length
    if(diags.pad[i + 1, j] == 0 & l >= l_thres) {
      ds <- append(ds, l)
    }
  }
  l <- 0
}

# total number of diagonal lines
Nline <- length(ds)

# Average length of lines
MEANLINE = mean(ds)

# This is the easy way to get MAXLINE. The long way above will save the max 
#  length even if none were above l_thres, which might be useful in some cases.
MAXLINE_2 <- max(ds) 

return(list(s_thres = s_thres, l_thres = l_thres, nrec = nrec, rec.p = rec.p, 
            RR.p = RR.p, DET.p = DET.p, MEANLINE = MEANLINE, 
            MAXLINE = max(MAXLINE, MAXLINE_2)))
}
