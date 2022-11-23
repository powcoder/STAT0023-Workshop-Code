https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
#######################################################################
#######################################################################
#######################################################################
######                                                           ######
######     Demonstration of rejection algorithm for sampling     ######
######       from a Beta(a,b) distribution with a>=1, b>=1       ######
######                                                           ######
#######################################################################
#######################################################################
#######################################################################
BetaSim <- function(n,a,b) {
  #######################################################################
  #
  #   Function to simulate n random variates from a Beta(a,b)
  #   distribution with a>=1, b>=1, using a rejection method
  #   with uniform proposals. The function returns a list
  #   result with components x (vector of sampled values) 
  #   and Ntries (vector counting the number of attempts
  #   required for each value).
  #
  #######################################################################
  if (a < 1 | b < 1) stop("This algorithm only works for a,b>=1")
  #
  #   Initialise: logical vector "NotDone" indicating which 
  #   elements of the result still need to be allocated, and the 
  #   bounding constant M (excluding the constant of proportionality
  #   K, which isn't needed). Also a vector for the result,
  #   and a counting vector for the number of tries.
  #
  NotDone <- rep(TRUE,n)
  Result <- rep(NA,n)
  Ntries <- rep(0,n)
  M <- ( ( (a-1)^(a-1) ) * ( (b-1)^(b-1) ) ) / ( (a+b-2)^(a+b-2) )
  #
  #   Loop until we've accepted all values. Take care with the
  #   brackets - don't get [] and () mixed up!
  #
  while (any(NotDone)) {
    NToDo <- sum(NotDone) # Number of values still to do
    Ntries[NotDone] <- Ntries[NotDone] + 1 # One more try for these values
    Y <- runif(NToDo)   # Proposed values from uniform distribution
    Result[NotDone] <- Y
    U <- runif(NToDo)   # Rejection step (formula in notes)
    Reject <- (U >  ( Y^(a-1)*(1-Y)^(b-1) ) / M )
    NotDone[NotDone] <- Reject # Anything just accepted becomes FALSE
  }
  list(x=Result,Ntries=Ntries)
}