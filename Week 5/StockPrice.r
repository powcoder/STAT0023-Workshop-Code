https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
#######################################################################
#######################################################################
#######################################################################
######                                                           ######
######     Starter code for the "stock sale price" example       ######
######                from STAT0023 Workshop 5                   ######
######                                                           ######
#######################################################################
#######################################################################
#######################################################################
nsims <- 10000                           # Number of simulations
mu <- 0.5; sigsq <- 0.01                 # Define parameter values
T <- rgamma(nsims,2,3)                   # Times of sale
Bt <- rnorm(nsims,mean=0,sd=sqrt(T))     # Values of B[T]
SalePrice <- exp( (mu-0.5*sigsq)*T+(sqrt(sigsq)*Bt) )
