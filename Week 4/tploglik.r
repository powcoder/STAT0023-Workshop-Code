https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
tploglik <- function(theta,y) {
  #
  # Returns the negative log likelihood of a truncated
  # Poisson distribution, up to a constant. Arguments:
  #
  # theta Parameter of the distribution
  # y Vector of observations
  #
  n <- length(y)
  ybar <- mean(y)
  n*(theta - (ybar*log(theta)) + log(1-exp(-theta)) )
}