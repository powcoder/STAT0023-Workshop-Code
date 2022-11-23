https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
Cauchy.tail.MC <- function(a,n=100){
  #
  # Use Monte Carlo integration to estimate theta=P(X>a), 
  # for a > 0, where X has a # standard Cauchy distribution, 
  # with p.d.f. f(x)=[pi*(1+x^2)]^(-1), for real x.
  # The exact value of theta is 1/2-arctan(a)/pi.
  # For example, theta = 0.25 when a=1,
  #          and theta ~ 0.1476 when a=2.
  #
  # Inputs:
  # a : a (must be positive)
  # n : the number of MC samples (larger n gives greater precision)
  # 
  # Author: Yvo Pokern, with small modifications by Richard Chandler
  #
  if (a <= 0){ # Note the restriction a > 0
    stop("Caution: Cauchy.tail.MC() not defined for a <= 0")
  }
  
  # Create required samples (all from the same initial U(0,1) sample).
  u <- runif(n)           # U(0,1) sample
  x <- tan(pi*(u-1/2))    # standard Cauchy sample
  u1a <- u/a              # uniform(0,1/a) sample
  ua <- u*a               # uniform(0,a) sample
  v <- a/u                # sample from g(x)=a/x^2 on (a,infinity)
  
  # Antithetic samples
  x2 <- tan(pi*(1-u+1/2)) # standard Cauchy sample
  u1a2 <- (1-u)/a         # (antithetic) uniform(0,1/a) sample
  ua2 <- (1-u)*a          # (antithetic) uniform(0,a) sample
  v2 <- a/(1-u)           # (antithetic) sample from g(x)=a/x^2 on (a,infinity)
  
  # 1. Hit-or-miss:
  f1 <- function(x) mean(x>a)
  t1 <- f1(x) 
  t1a <- ( t1 + f1(x2) ) / 2
  
  # 2. Hit-or-miss using symmetry I
  f2 <- function(x) mean(abs(x)>a)/2
  t2 <- f2(x)
  t2a <- ( t2 + f2(x2) ) / 2
  
  # 3. Using U(0,a) variates
  f3 <- function(ua) (1-2*mean(a/(pi*(1+ua^2))))/2
  t3 <- f3(ua)
  t3a <- ( t3 + f3(ua2) ) / 2
  
  # 4. Using property that int_0^1/2 = int_2^infinity (for standard Cauchy)
  f4 <- function(u1a) mean(1/(a*pi*(1+u1a^2)))
  t4 <- f4(u1a)
  t4a <- ( t4 + f4(u1a2) ) / 2
  
  # 5. Importance sampling
  f5 <- function(v) mean(v^2/(a*pi*(1+v^2)))
  t5 <- f5(v)
  t5a <- ( t5 + f5(v2) ) / 2
  
  list(basic=c(t1,t2,t3,t4,t5),anti=c(t1a,t2a,t3a,t4a,t5a))
}
