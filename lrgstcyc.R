## largest cycle of a random permutation on n symbols (n->infinity)

smry <- function(X){       
        mu <- mean(X)
        sigma <- sd(X)
        a <- min(X)
        q25 <- quantile(X,0.25)
        q50 <- quantile(X,0.50)
        q75 <- quantile(X,0.75)
        b <- max(X)
        c(mu=mu,sigma=sigma,min=a,q25,q50,q75,max=b)}

f <- function(X)
  {
    n <- length(X)
    J <- rep(1,n)
    Y <- seq(1,n)    # same as 1:n
    k <- 1
    while (max(J)>0)
      {
        J <- J & (X[Y]-1:n)
        Y <- X[Y]
        k <- k+1
      }  
    k-1
  }

f(c(3,5,7,4,9,6,8,10,2,1))
f(c(2,1,4,3,6,5,8,7,10,9))


F <- function(m,n)
{    
    sapply(1:m, function(o) f(sample(n)))     
}                                             

m <- 5000
n <- 100
X <- F(m,n)               
smry(X)                   

mean(X)/n      # what is the limit of this ratio as n -> infinity?

c <- -psigamma(1)     # Euler's constant (from calculus)
i <- 1:25             # exponential integral Ei(x) (using series)
Ei <- function(x) {c+log(abs(x))+sum(x^i/(i*factorial(i)))}
Hi <- function(x) exp(Ei(log(x)))    # my attempt to be cute
integrate(Vectorize(Hi),0,1)    # Golomb's constant 0.6243299885...
                                # (publicized as such in 1964)

# ASIDE: extra command Vectorize sometimes needed for integrand
# to be correctly interpreted & evaluated at vector inputs

## largest prime factor of a random integer n, where 1<=n<=N
## (N->infinity)

primes <- function(n) 
{
    x <- 1:n
    x[1] <- 0
    p <- 1
    m <- floor(sqrt(n))   # p-update occurs just prior to p<=m check 
    while ((p <- p+1) <= m) if (x[p] != 0)   
        x[seq(p^2,n,p)] <- 0    # composites p^2, p^2+p=p(p+1), 
    x[x>0]                      # p^2+2*p=p(p+2), ...
}

N <- 5000000
P <- primes(N)            # most efficient to hardcode this in

g <- function(n) 
{
    tbl <- P[P<=n]
    fac <- tbl[n%%tbl==0]
    max(fac)
}

3*3*5
g(45)

2*2*2*3*3*5*5*7
g(12600)

G <- function(m,N)
{    
    sapply(1:m, function(o) g(floor(N*runif(1))+1))     
}                                                   

m <- 250
Y <- unlist(G(m,N))
smry(log(Y))                  

mean(log(Y))/log(N)   # what is limit of this ratio as N -> infinity?
                      # called Dickman's constant (from 1930)

# FACT: Dickman's constant = Golomb's constant 0.6243299885...

# largest prime factors in random integers ~ largest cycles in random permutations
# (but only loosely since E(f(n))/n for latter and E(log(g(N))/log(N)) for former)
