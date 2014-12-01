# Distribution of the sample median
# (middle order statistic, assuming sample size n is odd)

# preliminaries:

q05 <- function(X,...) quantile(X,probs=0.05,na.rm=TRUE,names=F)
q95 <- function(X,...) quantile(X,probs=0.95,na.rm=TRUE,names=F)

w <- function(n,j)  # weighting function (interpretation forthcoming)
{
  m<-(n-1)/2
  pbinom(m,n,(j-1)/n)-pbinom(m,n,j/n)
}

W <- function(n)      # weighting vector (symmetric about middle)
{
  m<-(n-1)/2
  v <- NULL
  for (j in 1:(m+1)) v <- c(v,w(n,j))
  for (j in m:1) v <- c(v,w(n,j))
  v
}

W(7)         # Maritz & Jarrett, Table 1

# dataset:

X <- c(0.3,0.4,0.5,0.5,0.6,0.9,1.7)     # cell lifetimes
n <- length(X)                # sample size
n

{
#x11()
hist(X, freq=F, breaks=seq(0.05,1.95,0.1),xlim=c(0,2),ylim=c(0,3),
     main="Histogram/density plots for cell lifetime data",
     xlab="absolute differences of sister cell lifetimes",
     ylab="density")  
}                     # keep graph for later reference   

M <- median(X)                # sample median
M

# How good of an estimate of the true median is this?
# What is its bias?  What is its uncertainty?

MEAN <- X%*%W(n)                        # theoretical mean of M
MEAN
SIGMA <- sqrt(X^2%*%W(n)-(X%*%W(n))^2)  # theoretical stddev of M
SIGMA

# How to verify these sampling results?  Use bootstrapping!

# Fundamental idea due to Efron:
#                     the population is to the sample
#                                   as
#                 the sample is to the bootstrap samples!

# Fact: w(n,j) is theoretical probability that
# bootstrap median=jth order statistic of original sample

# bootstrap replicates of X:

sample(X,replace=TRUE)    # replace=TRUE is imperative
sample(X,replace=TRUE)    # (replace=FALSE only permutes X)
sample(X,replace=TRUE)

# which effectively are same as

X[sample(NROW(X),replace=TRUE)]
X[sample(NROW(X),replace=TRUE)]
X[sample(NROW(X),replace=TRUE)]

# (the former suffices for X; we'll need to
# use latter for dataframes with >=2 columns)

f <- function(m)            # bootstrap using 'sample' function
{    
  sapply(1:m, function(o) median(sample(X,replace=TRUE))) 
}              

time.start <- proc.time( )
Y <- f(50000)
time.used <- proc.time( ) - time.start 
cat('User time elapsed:', time.used[1], '\n')


#install.packages("MASS")
library(MASS)               # user-contributed library
{
#x11()
truehist(Y, h=0.1, x0=0.05, # alternative plotting tool (in MASS)
     main="Histogram of bootstrap distribution for median",
     xlab="absolute differences of sister cell lifetimes",
     ylab="density",col="white",border="darkblue")
     lines(c(0.26,0.34), c(10*w(n,1),10*w(n,1)),  col="red")
     lines(c(0.36,0.44), c(10*w(n,2),10*w(n,2)),  col="red")
     lines(c(0.46,0.54), c(10*sum(w(n,3:4)),10*sum(w(n,3:4))), col="red")
     lines(c(0.56,0.64), c(10*w(n,5),10*w(n,5)),  col="red")
     lines(c(0.86,0.94), c(10*w(n,6),10*w(n,6)),  col="red")
     lines(c(1.66,1.74), c(10*w(n,7),10*w(n,7)),  col="red")
}
graphics.off()   # close plots

c(MEAN,mean(Y))
c(SIGMA,sd(Y))

# several different definitions of 90% confidence intervals:

c(2*M-MEAN-1.645*SIGMA,2*M-MEAN+1.645*SIGMA)
# called "bias-corrected CI using normal approximation"

c(q05(Y),q95(Y))
# called "CI using percentile bootstrap method"

c(2*M-q95(Y),2*M-q05(Y))
# called "CI using basic bootstrap method"


# there is considerable controversy concerning use 
# of bootstrap confidence intervals

# asymptotically, you can do better than the three types
# of CIs mentioned here (normal, percent & basic),
# but for small samples, things are more complicated

# "bias-corrected accelerated percentile" and "studentized"
# methods are available in the following package:

install.packages("boot")
library(boot)        # user-contributed library

# more execution required: see r14.R notes for more

# another dataset appears in r15.R: velocities of galaxies 

#REFERENCES
#
#J.S. Maritz & R.G. Jarrett, A note on estimating the variance of the
#sample median, J. Amer. Statist. Assoc. 73 (1978) 194-196.
#
#B. Efron, Bootstrap methods: another look at the jackknife, Annals of
#Statist. 7 (1979) 1-26.
