## apply, lapply, sapply & replicate

X <- function(m,n)       # example from lecture r1.R on n-permutations
{    
    t(sapply(1:m, function(o) sample(n))) # o is only a dummy variable
}                                             # t means 'transpose'
X(7,10)                                       # output in matrix form

X <- function(m,n)
{    
    lapply(1:m, function(o) sample(n))  # output in list form instead
}                           # 'l' stands for list; 's' for simplified
X(7,10)

X <- function(m,n)
{                                 # the original 'apply' is inconvenient here
    mtrx <- matrix(1:m, c(m,1))   # must redimension vector as column matrix
    t(apply(mtrx, 1, function(o) sample(n)))  # 1 means 'apply over rows'      
}                                             # t means 'transpose'
X(7,10)                                       # output in matrix form

X <- function(m,n)
{                                    # alternatively redimension vector as
    mtrx <- matrix(1:m, c(1,m))              # row matrix
    t(apply(mtrx, 2, function(o) sample(n))) # 2 means 'apply over columns'   
}                                            # still need 'transpose', however
X(7,10)                                       

                                             # most concise is the following
X <- function(m,n) t(replicate(m,sample(n))) # 'replicate' is a wrapper for 
X(7,10)                                      # such common uses of 'sapply'

rep(1,7)     # potentially very confusing: 'rep' is not the same as 'replicate'
rep(c(2,3,5,6),7)
t(matrix(rep(c(2,3,5,6),7),c(4,7)))
t(matrix(rep(sample(10),7),c(10,7)))  # only 1 random permutation here, not 7

Y <- function(k)         # another example
{    
  sapply(1:k, function(o) median(rexp(12,1))) # o is a dummy variable
}                                             # no 'transpose' this time
Y(7)                                          # output in vector form

Y <- function(k)         
{    
  lapply(1:k, function(o) median(rexp(12,1))) # output in list form instead
}        
Y(7)

Y <- function(k) replicate(k,median(rexp(12,1))) # 'replicate' is shortest 
Y(7)                                             # of all

## tapply, by and aggregate

cars0 <- read.table(
"C:/Documents and Settings/sfinch/Desktop/93cars.txt",
na.strings="*",header=T)

cars <- subset(cars0,select = c(manu,type,price,ctympg,hwympg))

attach(cars)        # see lecture r13.R

summary(cars)

tapply(price,type,summary)  # price of car, stratified by car type
                            # 't' stands for table

by(price,type,summary)      # 'by' is essentially the same as 'tapply'
                            # from r-longitudinal.R

by(price,list(TYPE=type),summary)      # changing label 'type' to 'TYPE'

aggregate(price,list(Type=type),mean)       # 'aggregate' is similar; needs
                                            # a list for 2nd argument and a
                                            # scalar function for 3rd argument
                            # would prefer x replaced by mean (bug in R?)  

m <- aggregate(price,list(Type=type),mean)
names(m)
names(m) <- c("Type","Mean")
names(m)
m

s <- aggregate(price,list(Type=type),sd)$x        # add in standard deviation
s
n <- aggregate(price,list(Type=type),length)$x    # add in sample size
n
msn <- cbind(m,s,n)
names(msn) <- c("Type","Mu","Sigma","N")
msn

aggregate(price,list(Manu=manu,Type=type),mean)   # two class variables (rather
                                                  # than just one)
# will return to the cars dataframe at end

## mapply and Vectorize

# recall two examples from vctrze.R

ABS <- function(x) {            # naive definition
       if (x<0) x <- -x
       x}

ABS(c(3,-3))

FACTORIAL <- function(n)        # naive definition
  {
    P <- 1
    for (k in 1:n) P<-P*k
    P
  }

FACTORIAL(1:9)

ABS.VEC <- Vectorize(ABS) 
ABS.VEC(c(3,-3))
      
FACT.VEC <- Vectorize(FACTORIAL)
FACT.VEC(1:9)

# in fact, Vectorize is a wrapper for mapply...

mapply(ABS,c(3,-3))

mapply(FACTORIAL,1:9)

# ... and mapply is a multivariate version of sapply

t(mapply(function(o) sample(10), 1:7))

# note that FUN comes first in mapply, ARG comes second
# it's the other way around in sapply!

mapply(function(o) median(rexp(12,1)), 1:7)

# can have multiple ARGs
# mapply applies FUN to first elements of each AGR, then
# to second elements, then to third elements, ...

list(rep(5,4), rep(6,3), rep(7,2), rep(8,1))   # tedious

mapply(rep, 5:8, 4:1)   # 5 & 4 first, 6 & 3 second, ...

mapply(rep, 5:9, 4:1)   # argument elements are recycled
                        # if necessary, with warning

noise <- function(mu,sigma) max(rnorm(1000,mu,sigma))
mapply(noise, 0:3, 1:2)   # no warning since 2 divides 4

B <- function(x,y) 100*(y-x^2)^2+(1-x)^2  # banana function
mapply(B, -2:2, -2:2)

# if B-values at *all* integer points in the 4x4 square are
# needed, use instead the outer product

outer(-2:2, -2:2, B) 

## writing HTML

install.packages("R2HTML")
library(R2HTML)

S <- summary(cars)

HTML(as.title("Summary of Cars Data"),
     file="C:/Documents and Settings/sfinch/Desktop/cars.html")
HTML(S,"C:/Documents and Settings/sfinch/Desktop/cars.html",
     innerBorder=1)

detach(cars)

## 25 random binary trees with 2N+1 vertices (N+1 leaves) 
## via what is called Remy's algorithm

# Error in parse(text = cmd) : contextstack overflow
# Often OK for N=200, but not for N=300

source("C:/Documents and Settings/sfinch/Desktop/remy.R")   # include as subroutine

N <- 100              # number of inner vertices

for (j in 1:10)
  {
    Sys.sleep(1.0)   #suspends execution for 1.0 sec
    W <- TRvctr(N)
    STlist(TRlist(W),1)   # using nested lists
  }
graphics.off()   # close all plots altogether






