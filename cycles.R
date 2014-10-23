# Homework 1: Counting 2-cycles of a permutation

X <- c(3,5,7,4,9,6,8,10,2,1)
X[X]
X[X]-1:10        # no 2-cycles (the two 0s correspond to the 1-cycles
                 # that we saw last time)
Y <- X-1:10
Z <- X[X]-1:10
(length(Z[Z==0])-length(Y[Y==0]))/2

X <- c(2,1,4,3,6,5,8,7,10,9) # 5 two-cycles
X[X]
X[X]-1:10        # all 0s, of course
Y <- X-1:10
Z <- X[X]-1:10
(length(Z[Z==0])-length(Y[Y==0]))/2

two.cycles <- function(m,n)
{    
    sapply(1:m, function(o)
             {
               X <- sample(n)
               Y <- X-1:n
               Z <- X[X]-1:n
               (length(Z[Z==0])-length(Y[Y==0]))/2
             })     
}                                             

smry <- function(X){ # more detailed than built-in 'summary' function
        mu <- mean(X)
        sigma <- sd(X)
        a <- min(X)
        q25 <- quantile(X,0.25)
        q50 <- quantile(X,0.50)
        q75 <- quantile(X,0.75)
        b <- max(X)
        c(mu=mu,sigma=sigma,min=a,q25,q50,q75,max=b)}

CY <- two.cycles(2500,10)
CY
dim(CY) <- c(2500,1) # regard CY not as a vector, but a column matrix         
t(apply(CY,2,smry))  # smry is evaluated over each column of CY -
                     # only one column here - 2 means columnwise         
H <- hist(CY, br=-0.5:7.5, plot=F)
H$counts
H$density

# the overall distribution resembles Poisson(1/2):

sapply(0:7, function(k) dpois(k,1/2))

{
#x11()
hist(CY, br=-0.5:7.5, freq=FALSE, border="darkblue", ylim=c(0,0.7), plot=TRUE)
                     # br means 'breaks' (cell boundaries)
                     # freq=F ensures plot uses densities, not counts
                     # ylim determines lower & upper limits on y-axis
for (j in 0:7) lines(c(j-0.4,j+0.4), c(dpois(j,1/2),dpois(j,1/2)), col="red", lwd=2)
                     # plot eight red line segements separately
                     # 'for' loop used - later
}   # blue is empirical data, red is theoretical fit

#graphics.off()   # close plot

# a random nonnegative integer K is Poisson(c) distributed if
# P(K=k) = c^k*exp(-c)/k!, where c>0 is constant
# it can be proved that E(K)=c=Var(K)

# let's check this for increasing permutation size n

prblty <- function(m,n)
{  
  CY <- two.cycles(m,n)
  dim(CY) <- c(m,1)
  #x11()     # keep old plot and open a new plotting window
  hist(CY, br=-0.5:7.5, freq=FALSE, border="darkblue", ylim=c(0,0.7), plot=TRUE)
  for (j in 0:7) lines(c(j-0.4,j+0.4), c(dpois(j,1/2),dpois(j,1/2)), col="red", lwd=2)
}

prblty(10000,5)
prblty(10000,7)
prblty(10000,9)

#graphics.off()   # close all plots altogether

# the Poisson(1/2) approximation improves as n increases
# and is asymptotically true as n approaches infinity

# how about counting 3-cycles?

# let's now look at a different approach 

TWO.CYCLES <- function(m,n)         # a student's code
{
  H<-rep(0,n+1)   # creates a vector of 0s (bins for 0,1,...,n counts)
  for (j in 1:m) {
    x<-sample(n)
    cnt<-0
    for (i in 1:n) {
      if (i!=x[i] & i==x[x[i]]) cnt<-cnt+1
    }
    cnt<-cnt/2+1
    H[cnt]<-H[cnt]+1
  }
  H
}

two.cycles <- function(m,n)         # my code
{    
    cnt <- sapply(1:m, function(o)
             {
               X <- sample(n) 
               Y <- X-1:n
               Z <- X[X]-1:n
               (length(Z[Z==0])-length(Y[Y==0]))/2
             })
    H <- hist(cnt, br=-0.5:(n+0.5), plot=F)$counts
    H
}                                             

time.start <- proc.time( )
TWO.CYCLES(50000,10)
time.used <- proc.time( ) - time.start 
cat('User time elapsed:', time.used[1], '\n')

time.start <- proc.time( )
two.cycles(50000,10)
time.used <- proc.time( ) - time.start
cat('User time elapsed:', time.used[1], '\n')

# my code is ~2.5 times faster than the student's code

# WARNING (quote from Venables & Smith, sec. 9.2.2):
# "for loops are used in R code much less often
# than in compiled languages.  Code that takes
# a 'whole object' view is likely to be both
# clearer and faster in R."

# Venables & Smith, An Introduction to R, ver. 2.3.1
# http://cran.r-project.org/manuals.html

# Another thing: remember my code for generating permutations?

P <- function(m,n)
{    
    t(sapply(1:m, function(o) sample(n))) # o is only a dummy variable
}                                         # t means 'transpose'
P(7,10)

# A student wrote different code for the same, based on recursion:

Q <- function(m,n)
{
    if (m==0) {
    }
    else {
        Q(m-1, n)
        print(sample(n))
    }
}
Q(7,10)

# Note the curious output: all rows begin with [1]

q<-Q(7,10)
q
# The 1st 6 rows are missing; only the 7th is mapped to q

# I don't see any way of recovering the 1st 6 rows (except
# via manual copy/paste)

time.start <- proc.time( )
p <- P(998,10)
time.used <- proc.time( ) - time.start 
cat('User time elapsed:', time.used[1], '\n')

time.start <- proc.time( )
q <- Q(998,10)
time.used <- proc.time( ) - time.start 
cat('User time elapsed:', time.used[1], '\n')

P(1000,10)   # succeeds

Q(1000,10)   # fails

# Actually, the above recursion isn't much different
# from the below iteration

R <- function(m,n)
{
    for(j in 1:m) print(sample(n))
}
R(7,10)

r<-R(7,10)
r

# Here, no rows are mapped to r at all, so it's worthless!

# If you _insist_ on using "for" loops, do like this...

S <- function(m,n)
{
    M <- matrix(0,nrow=m,ncol=n)    # initializes a zero mxn matrix
    for(j in 1:m) M[j,] <- sample(n)
    M
}
S(7,10)

# ... _not_ like this

T <- function(m,n)
{
    M <- NULL          # initializes an empty matrix
    for(j in 1:m) M <- rbind(M,sample(n))
    M
}
T(7,10)

# Using rbind to build a large matrix isn't recommended
# (it's better to fill the rows of an existing zero matrix
# rather than stacking vectors one-by-one)

time.start <- proc.time( )
s <- S(10000,10)
time.used <- proc.time( ) - time.start 
cat('User time elapsed:', time.used[1], '\n')

time.start <- proc.time( )
t <- T(10000,10)
time.used <- proc.time( ) - time.start 
cat('User time elapsed:', time.used[1], '\n')

# Filling an existing matrix is much more efficient than
# using rbind and/or cbind to construct one incrementally
