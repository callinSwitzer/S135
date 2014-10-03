# Let's return to the fixed-point generating algorithm from r1.R 

fixed.points.fast <- function(m,n)
{    
    sapply(1:m, function(o)
             {
               Z <- sample(n)-1:n
               length(Z[Z==0])
             })     
}             

time.start <- proc.time( )
FP.fast <- fixed.points.fast(100000,10)
time.used <- proc.time( ) - time.start 
cat('User time elapsed:', time.used[1], '\n')

# here is the same, using "for" loops (as in C) rather
# than the "whole-object" approach I prefer for R

fixed.points.slow <- function(m,n)
{
    FP <- rep(0,m)   # gives a zero vector of length m
    for (j in 1:m)
	{
	    W <- sample(n)
            k <- 0
            for (i in 1:n)
	        {
                   if (W[i]==i) k <- k+1
                }
            FP[j] <- k
        }
     FP                   
}             

time.start <- proc.time( )
FP.slow <- fixed.points.slow(100000,10)
time.used <- proc.time( ) - time.start 
cat('User time elapsed:', time.used[1], '\n')

# the fast approach takes only 60% as long as the slow approach
