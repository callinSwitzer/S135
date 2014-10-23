smry <- function(X){             # more detailed than built-in 'summary' function
        mu <- mean(X)
        sigma <- sd(X)
        a <- min(X)
        q25 <- quantile(X,0.25)
        q50 <- quantile(X,0.50)
        q75 <- quantile(X,0.75)
        b <- max(X)
        c(mu=mu,sigma=sigma,min=a,q25,q50,q75,max=b)}

cc <- smry(c(1,2,4,4,5,5,5,6,6,7,7,7,7,7))
mode(cc)
# SIMULATION (gambling)

#Suppose you want to play a game that has the following rules:
#
#1. Each play of the game involves repeatedly flipping an unbiased coin
#until the absolute difference between the number of heads tossed and the
#number of tails tossed is 3.
#
#2. If you decide to play the game, you are required to pay $1 for each
#flip of the coin. You are not allowed to quit during a play of the game.
#
#3. You receive $8 at the end of each play of the game.
#
#Thus you win money if the number of flips required is fewer than 8, but you
#lose money if more than 8 flips are required. What is the expected payoff
#(or loss) when playing this game?

n <- 120
x <- rbinom(n,1,0.5)  # n coin tosses (Bernoulli trials)
x
cumsum(x)             # running count of the number of 1s
(1:n)-cumsum(x)         # running count of the number of 0s
abs(1:n-2*cumsum(x))  # absolute difference between these -- we want R to find the threes
x

d <- abs(1:n-2*cumsum(x))
which(d==3)                 # indices corresponding to 3s (possibly none)
min(c(which(d==3),Inf))     # first such index (if none, then infinite)

rnd.trials <- function(m,n)
{    
    sapply(1:m, function(o)
             {
               D <- abs(1:n-2*cumsum(rbinom(n,1,0.5)))
               min(c(which(D==3),Inf))
             })     
}                    

time.start <- proc.time( )
TM <- rnd.trials(50000,120)
time.used <- proc.time( ) - time.start 
cat('User time elapsed:', time.used[1], '\n')

# another timing method
system.time({
     foo <- rnd.trials(50000, 120)
})

TM
smry(TM)                      # ASIDE: TM is simply a vector, hence
                              # smry can be easily applied...

dim(TM) <- c(50000,1)         # ... but I prefer the brevity of the         
t(apply(TM,2,smry))           # following layout (my opinion only)

# the mean time is close to the theoretical value 9
# expected payoff is -1, i.e., you lose $1 on average

#x11()
hist(TM[TM<=50], freq=FALSE, breaks=2.5:50.5, border="darkblue", plot=TRUE)
#graphics.off()   # close plot

# what about using a while loop instead?
# wouldn't this be vastly more efficient?
# if newly generated number is 1, add 1 to running signed total s;
# if 0, add -1 instead; record first time that abs(s)=3

s <- 0
k <- 0
x <- NULL            # clearing x of its previous vector value
while(abs(s)<3)
{
   y <- rbinom(1,1,0.5)
   s <- s+(2*y-1)
   k <- k+1
   x[k] <- y
}
k
x

rnd.TRIALS <- function(m)
{    
    sapply(1:m, function(o)
             {
               s <- 0
               k <- 0
               while(abs(s)<3)
               {
                   s <- s+(2*rbinom(1,1,0.5)-1)
                   k <- k+1
               }
               k
             })     
}                 

time.start <- proc.time( )
TM <- rnd.TRIALS(50000)
time.used <- proc.time( ) - time.start 
cat('User time elapsed:', time.used[1], '\n')

dim(TM) <- c(50000,1)         # regard TM not as a vector, but a column matrix         
t(apply(TM,2,smry))

# no surprising difference between the two methods
# the first method is more concise than the second
# the 120 upper limit might be too small (but very rarely)
# more programmers would probably use the while loop approach


# REFERENCE
#
# S. E. Alm, Simple random walk, 
# http://www.math.uu.se/~sea/kurser/stokprocmn1/slumpvandring_eng.pdf 
