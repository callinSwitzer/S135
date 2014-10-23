# SIMULATION (more gambling)

#The Game of Craps
#
#Throw two dice until either:
#
#You win (if the 1st throw results in 7 or 11, or if
#the 1st throw is 4,5,6,8,9 or 10 and the same result
#appears before a 7 appears);
#
#You lose (if the 1st throw results in 2,3 or 12, or if
#the 1st throw is 4,5,6,8,9 or 10 and a 7 appears before
#the same result appears).
#
#What is the probability of winning?  What is the mean
#duration of the game?

rnd.rolls <- function(n)     # sequence of n rolls of two dice
{
    S <- sample(6,n,replace=TRUE)+sample(6,n,replace=TRUE)
    S                 
}                     

# sample(6) is same as sample(6,6,replace=FALSE)
sample(6)
sample(6,6,replace=FALSE)
sample(6,6,replace=TRUE)
sample(6,6,replace=TRUE)

# sample(6,n,replace=FALSE) gives an error for n>6...
sample(6,7,replace=FALSE)
# but no such restriction exists for sample(6,n,replace=TRUE)
sample(6,7,replace=TRUE)

rnd.rolls(80)

#x11()
hist(rnd.rolls(1000000),freq=FALSE,breaks=1.5:12.5,border="darkblue",plot=TRUE)
segments(6.5, 6/36, 7.5, 6/36, col = "red") # theoretical value
# triangular distribution for two dice
#graphics.off()   # close plot

sim.craps <- function(SQNC)
{
dur <- 1
if (SQNC[1]==7 | SQNC[1]==11)
  win <- 1
else if (SQNC[1]==2 | SQNC[1]==3 | SQNC[1]==12)
  win <- 0
else     # first roll is (>=4 & <=6) | (>=8 & <=10)
  {
  TMP <- SQNC[2:length(SQNC)]
  rtrn.init <- min(c(which(TMP==SQNC[1]),Inf)) # which function give indecies
  hit.seven <- min(c(which(TMP==7),Inf))
  if (rtrn.init < hit.seven)
     {
     win <- 1
     dur <- 1+rtrn.init
     }
  else
     {
     win <- 0
     dur <- 1+hit.seven
     }
  }
c(win=win,dur=dur)
}

SQNC <- rnd.rolls(80)
SQNC
sim.craps(SQNC)

SQNC <- rnd.rolls(80)
SQNC
sim.craps(SQNC)

SQNC <- rnd.rolls(80)
SQNC
sim.craps(SQNC)

SQNC <- rnd.rolls(80)
SQNC
sim.craps(SQNC)

SQNC <- rnd.rolls(80)
SQNC
sim.craps(SQNC)

SQNC <- rnd.rolls(80)
SQNC
sim.craps(SQNC)

SQNC <- rnd.rolls(80)
SQNC
sim.craps(SQNC)

rnd.trials <- function(m,n)
{    
    t(sapply(1:m, function(o) sim.craps(rnd.rolls(n))))    
}

WD <- rnd.trials(50000,80)
head(WD)
         
t(apply(WD,2,summary))  # theoretical probability of winning
                        # is 244/495 = 0.493

X <- WD[,2][WD[,1]==1]
#summary(X)
dim(X) <- c(length(X),1)
t(apply(X,2,summary))   # assuming a win, what is duration of game?

Y <- WD[,2][WD[,1]==0]
dim(Y) <- c(length(Y),1)
t(apply(Y,2,summary))   # assuming a loss, what is duration of game?

{
#x11()
hist(WD[,2][WD[,2]<=20],freq=FALSE,breaks=0.5:20.5,ylim=c(0,0.5),
     border="darkblue",plot=TRUE)
hx <- hist(X[X<=20],breaks=0.5:20.5,plot=FALSE)$density
for (j in 1:20) lines(c(j-0.4,j+0.4), c(hx[j],hx[j]),col="red",lwd=2)
hy <- hist(Y[Y<=20],breaks=0.5:20.5,plot=FALSE)$density
for (j in 1:20) lines(c(j-0.4,j+0.4), c(hy[j],hy[j]),col="green",lwd=2)
}
#graphics.off()   # close plot

# a while loop approach is clearly possible (homework exercise!)
# the 80 upper limit might be too small (but very rarely)
