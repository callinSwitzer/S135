smry <- function(X){             # more detailed than built-in 'summary' function
        mu <- mean(X)
        sigma <- sd(X)
        a <- min(X)
        q25 <- quantile(X,0.25)
        q50 <- quantile(X,0.50)
        q75 <- quantile(X,0.75)
        b <- max(X)
        c(mu=mu,sigma=sigma,min=a,q25,q50,q75,max=b)}

# SIMULATION (from geometry)

CL <- function(o) floor(657*runif(1))+1     # random color

# 1.  Select two points randomly in a rectangle of length=2 and width=1.
#     Compute summary statistics & histogram as in class.
#     In particular, what is the (estimated) mean distance between the points?

# must understand precisely which entries in the 2X2 matrix are xs & ys,
# as well as the more general runif(n,a,b) random number generator

matrix(c(c(6,7), c(8,9)), nrow=2, ncol=2)    # first column filled first, then second column

LS <- function(o)     # select two points randomly in the unit square
{
    XY <- matrix(c(runif(2,0,2),runif(2,0,1)), nrow=2, ncol=2)  # two xs, then two ys
    #x11(width=9,height=5.5)    # keep old plot and open a new plotting window
    plot(XY,xlim=c(0,2),ylim=0:1)     # 0:2 doesn't work since it gives c(0,1,2); want c(0,2) instead
    lines(XY,col=CL(),lwd=3)
    XY
}

LS()
LS()
LS()

for (j in 1:50)
  {
    Sys.sleep(1.0)   #suspends execution for 1.0 sec
    LS() 
  }

#graphics.off()   # close all plots altogether

rnd.dist <- function(m)
{
  sapply(1:m, function(o)
           {
             XY <- matrix(c(runif(2,0,2),runif(2,0,1)), nrow=2, ncol=2)  # two xs, then two ys
             D <- sqrt((XY[2,1]-XY[1,1])^2+(XY[2,2]-XY[1,2])^2)
             D
           })
}

DS <- rnd.dist(10000)
DS
dim(DS) <- c(10000,1)         # regard DS not as a vector, but a column matrix         
t(apply(DS,2,smry))

# expressions for the theoretical mean & standard deviation exist,
# as well as for the theoretical density function (omitted)

# our call to the 'matrix' function is the same as:

matrix(c(c(6,7), c(8,9)), nrow=2, ncol=2, byrow=FALSE)  # first column filled first, then second column

# the following gives the transpose of the above:

matrix(c(c(6,7), c(8,9)), nrow=2, ncol=2, byrow=TRUE)   # first row filled first, then second row

