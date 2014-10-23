smry <- function(X){	         # more detailed than built-in 'summary' function
        mu <- mean(X)
        sigma <- sd(X)
        a <- min(X)
        q25 <- quantile(X,0.25)
        q50 <- quantile(X,0.50)
        q75 <- quantile(X,0.75)
        b <- max(X)
        c(mu=mu,sigma=sigma,min=a,q25,q50,q75,max=b)}

CL <- function(o) floor(657*runif(1))+1     # random color

LS <- function(o)     # select two points randomly in the unit disk
{
    R <- matrix(runif(2), ncol=1)
    Theta <- matrix(runif(2,0,2*pi), ncol=1)
    XY <- cbind(sqrt(R)*cos(Theta),sqrt(R)*sin(Theta))
    x11(width=5,height=5.5)    # keep old plot and open a new plotting window
    plot(XY,xlim=c(-1,1),ylim=c(-1,1))
    lines(XY,col=CL(),lwd=3)
    upp.crcl <- function(x) sqrt(1-x^2)
    low.crcl <- function(x) -sqrt(1-x^2)
    curve(upp.crcl,from=-1,to=1,add=T)
    curve(low.crcl,from=-1,to=1,add=T)
    XY
}

for (j in 1:50)
  {
    Sys.sleep(1.0)   #suspends execution for 1.0 sec
    LS() 
  }

graphics.off()   # close all plots altogether


rnd.dist <- function(m)
{
  sapply(1:m, function(o)
           {
             	R <- matrix(runif(2), ncol=1)
  		Theta <- matrix(runif(2,0,2*pi), ncol=1)
  		XY <- cbind(sqrt(R)*cos(Theta),sqrt(R)*sin(Theta))
             	D <- sqrt((XY[2,1]-XY[1,1])^2+(XY[2,2]-XY[1,2])^2)
             	D
           })
}

DS <- rnd.dist(10000)
dim(DS) <- c(10000,1)         # regard DS not as a vector, but a column matrix         
t(apply(DS,2,smry))
128/(45*pi)

{
hist(DS, freq=FALSE, border="darkblue", plot=TRUE)
pdf <- function(x) (4*x/pi)*acos(x/2)-(x^2/pi)*sqrt(4-x^2)
curve(pdf,from=0,to=2,add=TRUE,col="red",lwd=2)
}
# the histogram is close to the theoretical density function
graphics.off()   # close plot

