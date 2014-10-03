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

colors()              # all built-in colors (657 choices)

CL <- function(o) floor(657*runif(1))+1     # random color
CL()
CL()
CL()
colors()[CL()]
colors()[CL()]
colors()[CL()]

LS <- function(o)     # select two points randomly in the unit square
{
    XY <- matrix(runif(4), ncol=2)
    x11(width=5,height=5.5)    # keep old plot and open a new plotting window
    plot(XY,xlim=0:1,ylim=0:1)
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

graphics.off()   # close all plots altogether

rnd.dist <- function(m)
{
  sapply(1:m, function(o)
           {
             XY <- matrix(runif(4), ncol=2)
             D <- sqrt((XY[2,1]-XY[1,1])^2+(XY[2,2]-XY[1,2])^2)
             D
           })
}

DS <- rnd.dist(10000)
DS
dim(DS) <- c(10000,1)         # regard DS not as a vector, but a column matrix         
t(apply(DS,2,smry))

# the mean is close to the theoretical value:

mn=(sqrt(2)+2+5*log(1+sqrt(2)))/15
mn

# and the standard deviation is likewise close
# to its prediction:

sqrt(1/3 - mn^2)

H <- hist(DS, plot=FALSE)
H$breaks
H$counts
H$density
{
  x11()
  hist(DS, freq=FALSE, border="darkblue", plot=TRUE)
  pdf.l <- function(x) 4*x*(pi/2-2*x+x^2/2)
  pdf.r <- function(x) 4*x*(asin(1/x)-acos(1/x)+2*sqrt(x^2-1)-x^2/2-1)
  curve(pdf.l,from=0,to=1,add=TRUE,col="red",lwd=2)
  curve(pdf.r,from=1,to=sqrt(2),add=TRUE,col="red",lwd=2)
}
# the histogram is close to the theoretical density function
graphics.off()   # close plot

TR <- function(o)     # select three points randomly in the unit square
{
    XY <- matrix(runif(6), ncol=2)
    XY <- rbind(XY, XY[1,])   # duplicates top row at bottom
    x11(width=5,height=5.5)    # keep old plot and open a new plotting window
    plot(XY,xlim=0:1,ylim=0:1)
    lines(XY,col=CL(),lwd=3)
    XY
}

TR()
TR()
TR()

for (j in 1:50)
  {
    Sys.sleep(1.0)   #suspends execution for 1.0 sec
    TR() 
  }

graphics.off()   # close all plots altogether

# Could study the area of random triangles, or perimeter,
# or maximum angles -- look at the latter only

angl <- function(v,w) acos(v%*%w/sqrt((v%*%v)*(w%*%w)))
                 # dot (inner) product denoted by %*%

rnd.mxangl <- function(m)
{
  sapply(1:m, function(o)
           {
             XY <- matrix(runif(6), ncol=2)
             A1 <- angl(XY[2,]-XY[1,],XY[3,]-XY[1,])
             A2 <- angl(XY[3,]-XY[2,],XY[1,]-XY[2,])
             A3 <- angl(XY[1,]-XY[3,],XY[2,]-XY[3,])
             max(A1,A2,A3)
           })
}

AG <- rnd.mxangl(10000)
AG[1:40]         # listing only the first 40 angles (rather than all 10000)
dim(AG) <- c(10000,1)         # regard DS not as a vector, but a column matrix         
t(apply(180*AG/pi,2,smry))    # express statistics using degrees, not radians

x11()
hist(180*AG/pi, freq=FALSE, border="darkblue", plot=TRUE)
# no nice expression for the theoretical density function is known
graphics.off()   # close plot

# the probability that a triangle is obtuse:
length(AG[AG>pi/2])/10000
# is close to the theoretical value:
97/150+pi/40

# this is (again) a remarkable occurrence of the number pi!

# REFERENCES
#
#B. Ghosh, Random distances within a rectangle and between two rectangles,
#Bull. Calcutta Math. Soc. 43 (1951) 17-24.
#
#E. Langford, The probability that a random triangle is obtuse,
#Biometrika 56 (1969) 689-690.
#
#L. A. Santalo, Integral Geometry and Geometric Probability,
#Addison-Wesley, 1976.
