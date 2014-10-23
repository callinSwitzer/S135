smry <- function(X){	         # more detailed than built-in 'summary' function
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

# 4.  Select two points randomly in the unit square.  Draw a line
#     segment connecting the two points.  Now select two more points 
#     randomly in the unit square.  Draw a new line segment connecting 
#     the two new points.  Estimate the probability that the two line
#     segments cross (that is, have a point in common).

# See the section entitled "Intersection point of two lines
# in 2 dimensions" midway through
# http://paulbourke.net/geometry/pointlineplane/
# for discussion of geometry.  I use Paul Bourke's notation.
# See also
# http://www.mathisfunforum.com/viewtopic.php?id=8541
# for alternative approach (application to video games!)

LS <- function(o)     # select four points randomly in the unit square
{
    XY <- matrix(runif(8), ncol=2)
    X <- XY[,1]
    Y <- XY[,2]
    den <- (Y[4]-Y[3])*(X[2]-X[1])-(X[4]-X[3])*(Y[2]-Y[1])
    ua <- ((X[4]-X[3])*(Y[1]-Y[3])-(Y[4]-Y[3])*(X[1]-X[3]))/den
    ub <- ((X[2]-X[1])*(Y[1]-Y[3])-(Y[2]-Y[1])*(X[1]-X[3]))/den
    x11(width=5,height=5.5)    # keep old plot and open a new plotting window
    plot(XY,xlim=0:1,ylim=0:1)
    lines(XY[1:2,],col=CL(),lwd=3)
    lines(XY[3:4,],col=CL(),lwd=3)
    p <- X[1]+ua*(X[2]-X[1])
    q <- Y[1]+ua*(Y[2]-Y[1])
    points(p, q, pch=5)
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

rnd.intrsctn <- function(m)
{
  sapply(1:m, function(o)
           {
             XY <- matrix(runif(8), ncol=2)
             X <- XY[,1]
             Y <- XY[,2]
             den <- (Y[4]-Y[3])*(X[2]-X[1])-(X[4]-X[3])*(Y[2]-Y[1])
             ua <- ((X[4]-X[3])*(Y[1]-Y[3])-(Y[4]-Y[3])*(X[1]-X[3]))/den
             ub <- ((X[2]-X[1])*(Y[1]-Y[3])-(Y[2]-Y[1])*(X[1]-X[3]))/den
             (0<ua)*(ua<1)*(0<ub)*(ub<1)   # is 1 if line segments intersect;
           })                              # otherwise 0
}

sum(rnd.intrsctn(10000))/10000
25/108

