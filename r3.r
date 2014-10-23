CL <- function(o) floor(657*runif(1))+1     # random color

# SIMULATION (more from geometry)

cnvx <- function(n)           # convex hull of n points in the unit square
{
  XY <- matrix(runif(2*n), ncol=2)
  #x11(width=5,height=5.5)    # keep old plot and open a new plotting window
  plot(XY,xlim=0:1,ylim=0:1,pch=22)
  h <- chull(XY)
  h <- c(h, h[1])
  lines(XY[h, ],col=CL(),lwd=3)
}

cnvx(12)
cnvx(12)
cnvx(12)

for (j in 1:50)
  {
    Sys.sleep(1.0)   #suspends execution for 1.0 sec
    cnvx(12) 
  }

#graphics.off()   # close all plots altogether

{                # what information does h contain?
XY <- rbind(c(0.1,0.2),c(0.3,0.4),c(0.5,1.0),c(0.6,0.7),c(0.9,0))
#x11(width=5,height=5.5)    # keep old plot and open a new plotting window
plot(XY,xlim=0:1,ylim=0:1,pch=22)
h <- chull(XY)
h.old <- h
h <- c(h, h[1])
h.new <- h
lines(XY[h, ],col=CL(),lwd=3)
}
h.old            # merely the vertices of the convex hull
h.new            # (as indexed in the original point set XY)
#graphics.off()   # close plot

# Select four points randomly in the square.  With probability 1,
# no three of the points are collinear, so the convex hull of the four
# points is either a triangle (one point "inside" the others) or a
# quadrilateral.  Estimate the probability that the convex hull is a
# triangle.
set.seed(12345)
cnvx(4)
cnvx(4)
cnvx(4) 

for (j in 1:50)
  {
    Sys.sleep(1.0)   #suspends execution for 1.0 sec
    cnvx(4) 
  }

#graphics.off()   # close all plots altogether

rnd.npts <- function(m,n)
{
  sapply(1:m, function(o)
           {
             XY <- matrix(runif(2*n), ncol=2)
             h <- chull(XY)
             length(h)
           })
}

m=10000
P <- rnd.npts(m,4)          # convex hulls for 4-point sets, repeated m times
P[1:10]
dim(P) <- c(10000,1)        # regard P not as a vector, but a column matrix 

# the probability that the convex hull is a triangle:
length(P[P==3])/10000
# is close to the theoretical value:
11/36        

# REFERENCES
#
#R. E. Pfiefer, The historical development of J. J. Sylvester's 
#four point problem, Math. Mag. 62 (1989) 309-317.
#
#L. A. Santalo, Integral Geometry and Geometric Probability,
#Addison-Wesley, 1976.
