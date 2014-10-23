smry <- function(XY){	         # more detailed than built-in 'summary' function
        mu <- mean(XY)
        sigma <- sd(XY)
        a <- min(XY)
        q25 <- quantile(XY,0.25)
        q50 <- quantile(XY,0.50)
        q75 <- quantile(XY,0.75)
        b <- max(XY)
        c(mu=mu,sigma=sigma,min=a,q25,q50,q75,max=b)}

# SIMULATION (from geometry)

CL <- function(o) floor(657*runif(1))+1     # random color

# 9.  Do the same as in the previous problem, except select the four points
#     randomly in an isosceles right triangular region.

# Without loss of generality, assume the points are in the triangle bounded
# by the lines y=x, x=0 & y=1, i.e., each y-coordinate > corresp x-coordinate.

# One student's code involved a "for" loop:

cnvx <- function(n)          
{
  XY <- matrix(runif(n*2), ncol=2) 
  tmp <- 0
  for (i in 1:n) {
       if (XY[i,1] > XY[i,2]) {
           tmp <- XY[i,1]
           XY[i,1] <- XY[i,2]
           XY[i,2] <- tmp
       }
   }
  XY
}

cnvx(5)

# which does exactly what the "sort" function does for each row:
 
CNVX <- function(n)          
{
  XY <- matrix(runif(n*2), ncol=2)
  t(apply(XY,1,sort)) 
}            

CNVX(5)

LS <- function(o)         # select four points randomly in isosceles right triangle
{
  XY <- CNVX(4)
  x11(width=5,height=5.5)    # keep old plot and open a new plotting window
  plot(XY,xlim=0:1,ylim=0:1,pch=22)
  h <- chull(XY)
  h <- c(h, h[1])
  lines(XY[h, ],col=CL(),lwd=3)
  lines(matrix(c(0,1,0,1),nrow=2,ncol=2),col="black",lwd=1)
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

rnd.P <- function(m)      # convex hull of 4 points in isosceles right triangle
{
  t(sapply(1:m, function (o)   
       {
  	XY <- CNVX(4)
  	h <- chull(XY)
  	P <- length(h)
	P
       }))
}

sum(rnd.P(10000)==3)/10000
1/3
