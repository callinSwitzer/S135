CL <- function(o) floor(657*runif(1))+1     # random color

rdisk <- function(q)         # how to generate random points in the unit disk?
{
  R <- matrix(runif(10000), ncol=1)
  Theta <- matrix(runif(10000,0,2*pi), ncol=1)
  XY <- cbind(R^q*cos(Theta),R^q*sin(Theta))
  #x11(width=5,height=5.5)    # keep old plot and open a new plotting window
  plot(XY,xlim=c(-1,1),ylim=c(-1,1),pch=".")
  upp.crcl <- function(x) sqrt(1-x^2)
  low.crcl <- function(x) -sqrt(1-x^2)
  curve(upp.crcl,from=-1,to=1,add=T)
  curve(low.crcl,from=-1,to=1,add=T)
}

rdisk(1)         # too much weight at center
rdisk(1/2)       # uniformly distributed (can be proved via calculus)
rdisk(1/9)       # too much weight at boundary
#graphics.off()   # close all plots altogether

# SIMULATION (more from geometry)

cnvx <- function(n)           # convex hull of n points in the unit disk (radius=1)
{
  R <- matrix(runif(n), ncol=1)
  Theta <- matrix(runif(n,0,2*pi), ncol=1)
  XY <- cbind(sqrt(R)*cos(Theta),sqrt(R)*sin(Theta))
  #x11(width=5,height=5.5)    # keep old plot and open a new plotting window
  plot(XY,xlim=c(-1,1),ylim=c(-1,1),pch=22)
  h <- chull(XY)
  h <- c(h, h[1])
  lines(XY[h, ],col='red',lwd=3)
  upp.crcl <- function(x) sqrt(1-x^2)
  low.crcl <- function(x) -sqrt(1-x^2)
  curve(upp.crcl,from=-1,to=1,add=T)
  curve(low.crcl,from=-1,to=1,add=T)
}

cnvx(4)
cnvx(12)
cnvx(12)

for (j in 1:50)
  {
    Sys.sleep(1.0)   #suspends execution for 1.0 sec
    cnvx(12) 
  }

# Fact: for the unit square, the number of convex hull vertices is ~(8/3)*log(n),
# while for the unit disk, the number of vertices is ~C*n^(1/3) for some constant C
# (asymptotic results as n approaches infinity)

#graphics.off()   # close all plots altogether

# As in r2.R, we can ask questions about the distance between two random points,
# or about the area/perimeter/maximum angle of random triangles

# As in r3.R, we can solve Sylvester's four point problem for the disk

# Here, we solve yet another problem. Define a "random chord" in the
# disk to be the line segment connecting two random points that are
# independent & uniformly distributed on the circumference of the circle.
# What is the distribution of the chordal length?

chrd1 <- function()            # first definition of random chord
{
  Theta <- matrix(runif(2,0,2*pi), ncol=1)   # note that R=1 for all points
  XY <- cbind(cos(Theta),sin(Theta))
  #x11(width=5,height=5.5)    # keep old plot and open a new plotting window
  plot(XY,xlim=c(-1,1),ylim=c(-1,1),pch=22)
  lines(XY[1:2, ],col=CL(),lwd=3)
  upp.crcl <- function(x) sqrt(1-x^2)
  low.crcl <- function(x) -sqrt(1-x^2)
  curve(upp.crcl,from=-1,to=1,add=T)
  curve(low.crcl,from=-1,to=1,add=T)
}

chrd1()
chrd1()
chrd1() 

for (j in 1:25)
  {
    Sys.sleep(1.0)   #suspends execution for 1.0 sec
    chrd1() 
  }

rnd.chrd1 <- function(m)
{
  sapply(1:m, function (o)
           {
             Theta <- matrix(runif(2,0,2*pi), ncol=1)
             XY <- cbind(cos(Theta),sin(Theta))
             U <- XY[2,]-XY[1,]             
             sqrt(U%*%U)                            
           })
}

DS <- rnd.chrd1(10000)
DS[1:30]
dim(DS) <- c(10000,1)         # regard DS not as a vector, but a column matrix         

{
#x11()
hist(DS, freq=FALSE, border="darkblue", plot=TRUE)
pdf <- function(x) (1/pi)/sqrt(1-x^2/4)  # very simple formula!
curve(pdf,from=0,to=2,add=TRUE,col="red",lwd=2)
}

# the histogram is close to the theoretical density function

length(DS[DS>sqrt(3)])/10000
1/3    # theoretical probability that random chord length is > sqrt(3)
#graphics.off()   # close plot

# The slope of the chord is irrelevant: only the distance between
# chordal midpoint and circular center (the origin) matters.

# Suppose instead that we generate a y-value uniformly on [-1,1]
# and then draw the chord horizontally at that position?

chrd2 <- function()            # second definition of random chord
{
  y <- runif(1,-1,1)
  x <- sqrt(1-y^2)
  XY <- matrix(c(x,-x,y,y), c(2,2))   
  #x11(width=5,height=5.5)    # keep old plot and open a new plotting window
  plot(XY,xlim=c(-1,1),ylim=c(-1,1),pch=22)
  lines(XY[1:2, ],col=CL(),lwd=3)
  upp.crcl <- function(x) sqrt(1-x^2)
  low.crcl <- function(x) -sqrt(1-x^2)
  curve(upp.crcl,from=-1,to=1,add=T)
  curve(low.crcl,from=-1,to=1,add=T)
}

chrd2()
chrd2()
chrd2()

for (j in 1:25)
  {
    Sys.sleep(1.0)   #suspends execution for 1.0 sec
    chrd2() 
  }

rnd.chrd2 <- function(m)
{
  sapply(1:m, function (o)
           {
             y <- runif(1,-1,1)
             2*sqrt(1-y^2)                          
           })
}

DS <- rnd.chrd2(10000)
DS[1:30]
dim(DS) <- c(10000,1)
length(DS[DS>sqrt(3)])/10000
1/2    # theoretical probability (not equal to 1/3!)
#graphics.off()   # close plot

# Suppose instead that we generate an x-value on [-1,1]
# such that x=sign(z)*sqrt(abs(z)), z~Unif[-1,1]
# and then draw the chord vertically at that position?

# (The x-values consequently will tend to cluster closer
# to the boundary points 1 & -1 and avoid the middle 0.)

chrd3 <- function()            # third definition of random chord
{
  z <- runif(1,-1,1)
  x <- sign(z)*sqrt(abs(z))
  y <- sqrt(1-x^2)
  XY <- matrix(c(x,x,y,-y), c(2,2))   
  #x11(width=5,height=5.5)    # keep old plot and open a new plotting window
  plot(XY,xlim=c(-1,1),ylim=c(-1,1),pch=22)
  lines(XY[1:2, ],col=CL(),lwd=3)
  upp.crcl <- function(x) sqrt(1-x^2)
  low.crcl <- function(x) -sqrt(1-x^2)
  curve(upp.crcl,from=-1,to=1,add=T)
  curve(low.crcl,from=-1,to=1,add=T)
}

chrd3()
chrd3()
chrd3()

for (j in 1:25)
  {
    Sys.sleep(1.0)   #suspends execution for 1.0 sec
    chrd3() 
  }

rnd.chrd3 <- function(m)
{
  sapply(1:m, function (o)
           {
             z <- runif(1,-1,1)
             2*sqrt(1-abs(z))                       
           })
}

DS <- rnd.chrd3(10000)
DS[1:30]
dim(DS) <- c(10000,1)
length(DS[DS>sqrt(3)])/10000
1/4    # theoretical probability (again, not equal to 1/3!)
#graphics.off()   # close plot

# The fact that three different definitions of "random chord"
# gave distinct probabilities was surprising 100 years ago
# (even paradoxical).  It's not surprising anymore.

rnd.chrd0 <- function(m)    # back to first definition, but examine distance
{                           # between chordal midpoint and circular center
  sapply(1:m, function (o)
           {
             Theta <- matrix(runif(2,0,2*pi), ncol=1)
             XY <- cbind(cos(Theta),sin(Theta))
             M <- (XY[1,]+XY[2,])/2             
             sqrt(M%*%M)                            
           })
}

MD <- rnd.chrd0(10000)
MD[1:30]
dim(MD) <- c(10000,1)         # regard DS not as a vector, but a column matrix         

{
#x11()
hist(MD, freq=FALSE, border="darkblue", plot=TRUE)
pdf1 <- function(x) (2/pi)/sqrt(1-x^2)
pdf2 <- function(x) 1+0*x   # x=y; y~Unif[0,1]
pdf3 <- function(x) 2*x     # x=sqrt(z); z~Unif[0,1]
curve(pdf1,from=0,to=1,add=TRUE,col="red",lwd=2)
curve(pdf2,from=0,to=1,add=TRUE,col="magenta",lwd=2)
curve(pdf3,from=0,to=1,add=TRUE,col="green",lwd=2)
}

#graphics.off()   # close plot

# REFERENCES
#
#P. Groeneboom, Limit theorems for convex hulls,
#Probab. Theory Relat. Fields 79 (1988) 327-368.
#
#S. Finch and I. Hueter, Random convex hulls: A variance revisited,
#Adv. Appl. Probab. 36 (2004) 981-986.
#
#"Bertrand's paradox" involving random chords is well-known: see
# http://web.mit.edu/urban_or_book/www/book/chapter3/3.3.2.html
# http://en.wikipedia.org/wiki/Bertrand_paradox_(probability)
# http://demonstrations.wolfram.com/RandomChordParadox/
