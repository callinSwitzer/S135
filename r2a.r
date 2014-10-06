# SIMULATION (more from geometry)

# What is the cross-correlation between area & perimeter
# of random triangles?

XY <- matrix(runif(4), ncol=2)
sqrt((XY[2,1]-XY[1,1])^2+(XY[2,2]-XY[1,2])^2)  # distance between
U <- XY[2,]-XY[1,]                             # two points is
sqrt(U%*%U)                                    # same as this

side <- function(u) sqrt(u%*%u)   

angl <- function(v,w) acos(v%*%w/sqrt((v%*%v)*(w%*%w)))

rnd.AreaPeri <- function(m)
{
  t(sapply(1:m, function(o)
           {
             XY <- matrix(runif(6), ncol=2)
             S1 <- side(XY[3,]-XY[2,])
             S2 <- side(XY[1,]-XY[3,])
             S3 <- side(XY[2,]-XY[1,])
             A1 <- angl(XY[2,]-XY[1,],XY[3,]-XY[1,])
             area <- (1/2)*S2*S3*sin(A1)
             peri <- S1+S2+S3
             c(area=area,peri=peri)
           }))
}

AP <- rnd.AreaPeri(10000)
AP[1:10,]         # first ten rows only
apply(AP,2,mean) 
c(11/144,(sqrt(2)+2+5*log(1+sqrt(2)))/5)    # theoretical means

AP[1:10,]^2       # first ten rows only
apply(AP^2,2,mean)  
1/96                  # theoretical mean square for perimeter is unknown

cor(AP)           # correlation matrix (off-diagonal element is what we want)

cor(AP)[2,1]          # theoretical correlation coefficient is unknown

x11()
plot(AP,pch=".")          # scatterplot (one point per triangle)
# same as plot(AP[,2]~AP[,1],pch=".")          

f <- function(x) sqrt(12*sqrt(3)*x)
curve(f,from=0,to=1/2,add=TRUE,col="red",lwd=2)     # lower bound

# expression for f comes from the fact that area is maximized, for fixed peri,
# when the triangle is equilateral; use Heron's formula to obtain f 

g <- function(t) sin(3*pi/4-t)
t <- seq(pi/4,pi/2,0.01)                            # parametric equations
x <- (1/2)*(1-sqrt(1/g(t)^2-1))
y <- 1/g(t)+(1-sqrt(1/g(t)^2-1))+sqrt(2)
lines(x,y,col="blue",lwd=2)                         # upper bound

# expressions for g, x, y are more difficult; come from maximizing perimeter, 
# for fixed area

a <- lm(AP[,2]~AP[,1])$coefficients[1]              # "lm" means "linear model"
b <- lm(AP[,2]~AP[,1])$coefficients[2]
h <- function(x) a+b*x
curve(h,from=0,to=1/2,add=TRUE,col="green",lwd=2)   # least squares line
  
summary(lm(AP[,2]~AP[,1]))$r.squared       # fraction of variance explained by the model
cor(AP)[2,1]^2                             # same as correlation coefficient squared

