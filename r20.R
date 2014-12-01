### Vectors, matrices, arrays and lists

v <- c(log(2),pi)
v
m <- c(log(2),pi,sin(1),cos(1),tan(1),exp(1))    # one way 
dim(m) <- c(2,3)         # columns are populated in order
m
n <- matrix(1:6,c(2,3))  # another way (done in one line!)
n
c(nrow(m),ncol(m))
c(nrow(v),ncol(v))  # a vector is not considered a column matrix
c(NROW(m),NCOL(m))  # NROW same as nrow over matrices; likewise NCOL same as ncol
c(NROW(v),NCOL(v))  # NROW and NCOL also work over vectors, thankfully!

v[2]       # extracting elements
m[1,3]
m[2,]      # extracting a row
m[,2]      # extracting a column

V <- array(1:30,30)      # a vector is a special case of an array...
V
M <- array(1:30,c(5,6))  # ... as is a matrix
M
A <- array(1:30,c(2,3,5))   # 5 blocks, each a 2x3 subarray
A

c(nrow(A),ncol(A))       # can get number of rows & columns as before...
dim(V)                   # ... but 'dim' is more general
dim(M)
dim(A)

A[1,2,3]   # extracting an element
A[1,,]     # extracting a row
A[,2,]     # extracting a column
A[,,3]     # extracting a block

apply(A,1,max)            # row-wise maxima
apply(A,2,max)            # column-wise maxima
apply(A,3,max)            # block-wise maxima

# an array is a generalization of a vector/matrix
# a list is a different generalization (includes data frames)
# we'll focus today on lists that give rise to binary trees
#
# (disregard for now the lengthy code that immediately follows)

is.scalar <- function(X)
  {
    if (!is.list(X) & NROW(X)==1 & NCOL(X)==1) TRUE else FALSE
  }  

F <- function(M,S,L)  # recursive walk (list paths to all leaves)
  {
    A <- S
    p <- if (is.scalar(A)) 0 else NROW(A)
    if (is.scalar(M)) 
      {
        if (is.scalar(L)) L[1] <- list(S)
        else L[NROW(L)+1] <- list(S)       
      }    
    else
      {  
        for (j in 1:NROW(M))
        {
          if (p==0) A[1] <- list(j-1)
          else A[p+1] <- list(j-1)
          L <- F(M[[j]],A,L)     
        }
      }
    L
  }

D <- function(M) F(M,1,0)

draw <- function(v)   # shortening/rotating tree branches for display
  {
    w <- c(NA, 0)
    f <- 2
    for (n in 1:NROW(v))
      {
        f <- f*0.8*exp((v[n]-1/2)*(-pi*1i/3))  
        w[n+2] <- w[n+1]+f
      }
    w
  }  

path <- function(E)   # concatenate line segments (many duplicates)
  {
    lsegm <- 0
    for (k in 1:NROW(E))
      {
        lsegm <- c(lsegm,draw(unlist(E[k])))         
      }
    lsegm
  }

CL <- function(o)              # random color
      sample(c("red","blue","forestgreen","darkviolet"))

TR <- function(X)
  {
    E <- D(X)
    Q <- path(E)
    plot(Re(Q),Im(Q),type="l",col=CL(),lwd=2,axes=FALSE,xlab="",ylab="")
  }

# elements of a list can be, for example, scalars (numbers)...

X <- list(0,0)
X
{
#x11(width=5,height=5)
TR(X)
}
#graphics.off()   # close plot

# ... or other lists of such scalars 

X <- list(list(0,0),0)
X
X[1]     # sublist of X consisting of the first element
X[2]     
X[[1]]   # first element in X (which is list(0,0) here)
X[[2]]   # [[ ]] means peeling away one set of parentheses
X[[1]][1]
X[[1]][[1]]  # two sets of parentheses peeled away
{
#x11(width=5,height=5)
TR(X)
}
#graphics.off()   # close plot

X <- list(list(0,0),list(0,0))
{
#x11(width=5,height=5)
TR(X)
}
#graphics.off()   # close plot

X <- list(list(0,0),list(0,list(0,0)))
{
#x11(width=5,height=5)
TR(X)
}
#graphics.off()   # close plot

X <- list(list(list(0,list(list(0,0),0)),list(0,0)),0)  
{
#x11(width=5,height=5)
TR(X)
}
#graphics.off()   # close plot (saved as tree.JPG; referred to later)

# to generate random binary trees is very simple!
# fix p = probability that, at any point, a branching into two twigs occurs

options(expressions = 5000)

t <- function(p)      # recursive construction 
  list(if(runif(1) < p) t(p) else 0, if(runif(1) < p) t(p) else 0)

T <- function(p)
  {
    X <- t(p)
    E <- D(X)
    Q <- path(E)
    #x11(width=5,height=5)    # keep old plot and open a new plotting window
    plot(Re(Q),Im(Q),type="l",col=CL(),lwd=2,axes=FALSE,xlab="",ylab="")
  }

for (j in 1:25)
  {
    Sys.sleep(1.0)   #suspends execution for 1.0 sec
    T(0.4)
  }
#graphics.off()   # close all plots altogether

for (j in 1:5)
  {
    Sys.sleep(2.0)   #suspends execution for 2.0 sec
    T(0.5)
  }
#graphics.off()   # close all plots altogether

# The root of any binary tree is the unique vertex from which the tree emanates;
# we think of edges connecting pairs of vertices and possessing direction,
# starting from the root and stopping at the leaves of the tree.
# The number of edges is equal to the number of vertices, V, minus 1.
# The number of leaves, L, is equal to the number of 0s in the nested array T(p).
# If p is suitably small, then we intuitively expect the program to terminate
# and thus for the number of vertices to be finite (see graph:  V=13 and L=7).

# Two other parameters characterizing the shape of a tree can be defined.
# First, a preliminary definition:  the nth level of a tree is the set of all vertices
# which are connected to the root by a self-avoiding path of exactly n edges,  n>0.
# Denote by Z.n the number of points in the nth level.  Note that Z.0=1 and that a tree
# is finite if and only if there exists N>0 such that Z.N=0.  Then define:
#
#              the height H of a tree = max {n : Z.n>0}
# and
#              the width W of a tree = max {Z.n : n>0}.
#
# In the graph, we have Z.0=1, Z.1=2, Z.2=2, Z.3=4, Z.4=2, Z.5=2, H=5 and W=4.
#
# The joint probability distribution of the four variables:
#       number of vertices, V,
#       number of leaves, L,
#       height, H,
#       width, W,
# would be great to understand someday (as a function of p!)

X <- list(list(list(0,list(list(0,0),0)),list(0,0)),0) 

height <- function(X)
  {
    if (is.scalar(X)) 0 else
      {
        max(height(X[[1]]),height(X[[2]]))+1
      }  
  }    
height(X)

leaves <- function(X) NROW(unlist(X))
leaves(X)

# Of many possible things, let's study the subcritical case p < 1/2
# and a(n) = probability that Z.n is 0 (i.e., the nth level is empty)

# FACT: the sequence a(0), a(1), a(2), ... satisfies the quadratic recurrence
#   a(n) = (1-p) + p*a(n-1)^2    for n>=1;         a(0) = 0.

# Confirm this fact by simulation for n=1,2,3,4,5,6.

a <- function(n,p)
  {
    if (n==0) 0 else (1-p) + p*a(n-1,p)^2
  }

H <- function(N,p)
{    
    sapply(1:N, function(o) height(t(p)))     # o is only a dummy variable
}                                               

N <- 20000; p <- 0.3      
A <- function(N,p)     # Pr{ht>=n, given ht>=1} = Pr{ht>=n and ht>=1}/p
  {      # thus (proportion of our trees with ht>=n)*p = Pr{Z.n>0} = 1-a(n)
    h <- H(N,p)
    k <- c(length(which(h>=1))/N, length(which(h>=2))/N,
           length(which(h>=3))/N, length(which(h>=4))/N,
           length(which(h>=5))/N, length(which(h>=6))/N)*p
    k
  }  
A(N,p)
options(digits=5)
c(1-a(1,p),1-a(2,p),1-a(3,p),1-a(4,p),1-a(5,p),1-a(6,p))

# Clearly lim a(n) = 1 as n -> infinity.  Can we be more precise about the
# convergence rate?

# FACT: C(p) = lim (1-a(n))/(2*p)^n exists as n->oo, is finite and nonzero

# Confirm this fact numerically in two different ways.

ONE <- function(n,p) (1-a(n,p))/(2*p)^n   # disaster!

c(ONE(15,0.2),ONE(20,0.2),ONE(25,0.2),ONE(30,0.2),ONE(35,0.2))
c(ONE(36,0.2),ONE(37,0.2),ONE(38,0.2),ONE(39,0.2),ONE(40,0.2))

# nice illustration of the loss of floating-point precision
# that occurs when subtracting two nearly equal quantities

# important in numerical analysis & scientific programming;
# good for statisticians to know as well!

# fortunately we have the infinite product expression too

a <- Vectorize(a)

TWO <- function(n,p) prod((1+a(0:n,p))/2)

c(TWO(15,0.2),TWO(20,0.2),TWO(25,0.2),TWO(30,0.2),TWO(35,0.2))
options(digits=16)
c(TWO(36,0.2),TWO(37,0.2),TWO(38,0.2))
c(TWO(40,0.2),TWO(60,0.2),TWO(80,0.2))
c(TWO(100,0.2),TWO(150,0.2))

