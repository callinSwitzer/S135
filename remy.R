dict <- function(x)
  {
    if(x==1) y <- 'list('
    else if(x==0) y <- '0'
    else if(x==-1) y <- ')'
    else y <- ','
    y
  }
dict <- Vectorize(dict)

is.scalar <- function(X)
  {
    if (!is.list(X) & NROW(X)==1 & NCOL(X)==1) TRUE else FALSE
  }  

height <- function(X)
  {
    if (is.scalar(X)) 0 else
      {
        max(height(X[[1]]),height(X[[2]]))+1
      }  
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

D <- function(X) F(X,1,0)

Contour <- function(X,E)
  {
    W <- rep(0,NROW(E))
    for (j in 1:NROW(E))
        {
           W[j] <- NROW(E[[j]])     
        }
    W
  }

draw <- function(v)   # shortening/rotating tree branches for display
  {
    u <- c(NA, 0)
    f <- 2
    for (n in 1:NROW(v))
      {
        f <- f*0.8*exp((v[n]-1/2)*(-pi*1i/3))  
        u[n+2] <- u[n+1]+f
      }
    u
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

Flatten <- function(M,n,Z)  # recursive flattening of binary tree
  {
    Z[n+1] <- Z[n+1]+1          
    if (!is.scalar(M))
      {
        for (j in 1:NROW(M))
        {
           Z <- Flatten(M[[j]],n+1,Z)     
        }
      }
    Z
  }

Profile <- function(X)
{
  v <- Flatten(X,0,rep(0,NROW(unlist(X))))
  v[v>0]
}

width <- function(X) max(Profile(X))

HEIGHT <- function(w)  # using complex vector solely, not nested lists
{
k <- min(c(which(w==1),Inf))
if (k==Inf) 0 else
  {
    m <- min(which(Re(cumsum(w[(k+1):length(w)]))==0))+k
    wa <- w[(k+1):m]
    wb <- w[(m+2):length(w)]   # skip slot m+1 because it must contain 1i (comma)
    max(HEIGHT(wa),HEIGHT(wb))+1
  }
}

FLATTEN <- function(w,j,Z)  # recursive flattening of binary tree
  {
    Z[j+1] <- Z[j+1]+1          
    if (!is.scalar(w))
      {
      k <- 2  
      while(k<=length(w)-1)       
       {
       if(w[k]==0)
          {
          Z <- FLATTEN(0,j+1,Z) 
          k <- k+1   
          }
       if(w[k]==1)
          {
          m <- min(which(Re(cumsum(w[k:length(w)]))==0))+k-1
          ws <- w[k:m]
          Z <- FLATTEN(ws,j+1,Z) 
          k <- m+1   
          } else k <- k+1
       }
      }
    Z
  }

PROFILE <- function(w)
{
  v <- FLATTEN(w,0,rep(0,HEIGHT(w)+1))
  v
}

WIDTH <- function(X) max(PROFILE(X))

lw <- function(X,E)
  {
   LW <- rep(0,NROW(E))
   for (j in 1:NROW(E)) LW[j] <- min(cumsum(2*unlist(E[[j]])-1))  
   min(c(0,LW))
  }

rw <- function(X,E)
  {
   RW <- rep(0,NROW(E))
   for (j in 1:NROW(E)) RW[j] <- max(cumsum(2*unlist(E[[j]])-1))  
   max(c(0,RW))
  }

TRvctr <- function(N)
{  
w <- 0
while(length(w[w==0])<=N)
{
which(w==0 | w==1)
k<-sample(which(w==0 | w==1),1)   
n<-length(w)
if (w[k]==0)
  {
    wa <- if(k>1) w[1:(k-1)] else NA
    wb <- if(k<n) w[(k+1):n] else NA
    w <- c(wa,1,0,1i,w[k],-1,wb)
  } else
  {
    m <- min(which(Re(cumsum(w[k:n]))==0))+k-1
    wa <- if(k>1) w[1:(k-1)] else NA
    wb <- if(m<n) w[(m+1):n] else NA
    if (runif(1) < 1/2) w <- c(wa,1,0,1i,w[k:m],-1,wb)
    else w <- c(wa,1,w[k:m],1i,0,-1,wb)
  }  
w<-w[!is.na(w)]
}
w
}

TRlist <- function(w)
{
   tre <- paste(dict(w),collapse="")
   eval(parse(text=tre))
}

STlist <- function(X,eps)    # eps=1 for one tree, eps=0 for many trees
  {
    E <- D(X)
    if (eps==1)
      {
       Q <- path(E)
       x11(width=5,height=5) # keep old plot and open a new plotting window
       plot(Re(Q),Im(Q),type="l",col=CL(),lwd=2,axes=FALSE,xlab="",ylab="")
      } 
    list(height(X),width(X),lw(X,E),rw(X,E),Contour(X,E),Profile(X))
  }

STvctr <- function(w)   
  {
    list(HEIGHT(w),WIDTH(w),PROFILE(w))
  }



