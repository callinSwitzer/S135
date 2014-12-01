
dice <- function(o){
     foo <- sample(x = 1:6, size = 30, replace = TRUE)
     sum(foo[order(foo)] == rep(1:6, each = 5)) == 30  
}


dice()

fft <- replicate(n = 1000000, dice())
sum(fft)/length(fft)
