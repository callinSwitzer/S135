# preliminaries:

q05 <- function(X,...) quantile(X,probs=0.05,na.rm=TRUE,names=F)
q95 <- function(X,...) quantile(X,probs=0.95,na.rm=TRUE,names=F)

YearTime <- read.table("Olympic.txt",header=T)

# single space (not tab) between columns: use read.table
# (we used read.delim in r11.R)

attach(YearTime)
YearTime         # times (in secs) recorded by winners of men's
#x11()
plot(Year,Time)  # Olympics 1500-m event from 1900 to 2000
#graphics.off()   # (there were no games in 1916, 1940 & 1944)

YearTime.fit <- nls(Time ~ a+b*exp(c*(Year-1900)), YearTime,
                    start = list(a=200, b=40, c=0.01), # this is an educated guess
                    algorithm = "port", trace = TRUE)

?nls
summary(YearTime.fit)   # how were p-values computed?

# slightly different starting values: almost identical outcome

{
#x11()
plot(Year,Time)
lines(Year,predict(YearTime.fit),col="blue",lwd=2)
}
graphics.off()   

# from online help: "... theory used to find the distribution 
# of the standard errors and of the residual standard error 
# (for t ratios) is based on linearization and is approximate, 
# maybe _very_ approximate"

coef(YearTime.fit)                 # coefficients only ...
coef(summary(YearTime.fit))        # ... plus uncertainties

# bootstrap replicates of YearTime

YearTime[sample(NROW(YearTime),replace=T),] 
YearTime[sample(NROW(YearTime),replace=T),] 
YearTime[sample(NROW(YearTime),replace=T),] 

f <- function(m)            # bootstrap using 'sample' function
{    
  t(sapply(1:m, function(o) 
     {
        coef(nls(Time ~ a+b*exp(c*(Year-1900)),
                               YearTime[sample(NROW(YearTime),replace=T),],
                               start = list(a=206, b=42, c=-0.01),
                               algorithm = "port"))
     }
  ))
} 

F <- f(1000)

c(coef(summary(YearTime.fit))[1,1], mean(F[,1]))
c(coef(summary(YearTime.fit))[2,1], mean(F[,2]))
c(coef(summary(YearTime.fit))[3,1], mean(F[,3]))

c(coef(summary(YearTime.fit))[1,2], sd(F[,1]))
c(coef(summary(YearTime.fit))[2,2], sd(F[,2]))
c(coef(summary(YearTime.fit))[3,2], sd(F[,3]))

{#x11()
hist(F[,1], br=seq(-0.5,249.5,by=2), border="darkblue", plot=T)} 

{#x11()
hist(F[,2], br=seq(-0.5,195.5,by=1), border="darkblue", plot=T)} 

{#x11()
hist(F[,3], br=seq(-0.1,0.01,by=0.001), border="darkblue", plot=T)}

c(q05(F[,1]), q95(F[,1]))
c(q05(F[,2]), q95(F[,2]))
c(q05(F[,3]), q95(F[,3]))
# called "CI using percentile bootstrap method"

graphics.off()  

# REFERENCES

# S. Chatterjee and S. Chatterjee, New lamps for old:
# An exploratory analysis of running times in Olympic games,
# Applied Statistics 31 (1982) 14-22.

# B. S. Everitt, A Handbook of Statistical Analyses
# using S-Plus, 2nd ed., Chapman & Hall/CRC, 2002,
# pp. 107-121.
