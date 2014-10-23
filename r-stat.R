### Daily energy intake in kJ for 11 women (Altman, 1991)
### One-sample t test

daily.intake <- c(5260,5470,5640,6180,6390,6515,
6805,7515,7515,8230,8770)

##x11()
plot(daily.intake)   # unsatisfactory
#graphics.off()   # close plot

#x11()
plot(daily.intake,xlab="subject",xaxt="n",main="Energy intake for 11 subjects")
# xaxt="n" suppresses plotting of axis
axis(1,at=seq(1,11,1))
# puts one tick mark per subject
#graphics.off()   # close plot

mean(daily.intake)
sd(daily.intake)
quantile(daily.intake)
summary(daily.intake)  # curiously does not give stddev!

# Does the women's energy intake deviate significantly from
# a recommended value of 7725 kJ?

t.test(daily.intake,mu=7725)   # we reject H0 at 5% significance level

# Same question, but want a 99% confidence interval instead

t.test(daily.intake,mu=7725,conf.level=0.99)   # must accept H0 at 1% level

# Is the women's energy intake significantly less than
# a recommended value of 7725 kJ?

t.test(daily.intake,mu=7725,conf.level=0.99,alt="l")   # we reject H0 at 1% level
                                                       # (just barely)

### Energy expenditure in groups of lean & obese women
### Two-sample t test

energy <- read.delim("energy.txt",header=TRUE)

# must use forward slash /, not backslash \, when specifying
# pathnames as above

energy                   # a dataframe
str(energy)              # structure (like PROC CONTENTS)
summary(energy)          # summary (like PROC MEANS & PROC FREQ)
str(energy$expend)            # overview of one variable at a time
summary(energy$expend)
str(energy$stature)  
summary(energy$stature)
tapply(energy$expend,energy$stature,summary)         # cross tabs
#x11()
plot(energy$expend~energy$stature,main="Plot of Energy Data")     # parallel boxplots
       # plot understood to be boxplot; tilde (~) to be explained later
#graphics.off()   # close plot

# It's inconvenient to be carrying "energy$" everywhere!  

tapply(expend,stature,summary)  # fails
plot(expend~stature)            # fails

search()        # objects in the workspace

attach(energy)
tapply(expend,stature,summary)  # now works
#x11()
plot(expend~stature,main="Plot of Energy Data")     # now works
#graphics.off()   # close plot

search()        # our dataset now added (no. 2 in search path)
                # detach(energy) would undo what we just did

# Is there a significant difference in energy levels between the two groups?

t.test(expend~stature)  # Welch's variant of t-test: variances not assumed equal

# tilde (~) means here that 'expend' is described by 'stature'

t.test(expend~stature, var.equal=TRUE)  # textbook t-test: variances assumed equal

# F test for comparing variances

var.test(expend~stature)

detach(energy)        # tidy up
search()

### Ventricular shortening velocity & blood glucose for diabetes patients
### Simple linear regression

thuesen <- read.delim("thuesen.txt",header=TRUE)

thuesen                  # another dataframe
str(thuesen)
summary(thuesen)

attach(thuesen)          # as before, for convenience

lm(short.velocity~blood.glucose)  # very brief output!

# much more information is seen via various extractor functions

summary(lm(short.velocity~blood.glucose))

#x11()
plot(short.velocity~blood.glucose,main="Plot of Thuesen Data")
abline(lm(short.velocity~blood.glucose), col="red", lwd="2")

# draws lines based on the intercept & slope (a & b)

# two more extractor functions

ff <- fitted(lm(short.velocity~blood.glucose))  # y-values we expect for observed x-values,
#points(blood.glucose, ff)      # this doesn't work, b/c we have missing data in our dataset



resid(lm(short.velocity~blood.glucose))   # vertical distances between obs & fit

# wish to visualize the residuals; memorize the above two vectors

FIT <- fitted(lm(short.velocity~blood.glucose))
RES <- resid(lm(short.velocity~blood.glucose))

segments(blood.glucose,FIT,
blood.glucose,short.velocity)    # (x1,y1,x2,y2)

# completely WRONG!  what could possibly be causing such mis-match?

# note that 16th point is missing (empty between 15th & 17th points)
# up to now, we've been careless about NAs (the agony of missing data!)
# to overlay the residuals on the plot, we must be more careful

#graphics.off()   # close plot

getOption("na.action")
options(na.action="na.exclude")      # pads FIT & RES with NAs in 16th slot

FIT <- fitted(lm(short.velocity~blood.glucose))
RES <- resid(lm(short.velocity~blood.glucose))
FIT
RES

#x11()
plot(short.velocity~blood.glucose,main="Plot of Thuesen Data")
abline(lm(short.velocity~blood.glucose), col="red", lwd="2")

segments(blood.glucose,FIT,
blood.glucose,short.velocity)    # (x1,y1,x2,y2)

#graphics.off()   # close plot

{
  #x11()
  qqnorm(RES)       # graphical test of residual normality
  qqline(RES)       # anchor line at the quartiles
}

#graphics.off()   # close plot

cor(blood.glucose,short.velocity)   # fails because of missing datapoint

cor(blood.glucose,short.velocity,use="complete.obs")  # different option

# Is this correlation significantly different from zero?

cor.test(blood.glucose,short.velocity,use="complete.obs")    # reject H0 at 5% significance level

cor.test(blood.glucose,short.velocity,use="complete.obs",conf.level=0.99)   # must accept H0 at 1% level

options(na.action="na.omit")    # tidy up (return to default setting)

# see http://biostat.mc.vanderbilt.edu/wiki/Main/PlottingModelResults
# for more about the residual/fitted values plotting example above 

# see also P. Dalgaard "Introductory Statistics with R" (available
# online under Course Resources



