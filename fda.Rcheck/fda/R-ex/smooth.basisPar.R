### Name: smooth.basisPar
### Title: Smooth Data Using a Directly Specified Roughness Penalty
### Aliases: smooth.basisPar
### Keywords: smooth

### ** Examples

##
## simplest call
##
girlGrowthSm <- with(growth, smooth.basisPar(argvals=age, y=hgtf))
plot(girlGrowthSm$fd, xlab="age", ylab="height (cm)",
         main="Girls in Berkeley Growth Study" )
plot(deriv(girlGrowthSm$fd), xlab="age", ylab="growth rate (cm / year)",
         main="Girls in Berkeley Growth Study" )
plot(deriv(girlGrowthSm$fd, 2), xlab="age",
        ylab="growth acceleration (cm / year^2)",
        main="Girls in Berkeley Growth Study" )
#  Shows the effects of smoothing
#  where the size of the third derivative is penalized.
#  The null space contains quadratic functions.

##
## Another simple call
##
lipSm <- smooth.basisPar(liptime, lip)
plot(lipSm)
# oversmoothing
plot(smooth.basisPar(liptime, lip, lambda=1e-9))
# more sensible 

##
## A third example 
##

x <- seq(-1,1,0.02)
y <- x + 3*exp(-6*x^2) + sin(1:101)/2
# sin not rnorm to make it easier to compare
# results across platforms 

#  set up a saturated B-spline basis
basisobj101 <- create.bspline.basis(x)
fdParobj101 <- fdPar(basisobj101, 2, lambda=1)
result101  <- smooth.basis(x, y, fdParobj101)

resultP <- smooth.basisPar(argvals=x, y=y, fdobj=basisobj101, lambda=1)
## Don't show: 
stopifnot(
## End Don't show
all.equal(result101, resultP)
## Don't show: 
)
## End Don't show
# TRUE 

result4 <- smooth.basisPar(argvals=x, y=y, fdobj=4, lambda=1)
## Don't show: 
stopifnot(
## End Don't show
all.equal(resultP, result4)
## Don't show: 
)
## End Don't show
# TRUE 

result4. <- smooth.basisPar(argvals=x, y=y, lambda=1)
## Don't show: 
stopifnot(
## End Don't show
all.equal(resultP, result4.)
## Don't show: 
)
## End Don't show
# TRUE

with(result4, c(df, gcv)) #  display df and gcv measures

result4.4 <- smooth.basisPar(argvals=x, y=y, lambda=1e-4)
with(result4.4, c(df, gcv)) #  display df and gcv measures
# less smoothing, more degrees of freedom, better fit 

plot(result4.4)
lines(result4, col='green')
lines(result4$fd, col='green') # same as lines(result4, ...)

result4.0 <- smooth.basisPar(x, y, basisobj101, lambda=0)

result4.0a <- smooth.basisPar(x, y, lambda=0)
## Don't show: 
stopifnot(
## End Don't show
all.equal(result4.0, result4.0a)
## Don't show: 
)
## End Don't show

with(result4.0, c(df, gcv)) #  display df and gcv measures
# no smoothing, degrees of freedom = number of points 
# but generalized cross validation = Inf
# suggesting overfitting.  

##
## fdnames?
##
girlGrow12 <- with(growth, smooth.basisPar(argvals=age, y=hgtf[, 1:2],
              fdnames=c('age', 'girl', 'height')) ) 
girlGrow12. <- with(growth, smooth.basisPar(argvals=age, y=hgtf[, 1:2],
    fdnames=list(age=age, girl=c('Carol', 'Sally'), value='height')) )




