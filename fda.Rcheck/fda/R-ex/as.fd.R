### Name: as.fd
### Title: Convert a spline object to class 'fd'
### Aliases: as.fd as.fd.fdSmooth as.fd.dierckx as.fd.function
###   as.fd.smooth.spline
### Keywords: smooth manip

### ** Examples

##
## as.fd.fdSmooth
##
girlGrowthSm <- with(growth, smooth.basisPar(argvals=age, y=hgtf))
girlGrowth.fd <- as.fd(girlGrowthSm)

##
## as.fd.dierckx
##
x <- 0:24
y <- c(1.0,1.0,1.4,1.1,1.0,1.0,4.0,9.0,13.0,
       13.4,12.8,13.1,13.0,14.0,13.0,13.5,
       10.0,2.0,3.0,2.5,2.5,2.5,3.0,4.0,3.5)
library(DierckxSpline) 
curfit.xy <- curfit(x, y, s=0)

curfit.fd <- as.fd(curfit.xy)
plot(curfit.fd) # as an 'fd' object 
points(x, y) # Curve goes through the points.  

x. <- seq(0, 24, length=241)
pred.y <- predict(curfit.xy, x.) 
lines(x., pred.y, lty="dashed", lwd=3, col="blue")
# dierckx and fd objects match.

## Don't show: 
stopifnot(
## End Don't show
all.equal(knots(curfit.xy, FALSE), knots(curfit.fd, FALSE))
## Don't show: 
)
## End Don't show
## Don't show: 
stopifnot(
## End Don't show
all.equal(coef(curfit.xy), as.vector(coef(curfit.fd)))
## Don't show: 
)
## End Don't show



##
## as.fd.function(splinefun(...), ...) 
## 
x2 <- 1:7
y2 <- sin((x2-0.5)*pi)
f <- splinefun(x2, y2)
fd. <- as.fd(f)
x. <- seq(1, 7, .02)
fx. <- f(x.)
fdx. <- eval.fd(x., fd.) 
plot(range(x2), range(y2, fx., fdx.), type='n')
points(x2, y2)
lines(x., sin((x.-0.5)*pi), lty='dashed') 
lines(x., f(x.), col='blue')
lines(x., eval.fd(x., fd.), col='red', lwd=3, lty='dashed')
# splinefun and as.fd(splineful(...)) are close
# but quite different from the actual function
# apart from the actual 7 points fitted,
# which are fitted exactly
# ... and there is no information in the data
# to support a better fit!

# Translate also a natural spline 
fn <- splinefun(x2, y2, method='natural')
fn. <- as.fd(fn)
lines(x., fn(x.), lty='dotted', col='blue')
lines(x., eval.fd(x., fn.), col='green', lty='dotted', lwd=3)

## Not run: 
##D # Will NOT translate a periodic spline
##D fp <- splinefun(x, y, method='periodic')
##D as.fd(fp)
##D #Error in as.fd.function(fp) : 
##D #  x (fp)  uses periodic B-splines, and as.fd is programmed
##D #   to translate only B-splines with coincident boundary knots.
##D 
## End(Not run)

##
## as.fd.smooth.spline
##
cars.spl <- with(cars, smooth.spline(speed, dist))
cars.fd <- as.fd(cars.spl)

plot(dist~speed, cars)
lines(cars.spl)
sp. <- with(cars, seq(min(speed), max(speed), len=101))
d. <- eval.fd(sp., cars.fd)
lines(sp., d., lty=2, col='red', lwd=3)



