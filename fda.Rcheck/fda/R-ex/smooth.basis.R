### Name: smooth.basis
### Title: Smooth Data with an Indirectly Specified Roughness Penalty
### Aliases: smooth.basis
### Keywords: smooth

### ** Examples

##
## Example 1:  Inappropriate smoothing  
##
# A toy example that creates problems with
# data2fd:  (0,0) -> (0.5, -0.25) -> (1,1)
b2.3 <- create.bspline.basis(norder=2, breaks=c(0, .5, 1))
# 3 bases, order 2 = degree 1 =
# continuous, bounded, locally linear
fdPar2 <- fdPar(b2.3, Lfdobj=2, lambda=1)

## Not run: 
##D # Penalize excessive slope Lfdobj=1;  
##D # second derivative Lfdobj=2 is discontinuous,
##D # so the following generates an error:
##D   fd2.3s0 <- smooth.basis(0:1, 0:1, fdPar2)
##D Derivative of order 2 cannot be taken for B-spline of order 2 
##D Probable cause is a value of the nbasis argument
##D  in function create.basis.fd that is too small.
##D Error in bsplinepen(basisobj, Lfdobj, rng) :
## End(Not run)

##
## Example 2.  Better 
##
b3.4 <- create.bspline.basis(norder=3, breaks=c(0, .5, 1))
# 4 bases, order 3 = degree 2 =
# continuous, bounded, locally quadratic 
fdPar3 <- fdPar(b3.4, lambda=1)
# Penalize excessive slope Lfdobj=1;  
# second derivative Lfdobj=2 is discontinuous.
fd3.4s0 <- smooth.basis(0:1, 0:1, fdPar3)

plot(fd3.4s0$fd)
# same plot via plot.fdSmooth
plot(fd3.4s0) 

##
## Example 3.  lambda = 1, 0.0001, 0
##
#  Example 3.  lambda = 1 
#  Shows the effects of three levels of smoothing
#  where the size of the third derivative is penalized.
#  The null space contains quadratic functions.
x <- seq(-1,1,0.02)
y <- x + 3*exp(-6*x^2) + rnorm(rep(1,101))*0.2
#  set up a saturated B-spline basis
basisobj <- create.bspline.basis(c(-1,1), 101)

fdPar1 <- fdPar(basisobj, 2, lambda=1)
result1  <- smooth.basis(x, y, fdPar1)
with(result1, c(df, gcv, SSE))

#  Example 2.  lambda = 0.0001
fdPar.0001 <- fdPar(basisobj, 2, lambda=0.0001)
result2  <- smooth.basis(x, y, fdPar.0001)
with(result2, c(df, gcv, SSE))
# less smoothing, more degrees of freedom,
# smaller gcv, smaller SSE 

#  Example 3.  lambda = 0
fdPar0 <- fdPar(basisobj, 2, lambda=0)
result3  <- smooth.basis(x, y, fdPar0)
with(result3, c(df, gcv, SSE))
# Saturate fit:  number of observations = nbasis 
# with no smoothing, so degrees of freedom = nbasis,
# gcv = Inf indicating overfitting;
# SSE = 0 (to within roundoff error)

plot(x,y)           # plot the data
lines(result1[['fd']], lty=2)  #  add heavily penalized smooth
lines(result2[['fd']], lty=1)  #  add reasonably penalized smooth
lines(result3[['fd']], lty=3)  #  add smooth without any penalty
legend(-1,3,c("1","0.0001","0"),lty=c(2,1,3))

plotfit.fd(y, x, result2[['fd']])  # plot data and smooth

##
## Example 4.  Supersaturated
##
basis104 <- create.bspline.basis(c(-1,1), 104)

fdPar104.0 <- fdPar(basis104, 2, lambda=0) 
result104.0  <- smooth.basis(x, y, fdPar104.0)
with(result104.0, c(df, gcv, SSE))

plotfit.fd(y, x, result104.0[['fd']], nfine=501)
# perfect (over)fit
# Need lambda > 0.

##
## Example 5.  gait
##
gaittime  <- (1:20)/21
gaitrange <- c(0,1)
gaitbasis <- create.fourier.basis(gaitrange,21)
lambda    <- 10^(-11.5)
harmaccelLfd <- vec2Lfd(c(0, 0, (2*pi)^2, 0))

gaitfdPar <- fdPar(gaitbasis, harmaccelLfd, lambda)
gaitfd <- smooth.basis(gaittime, gait, gaitfdPar)$fd
## Not run: 
##D # by default creats multiple plots, asking for a click between plots 
##D plotfit.fd(gait, gaittime, gaitfd)
## End(Not run)



