### Name: smooth.fdPar
### Title: Smooth a functional data object using a directly specified
###   roughness penalty
### Aliases: smooth.fdPar
### Keywords: smooth

### ** Examples

#  Shows the effects of two levels of smoothing
#  where the size of the third derivative is penalized.
#  The null space contains quadratic functions.
x <- seq(-1,1,0.02)
y <- x + 3*exp(-6*x^2) + rnorm(rep(1,101))*0.2
#  set up a saturated B-spline basis
basisobj <- create.bspline.basis(c(-1,1),81)
#  convert to a functional data object that interpolates the data.
result <- smooth.basis(x, y, basisobj)
yfd  <- result$fd
#  set up a functional parameter object with smoothing
#  parameter 1e-6 and a penalty on the 2nd derivative.
yfdPar <- fdPar(yfd, 2, 1e-6)
yfd1 <- smooth.fd(yfd, yfdPar)

yfd1. <- smooth.fdPar(yfd, 2, 1e-6)
all.equal(yfd1, yfd1.)
# TRUE

#  set up a functional parameter object with smoothing
#  parameter 1 and a penalty on the 2nd derivative.
yfd2 <- smooth.fdPar(yfd, 2, 1)

#  plot the data and smooth
plot(x,y)           # plot the data
lines(yfd1, lty=1)  #  add moderately penalized smooth
lines(yfd2, lty=3)  #  add heavily  penalized smooth
legend(-1,3,c("0.000001","1"),lty=c(1,3))
#  plot the data and smoothing using function plotfit.fd
plotfit.fd(y, x, yfd1)  # plot data and smooth




