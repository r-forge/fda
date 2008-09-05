### Name: smooth.monotone
### Title: Monotone Smoothing of Data
### Aliases: smooth.monotone
### Keywords: smooth

### ** Examples


#  Estimate the acceleration functions for growth curves
#  See the analyses of the growth data.
#  Set up the ages of height measurements for Berkeley data
age <- c( seq(1, 2, 0.25), seq(3, 8, 1), seq(8.5, 18, 0.5))
#  Range of observations
rng <- c(1,18)
#  First set up a basis for monotone smooth
#  We use b-spline basis functions of order 6
#  Knots are positioned at the ages of observation.
norder <- 6
nage   <- 31
nbasis <- nage + norder - 2
wbasis <- create.bspline.basis(rng, nbasis, norder, age)
#  starting values for coefficient
cvec0 <- matrix(0,nbasis,1)
Wfd0  <- fd(cvec0, wbasis)
#  set up functional parameter object
Lfdobj    <- 3          #  penalize curvature of acceleration
lambda    <- 10^(-0.5)  #  smoothing parameter
growfdPar <- fdPar(Wfd0, Lfdobj, lambda)
#  Set up wgt vector
wgt   <- rep(1,nage)
#  Smooth the data for the first girl
hgt1 = growth$hgtf[,1]
result <- smooth.monotone(age, hgt1, growfdPar, wgt)
#  Extract the functional data object and regression
#  coefficients
Wfd  <- result$Wfdobj
beta <- result$beta
#  Evaluate the fitted height curve over a fine mesh
agefine <- seq(1,18,len=101)
hgtfine <- beta[1] + beta[2]*eval.monfd(agefine, Wfd)
#  Plot the data and the curve
plot(age, hgt1, type="p")
lines(agefine, hgtfine)
#  Evaluate the acceleration curve
accfine <- beta[2]*eval.monfd(agefine, Wfd, 2)
#  Plot the acceleration curve
plot(agefine, accfine, type="l")
lines(c(1,18),c(0,0),lty=4)




