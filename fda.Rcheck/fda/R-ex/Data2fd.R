### Name: Data2fd
### Title: Create a functional data object from data
### Aliases: Data2fd
### Keywords: smooth

### ** Examples

##
## Simplest possible example:  step function 
##
b1.1 <- create.bspline.basis(nbasis=1, norder=1)
# 1 basis, order 1 = degree 0 = step function

y12 <- 1:2
fd1.1 <- Data2fd(y12, basisobj=b1.1)
plot(fd1.1)
# fd1.1 = mean(y12) = 1.5 

fd1.1.5 <- Data2fd(y12, basisobj=b1.1, lambda=0.5)
eval.fd(seq(0, 1, .2), fd1.1.5)
# fd1.1.5 = sum(y12)/(n+lambda*integral(over arg=0 to 1 of 1))
#         = 3 / (2+0.5) = 1.2

##
## 3 step functions
##
b1.2 <- create.bspline.basis(nbasis=2, norder=1)
# 2 bases, order 1 = degree 0 = step functions
fd1.2 <- Data2fd(1:2, basisobj=b1.2)

op <- par(mfrow=c(2,1))
plot(b1.2, main='bases') 
plot(fd1.2, main='fit')
par(op) 
# A step function:  1 to 0.5, then 2 

##
## Simple oversmoothing
##
b1.3 <- create.bspline.basis(nbasis=3, norder=1)
fd1.3.5 <- Data2fd(y12, basisobj=b1.3, lambda=0.5)
plot(0:1, c(0, 2), type='n')
points(0:1, y12)
lines(fd1.3.5)
# Fit = penalized least squares with penalty = 
#          = lambda * integral(0:1 of basis^2),
#            which shrinks the points towards 0.
# X1.3 = matrix(c(1,0, 0,0, 0,1), 2)
# XtX = crossprod(X1.3) = diag(c(1, 0, 1))
# penmat = diag(3)/3
#        = 3x3 matrix of integral(over arg=0:1 of basis[i]*basis[j])
# Xt.y = crossprod(X1.3, y12) = c(1, 0, 2)
# XtX + lambda*penmat = diag(c(7, 1, 7)/6 
# so coef(fd1.3.5) = solve(XtX + lambda*penmat, Xt.y)
#                  = c(6/7, 0, 12/7)

##
## linear spline fit 
##
b2.3 <- create.bspline.basis(norder=2, breaks=c(0, .5, 1))
# 3 bases, order 2 = degree 1 =
# continuous, bounded, locally linear

fd2.3 <- Data2fd(0:1, basisobj=b2.3)
round(fd2.3$coefs, 4)
# (0, 0, 1), 
# though (0, a, 1) is also a solution for any 'a' 
op <- par(mfrow=c(2,1))
plot(b2.3, main='bases') 
plot(fd2.3, main='fit')
par(op)

# smoothing?  
fd2.3. <- Data2fd(0:1, basisobj=b2.3, lambda=1)
## Don't show: 
stopifnot(
## End Don't show
all.equal(as.vector(round(fd2.3.$coefs, 4)),
          c(0.0159, -0.2222, 0.8730) )
## Don't show: 
)
## End Don't show
# The default smoothing with spline of order 2, degree 1
# has nderiv = max(0, norder-2) = 0.
# Direct computations confirm that the optimal B-spline
# weights in this case are the numbers given above.  

op <- par(mfrow=c(2,1))
plot(b2.3, main='bases') 
plot(fd2.3., main='fit')
par(op)

##
## quadratic spline fit
##
b3.4 <- create.bspline.basis(norder=3, breaks=c(0, .5, 1))
# 4 bases, order 3 = degree 2 =
# continuous, bounded, locally quadratic 

fd3.4 <- Data2fd(0:1, basisobj=b3.4)
round(fd3.4$coefs, 4)
# (0, 0, 0, 1),
# but (0, a, b, 1) is also a solution for any 'a' and 'b' 
op <- par(mfrow=c(2,1))
plot(b3.4) 
plot(fd3.4)
par(op)

#  try smoothing?  
fd3.4. <- Data2fd(0:1, basisobj=b3.4, lambda=1)
round(fd3.4.$coef, 4)

op <- par(mfrow=c(2,1))
plot(b3.4) 
plot(fd3.4.)
par(op)

##
##  A simple Fourier example 
##
gaitbasis3 <- create.fourier.basis(nbasis=3)
# note:  'names' for 3 bases
gaitfd3 <- Data2fd(gait, basisobj=gaitbasis3)
# Note: dimanes for 'coefs' + basis[['names']]
# + 'fdnames'

#    set up the fourier basis
daybasis <- create.fourier.basis(c(0, 365), nbasis=65)
#  Make temperature fd object
#  Temperature data are in 12 by 365 matrix tempav
#    See analyses of weather data.

#  Convert the data to a functional data object
tempfd <- Data2fd(CanadianWeather$dailyAv[,,"Temperature.C"],
                  day.5, daybasis)
#  plot the temperature curves
plot(tempfd)

##
## Terrifying interpolation
##
hgtbasis <- with(growth, create.bspline.basis(range(age), 
                                              breaks=age, norder=6))
girl.data2fd <- with(growth, Data2fd(hgtf, age, hgtbasis))
age2 <- with(growth, sort(c(age, (age[-1]+age[-length(age)])/2)))
girlPred <- eval.fd(age2, girl.data2fd)
range(growth$hgtf)
range(growth$hgtf-girlPred[seq(1, by=2, length=31),])
# 5.5e-6 0.028 <
# The predictions are consistently too small
# but by less than 0.05 percent 

matplot(age2, girlPred, type="l")
with(growth, matpoints(age, hgtf))
# girl.data2fd fits the data fine but goes berzerk
# between points

# Smooth 
girl.data2fd1 <- with(growth, Data2fd(age, hgtf, hgtbasis, lambda=1))
girlPred1 <- eval.fd(age2, girl.data2fd1)

matplot(age2, girlPred1, type="l")
with(growth, matpoints(age, hgtf))

# problems splikes disappear 




