### Name: bsplinepen
### Title: B-Spline Penalty Matrix
### Aliases: bsplinepen
### Keywords: smooth

### ** Examples

##
## bsplinepen with only one basis function
##
bspl1.1 <- create.bspline.basis(nbasis=1, norder=1)
pen1.1 <- bsplinepen(bspl1.1, 0) 

##
## bspline pen for a cubic spline with knots at seq(0, 1, .1)
##
basisobj <- create.bspline.basis(c(0,1),13)
#  compute the 13 by 13 matrix of inner products of second derivatives
penmat <- bsplinepen(basisobj)



