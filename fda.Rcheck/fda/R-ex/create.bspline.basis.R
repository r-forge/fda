### Name: create.bspline.basis
### Title: Create a B-spline Basis
### Aliases: create.bspline.basis
### Keywords: smooth

### ** Examples

##
## The simplest basis currently available with this function:
##
bspl1.1 <- create.bspline.basis(norder=1, breaks=2:3)
plot(bspl1.1)
# 1 basis function, order 1 = degree 0 = step function:  
# constant 1 between 2 and 3.  

bspl1.2 <- create.bspline.basis(norder=1, breaks=c(0,.5, 1))
plot(bspl1.2)
# 2 bases, order 1 = degree 0 = step functions:  
# (1) constant 1 between 0 and 0.5 and 0 otherwise
# (2) constant 1 between 0.5 and 1 and 0 otherwise.

bspl2.3 <- create.bspline.basis(norder=2, breaks=c(0,.5, 1))
plot(bspl2.3)
# 3 bases:  order 2 = degree 1 = linear 
# (1) line from (0,1) down to (0.5, 0), 0 after
# (2) line from (0,0) up to (0.5, 1), then down to (1,0)
# (3) 0 to (0.5, 0) then up to (1,1).

bspl3.4 <- create.bspline.basis(norder=3, breaks=c(0,.5, 1))
plot(bspl3.4)
# 4 bases:  order 3 = degree 2 = parabolas.  
# (1) (x-.5)^2 from 0 to .5, 0 after
# (2) 2*(x-1)^2 from .5 to 1, and a parabola
#     from (0,0 to (.5, .5) to match
# (3 & 4) = complements to (2 & 1).  

# Default B-spline basis
bSpl4.23 <- create.bspline.basis()
# Cubic bspline (norder=4) with nbasis=23,
# so nbreaks = nbasis-norder+2 = 21, 
# 2 of which are rangeval, leaving 19 Interior knots.

bSpl4. <- create.bspline.basis(c(-1,1))
# Same as bSpl4.23 but over (-1,1) rather than (0,1).

# set up the b-spline basis for the lip data, using 23 basis functions,
#   order 4 (cubic), and equally spaced knots.
#  There will be 23 - 4 = 19 interior knots at 0.05, ..., 0.95
lipbasis <- create.bspline.basis(c(0,1), 23)
all.equal(bSpl4.23, lipbasis)
# TRUE 
# plot the basis functions
plot(lipbasis)

bSpl.growth <- create.bspline.basis(growth$age)
# cubic spline (order 4) 

bSpl.growth6 <- create.bspline.basis(growth$age,norder=6)
# quintic spline (order 6) 



