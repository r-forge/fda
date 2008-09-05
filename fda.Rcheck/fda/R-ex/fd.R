### Name: fd
### Title: Define a Functional Data Object
### Aliases: fd
### Keywords: smooth internal

### ** Examples

##
## The simplest b-spline basis:  order 1, degree 0, zero interior knots:  
##       a single step function 
##
bspl1.1 <- create.bspline.basis(norder=1, breaks=0:1)
fd.bspl1.1 <- fd(0, basisobj=bspl1.1)

fd.bspl1.1a <- fd(basisobj=bspl1.1)
## Don't show: 
 stopifnot( 
## End Don't show
all.equal(fd.bspl1.1, fd.bspl1.1a)
## Don't show: 
 ) 
## End Don't show
# TRUE

## Not run: 
##D fd.bspl1.1b <- fd(0)
##D Error in fd(0) : 
##D   Number of coefficients does not match number of basis functions.
##D 
##D ... because fd by default wants to create a cubic spline 
## End(Not run)
##
## Cubic spline:  4  basis functions 
##
bspl4 <- create.bspline.basis(nbasis=4)
plot(bspl4) 
parab4.5 <- fd(c(3, -1, -1, 3)/3, bspl4)
# = 4*(x-.5)^2
plot(parab4.5) 




