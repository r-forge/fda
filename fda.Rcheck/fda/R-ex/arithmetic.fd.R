### Name: arithmetic.fd
### Title: Arithmetic on functional data ('fd') objects
### Aliases: arithmetic.fd +.fd plus.fd -.fd minus.fd *.fd times.fd
### Keywords: smooth

### ** Examples

##
## add a parabola to itself
##
bspl4 <- create.bspline.basis(nbasis=4)
parab4.5 <- fd(c(3, -1, -1, 3)/3, bspl4)
str(parab4.5+parab4.5)
# coefs = c(6, -2, -2, 6)/3
str(parab4.5-parab4.5)
# coefs = c(0, 0, 0, 0)

##
## Same example with interior knots at 1/3 and 1/5
##
bspl5.3 <- create.bspline.basis(breaks=c(0, 1/3, 1))
plot(bspl5.3)
x. <- seq(0, 1, .1)
para4.5.3 <- smooth.basis(x., 4*(x.-0.5)^2, fdParobj=bspl5.3)[['fd']]
plot(para4.5.3)

bspl5.2 <- create.bspline.basis(breaks=c(0, 1/2, 1))
plot(bspl5.2)
para4.5.2 <- smooth.basis(x., 4*(x.-0.5)^2, fdParobj=bspl5.2)[['fd']]
plot(para4.5.2)

str(para4.5.3+para4.5.2)
str(para4.5.3-para4.5.2)
str(para4.5.3*para4.5.2)
# interior knots of the sum
# = union(interior knots of the summands);
# ditto for difference and product.  




