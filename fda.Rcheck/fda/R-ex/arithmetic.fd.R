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
## Don't show: 
stopifnot(
## End Don't show
all.equal(coef(parab4.5+parab4.5), matrix(c(6, -2, -2, 6)/3, 4))
## Don't show: 
)
## End Don't show
## Don't show: 
stopifnot(
## End Don't show
all.equal(coef(parab4.5-parab4.5), matrix(rep(0, 4), 4))
## Don't show: 
)
## End Don't show

##
## Same example with interior knots at 1/3 and 1/2
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
## Don't show: 
stopifnot(
## End Don't show
all.equal(coef(para4.5.3-para4.5.2), matrix(0, 9, 1))
## Don't show: 
)
## End Don't show

str(para4.5.3*para4.5.2)
# interior knots of the sum
# = union(interior knots of the summands);
# ditto for difference and product.
plot(para4.5.3*para4.5.2)

##
## fd+numeric
##
## Don't show: 
stopifnot(
## End Don't show
all.equal(coef(parab4.5+1), matrix(c(6, 2, 2, 6)/3, 4))
## Don't show: 
)
## End Don't show

## Don't show: 
stopifnot(
## End Don't show
all.equal(1+parab4.5, parab4.5+1)
## Don't show: 
)
## End Don't show

##
## fd-numeric
##
## Don't show: 
stopifnot(
## End Don't show
all.equal(coef(-parab4.5), matrix(c(-3, 1, 1, -3)/3, 4))
## Don't show: 
)
## End Don't show

plot(parab4.5-1)

plot(1-parab4.5)




