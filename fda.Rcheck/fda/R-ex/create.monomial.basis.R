### Name: create.monomial.basis
### Title: Create a Monomial Basis
### Aliases: create.monomial.basis
### Keywords: smooth

### ** Examples

##
## simplest example: one constant 'basis function' 
##
m0 <- create.monomial.basis(nbasis=1, exponents=0)
plot(m0)

##
## Create a monomial basis over the interval [-1,1]
##  consisting of the first three powers of t
##
basisobj <- create.monomial.basis(c(-1,1), 3)
#  plot the basis
plot(basisobj)



