### Name: create.polynomial.basis
### Title: Create a Polynomial Basis
### Aliases: create.polynomial.basis
### Keywords: smooth

### ** Examples

#  Create a polynomial basis over the years in the 20th century
#  and center the basis functions on 1950.
basisobj <- create.polynomial.basis(c(1900, 2000), nbasis=3, ctr=1950)
#  plot the basis
# The following should work but doesn't;  2007.05.01
#plot(basisobj)



