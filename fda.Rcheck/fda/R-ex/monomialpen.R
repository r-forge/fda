### Name: monomialpen
### Title: Evaluate Monomial Roughness Penalty Matrix
### Aliases: monomialpen
### Keywords: smooth

### ** Examples


# set up a monomial basis for the first five powers
nbasis   <- 5
basisobj <- create.monomial.basis(c(-1,1),nbasis)
#  evaluate the rougness penalty matrix for the
#  second derivative.
penmat <- monomialpen(basisobj, 2)




