### Name: getbasismatrix
### Title: Values of Basis Functions or their Derivatives
### Aliases: getbasismatrix
### Keywords: smooth

### ** Examples

# Minimal example:  a B-spline of order 1, i.e., a step function
# with 0 interior knots:
bspl1.1 <- create.bspline.basis(norder=1, breaks=0:1)
getbasismatrix(seq(0, 1, .2), bspl1.1)




