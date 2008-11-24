### Name: Fperm.fd
### Title: Permutation F-test for functional linear regression.
### Aliases: Fperm.fd
### Keywords: smooth

### ** Examples

# The very simplest example is the equivalent of the permutation
# t-test on the growth data. 

# First set up a basis system to hold the smooths

knots  <- growth$age
norder <- 6
nbasis <- length(knots) + norder - 2
hgtbasis <- create.bspline.basis(range(knots), nbasis, norder, knots)

# Now smooth with a fourth-derivative penalty and a very small smoothing
# parameter

Lfdobj <- 4
lambda <- 1e-2
growfdPar <- fdPar(hgtbasis, Lfdobj, lambda)

hgtfd <- smooth.basis(growth$age, cbind(growth$hgtm,growth$hgtf),growfdPar)$fd

# Now set up factors for fRegress:

cbasis = create.constant.basis(range(knots))

maleind = c(rep(1,ncol(growth$hgtm)),rep(0,ncol(growth$hgtf)))

constfd = fd( matrix(1,1,length(maleind)),cbasis)
maleindfd = fd( matrix(maleind,1,length(maleind)),cbasis)

xfdlist = list(constfd,maleindfd)

# The fdPar object for the coefficients and call Fperm.fd

betalist = list(fdPar(hgtbasis,2,1e-6),fdPar(hgtbasis,2,1e-6))

Fres = Fperm.fd(hgtfd,xfdlist,betalist)




