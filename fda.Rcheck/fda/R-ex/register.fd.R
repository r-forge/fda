### Name: register.fd
### Title: Register Functional Data Objects Using a Continuous Criterion
### Aliases: register.fd
### Keywords: smooth

### ** Examples

#See the analyses of the growth data for examples.
##
## 1.  Simplest call
##
# Specify smoothing weight 
lambda.gr2.3 <- .03

# Specify what to smooth, namely the rate of change of curvature
Lfdobj.growth    <- 2 

# Establish a B-spline basis
nage <- length(growth$age)
norder.growth <- 6
nbasis.growth <- nage + norder.growth - 2
rng.growth <- range(growth$age)
# 1 18 
wbasis.growth <- create.bspline.basis(rangeval=rng.growth,
                   nbasis=nbasis.growth, norder=norder.growth,
                   breaks=growth$age)

# Smooth consistent with the analysis of these data
# in afda-ch06.R, and register to individual smooths:  
cvec0.growth <- matrix(0,nbasis.growth,1)
Wfd0.growth  <- fd(cvec0.growth, wbasis.growth)
growfdPar2.3 <- fdPar(Wfd0.growth, Lfdobj.growth, lambda.gr2.3)
# Create a functional data object for all the boys
hgtmfd.all <- with(growth, smooth.basis(age, hgtm, growfdPar2.3))

nBoys <- 2
# nBoys <- dim(growth[["hgtm"]])[2]
# register.fd takes time, so use only 2 curves as an illustration
# to minimize compute time in this example;  

#Alternative to subsetting later is to subset now:  
#hgtmfd.all<-with(growth,smooth.basis(age, hgtm[,1:nBoys],growfdPar2.3))

# Register the growth velocity rather than the
# growth curves directly 
smBv <- deriv(hgtmfd.all$fd, 1)

# This takes time, so limit the number of curves registered to nBoys

## Not run: 
##D smB.reg.0 <- register.fd(smBv[1:nBoys])
##D 
##D smB.reg.1 <- register.fd(smBv[1:nBoys],WfdParobj=c(Lfdobj=Lfdobj.growth, lambda=lambda.gr2.3))
##D 
##D ##
##D ## 2.  Call providing the target
##D ##
##D 
##D smBv.mean <- deriv(mean(hgtmfd.all$fd[1:nBoys]), 1)
##D smB.reg.2a <- register.fd(smBv.mean, smBv[1:nBoys],
##D                WfdParobj=c(Lfdobj=Lfdobj.growth, lambda=lambda.gr2.3))
##D 
##D smBv.mean <- mean(smBv[1:nBoys]) 
##D smB.reg.2 <- register.fd(smBv.mean, smBv[1:nBoys],
##D                WfdParobj=c(Lfdobj=Lfdobj.growth, lambda=lambda.gr2.3))
##D all.equal(smB.reg.1, smB.reg.2) 
##D 
##D ##
##D ## 3.  Call using WfdParobj
##D ##
##D 
##D # Create a dummy functional data object
##D # to hold the functional data objects for the
##D # time warping function
##D # ... start with a zero matrix (nbasis.growth, nBoys) 
##D smBc0 <- matrix(0, nbasis.growth, nBoys)
##D # ... convert to a functional data object 
##D smBwfd0 <- fd(smBc0, wbasis.growth)
##D # ... convert to a functional parameter object 
##D smB.wfdPar <- fdPar(smBwfd0, Lfdobj.growth, lambda.gr2.3)
##D 
##D smB.reg.3<- register.fd(smBv[1:nBoys], WfdParobj=smB.wfdPar)
##D all.equal(smB.reg.1, smB.reg.3)
## End(Not run)




