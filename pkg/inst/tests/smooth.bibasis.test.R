library(fda)
snbasis = 15
sbasis  = create.bspline.basis(c(0,1),snbasis)
tnbasis = 15
tbasis  = create.bspline.basis(c(0,1),tnbasis)
lambdas = 1e-3
fdPars  = fdPar(sbasis, 2, lambdas)
lambdat = 1e-3
fdPart  = fdPar(tbasis, 2, lambdat)

ns = 15
nt = 15

sarg = seq(0,1,len=ns)
targ = seq(0,1,len=nt)

sigma = 0.2
y0 = outer(sin(2*pi*sarg),cos(2*pi*targ))
y  = y0 + rnorm(matrix(0, ns,nt))*sigma

result = smooth.bibasis(sarg, targ, y, fdPars, fdPart)
bifdobj = result$bifdobj

sfine = seq(0,1,len=51)
tfine = seq(0,1,len=51)

bimat = eval.bifd(sfine, tfine, bifdobj)

contour(bimat, xlab="s", ylab="t")
contour(y0,    xlab="s", ylab="t")
contour(y,     xlab="s", ylab="t")

persp(bimat, xlab="s", ylab="t")
