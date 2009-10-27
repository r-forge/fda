rngs = c(0,1)
rngt = c(0,2*pi)

nfine = 201
tfine = seq(rngt[1],rngt[2],len=nfine)

N = 100

snbasis = 8
sbasis  = create.bspline.basis(rngs,snbasis)

tnbasis = 10
tbasis  = create.bspline.basis(rngt,tnbasis)

alphanbasis = 20
alphabasis  = create.bspline.basis(rngt,alphanbasis)
sigma       = 1
alphacoef0  = sigma*rnorm(alphanbasis,1)
alphafd0    = fd(alphacoef0, alphabasis)

# alphanbasis = 1
# alphabasis  = create.constant.basis(rngt)
# alphacoef0  = 0
# alphafd0    = fd(alphacoef0, alphabasis)

betascoef0 = matrix(rnorm(snbasis),snbasis,1)
betasfd0   = fd(betascoef0,sbasis)

betatcoef0 = matrix(rnorm(tnbasis),tnbasis,1)
betatfd0   = fd(betatcoef0,tbasis)

xnbasis = 53
xbasis  = create.bspline.basis(rngs, xnbasis)
sigmaX  = 10
xfd     = fd(sigmaX*matrix(rnorm(xnbasis*N),xnbasis,N),xbasis)

sigmaE  = 0.0
enbasis = 53
ebasis  = create.bspline.basis(rngt, enbasis)
efd     = fd(matrix(rnorm(enbasis*N),enbasis,N)*sigmaE, ebasis)

Hmat     = inprod(xfd, sbasis)

betacoef0 = matrix(rnorm(snbasis*tnbasis), snbasis, tnbasis)

xcoef = t(Hmat %*% betacoef0)
xbetafd0 = fd(xcoef, tbasis)

yhatmat = eval.fd(tfine, alphafd0) %*% matrix(1,1,N) + 
          eval.fd(tfine, xbetafd0)
yhatfd0 = smooth.basis(tfine, yhatmat, ebasis)$fd
ymat = yhatmat + eval.fd(tfine, efd)
yfd  = smooth.basis(tfine, ymat, ebasis)$fd

betaList = vector("list",3)
betaList[[1]] = fdPar(alphabasis, 2, 1e4)
betaList[[2]] = fdPar(betasfd0, 2, 0)
betaList[[3]] = fdPar(betatfd0, 2, 1e4)

linmodList = linmod(xfd, yfd, betaList)

alphafd = linmodList$beta0estfd
betafd  = linmodList$beta1estbifd
yhatfd  = linmodList$yhatfdobj

print(rbind(alphafd$coef,alphafd0$coef))

par(mfrow=c(1,1))
plot(alphafd)
lines(alphafd0)

print(betafd$coef)
print(betacoef0)

par(mfrow=c(2,1))
plot(yhatfd)
plot(yhatfd0)

#  set up problem  for fRegress

xfdList = vector("list",1+snbasis)

xfdList[[1]] = matrix(1,N,1)
for (j in 2:(snbasis+1)) {
    xfdList[[j]] = Hmat[,j-1]
}

betaListfR = vector("list",1+snbasis)
betaListfR[[1]] = fdPar(alphafd0)
tfd0 = fd(matrix(1,tnbasis,1),tbasis)
tfdPar = fdPar(tfd0)
for (j in 2:(snbasis+1)) {
    betaListfR[[j]] = tfdPar
}

fregressList = fRegress(yfd, xfdList, betaListfR)

betaestList = fregressList$betaestlist

yhatfd = fregressList$yhatfdobj

par(mfrow=c(2,1))
plot(yhatfd)
plot(yhatfd0)

alphafdPar = betaestList[[1]]
alphafd = alphafdPar$fd

print(cbind(alphafd$coef,alphacoef0))

par(mfrow=c(1,1))
plot(alphafd)
lines(alphafd0)

for (j in 1:snbasis) {
    print(t(betaestList[[j+1]]$fd$coef))
}

print(betacoef0)




