###
###
### Ramsey, Hooker & Graves (2009)
### Functional Data Analysis with R and Matlab (Springer)
###
### ch.  10.  Linear Models for Functional Responses
###
library(fda)

##
## Section 10.1  Functional Responses and an Analysis of Variance Model
##
#  Section 10.1.1 Climate Region Effects on Temperature
regions.          = unique(CanadianWeather$region)
p                = length(regions.) + 1
regionList       = vector("list", p)
names(regionList)= regions.
regionList[[1]]  = c(rep(1,35),0)
for (j in 2:p) {
  xj             = (CanadianWeather$region == regions.[j-1])
  regionList[[j]]= c(xj,1)
}

# tempfd from chapter 9

Lcoef       = c(0,(2*pi/365)^2,0)
harmaccelLfd= vec2Lfd(Lcoef, c(0,365))
tempbasis   = create.fourier.basis(c(0, 365), 65)
lambda      = 1e6
tempfdPar65 = fdPar(tempbasis, harmaccelLfd, lambda)
tempShifted = daily$tempav[dayOfYearShifted, ]
tempSmooth65= smooth.basis(day.5, tempShifted, tempfdPar65)
tempfd      = tempSmooth65$fd

coef    = tempfd$coef
coef36  = cbind(coef,matrix(0,65,1))
temp36fd= fd(coef36,tempbasis,tempfd$fdnames)

betabasis      = create.fourier.basis(c(0, 365), 11)
betafdPar      = fdPar(betabasis)
betaList       = vector("list",p)
names(betaList)= regions.
for (j in 1:p) betaList[[j]] = betafdPar

fRegressList= fRegress(temp36fd, regionList, betaList)
betaestList = fRegressList$betaestlist
regionFit   = fRegressList$yhatfd
regions     = c("Canada", regions.)
op          = par(mfrow=c(2,3),cex=1)
for (j in 1:p) plot(betaestList[[j]]$fd, lwd=2,
                    xlab="Day (July 1 to June 30)",
                    ylab="", main=regions[j])
plot(regionFit, lwd=2, col=1, lty=1,
     xlab="Day (July 1 to June 30)", ylab="", main="Prediction")
par(op)

# 10.1.2 Trends in Sea Bird Populations on Kodiak Island

#  select only the data for sites Uyak and Uganik, which have data
#  from 1986 to 2005, except for 1998
sel = (seabird$Bay %in% c('Uganik', 'Uyak'))
UU  = seabird[sel,]

# Drop 2 species with many NAs

NAs  = sapply(UU, function(x)sum(is.na(x)))
NAs. = which(NAs > 2)
birdindex= (1:15)[-NAs.]
birds = names(UU)[birdindex]

#  select the columns with counts and
#  the years on which counts were taken

meanCounts = matrix(NA, 20, 13)
dimnames(meanCounts) = list(1986:2005, birds)

for(i in 1:20){
  sel <- (UU$Year == rownames(meanCounts)[i])
  meanCounts[i, ] <- sapply(UU[sel, birds], mean, na.rm=TRUE)
}

selYear <- !is.na(meanCounts[, 1])
logCounts <- log10(meanCounts[selYear,])
yearObs <- as.numeric(rownames(logCounts))
yearCode <- (1:20)[selYear]

op <- par(cex=1.3)
matplot(yearObs, logCounts, type='b', xlab='Year',
        ylab='log10(Mean count)', col=1, lty=1)
par(op)

birdSmooth <- Data2fd(yearCode, logCounts)

shellfish = as.numeric((1:13) %in% c(1,2,5,6,12,13))

fitShellfish <- fRegress(birdSmooth~shellfish)

betaestlist <- fitShellfish$betaestlist

sapply(betaestlist, class)

op <- par(mfrow=c(2,1))
plot(betaestlist$const$fd)
plot(betaestlist$shellfish$fd)
par(op)








birdSmooth <- smooth.basisPar(yearCode, logCounts)



birdlabels= colnames(UyakUganik)[birdindex]
nbird     = length(birdlabels)

# Specific plots of ground at which counts were taken.
transect  = unique(UU$Transect)

#  indices for years except for 1998, for which all data are missing
no98     = c(1:12,14:20)
birdtime = no98 - 1;

#  set up a basis for smoothing the counts.
#  this basis is for order 4 (cubic) B-splines, with a knot at
#  each year, except for 1998 when there were no observations

birdbasis  = create.bspline.basis(breaks=birdtime)
nbirdbasis = birdbasis$nbasis

#  set up arrays for accumulated coefficients and counts

birdcoefmat  = matrix(0,nbirdbasis,2*nbird)
birdcountmat = matrix(0,20,2*nbird)
birdnmat     = birdcountmat

# Smooth on curvature with lambda = 10

birdfdPar = fdPar(birdbasis, Lfdobj=2, lambda=1e1)

#  accumulate coefficients and counts for Uyak

Uyak       = (UU$Bay %in% 'Uyak')
tUy        = (transect %in% UU$Transect[Uyak])
transUyak  = transect[tUy]
transUganik= transect[!tUy]

for (i in transUyak) {
    #  select the data and count years for this transect
    index    = UU$Transect == i
    Seabirdi = UU[index,c(birdindex,16)]
    timei    = Seabirdi[,nbird+1]
    #  loop through the birds to be used, smoothing each in turn
    for (j in 1:nbird) {
#  select times corresponding to non-missing count data
        indexj        = !is.na(Seabirdi[,j])
        onesj         = rep(1,19)
        onesj[is.na(Seabirdi[,j])] = 0
        timeij        = timei[indexj]-1986
        Seabirdij     = Seabirdi[indexj,j]
#  smooth the data, selecting the functional data object to output
        Seabirdfdij   =
            smooth.basis(timeij, Seabirdij, birdfdPar)$fd
        birdcoefmat[         ,j] =
           birdcoefmat[         ,j] + Seabirdfdij$coef
        birdcountmat[timeij+1,j] =
           birdcountmat[timeij+1,j] + Seabirdij
        birdnmat[no98,j]         =
           birdnmat[no98,j]         + onesj
    }
}

#  accumulate coefficients and counts for Uganik

for (i in transUganik) {
    #  select the data and count years for this transect
    index    = UU$Transect == i
    Seabirdi = UU[index,c(birdindex,16)]
    timei    = Seabirdi[,nbird+1]
    #  loop through the birds to be used, smoothing each in turn
    for (j in 1:nbird) {
#  select times corresponding to non-missing count data
        indexj      = !is.na(Seabirdi[,j])
        onesj       = rep(1,19)
        onesj[is.na(Seabirdi[,j])] = 0
        timeij      = timei[indexj]-1986
        Seabirdij   = Seabirdi[indexj,j]
#  smooth the data, selecting the functional data object to output
        Seabirdfdij   =
            smooth.basis(timeij, Seabirdij, birdfdPar)$fd
        birdcoefmat[         ,j+nbird] =
          birdcoefmat[         ,j+nbird] + Seabirdfdij$coef
        birdcountmat[timeij+1,j+nbird] =
          birdcountmat[timeij+1,j+nbird] + Seabirdij
        birdnmat[        no98,j+nbird] =
          birdnmat[        no98,j+nbird] + onesj
    }
}

#  normalize coefficients and counts by dividing by number of transects

ind1 = 1:nbird
ind2 = ind1 + nbird
birdcoefmat[,ind1]  = birdcoefmat[,ind1]/79
birdcoefmat[,ind2]  = birdcoefmat[,ind2]/50
birdcountmat[,ind1] = birdcountmat[,ind1]/79
birdcountmat[,ind2] = birdcountmat[,ind2]/50

#  get total counts

birdtotalcount = birdcountmat[,ind1] +  birdcountmat[,ind2]

# Figure 10.1

op <- par(cex=1.2)
matplot(birdtime+1986, log10(birdtotalcount[c(1:12,14:20),]),
        type="b", lty=1, col=1, lwd=2, xlab="Year",
        ylab="log10(Mean count)")
par(op)

#  replace 0 counts in 1998 by NA's

birdcountmat[13,] = NA

birdfdnames = list("Year", "Birds and sites", "Mean count per transect")
birdfd      = fd(birdcoefmat, birdbasis, birdfdnames)

plot(birdfd)

plotyear = seq(0,19,len=101)
birdmat  = eval.fd(plotyear, birdfd)




# Figure 10.2

fooddummy1 = matrix(0,13,1)
foodindex = c(1,2,5,6,12,13)
fooddummy1[foodindex] = 1
fooddummy = rbind(fooddummy1, fooddummy1)
birddummy = diag(rep(1,13))

Zmat         = matrix(0,28,15)
Zmat[1:26,1] = rep(1,26)
Zmat[1:26,2] = fooddummy

Zmat[ 1:13,3:15] = birddummy
Zmat[14:26,3:15] = birddummy

Zmat[27,  foodindex+2 ] = 1
Zmat[28,-(foodindex+2)] = 1

logbirdcoef = logbirdfd$coefs
logbirdcoef0 = cbind(logbirdcoef,
matrix(0,nbirdbasis,2))
logbirdfd0 = fd(logbirdcoef0,birdbasis)

p = 15
xfdlist = vector("list",p)
for (j in 1:p) xfdlist[[j]] = Zmat[,j]
betalist = vector("list",p)
foodbasis = create.bspline.basis(c(0,19),5)
betalist[[1]] = fdPar(foodbasis)
betalist[[2]] = fdPar(foodbasis)
birdbasis = create.constant.basis(c(0,19))
for (j in 3:p) betalist[[j]] = fdPar(birdbasis)

fRegressList = fRegress(logbirdfd0,xfdlist,betalist)
betaestlist = fRegressList$betaestlist
yhatfdobj = fRegressList$yhatfdobj

# Figure 10.3 ???




# Section 10.1.3 Choosing Smoothing Parameters

loglam = seq(-2,0,0.25)
SSE.CV = rep(0,length(loglam))
betafdPari = betafdPar
for(i in 1:length(loglam)){
  betafdPari$lambda = 10^loglam[i]
  betalisti = betalist
  for (j in 1:2) betalisti[[j]] <- betafdPari

# *** Need logbirdfd0

  SSE.CV[i] <- fRegress.CV(logbirdfd0, xfdlist,
                           betalisti,CVobs=1:26)$SSE.CV
}

# Figure 10.4



##
## Section 10.2 Functional Responses with Functional Predictors:
##              The Concurrent Model
##
#  Section 10.2.1 Estimation for the Concurrent Model

#  Section 10.2.2 Confidence Intervals for Regression Functions

yhatmat = eval.fd(plotyear, yhatfdobj)
ymat = eval.fd(plotyear, logbirdfd0)

rmat = ymat - yhatmat
SigmaE = var(t(rmat))
stddevE = sqrt(diag(SigmaE))
SigmaE = diag(stddevEˆ2)

birdbasismat = eval.basis(plotyear, birdbasis)
y2cMap = solve(crossprod(birdbasismat)), t(birdbasismat))

stderrList = fRegress.stderr(fRegressList, y2cMap,
     SigmaE)
betastderrlist = stderrList$betastderrlist

par(mfrow=c(2,1),ask=FALSE)
titlelist = vector("list", p)
titlelist[[1]] = "Intercept"
titlelist[[2]] = "Feed effect"
plotbeta(betaestlist, betastderrlist,
        titlelist=titlelist, index=1:2)

# Section 10.2.3 Knee Angle Predicted from Hip Angle

xfdlist = list(rep(1,39), hipfd)

# *** Where's hipfd ???

betafdPar = fdPar(gaitbasis, harmaccelLfd)
betalist = list(betafdPar,betafdPar)
fRegressList = fRegress(kneefd, xfdlist, betalist)
kneehatfd = fRegressList$yhatfd
betaestlist = fRegressList$betaestlist

kneemat = eval.fd(gaittime, kneefd)

kneehatmat = eval.fd(gaittime, kneehatfd)
resmat = kneemat - kneehatmat
SigmaE = cov(t(resmat))

kneefinemat = eval.fd(gaitfine, kneefd)
kneemeanvec = eval.fd(gaitfine, kneemeanfd)
kneehatfinemat = eval.fd(gaitfine, kneehatfd)
resmat = kneefinemat - kneehatfinemat
resmat0 = kneefinemat -
kneemeanvec %*% matrix(1,1,ncurve)
SSE0 = apply((resmat0)ˆ2, 1, sum)
SSE1 = apply(resmatˆ2, 1, sum)
Rsqr = (SSE0-SSE1)/SSE0

fRegressList1 = fRegress(kneefd, xfdlist, betalist,
           y2cMap, SigmaE)

fRegressList2 = fRegress.stderr(fRegressList1, y2cMap, SigmaE)
betastderrlist = fRegressList2$betastderrlist
titlelist = list("Intercept", "Hip coefficient")
plotbeta(betaestlist, betastderrlist, gaitfine, titlelist)

##
## Section 10.3 Beyond the Concurrent Model
##
#  (no computations in this section)

##
## Section 10.4 A Functional Linear Model for Swedish Mortality
##
betabasis = create.bspline.basis(c(0,80),23)
beta0Par = fdPar(betabasis, 2, 1e-5)
beta1sPar = fdPar(betabasis, 2, 1e3)
beta1tPar = fdPar(betabasis, 2, 1e3)
betaList = list(beta0Par, beta1sPar, beta1tPar)

linmodSmooth = linmod(NextYear, LastYear, betaList)

# Where's LastYear?  ???

# Figure 10.11






##
## Section 10.5 Permutation Tests of Functional Hypotheses
##
#  Section 10.5.1 Functional t-Tests

tperm.fd(hgtmfd,hgtffd)

# Figure 10.12

# Section 10.5.2 Functional F-Tests

F.res = Fperm.fd(temp36fd, regionlist, betaList)









##
## 10.6 Details for R Functions fRegress, fRegress.CV and fRegress.stderr
##
help(fRegress)
help(fRegress.CV)
help(fRegress.stderr)

##
## 10.7 Details for Function plotbeta
##
help(plotbeta)

##
## 10.8 Details for Function linmod
##
help(linmod)

##
## 10.9 Details for Functions Fperm.fd and tperm.fd
##
help(Fperm.fd)
help(tperm.fd)

##
## Section 10.10 Some Things to Try
##
# (exercises for the reader)

##
## Section 10.11  More to Read
##
