library(fda)

##
## Slides 5-6:  fRegress.numeric:  Scalar Response
##
# Ramsay, Hooker, Graves, sec. 9.4.1

logAnnPrecip   = log10(apply(daily$precav,2,sum))

tempbasis65  = create.fourier.basis(c(0,365),65)
tempSmooth65 = smooth.basis(day.5, daily$tempav, tempbasis65)
tempfd65     = tempSmooth65$fd

templist      = vector("list",2)
templist[[1]] = rep(1,35)
templist[[2]] = tempfd65

conbasis   = create.constant.basis(c(0,365))
betabasis5 = create.fourier.basis(c(0,365),5)
betalist1  = vector("list",2)
betalist1[[1]] = conbasis
betalist1[[2]] = betabasis5

fRegressList1 = fRegress(logAnnPrecip, templist, betalist1)

betaestlist1  = fRegressList1$betaestlist
tempbetafd1   = betaestlist1[[2]]$fd

# Figure 9.1

plot(tempbetafd1, xlab="Day", ylab="Beta for temperature")

plot(tempbetafd1, xlab="", ylab="Beta for temperature", axes=FALSE,
     cex.lab=1.5)
axis(2, 0)
axisIntervals(cex.axis=1.5)
text(50, .0012, expression(hat(beta)(t)), cex=4)

##
## Slide 7.  fRegress.numeric: functional response, x = scalar
##

#  Section 10.1.1 Climate Region Effects on Temperature

(regions.         = unique(CanadianWeather$region)[c(3, 2, 4, 1)])
regions     = c("Canada", regions.)
p                = length(regions)
regionList       = vector("list", p)
names(regionList)= regions
regionList[[1]]  = c(rep(1,35),0)
# 35 locations plus a constraint
# that the region effects sum to zero

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
names(betaList)= regions
for (j in 1:p) betaList[[j]] = betafdPar

fRegressList= fRegress(temp36fd, regionList, betaList)
betaestList = fRegressList$betaestlist
regionFit   = fRegressList$yhatfd

# Figure 10.1

op          = par(mfrow=c(2,3),cex=1)
for (j in 1:p) plot(betaestList[[j]]$fd, lwd=2,
                    xlab="Day (July 1 to June 30)",
                    ylab="", main=regions[j])
plot(regionFit, lwd=2, col=1, lty=1,
     xlab="Day (July 1 to June 30)", ylab="", main="Prediction")
par(op)

dayFine <- 1:365
beta.rgn <- lapply(betaestList[-1], predict, newdata=dayFine)
str(beta.rgn)

rgns <- ordered(regions., levels=regions.)
predTemp <- data.frame(day=dayFine,
                       region=rep(rgns, each=365),
                       beta.region=unlist(beta.rgn))
str(predTemp)
sapply(predTemp, length)

library(lattice)
xyplot(beta.region~day | region, predTemp, layout=c(4, 1), type='l')

# x axis ticks & labels?  ???
#xyplot(beta.region~day | region, predTemp, layout=c(4, 1), type='l',
#       scales=list(bottom=list(ticks=seq(1, 365, length=5)),
#                   labels=list(at=seq(365/8, 7*365/8, length=4),
#                               labels=c("Q1", "Q2", "Q3", "Q4") ) ) )
#

xyplot(beta.region~day | region, predTemp, layout=c(4, 1), type='l',
       scales=list(x=seq(1, 7, 2)*365/4,
           labels=c('duhh', "Q1", "Q2", "Q3", "Q4") ),
       xlab='')

