###
###
### Ramsey & Silverman (2006) Functional Data Analysis, 2nd ed. (Springer)
###
### ch. 13.  Modeling Functional Responses with Multivariate Covariates
###
library(fda)
##
## Section 13.1.  Introduction
##

##
## Section 13.2.  Predicting Temperature Curves from climate Zones
##
#  p. 226, Figure 13.1.  Region effects for the temperature function
harmaccelLfd365 <- vec2Lfd(c(0,(2*pi/365)^2,0), c(0, 365))
smallbasis  <- create.fourier.basis(c(0, 365), 65)

tempfd      <- smooth.basis(day.5,
                            CanadianWeather$dailyAv[,,"Temperature.C"],
                            smallbasis)$fd
smallbasismat <- eval.basis(day.5, smallbasis)
y2cMap <- solve(crossprod(smallbasismat), t(smallbasismat))

#  names for climate zones

zonenames <- c("Canada  ",
               "Atlantic", "Pacific ", "Contintal", "Arctic  ")

#  Set up a design matrix having a column for (the grand mean, and
#    a column for (each climate zone effect. Add a dummy contraint
#    observation

regions <- c("Atlantic", "Pacific", "Continental", "Arctic")
zmat. <- outer(CanadianWeather$region, regions, "==")
dimnames(zmat.)[[2]] <- regions
zmat1 <- cbind(ones=1, zmat.)

#  attach a row of 0, 1, 1, 1, 1 to force zone
#  effects to sum to zero, and define first regression
#  function as grand mean for (all stations

zmat <- rbind(zmat1, zoneconstraint=c(0, 1, 1, 1, 1))

#  revise YFDOBJ by adding a zero function

coef   <- tempfd$coefs
str(coef)
# add a 0 column # 36 to coef
coef36 <- cbind(coef,zero=0)
str(coef36)
tempfd$coefs <- coef36

# Convert zmat to a list
xfdlist <- as.list(as.data.frame(zmat))
str(xfdlist)

#  set up the basis for (the regression functions

nbetabasis <- 11
betabasis  <- create.fourier.basis(c(0, 365), nbetabasis)

#  set up the functional parameter object for (the regression fns.

betafd    <- fd(matrix(0,nbetabasis,1), betabasis)
estimate  <- TRUE
lambda    <- 0
betafdPar <- fdPar(betafd, harmaccelLfd365, lambda, estimate)

p <- length(xfdlist)
betalist <- vector("list",p)
names(betalist) <- names(xfdlist)
for (j in 1:p) betalist[[j]] <- betafdPar

#  compute regression coefficient functions and
#  predicted functions

fRegressList <- fRegress(tempfd, xfdlist, betalist)

#  plot regression functions

betaestlist <- fRegressList$betaestlist

op <- par(mfrow=c(2,2))
ylim <- c(-25, 25)
for (j in 2:p) {
	betaestParfdj <- betaestlist[[j]]
	plot(betaestParfdj$fd, xlab="Day", ylab="Temp.",
	     main=zonenames[j], ylim=ylim)
        abline(h=0, lty='dashed')
}
par(op)

#  plot predicted functions

yhatfdobj <- fRegressList$yhatfdobj
plot(yhatfdobj,main='Predicted Temperature',)

#  compute residual matrix and get covariance of residuals

yhatmat  <- eval.fd(day.5, yhatfdobj)
ymat     <- eval.fd(day.5, tempfd)
temprmat <- ymat[,1:35] - yhatmat[,1:35]
SigmaE   <- var(t(temprmat))

#  plot covariance surface for errors

par(mfrow=c(1,1))
contour(SigmaE, xlab="Day", ylab="Day")
lines(c(0, 365), c(0, 365),lty=4)

#  plot standard deviation of errors

par(mfrow=c(1,1), mar=c(5,5,3,2), pty="m")
stddevE <- sqrt(diag(SigmaE))
plot(day.5, stddevE, type="l",
     xlab="Day", ylab="Standard error (deg C)")

#  Repeat regression, this time outputting results for
#  confidence intervals

stderrList <- fRegress.stderr(fRegressList, y2cMap, SigmaE)

betastderrlist <- stderrList$betastderrlist

#  plot regression function standard errors

op <- par(mfrow=c(2,3), pty="s")
for (j in 1:p) {
	betastderrj <- eval.fd(day.5, betastderrlist[[j]])
	plot(day.5, betastderrj,
	        type="l",lty=1, xlab="Day", ylab="Reg. Coeff.",
	        main=zonenames[j])
}
par(op)

#  plot regression functions with confidence limits

op <- par(mfrow=c(2,2))
for (j in 2:p) {
	betafdParj  <- betaestlist[[j]]
	betafdj     <- betafdParj$fd
	betaj       <- eval.fd(day.5, betafdj)
	betastderrj <- eval.fd(day.5, betastderrlist[[j]])
	matplot(day.5, cbind(betaj, betaj+2*betastderrj, betaj-2*betastderrj),
	        type="l",lty=c(1,4,4), xlab="Day", ylab="Reg. Coeff.",
	        main=zonenames[j], ylim=ylim)
}
par(op)

# Figure 13.2
op <- par(mfrow=c(2,2))
ylim2 <- c(-30, 25)
beta0 <- betaestlist[[1]]$fd
for (j in 2:p) {
	betaj <- betaestlist[[j]]$fd
	plot(beta0+betaj, xlab="Day", ylab="Temp.",
	     main=zonenames[j], ylim=ylim2)
        lines(beta0, lty='dashed')
}
par(op)

# p. 227

sapply(fRegressList, class)
