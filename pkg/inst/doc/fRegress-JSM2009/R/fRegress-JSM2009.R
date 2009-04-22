library(fda)

# fRegress.numeric:  Scalar Response

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





