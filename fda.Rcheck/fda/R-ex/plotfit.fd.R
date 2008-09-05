### Name: plotfit
### Title: Plot a Functional Data Object With Data
### Aliases: plotfit.fd plotfit.fdSmooth
### Keywords: smooth hplot

### ** Examples

daybasis65 <- create.fourier.basis(c(0, 365), 65)

daytempfd <- with(CanadianWeather, data2fd(
       dailyAv[,,"Temperature.C"], day.5, 
       daybasis65, argnames=list("Day", "Station", "Deg C")) )
 
with(CanadianWeather, plotfit.fd(dailyAv[, , "Temperature.C"],
     argvals= day.5, daytempfd, index=1, titles=place, axes=FALSE) )
# Default ylab = daytempfd[['fdnames']] 

with(CanadianWeather, plotfit.fd(dailyAv[, , "Temperature.C", drop=FALSE],
     argvals= day.5, daytempfd, index=1, titles=place, axes=FALSE) )
# Better:  ylab = dimnames(y)[[3]]

# Label the horizontal axis with the month names
axis(1, monthBegin.5, labels=FALSE)
axis(1, monthEnd.5, labels=FALSE)
axis(1, monthMid, monthLetters, tick=FALSE)
axis(2)

## Not run: 
##D # The following pauses to request page changes.
##D # (Without 'dontrun', the package build process
##D # might encounter problems with the par(ask=TRUE)
##D # feature.)
##D with(CanadianWeather, plotfit.fd(
##D           dailyAv[,, "Temperature.C"], day.5,
##D           daytempfd, ask=TRUE) )
## End(Not run)

# If you want only the fitted functions, use plot(daytempfd)

# To plot only a single fit vs. observations, use index
# to request which one you want.  

op <- par(mfrow=c(2,1), xpd=NA, bty="n")
# xpd=NA:  clip lines to the device region,
#       not the plot or figure region
# bty="n":  Do not draw boxes around the plots.  
ylim <- range(CanadianWeather$dailyAv[,,"Temperature.C"])
# Force the two plots to have the same scale 
with(CanadianWeather, plotfit.fd(dailyAv[,,"Temperature.C"], day.5, 
          daytempfd, index=2, titles=place, ylim=ylim, axes=FALSE) )
axis(1, monthBegin.5, labels=FALSE)
axis(1, monthEnd.5, labels=FALSE)
axis(1, monthMid, monthLetters, tick=FALSE)
axis(2)

with(CanadianWeather, plotfit.fd(dailyAv[,,"Temperature.C"], day.5, 
          daytempfd, index=35, titles=place, ylim=ylim) )
axis(1, monthBegin.5, labels=FALSE)
axis(1, monthEnd.5, labels=FALSE)
axis(1, monthMid, monthLetters, tick=FALSE)
axis(2)
par(op)

# plot residuals
with(CanadianWeather, plotfit.fd(dailyAv[, , "Temperature.C"], 
          day.5, daytempfd, residual=TRUE) )
# Can't read this, so try with 2 lines per page with ask=TRUE, 
# and limiting length(col), length(lty), etc. <=2
## Not run: 
##D with(CanadianWeather, plotfit.fd(
##D           dailyAv[,,"Temperature.C"], day.5, 
##D           daytempfd, residual=TRUE, col=1:2, lty=1, ask=TRUE) )
## End(Not run)




