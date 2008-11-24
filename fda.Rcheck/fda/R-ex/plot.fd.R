### Name: plot.fd
### Title: Plot a Functional Data Object
### Aliases: plot.fd plot.fdSmooth
### Keywords: smooth hplot

### ** Examples

##
## plot.df
##
#daytime   <- (1:365)-0.5
#dayrange  <- c(0,365)
#dayperiod <- 365
#nbasis     <- 65
#dayrange  <- c(0,365)

daybasis65 <- create.fourier.basis(c(0, 365), 65)
harmaccelLfd <- vec2Lfd(c(0,(2*pi/365)^2,0), c(0, 365))

harmfdPar     <- fdPar(daybasis65, harmaccelLfd, lambda=1e5)

daytempfd <- with(CanadianWeather, Data2fd(day.5, 
        dailyAv[,,"Temperature.C"], daybasis65)) 

#  plot all the temperature functions for the monthly weather data
plot(daytempfd, main="Temperature Functions")

## Not run: 
##D # To plot one at a time:  
##D # The following pauses to request page changes.
##D ## Don't show: 
##D # (Without 'dontrun', the package build process
##D # might encounter problems with the par(ask=TRUE)
##D # feature.)
##D ## End Don't show
##D plot(daytempfd, ask=TRUE)
## End(Not run)

##
## plot.fdSmooth
##
b3.4 <- create.bspline.basis(norder=3, breaks=c(0, .5, 1))
# 4 bases, order 3 = degree 2 =
# continuous, bounded, locally quadratic 
fdPar3 <- fdPar(b3.4, lambda=1)
# Penalize excessive slope Lfdobj=1;  
# (Can not smooth on second derivative Lfdobj=2 at it is discontinuous.)
fd3.4s0 <- smooth.basis(0:1, 0:1, fdPar3)

# using plot.fd directly 
plot(fd3.4s0$fd)

# same plot via plot.fdSmooth 
plot(fd3.4s0)




