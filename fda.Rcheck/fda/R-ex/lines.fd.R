### Name: lines.fd
### Title: Add Lines from Functional Data to a Plot
### Aliases: lines.fd lines.fdSmooth
### Keywords: smooth

### ** Examples

##
## plot a fit with 3 levels of smoothing
##
x <- seq(-1,1,0.02)
y <- x + 3*exp(-6*x^2) + sin(1:101)/2
# sin not rnorm to make it easier to compare
# results across platforms 

result4. <- smooth.basisPar(argvals=x, y=y, lambda=1)
result4.4 <- smooth.basisPar(argvals=x, y=y, lambda=1e-4)
result4.0 <- smooth.basisPar(x, y, lambda=0)

plot(x, y)
lines(result4.)
lines(result4.4, col='green')
lines.fdSmooth(result4.0, col='red') 

plot(x, y, xlim=c(0.5, 1))
lines.fdSmooth(result4.)
lines.fdSmooth(result4.4, col='green')
lines.fdSmooth(result4.0, col='red')  
lines.fdSmooth(result4.0, col='red', nx=101)
# no visible difference from the default?  

lines.fdSmooth(result4.0, col='orange', nx=31)
# Clear difference, especially between 0.95 and 1  




