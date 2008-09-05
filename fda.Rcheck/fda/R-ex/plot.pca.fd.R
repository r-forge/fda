### Name: plot.pca.fd
### Title: Plot Functional Principal Components
### Aliases: plot.pca.fd
### Keywords: smooth

### ** Examples


#  carry out a PCA of temperature
#  penalize harmonic acceleration, use varimax rotation

daybasis65 <- create.fourier.basis(c(0, 365), nbasis=65, period=365)

harmaccelLfd <- vec2Lfd(c(0,(2*pi/365)^2,0), c(0, 365))
harmfdPar     <- fdPar(daybasis65, harmaccelLfd, lambda=1e5)
daytempfd <- data2fd(CanadianWeather$dailyAv[,,"Temperature.C"],
      day.5, daybasis65, argnames=list("Day", "Station", "Deg C"))

daytemppcaobj <- pca.fd(daytempfd, nharm=4, harmfdPar)
#  plot harmonics, asking before each new page after the first:  
plot.pca.fd(daytemppcaobj)

# plot 4 on 1 page
op <- par(mfrow=c(2,2))
plot.pca.fd(daytemppcaobj, cex.main=0.9)
par(op)




