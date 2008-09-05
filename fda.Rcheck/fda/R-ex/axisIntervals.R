### Name: axisIntervals
### Title: Mark Intervals on a Plot Axis
### Aliases: axisIntervals
### Keywords: smooth hplot

### ** Examples

daybasis65 <- create.fourier.basis(c(0, 365), 65)

daytempfd <- with(CanadianWeather, data2fd(
       dailyAv[,,"Temperature.C"], day.5,
       daybasis65, argnames=list("Day", "Station", "Deg C")) )
 
with(CanadianWeather, plotfit.fd(
      dailyAv[,,"Temperature.C"], argvals=day.5,
          daytempfd, index=1, titles=place, axes=FALSE) )
# Label the horizontal axis with the month names
axisIntervals(1) 
axis(2)
# Depending on the physical size of the plot,
# axis labels may not all print.
# In that case, there are 2 options:
# (1) reduce 'cex.lab'.
# (2) Use different labels as illustrated by adding
#     such an axis to the top of this plot 

axisIntervals(3, labels=monthLetters, cex.lab=1.2, line=-0.5) 
# 'line' argument here is passed to 'axis' via '...' 




