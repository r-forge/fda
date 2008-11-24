### Name: CanadianWeather
### Title: Canadian average annual weather cycle
### Aliases: CanadianWeather daily
### Keywords: datasets

### ** Examples

##
## 1.  Plot (latitude & longitude) of stations by region
##
with(CanadianWeather, plot(-coordinates[, 2], coordinates[, 1], type='n',
                           xlab="West Latitude", ylab="North Longitude",
                           axes=FALSE) )
Wlat <- pretty(CanadianWeather$coordinates[, 2])
axis(1, -Wlat, Wlat)
axis(2)

arctic <- with(CanadianWeather, coordinates[region=='Arctic', ])
atl <- with(CanadianWeather, coordinates[region=='Atlantic', ])
contl <- with(CanadianWeather, coordinates[region=='Continental', ])
pac <- with(CanadianWeather, coordinates[region=='Pacific', ])
points(-arctic[, 2], arctic[, 1], col=1, pch=1)
points(-atl[, 2], atl[, 1], col=2, pch=2)
points(-contl[, 2], contl[, 1], col=3, pch=3)
points(-pac[, 2], pac[, 1], col=4, pch=4)

legend('topright', legend=c('Arctic', 'Atlantic', 'Pacific', 'Continental'),
       col=1:4, pch=1:4)

##
## 2.  Plot dailyAv[, 'Temperature.C'] for 4 stations
##
data(CanadianWeather)
# Expand the left margin to allow space for place names
op <- par(mar=c(5, 4, 4, 5)+.1)
# Plot
stations <- c("Pr. Rupert", "Montreal", "Edmonton", "Resolute")
matplot(day.5, CanadianWeather$dailyAv[, stations, "Temperature.C"],
        type="l", axes=FALSE, xlab="", ylab="Mean Temperature (deg C)")
axis(2, las=1)
# Label the horizontal axis with the month names
axis(1, monthBegin.5, labels=FALSE)
axis(1, monthEnd.5, labels=FALSE)
axis(1, monthMid, monthLetters, tick=FALSE)
# Add the monthly averages
matpoints(monthMid, CanadianWeather$monthlyTemp[, stations])
# Add the names of the weather stations
mtext(stations, side=4,
      at=CanadianWeather$dailyAv[365, stations, "Temperature.C"],
     las=1)
# clean up
par(op)



