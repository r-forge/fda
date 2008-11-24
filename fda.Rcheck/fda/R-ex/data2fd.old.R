### Name: data2fd.old
### Title: Depricated: use 'Data2fd'
### Aliases: data2fd
### Keywords: smooth

### ** Examples

# Simplest possible example
b1.2 <- create.bspline.basis(norder=1, breaks=c(0, .5, 1))
# 2 bases, order 1 = degree 0 = step functions

str(fd1.2 <- data2fd(0:1, basisobj=b1.2))
plot(fd1.2)
# A step function:  0 to time=0.5, then 1 after 

b2.3 <- create.bspline.basis(norder=2, breaks=c(0, .5, 1))
# 3 bases, order 2 = degree 1 =
# continuous, bounded, locally linear

str(fd2.3 <- data2fd(0:1, basisobj=b2.3))
round(fd2.3$coefs, 4)
# 0, -.25, 1 
plot(fd2.3)
# Officially acceptable but crazy:
# Initial negative slope from (0,0) to (0.5, -0.25),
# then positive slope to (1,1).  

b3.4 <- create.bspline.basis(norder=3, breaks=c(0, .5, 1))
# 4 bases, order 3 = degree 2 =
# continuous, bounded, locally quadratic 

str(fd3.4 <- data2fd(0:1, basisobj=b3.4))
round(fd3.4$coefs, 4)
# 0, .25, -.5, 1 
plot(fd3.4)
# Officially acceptable but crazy:
# Initial positive then swings negative
# between 0.4 and ~0.75 before becoming positive again
# with a steep slope running to (1,1).  


#  Simple example 
gaitbasis3 <- create.fourier.basis(nbasis=3)
str(gaitbasis3) # note:  'names' for 3 bases
gaitfd3 <- data2fd(gait, basisobj=gaitbasis3)
str(gaitfd3)
# Note: dimanes for 'coefs' + basis[['names']]
# + 'fdnames'

#    set up the fourier basis
daybasis <- create.fourier.basis(c(0, 365), nbasis=65)
#  Make temperature fd object
#  Temperature data are in 12 by 365 matrix tempav
#    See analyses of weather data.

#  Convert the data to a functional data object
tempfd <- data2fd(CanadianWeather$dailyAv[,,"Temperature.C"],
                  day.5, daybasis)
#  plot the temperature curves
plot(tempfd)

# Terrifying interpolation
hgtbasis <- with(growth, create.bspline.basis(range(age), 
                                              breaks=age, norder=6))
girl.data2fd <- with(growth, data2fd(hgtf, age, hgtbasis))
age2 <- with(growth, sort(c(age, (age[-1]+age[-length(age)])/2)))
girlPred <- eval.fd(age2, girl.data2fd)
range(growth$hgtf)
range(growth$hgtf-girlPred[seq(1, by=2, length=31),])
# 5.5e-6 0.028 <
# The predictions are consistently too small
# but by less than 0.05 percent 

matplot(age2, girlPred, type="l")
with(growth, matpoints(age, hgtf))
# girl.data2fd fits the data fine but goes berzerk
# between points 




