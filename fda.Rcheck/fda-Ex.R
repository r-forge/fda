### * <HEADER>
###
attach(NULL, name = "CheckExEnv")
assign("nameEx", 
       local({
	   s <- "__{must remake R-ex/*.R}__"
           function(new) {
               if(!missing(new)) s <<- new else s
           }
       }),
       pos = "CheckExEnv")
## Add some hooks to label plot pages for base and grid graphics
assign("base_plot_hook",
       function() {
           pp <- par(c("mfg","mfcol","oma","mar"))
           if(all(pp$mfg[1:2] == c(1, pp$mfcol[2]))) {
               outer <- (oma4 <- pp$oma[4]) > 0; mar4 <- pp$mar[4]
               mtext(sprintf("help(\"%s\")", nameEx()), side = 4,
                     line = if(outer)max(1, oma4 - 1) else min(1, mar4 - 1),
               outer = outer, adj = 1, cex = .8, col = "orchid", las=3)
           }
       },
       pos = "CheckExEnv")
assign("grid_plot_hook",
       function() {
           grid::pushViewport(grid::viewport(width=grid::unit(1, "npc") - 
                              grid::unit(1, "lines"), x=0, just="left"))
           grid::grid.text(sprintf("help(\"%s\")", nameEx()),
                           x=grid::unit(1, "npc") + grid::unit(0.5, "lines"),
                           y=grid::unit(0.8, "npc"), rot=90,
                           gp=grid::gpar(col="orchid"))
       },
       pos = "CheckExEnv")
setHook("plot.new",     get("base_plot_hook", pos = "CheckExEnv"))
setHook("persp",        get("base_plot_hook", pos = "CheckExEnv"))
setHook("grid.newpage", get("grid_plot_hook", pos = "CheckExEnv"))
assign("cleanEx",
       function(env = .GlobalEnv) {
	   rm(list = ls(envir = env, all.names = TRUE), envir = env)
           RNGkind("default", "default")
	   set.seed(1)
   	   options(warn = 1)
	   .CheckExEnv <- as.environment("CheckExEnv")
	   delayedAssign("T", stop("T used instead of TRUE"),
		  assign.env = .CheckExEnv)
	   delayedAssign("F", stop("F used instead of FALSE"),
		  assign.env = .CheckExEnv)
	   sch <- search()
	   newitems <- sch[! sch %in% .oldSearch]
	   for(item in rev(newitems))
               eval(substitute(detach(item), list(item=item)))
	   missitems <- .oldSearch[! .oldSearch %in% sch]
	   if(length(missitems))
	       warning("items ", paste(missitems, collapse=", "),
		       " have been removed from the search path")
       },
       pos = "CheckExEnv")
assign("ptime", proc.time(), pos = "CheckExEnv")
## at least one package changes these via ps.options(), so do this
## before loading the package.
## Use postscript as incomplete files may be viewable, unlike PDF.
## Choose a size that is close to on-screen devices, fix paper
ps.options(width = 7, height = 7, paper = "a4", reset = TRUE)
grDevices::postscript("fda-Ex.ps")
		      
assign("par.postscript", graphics::par(no.readonly = TRUE), pos = "CheckExEnv")
options(contrasts = c(unordered = "contr.treatment", ordered = "contr.poly"), pager="console")
options(warn = 1)    
library('fda')

assign(".oldSearch", search(), pos = 'CheckExEnv')
assign(".oldNS", loadedNamespaces(), pos = 'CheckExEnv')
cleanEx(); nameEx("CSTR")
### * CSTR

flush(stderr()); flush(stdout())

### Name: CSTR
### Title: Continuously Stirred Temperature Reactor
### Aliases: CSTR CSTR2in CSTR2 CSTRfn CSTRfitLS CSTRres CSTRres0 CSTRsse
### Keywords: smooth

### ** Examples

###
###
### 1.  lsoda(y, times, func=CSTR2, parms=...)
###
###
#  The system of two nonlinear equations has five forcing or  
#  input functions.
#  These equations are taken from
#  Marlin, T. E. (2000) Process Control, 2nd Edition, McGraw Hill,
#  pages 899-902.
##
##  Set up the problem 
##
fitstruct <- list(V    = 1.0,#  volume in cubic meters 
                  Cp   = 1.0,#  concentration in cal/(g.K)
                  rho  = 1.0,#  density in grams per cubic meter 
                  delH = -130.0,# cal/kmol
                  Cpc  = 1.0,#  concentration in cal/(g.K)
                  rhoc = 1.0,#  cal/kmol
                  Tref = 350)#  reference temperature
#  store true values of known parameters 
EoverRtru = 0.83301#   E/R in units K/1e4
kreftru   = 0.4610 #   reference value
atru      = 1.678#     a in units (cal/min)/K/1e6
btru      = 0.5#       dimensionless exponent

#

fitstruct[["kref"]]   = kreftru#      
fitstruct[["EoverR"]] = EoverRtru#  kref = 0.4610
fitstruct[["a"]]      = atru#       a in units (cal/min)/K/1e6
fitstruct[["b"]]      = btru#       dimensionless exponent
 
Tlim  = 64#    reaction observed over interval [0, Tlim]
delta = 1/12#  observe every five seconds
tspan = seq(0, Tlim, delta)#

coolStepInput <- CSTR2in(tspan, 'all.cool.step')

#  set constants for ODE solver

#  cool condition solution
#  initial conditions

Cinit.cool = 1.5965#  initial concentration in kmol per cubic meter
Tinit.cool = 341.3754# initial temperature in deg K
yinit = c(Conc = Cinit.cool, Temp=Tinit.cool)

#  load cool input into fitstruct

fitstruct[["Tcin"]] = coolStepInput[, "Tcin"];

#  solve  differential equation with true parameter values

if (require(odesolve)) {
coolStepSoln <- lsoda(y=yinit, times=tspan, func=CSTR2,
  parms=list(fitstruct=fitstruct, condition='all.cool.step', Tlim=Tlim) )
}
###
###
### 2.  CSTRfn 
###
###

# See the script in '~R\library\fda\scripts\CSTR\CSTR_demo.R'
#  for more examples.  




cleanEx(); nameEx("CanadianWeather")
### * CanadianWeather

flush(stderr()); flush(stdout())

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



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx(); nameEx("Data2fd")
### * Data2fd

flush(stderr()); flush(stdout())

### Name: Data2fd
### Title: Create a functional data object from data
### Aliases: Data2fd
### Keywords: smooth

### ** Examples

##
## Simplest possible example:  step function 
##
b1.1 <- create.bspline.basis(nbasis=1, norder=1)
# 1 basis, order 1 = degree 0 = step function

y12 <- 1:2
fd1.1 <- Data2fd(y12, basisobj=b1.1)
plot(fd1.1)
# fd1.1 = mean(y12) = 1.5 

fd1.1.5 <- Data2fd(y12, basisobj=b1.1, lambda=0.5)
eval.fd(seq(0, 1, .2), fd1.1.5)
# fd1.1.5 = sum(y12)/(n+lambda*integral(over arg=0 to 1 of 1))
#         = 3 / (2+0.5) = 1.2

##
## 3 step functions
##
b1.2 <- create.bspline.basis(nbasis=2, norder=1)
# 2 bases, order 1 = degree 0 = step functions
fd1.2 <- Data2fd(1:2, basisobj=b1.2)

op <- par(mfrow=c(2,1))
plot(b1.2, main='bases') 
plot(fd1.2, main='fit')
par(op) 
# A step function:  1 to 0.5, then 2 

##
## Simple oversmoothing
##
b1.3 <- create.bspline.basis(nbasis=3, norder=1)
fd1.3.5 <- Data2fd(y12, basisobj=b1.3, lambda=0.5)
plot(0:1, c(0, 2), type='n')
points(0:1, y12)
lines(fd1.3.5)
# Fit = penalized least squares with penalty = 
#          = lambda * integral(0:1 of basis^2),
#            which shrinks the points towards 0.
# X1.3 = matrix(c(1,0, 0,0, 0,1), 2)
# XtX = crossprod(X1.3) = diag(c(1, 0, 1))
# penmat = diag(3)/3
#        = 3x3 matrix of integral(over arg=0:1 of basis[i]*basis[j])
# Xt.y = crossprod(X1.3, y12) = c(1, 0, 2)
# XtX + lambda*penmat = diag(c(7, 1, 7)/6 
# so coef(fd1.3.5) = solve(XtX + lambda*penmat, Xt.y)
#                  = c(6/7, 0, 12/7)

##
## linear spline fit 
##
b2.3 <- create.bspline.basis(norder=2, breaks=c(0, .5, 1))
# 3 bases, order 2 = degree 1 =
# continuous, bounded, locally linear

fd2.3 <- Data2fd(0:1, basisobj=b2.3)
round(fd2.3$coefs, 4)
# (0, 0, 1), 
# though (0, a, 1) is also a solution for any 'a' 
op <- par(mfrow=c(2,1))
plot(b2.3, main='bases') 
plot(fd2.3, main='fit')
par(op)

# smoothing?  
fd2.3. <- Data2fd(0:1, basisobj=b2.3, lambda=1)
## Don't show: 
stopifnot(
## End Don't show
all.equal(as.vector(round(fd2.3.$coefs, 4)),
          c(0.0159, -0.2222, 0.8730) )
## Don't show: 
)
## End Don't show
# The default smoothing with spline of order 2, degree 1
# has nderiv = max(0, norder-2) = 0.
# Direct computations confirm that the optimal B-spline
# weights in this case are the numbers given above.  

op <- par(mfrow=c(2,1))
plot(b2.3, main='bases') 
plot(fd2.3., main='fit')
par(op)

##
## quadratic spline fit
##
b3.4 <- create.bspline.basis(norder=3, breaks=c(0, .5, 1))
# 4 bases, order 3 = degree 2 =
# continuous, bounded, locally quadratic 

fd3.4 <- Data2fd(0:1, basisobj=b3.4)
round(fd3.4$coefs, 4)
# (0, 0, 0, 1),
# but (0, a, b, 1) is also a solution for any 'a' and 'b' 
op <- par(mfrow=c(2,1))
plot(b3.4) 
plot(fd3.4)
par(op)

#  try smoothing?  
fd3.4. <- Data2fd(0:1, basisobj=b3.4, lambda=1)
round(fd3.4.$coef, 4)

op <- par(mfrow=c(2,1))
plot(b3.4) 
plot(fd3.4.)
par(op)

##
##  A simple Fourier example 
##
gaitbasis3 <- create.fourier.basis(nbasis=3)
# note:  'names' for 3 bases
gaitfd3 <- Data2fd(gait, basisobj=gaitbasis3)
# Note: dimanes for 'coefs' + basis[['names']]
# + 'fdnames'

#    set up the fourier basis
daybasis <- create.fourier.basis(c(0, 365), nbasis=65)
#  Make temperature fd object
#  Temperature data are in 12 by 365 matrix tempav
#    See analyses of weather data.

#  Convert the data to a functional data object
tempfd <- Data2fd(CanadianWeather$dailyAv[,,"Temperature.C"],
                  day.5, daybasis)
#  plot the temperature curves
plot(tempfd)

##
## Terrifying interpolation
##
hgtbasis <- with(growth, create.bspline.basis(range(age), 
                                              breaks=age, norder=6))
girl.data2fd <- with(growth, Data2fd(hgtf, age, hgtbasis))
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

# Smooth 
girl.data2fd1 <- with(growth, Data2fd(age, hgtf, hgtbasis, lambda=1))
girlPred1 <- eval.fd(age2, girl.data2fd1)

matplot(age2, girlPred1, type="l")
with(growth, matpoints(age, hgtf))

# problems splikes disappear 




graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx(); nameEx("Eigen")
### * Eigen

flush(stderr()); flush(stdout())

### Name: Eigen
### Title: Eigenanalysis preserving dimnames
### Aliases: Eigen
### Keywords: array

### ** Examples

X <- matrix(1:4, 2, dimnames=list(LETTERS[1:2], letters[3:4]))
eigen(X)
Eigen(X)
Eigen(X, valuenames='eigval')

Y <- matrix(1:4, 2, dimnames=list(letters[5:6], letters[5:6]))
Eigen(Y)

Eigen(Y, symmetric=TRUE)
# only the lower triangle is used;
# the upper triangle is ignored.  



cleanEx(); nameEx("Fperm.fd")
### * Fperm.fd

flush(stderr()); flush(stdout())

### Name: Fperm.fd
### Title: Permutation F-test for functional linear regression.
### Aliases: Fperm.fd
### Keywords: smooth

### ** Examples

# The very simplest example is the equivalent of the permutation
# t-test on the growth data. 

# First set up a basis system to hold the smooths

knots  <- growth$age
norder <- 6
nbasis <- length(knots) + norder - 2
hgtbasis <- create.bspline.basis(range(knots), nbasis, norder, knots)

# Now smooth with a fourth-derivative penalty and a very small smoothing
# parameter

Lfdobj <- 4
lambda <- 1e-2
growfdPar <- fdPar(hgtbasis, Lfdobj, lambda)

hgtfd <- smooth.basis(growth$age, cbind(growth$hgtm,growth$hgtf),growfdPar)$fd

# Now set up factors for fRegress:

cbasis = create.constant.basis(range(knots))

maleind = c(rep(1,ncol(growth$hgtm)),rep(0,ncol(growth$hgtf)))

constfd = fd( matrix(1,1,length(maleind)),cbasis)
maleindfd = fd( matrix(maleind,1,length(maleind)),cbasis)

xfdlist = list(constfd,maleindfd)

# The fdPar object for the coefficients and call Fperm.fd

betalist = list(fdPar(hgtbasis,2,1e-6),fdPar(hgtbasis,2,1e-6))

Fres = Fperm.fd(hgtfd,xfdlist,betalist)




cleanEx(); nameEx("Lfd")
### * Lfd

flush(stderr()); flush(stdout())

### Name: Lfd
### Title: Define a Linear Differential Operator Object
### Aliases: Lfd
### Keywords: smooth

### ** Examples

#  Set up the harmonic acceleration operator
dayrange  <- c(0,365)
Lbasis  <- create.constant.basis(dayrange)
Lcoef   <- matrix(c(0,(2*pi/365)^2,0),1,3)
bfdobj  <- fd(Lcoef,Lbasis)
bwtlist <- fd2list(bfdobj)
harmaccelLfd <- Lfd(3, bwtlist)



cleanEx(); nameEx("TaylorSpline")
### * TaylorSpline

flush(stderr()); flush(stdout())

### Name: TaylorSpline
### Title: Taylor representation of a B-Spline
### Aliases: TaylorSpline TaylorSpline.fd TaylorSpline.fdPar
###   TaylorSpline.fdSmooth TaylorSpline.dierckx
### Keywords: smooth manip

### ** Examples

##
## The simplest b-spline basis:  order 1, degree 0, zero interior knots:
##       a single step function
##
library(DierckxSpline)
bspl1.1 <- create.bspline.basis(norder=1, breaks=0:1)
# ... jump to pi to check the code
fd.bspl1.1pi <- fd(pi, basisobj=bspl1.1)
bspl1.1pi <- TaylorSpline(fd.bspl1.1pi)
## Don't show: 
bsp1.1ref <- list(knots=0:1, midpoints=0.5,
      coef=array(pi, dim=rep(1, 3), dimnames=list(NULL, 'b0', NULL)), 
      deriv=array(pi, dim=rep(1, 3), dimnames=list(NULL, 'D0', NULL)) )
class(bsp1.1ref) <- "Taylor" 
stopifnot(all.equal(bspl1.1pi, bsp1.1ref))
## End Don't show

##
## Cubic spline:  4  basis functions
##
bspl4 <- create.bspline.basis(nbasis=4)
plot(bspl4)
parab4.5 <- fd(c(3, -1, -1, 3)/3, bspl4)
# = 4*(x-.5)
TaylorSpline(parab4.5)

##
## A more realistic example
##
data(titanium)
#  Cubic spline with 5 interior knots (6 segments)
titan10 <- with(titanium, curfit.free.knot(x, y))
(titan10T <- TaylorSpline(titan10) )




cleanEx(); nameEx("argvalsy.swap")
### * argvalsy.swap

flush(stderr()); flush(stdout())

### Name: argvalsy.swap
### Title: Swap argvals with y if the latter is simpler.
### Aliases: argvalsy.swap
### Keywords: smooth

### ** Examples

##
## one argument:  y
##
argvalsy.swap(1:5)
# warning ... 

##
## (argvals, y), same dimensions:  retain order 
##
argy1 <- argvalsy.swap(seq(0, 1, .2), 1:6)
argy1a <- argvalsy.swap(1:6, seq(0, 1, .2))

## Don't show: 
stopifnot(
## End Don't show
all.equal(argy1[[1]], argy1a[[2]]) &&
all.equal(argy1[[2]], argy1a[[1]])
# TRUE;  basisobj different 
## Don't show: 
)
## End Don't show

# lengths do not match 
## Not run: 
##D argvalsy.swap(1:4, 1:5)
## End(Not run) 

##
## two numeric arguments, different dimensions:  put simplest first 
##
argy2 <- argvalsy.swap(seq(0, 1, .2), matrix(1:12, 6))

## Don't show: 
stopifnot(
## End Don't show
all.equal(argy2,
argvalsy.swap(matrix(1:12, 6), seq(0, 1, .2)) )
# TRUE with a warning ... 
## Don't show: 
)
## End Don't show

## Not run: 
##D argvalsy.swap(seq(0, 1, .2), matrix(1:12, 2))
##D # ERROR:  first dimension does not match 
## End(Not run)

##
## one numeric, one basisobj
##
argy3 <- argvalsy.swap(1:6, b=4)
# warning:  argvals assumed seq(0, 1, .2) 

argy3. <- argvalsy.swap(1:6, b=create.bspline.basis(breaks=0:1))
# warning:  argvals assumed seq(0, 1, .2) 

argy3.6 <- argvalsy.swap(seq(0, 1, .2), b=create.bspline.basis(breaks=1:3))
# warning:  argvals assumed seq(1, 3 length=6)

##
## two numeric, one basisobj:  first matches basisobj
##
#  OK 
argy3a <- argvalsy.swap(1:6, seq(0, 1, .2),
              create.bspline.basis(breaks=c(1, 4, 8))) 

#  Swap (argvals, y) 
## Don't show: 
stopifnot(
## End Don't show
all.equal(argy3a,
argvalsy.swap(seq(0, 1, .2), 1:6, 
              create.bspline.basis(breaks=c(1, 4, 8))) )
# TRUE with a warning 
## Don't show: 
)
## End Don't show

## Not run: 
##D # neither match basisobj:  error  
##D argvalsy.swap(seq(0, 1, .2), 1:6, 
##D               create.bspline.basis(breaks=1:3) ) 
## End(Not run)




cleanEx(); nameEx("arithmetic.fd")
### * arithmetic.fd

flush(stderr()); flush(stdout())

### Name: arithmetic.fd
### Title: Arithmetic on functional data ('fd') objects
### Aliases: arithmetic.fd +.fd plus.fd -.fd minus.fd *.fd times.fd
### Keywords: smooth

### ** Examples

##
## add a parabola to itself
##
bspl4 <- create.bspline.basis(nbasis=4)
parab4.5 <- fd(c(3, -1, -1, 3)/3, bspl4)
## Don't show: 
stopifnot(
## End Don't show
all.equal(coef(parab4.5+parab4.5), matrix(c(6, -2, -2, 6)/3, 4))
## Don't show: 
)
## End Don't show
## Don't show: 
stopifnot(
## End Don't show
all.equal(coef(parab4.5-parab4.5), matrix(rep(0, 4), 4))
## Don't show: 
)
## End Don't show

##
## Same example with interior knots at 1/3 and 1/2
##
bspl5.3 <- create.bspline.basis(breaks=c(0, 1/3, 1))
plot(bspl5.3)
x. <- seq(0, 1, .1)
para4.5.3 <- smooth.basis(x., 4*(x.-0.5)^2, fdParobj=bspl5.3)[['fd']]
plot(para4.5.3)

bspl5.2 <- create.bspline.basis(breaks=c(0, 1/2, 1))
plot(bspl5.2)
para4.5.2 <- smooth.basis(x., 4*(x.-0.5)^2, fdParobj=bspl5.2)[['fd']]
plot(para4.5.2)

str(para4.5.3+para4.5.2)
## Don't show: 
stopifnot(
## End Don't show
all.equal(coef(para4.5.3-para4.5.2), matrix(0, 9, 1))
## Don't show: 
)
## End Don't show

str(para4.5.3*para4.5.2)
# interior knots of the sum
# = union(interior knots of the summands);
# ditto for difference and product.
plot(para4.5.3*para4.5.2)

##
## fd+numeric
##
## Don't show: 
stopifnot(
## End Don't show
all.equal(coef(parab4.5+1), matrix(c(6, 2, 2, 6)/3, 4))
## Don't show: 
)
## End Don't show

## Don't show: 
stopifnot(
## End Don't show
all.equal(1+parab4.5, parab4.5+1)
## Don't show: 
)
## End Don't show

##
## fd-numeric
##
## Don't show: 
stopifnot(
## End Don't show
all.equal(coef(-parab4.5), matrix(c(-3, 1, 1, -3)/3, 4))
## Don't show: 
)
## End Don't show

plot(parab4.5-1)

plot(1-parab4.5)




cleanEx(); nameEx("as.array3")
### * as.array3

flush(stderr()); flush(stdout())

### Name: as.array3
### Title: Reshape a vector or array to have 3 dimensions.
### Aliases: as.array3 as.array3
### Keywords: utilities

### ** Examples

##
## vector -> array 
##
as.array3(c(a=1, b=2)) 

##
## matrix -> array 
##
as.array3(matrix(1:6, 2))
as.array3(matrix(1:6, 2, dimnames=list(letters[1:2], LETTERS[3:5]))) 

##
## array -> array 
##
as.array3(array(1:6, 1:3)) 

##
## 4-d array 
##
## Not run: 
##D as.array3(array(1:24, 1:4)) 
##D Error in as.array3(array(1:24, 1:4)) : 
##D   length(dim(array(1:24, 1:4)) = 4 > 3
## End(Not run)



cleanEx(); nameEx("as.fd")
### * as.fd

flush(stderr()); flush(stdout())

### Name: as.fd
### Title: Convert a spline object to class 'fd'
### Aliases: as.fd as.fd.fdSmooth as.fd.dierckx as.fd.function
###   as.fd.smooth.spline
### Keywords: smooth manip

### ** Examples

##
## as.fd.fdSmooth
##
girlGrowthSm <- with(growth, smooth.basisPar(argvals=age, y=hgtf))
girlGrowth.fd <- as.fd(girlGrowthSm)

##
## as.fd.dierckx
##
x <- 0:24
y <- c(1.0,1.0,1.4,1.1,1.0,1.0,4.0,9.0,13.0,
       13.4,12.8,13.1,13.0,14.0,13.0,13.5,
       10.0,2.0,3.0,2.5,2.5,2.5,3.0,4.0,3.5)
library(DierckxSpline) 
curfit.xy <- curfit(x, y, s=0)

curfit.fd <- as.fd(curfit.xy)
plot(curfit.fd) # as an 'fd' object 
points(x, y) # Curve goes through the points.  

x. <- seq(0, 24, length=241)
pred.y <- predict(curfit.xy, x.) 
lines(x., pred.y, lty="dashed", lwd=3, col="blue")
# dierckx and fd objects match.

## Don't show: 
stopifnot(
## End Don't show
all.equal(knots(curfit.xy, FALSE), knots(curfit.fd, FALSE))
## Don't show: 
)
## End Don't show
## Don't show: 
stopifnot(
## End Don't show
all.equal(coef(curfit.xy), as.vector(coef(curfit.fd)))
## Don't show: 
)
## End Don't show



##
## as.fd.function(splinefun(...), ...) 
## 
x2 <- 1:7
y2 <- sin((x2-0.5)*pi)
f <- splinefun(x2, y2)
fd. <- as.fd(f)
x. <- seq(1, 7, .02)
fx. <- f(x.)
fdx. <- eval.fd(x., fd.) 
plot(range(x2), range(y2, fx., fdx.), type='n')
points(x2, y2)
lines(x., sin((x.-0.5)*pi), lty='dashed') 
lines(x., f(x.), col='blue')
lines(x., eval.fd(x., fd.), col='red', lwd=3, lty='dashed')
# splinefun and as.fd(splineful(...)) are close
# but quite different from the actual function
# apart from the actual 7 points fitted,
# which are fitted exactly
# ... and there is no information in the data
# to support a better fit!

# Translate also a natural spline 
fn <- splinefun(x2, y2, method='natural')
fn. <- as.fd(fn)
lines(x., fn(x.), lty='dotted', col='blue')
lines(x., eval.fd(x., fn.), col='green', lty='dotted', lwd=3)

## Not run: 
##D # Will NOT translate a periodic spline
##D fp <- splinefun(x, y, method='periodic')
##D as.fd(fp)
##D #Error in as.fd.function(fp) : 
##D #  x (fp)  uses periodic B-splines, and as.fd is programmed
##D #   to translate only B-splines with coincident boundary knots.
##D 
## End(Not run)

##
## as.fd.smooth.spline
##
cars.spl <- with(cars, smooth.spline(speed, dist))
cars.fd <- as.fd(cars.spl)

plot(dist~speed, cars)
lines(cars.spl)
sp. <- with(cars, seq(min(speed), max(speed), len=101))
d. <- eval.fd(sp., cars.fd)
lines(sp., d., lty=2, col='red', lwd=3)



cleanEx(); nameEx("axisIntervals")
### * axisIntervals

flush(stderr()); flush(stdout())

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




cleanEx(); nameEx("bifd")
### * bifd

flush(stderr()); flush(stdout())

### Name: bifd
### Title: Create a bivariate functional data object
### Aliases: bifd
### Keywords: attribute

### ** Examples

Bspl2 <- create.bspline.basis(nbasis=2, norder=1)
Bspl3 <- create.bspline.basis(nbasis=3, norder=2)

(bBspl2.3 <- bifd(array(1:6, dim=2:3), Bspl2, Bspl3))
str(bBspl2.3)




cleanEx(); nameEx("bsplineS")
### * bsplineS

flush(stderr()); flush(stdout())

### Name: bsplineS
### Title: B-spline Basis Function Values
### Aliases: bsplineS
### Keywords: smooth

### ** Examples

# Minimal example:  A B-spline of order 1 (i.e., a step function)
# with 0 interior knots:
bsplineS(seq(0, 1, .2), 0:1, 1, 0)

#  set up break values at 0.0, 0.2,..., 0.8, 1.0.
breaks <- seq(0,1,0.2)
#  set up a set of 11 argument values
x <- seq(0,1,0.1)
#  the order willl be 4, and the number of basis functions
#  is equal to the number of interior break values (4 here)
#  plus the order, for a total here of 8.
norder <- 4
#  compute the 11 by 8 matrix of basis function values
basismat <- bsplineS(x, breaks, norder)



cleanEx(); nameEx("bsplinepen")
### * bsplinepen

flush(stderr()); flush(stdout())

### Name: bsplinepen
### Title: B-Spline Penalty Matrix
### Aliases: bsplinepen
### Keywords: smooth

### ** Examples

##
## bsplinepen with only one basis function
##
bspl1.1 <- create.bspline.basis(nbasis=1, norder=1)
pen1.1 <- bsplinepen(bspl1.1, 0) 

##
## bspline pen for a cubic spline with knots at seq(0, 1, .1)
##
basisobj <- create.bspline.basis(c(0,1),13)
#  compute the 13 by 13 matrix of inner products of second derivatives
penmat <- bsplinepen(basisobj)



cleanEx(); nameEx("cca.fd")
### * cca.fd

flush(stderr()); flush(stdout())

### Name: cca.fd
### Title: Functional Canonical Correlation Analysis
### Aliases: cca.fd
### Keywords: smooth

### ** Examples

#  Canonical correlation analysis of knee-hip curves

gaittime  <- (1:20)/21
gaitrange <- c(0,1)
gaitbasis <- create.fourier.basis(gaitrange,21)
lambda    <- 10^(-11.5)
harmaccelLfd <- vec2Lfd(c(0, 0, (2*pi)^2, 0))

gaitfdPar <- fdPar(gaitbasis, harmaccelLfd, lambda)
gaitfd <- smooth.basis(gaittime, gait, gaitfdPar)$fd

ccafdPar <- fdPar(gaitfd, harmaccelLfd, 1e-8)
ccafd0    <- cca.fd(gaitfd[,1], gaitfd[,2], ncan=3, ccafdPar, ccafdPar)
#  compute a VARIMAX rotation of the canonical variables
ccafd <- varmx.cca.fd(ccafd0)
#  plot the canonical weight functions
op <- par(mfrow=c(2,1))
#plot.cca.fd(ccafd, cex=1.2, ask=TRUE)
#plot.cca.fd(ccafd, cex=1.2)
#  display the canonical correlations
#round(ccafd$ccacorr[1:6],3)
par(op)



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx(); nameEx("center.fd")
### * center.fd

flush(stderr()); flush(stdout())

### Name: center.fd
### Title: Center Functional Data
### Aliases: center.fd
### Keywords: smooth

### ** Examples

daytime    <- (1:365)-0.5
daybasis   <- create.fourier.basis(c(0,365), 365)
harmLcoef  <- c(0,(2*pi/365)^2,0)
harmLfd    <- vec2Lfd(harmLcoef, c(0,365))
templambda <- 0.01
tempfdPar  <- fdPar(daybasis, harmLfd, templambda)
tempfd     <- smooth.basis(daytime,
       CanadianWeather$dailyAv[,,"Temperature.C"], tempfdPar)$fd
tempctrfd  <- center.fd(tempfd)

plot(tempctrfd, xlab="Day", ylab="deg. C",
     main = "Centered temperature curves")



cleanEx(); nameEx("checkDims3")
### * checkDims3

flush(stderr()); flush(stdout())

### Name: checkDims3
### Title: Compare dimensions and dimnames of arrays
### Aliases: checkDim3 checkDims3
### Keywords: utilities

### ** Examples

# Select the first two rows of y 
stopifnot(all.equal( 
checkDim3(1:2, 3:5),
list(x=array(1:2, c(2,1,1), list(c('x1','x2'), NULL, NULL)), 
     y=array(3:4, c(2,1,1), list(c('x1','x2'), NULL, NULL)) )
)) 

# Select the first two rows of a matrix y 
stopifnot(all.equal(
checkDim3(1:2, matrix(3:8, 3)),
list(x=array(1:2,         c(2,1,1), list(c('x1','x2'), NULL, NULL)), 
     y=array(c(3:4, 6:7), c(2,2,1), list(c('x1','x2'), NULL, NULL)) )
))

# Select the first column of y
stopifnot(all.equal(
checkDim3(1:2, matrix(3:8, 3), 2, 2), 
list(x=array(1:2,         c(2,1,1), list(NULL, 'x', NULL)), 
     y=array(3:5, c(3,1,1), list(NULL, 'x', NULL)) )
))

# Select the first two rows and the first column of y
stopifnot(all.equal(
checkDims3(1:2, matrix(3:8, 3), 1:2, 1:2),
list(x=array(1:2, c(2,1,1), list(c('x1','x2'), 'x', NULL)), 
     y=array(3:4, c(2,1,1), list(c('x1','x2'), 'x', NULL)) ) 
))

# Select the first 2 rows of y 
x1 <- matrix(1:4, 2, dimnames=list(NULL, LETTERS[2:3]))
x1a <- x1. <- as.array3(x1)
dimnames(x1a)[[1]] <- c('x1', 'x2') 
y1 <- matrix(11:19, 3, dimnames=list(NULL, LETTERS[1:3]))
y1a <- y1. <- as.array3(y1) 
dimnames(y1a)[[1]] <- c('x1', 'x2', 'x3')

stopifnot(all.equal(
checkDim3(x1, y1),
list(x=x1a, y=y1a[1:2, , , drop=FALSE])
))

# Select columns 2 & 3 of y 
stopifnot(all.equal(
checkDim3(x1, y1, 2, 2),
list(x=x1., y=y1.[, 2:3, , drop=FALSE ])
))

# Select the first 2 rows and  columns 2 & 3 of y 
stopifnot(all.equal(
checkDims3(x1, y1, 1:2, 1:2),
list(x=x1a, y=y1a[1:2, 2:3, , drop=FALSE ])
)) 

# y = columns 2 and 3 of x 
x23 <- matrix(1:6, 2, dimnames=list(letters[2:3], letters[1:3]))
x23. <- as.array3(x23) 
stopifnot(all.equal(
checkDim3(x23, xdim=1, ydim=2),
list(x=x23., y=x23.[, 2:3,, drop=FALSE ])
))

# Transfer dimnames from y to x
x4a <- x4 <- matrix(1:4, 2)
y4 <- matrix(5:8, 2, dimnames=list(letters[1:2], letters[3:4]))
dimnames(x4a) <- dimnames(t(y4))
stopifnot(all.equal(
checkDims3(x4, y4, 1:2, 2:1),
list(x=as.array3(x4a), y=as.array3(y4))
))

# as used in plotfit.fd
daybasis65 <- create.fourier.basis(c(0, 365), 65)

daytempfd <- with(CanadianWeather, data2fd(
       dailyAv[,,"Temperature.C"], day.5, 
       daybasis65, argnames=list("Day", "Station", "Deg C")) )

defaultNms <- with(daytempfd, c(fdnames[2], fdnames[3], x='x'))
subset <- checkDims3(CanadianWeather$dailyAv[, , "Temperature.C"],
               daytempfd$coef, defaultNames=defaultNms)
# Problem:  dimnames(...)[[3]] = '1' 
# Fix:  
subset3 <- checkDims3(
        CanadianWeather$dailyAv[, , "Temperature.C", drop=FALSE],
               daytempfd$coef, defaultNames=defaultNms)



cleanEx(); nameEx("checkLogicalInteger")
### * checkLogicalInteger

flush(stderr()); flush(stdout())

### Name: checkLogicalInteger
### Title: Does an argument satisfy required conditions?
### Aliases: checkLogical checkNumeric checkLogicalInteger
### Keywords: attribute utilities

### ** Examples

##
## checkLogical
##
checkLogical(NULL, length=3, warnOnly=TRUE)
checkLogical(c(FALSE, TRUE, TRUE), length=4, warnOnly=TRUE)
checkLogical(c(FALSE, TRUE, TRUE), length=3)

##
## checkNumeric
##
checkNumeric(NULL, lower=1, upper=3)
checkNumeric(1:3, 1, 3)
checkNumeric(1:3, 1, 3, inclusion=FALSE, warnOnly=TRUE)
checkNumeric(pi, 1, 4, integer=TRUE, warnOnly=TRUE)
checkNumeric(c(1, 1), 1, 4, warnOnly=TRUE)
checkNumeric(c(1, 1), 1, 4, unique=FALSE, warnOnly=TRUE)

##
## checkLogicalInteger
##
checkLogicalInteger(NULL, 3)
checkLogicalInteger(c(FALSE, TRUE), warnOnly=TRUE) 
checkLogicalInteger(1:2, 3) 
checkLogicalInteger(2, warnOnly=TRUE) 
checkLogicalInteger(c(2, 4), 3, warnOnly=TRUE)

##
## checkLogicalInteger names its calling function 
## rather than itself as the location of error detection
## if possible
##
tstFun <- function(x, length., warnOnly=FALSE){
   checkLogicalInteger(x, length., warnOnly) 
}
tstFun(NULL, 3)
tstFun(4, 3, warnOnly=TRUE)

tstFun2 <- function(x, length., warnOnly=FALSE){
   tstFun(x, length., warnOnly)
}
tstFun2(4, 3, warnOnly=TRUE)




cleanEx(); nameEx("coef")
### * coef

flush(stderr()); flush(stdout())

### Name: coef.fd
### Title: Extract functional coefficients
### Aliases: coef.fd coef.fdPar coef.fdSmooth coef.Taylor coefficients.fd
###   coefficients.fdPar coefficients.fdSmooth coefficients.Taylor
### Keywords: utilities

### ** Examples

##
## coef.fd
##
bspl1.1 <- create.bspline.basis(norder=1, breaks=0:1)
fd.bspl1.1 <- fd(0, basisobj=bspl1.1)
coef(fd.bspl1.1)
## Don't show: 
stopifnot(all.equal(coef(fd.bspl1.1), coefficients(fd.bspl1.1)))
## End Don't show

##
## coef.fdPar 
##
rangeval <- c(-3,3)
#  set up some standard normal data
x <- rnorm(50)
#  make sure values within the range
x[x < -3] <- -2.99
x[x >  3] <-  2.99
#  set up basis for W(x)
basisobj <- create.bspline.basis(rangeval, 11)
#  set up initial value for Wfdobj
Wfd0 <- fd(matrix(0,11,1), basisobj)
WfdParobj <- fdPar(Wfd0)

coef(WfdParobj)
## Don't show: 
stopifnot(all.equal(coef(WfdParobj), coefficients(WfdParobj)))
## End Don't show

##
## coef.fdSmooth
##

girlGrowthSm <- with(growth, smooth.basisPar(argvals=age, y=hgtf))
coef(girlGrowthSm)
## Don't show: 
stopifnot(
all.equal(coef(girlGrowthSm), coefficients(girlGrowthSm)) 
)
## End Don't show

##
## coef.Taylor 
##
# coming soon.




cleanEx(); nameEx("cor.fd")
### * cor.fd

flush(stderr()); flush(stdout())

### Name: cor.fd
### Title: Correlation matrix from functional data object(s)
### Aliases: cor.fd
### Keywords: smooth

### ** Examples

daybasis3 <- create.fourier.basis(c(0, 365))
daybasis5 <- create.fourier.basis(c(0, 365), 5)
tempfd3 <- with(CanadianWeather, data2fd(
       dailyAv[,,"Temperature.C"], day.5,
       daybasis3, argnames=list("Day", "Station", "Deg C")) )
precfd5 <- with(CanadianWeather, data2fd(
       dailyAv[,,"log10precip"], day.5,
       daybasis5, argnames=list("Day", "Station", "Deg C")) )

# Correlation matrix for a single functional data object
(tempCor3 <- cor.fd(seq(0, 356, length=4), tempfd3))

# Cross correlation matrix between two functional data objects 
# Compare with structure described above under 'value':
(tempPrecCor3.5 <- cor.fd(seq(0, 365, length=4), tempfd3,
                          seq(0, 356, length=6), precfd5))

# The following produces contour and perspective plots

daybasis65 <- create.fourier.basis(rangeval=c(0, 365), nbasis=65)
daytempfd <- with(CanadianWeather, data2fd(
       dailyAv[,,"Temperature.C"], day.5,
       daybasis65, argnames=list("Day", "Station", "Deg C")) )
dayprecfd <- with(CanadianWeather, data2fd(
       dailyAv[,,"log10precip"], day.5,
       daybasis65, argnames=list("Day", "Station", "log10(mm)")) )

str(tempPrecCor <- cor.fd(weeks, daytempfd, weeks, dayprecfd))
# dim(tempPrecCor)= c(53, 53)

op <- par(mfrow=c(1,2), pty="s")
contour(weeks, weeks, tempPrecCor, 
        xlab="Average Daily Temperature",
        ylab="Average Daily log10(precipitation)",
        main=paste("Correlation function across locations\n",
          "for Canadian Anual Temperature Cycle"),
        cex.main=0.8, axes=FALSE)
axisIntervals(1, atTick1=seq(0, 365, length=5), atTick2=NA, 
            atLabels=seq(1/8, 1, 1/4)*365,
            labels=paste("Q", 1:4) )
axisIntervals(2, atTick1=seq(0, 365, length=5), atTick2=NA, 
            atLabels=seq(1/8, 1, 1/4)*365,
            labels=paste("Q", 1:4) )
persp(weeks, weeks, tempPrecCor,
      xlab="Days", ylab="Days", zlab="Correlation")
mtext("Temperature-Precipitation Correlations", line=-4, outer=TRUE)
par(op)

# Correlations and cross correlations
# in a bivariate functional data object
gaitbasis5 <- create.fourier.basis(nbasis=5)
gaitfd5 <- data2fd(gait, basisobj=gaitbasis5)

gait.t3 <- (0:2)/2
(gaitCor3.5 <- cor.fd(gait.t3, gaitfd5))
# Check the answers with manual computations
gait3.5 <- eval.fd(gait.t3, gaitfd5)
all.equal(cor(t(gait3.5[,,1])), gaitCor3.5[,,,1])
# TRUE
all.equal(cor(t(gait3.5[,,2])), gaitCor3.5[,,,3])
# TRUE
all.equal(cor(t(gait3.5[,,2]), t(gait3.5[,,1])),
               gaitCor3.5[,,,2])
# TRUE

# NOTE:
dimnames(gaitCor3.5)[[4]]
# [1] Hip-Hip
# [2] Knee-Hip 
# [3] Knee-Knee
# If [2] were "Hip-Knee", then
# gaitCor3.5[,,,2] would match 
# cor(t(gait3.5[,,1]), t(gait3.5[,,2]))
# *** It does NOT.  Instead, it matches:  
# cor(t(gait3.5[,,2]), t(gait3.5[,,1]))




graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx(); nameEx("create.bspline.basis")
### * create.bspline.basis

flush(stderr()); flush(stdout())

### Name: create.bspline.basis
### Title: Create a B-spline Basis
### Aliases: create.bspline.basis
### Keywords: smooth

### ** Examples

##
## The simplest basis currently available with this function:
##
bspl1.1 <- create.bspline.basis(norder=1)
plot(bspl1.1)
# 1 basis function, order 1 = degree 0 = step function:

# should be the same as above:
b1.1 <- create.bspline.basis(0:1, nbasis=1, norder=1, breaks=0:1)
## Don't show: 
stopifnot(
## End Don't show
all.equal(bspl1.1, b1.1)
## Don't show: 
)
## End Don't show

bspl2.2 <- create.bspline.basis(norder=2)
plot(bspl2.2)
# PROBLEM:  Should be 2 right triangles;  isn't

bspl3.3 <- create.bspline.basis(norder=3)
plot(bspl3.3)
# PROBLEM:  Should be 3 parabolas, x^2, 1-(x-.5)^2, (x-1)^2
# isn't

bspl4.4 <- create.bspline.basis()
plot(bspl4.4)
# PROBLEM:  Should be 4 cubics;  isn't

bspl1.2 <- create.bspline.basis(norder=1, breaks=c(0,.5, 1))
plot(bspl1.2)
# 2 bases, order 1 = degree 0 = step functions:
# (1) constant 1 between 0 and 0.5 and 0 otherwise
# (2) constant 1 between 0.5 and 1 and 0 otherwise.

bspl2.3 <- create.bspline.basis(norder=2, breaks=c(0,.5, 1))
plot(bspl2.3)
# 3 bases:  order 2 = degree 1 = linear
# (1) line from (0,1) down to (0.5, 0), 0 after
# (2) line from (0,0) up to (0.5, 1), then down to (1,0)
# (3) 0 to (0.5, 0) then up to (1,1).

bspl3.4 <- create.bspline.basis(norder=3, breaks=c(0,.5, 1))
plot(bspl3.4)
# 4 bases:  order 3 = degree 2 = parabolas.
# (1) (x-.5)^2 from 0 to .5, 0 after
# (2) 2*(x-1)^2 from .5 to 1, and a parabola
#     from (0,0 to (.5, .5) to match
# (3 & 4) = complements to (2 & 1).

bSpl4. <- create.bspline.basis(c(-1,1))
plot(bSpl4.)
# Same as bSpl4.23 but over (-1,1) rather than (0,1).

# set up the b-spline basis for the lip data, using 23 basis functions,
#   order 4 (cubic), and equally spaced knots.
#  There will be 23 - 4 = 19 interior knots at 0.05, ..., 0.95
lipbasis <- create.bspline.basis(c(0,1), 23)
plot(lipbasis)

bSpl.growth <- create.bspline.basis(growth$age)
# cubic spline (order 4)

bSpl.growth6 <- create.bspline.basis(growth$age,norder=6)
# quintic spline (order 6)



cleanEx(); nameEx("create.constant.basis")
### * create.constant.basis

flush(stderr()); flush(stdout())

### Name: create.constant.basis
### Title: Create a Constant Basis
### Aliases: create.constant.basis
### Keywords: smooth

### ** Examples


basisobj <- create.constant.basis(c(-1,1))




cleanEx(); nameEx("create.exponential.basis")
### * create.exponential.basis

flush(stderr()); flush(stdout())

### Name: create.exponential.basis
### Title: Create an Exponential Basis
### Aliases: create.exponential.basis
### Keywords: smooth

### ** Examples


#  Create an exponential basis over interval [0,5]
#  with basis functions 1, exp(-t) and exp(-5t)
basisobj <- create.exponential.basis(c(0,5),3,c(0,-1,-5))
#  plot the basis
plot(basisobj)




cleanEx(); nameEx("create.fourier.basis")
### * create.fourier.basis

flush(stderr()); flush(stdout())

### Name: create.fourier.basis
### Title: Create a Fourier Basis
### Aliases: create.fourier.basis
### Keywords: smooth

### ** Examples

# Create a minimal Fourier basis for the monthly temperature data,
#  using 3 basis functions with period 12 months.
monthbasis3 <- create.fourier.basis(c(0,12) )
#  plot the basis
plot(monthbasis3)

# set up the Fourier basis for the monthly temperature data,
#  using 9 basis functions with period 12 months.
monthbasis <- create.fourier.basis(c(0,12), 9, 12.0)

#  plot the basis
plot(monthbasis)

# Create a false Fourier basis using 1 basis function.
falseFourierBasis <- create.fourier.basis(nbasis=1)
#  plot the basis:  constant
plot(falseFourierBasis)




cleanEx(); nameEx("create.monomial.basis")
### * create.monomial.basis

flush(stderr()); flush(stdout())

### Name: create.monomial.basis
### Title: Create a Monomial Basis
### Aliases: create.monomial.basis
### Keywords: smooth

### ** Examples

##
## simplest example: one constant 'basis function'
##
m0 <- create.monomial.basis(nbasis=1)
plot(m0)

##
## Create a monomial basis over the interval [-1,1]
##  consisting of the first three powers of t
##
basisobj <- create.monomial.basis(c(-1,1), 5)
#  plot the basis
plot(basisobj)



cleanEx(); nameEx("create.polygonal.basis")
### * create.polygonal.basis

flush(stderr()); flush(stdout())

### Name: create.polygonal.basis
### Title: Create a Polygonal Basis
### Aliases: create.polygonal.basis
### Keywords: smooth

### ** Examples

#  Create a polygonal basis over the interval [0,1]
#  with break points at 0, 0.1, ..., 0.95, 1
(basisobj <- create.polygonal.basis(seq(0,1,0.1)))
#  plot the basis
plot(basisobj)



cleanEx(); nameEx("create.polynomial.basis")
### * create.polynomial.basis

flush(stderr()); flush(stdout())

### Name: create.polynomial.basis
### Title: Create a Polynomial Basis
### Aliases: create.polynomial.basis
### Keywords: smooth

### ** Examples

#  Create a polynomial basis over the years in the 20th century
#  and center the basis functions on 1950.
basisobj <- create.polynomial.basis(c(1900, 2000), nbasis=3, ctr=1950)
#  plot the basis
# The following should work but doesn't;  2007.05.01
#plot(basisobj)



cleanEx(); nameEx("create.power.basis")
### * create.power.basis

flush(stderr()); flush(stdout())

### Name: create.power.basis
### Title: Create a Power Basis Object
### Aliases: create.power.basis
### Keywords: smooth

### ** Examples


#  Create a power basis over the interval [1e-7,1]
#  with powers or exponents -1, -0.5, 0, 0.5 and 1
basisobj <- create.power.basis(c(1e-7,1), 5, seq(-1,1,0.5))
#  plot the basis
plot(basisobj)




cleanEx(); nameEx("data2fd.old")
### * data2fd.old

flush(stderr()); flush(stdout())

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




cleanEx(); nameEx("dateAccessories")
### * dateAccessories

flush(stderr()); flush(stdout())

### Name: dateAccessories
### Title: Numeric and character vectors to facilitate working with dates
### Aliases: dateAccessories monthAccessories dayOfYear day.5 daysPerMonth
###   monthEnd monthEnd.5 monthBegin.5 monthMid monthLetters weeks
### Keywords: datasets

### ** Examples

daybasis65 <- create.fourier.basis(c(0, 365), 65)
daytempfd <- with(CanadianWeather, smooth.basisPar(day.5, 
    dailyAv[,,"Temperature.C"]) )
plot(daytempfd, axes=FALSE)
axisIntervals(1) 
# axisIntervals by default uses
# monthBegin.5, monthEnd.5, monthMid, and month.abb
axis(2)  



cleanEx(); nameEx("density.fd")
### * density.fd

flush(stderr()); flush(stdout())

### Name: density.fd
### Title: Compute a Probability Density Function
### Aliases: density.fd
### Keywords: smooth

### ** Examples


#  set up range for density
rangeval <- c(-3,3)
#  set up some standard normal data
x <- rnorm(50)
#  make sure values within the range
x[x < -3] <- -2.99
x[x >  3] <-  2.99
#  set up basis for W(x)
basisobj <- create.bspline.basis(rangeval, 11)
#  set up initial value for Wfdobj
Wfd0 <- fd(matrix(0,11,1), basisobj)
WfdParobj <- fdPar(Wfd0)
#  estimate density
denslist <- density.fd(x, WfdParobj)
#  plot density
xval <- seq(-3,3,.2)
wval <- eval.fd(xval, denslist$Wfdobj)
pval <- exp(wval)/denslist$C
plot(xval, pval, type="l", ylim=c(0,0.4))
points(x,rep(0,50))




cleanEx(); nameEx("deriv.fd")
### * deriv.fd

flush(stderr()); flush(stdout())

### Name: deriv.fd
### Title: Compute a Derivative of a Functional Data Object
### Aliases: deriv.fd
### Keywords: smooth

### ** Examples


#  Estimate the acceleration functions for growth curves
#  See the analyses of the growth data.
#  Set up the ages of height measurements for Berkeley data
age <- c( seq(1, 2, 0.25), seq(3, 8, 1), seq(8.5, 18, 0.5))
#  Range of observations
rng <- c(1,18)
#  Set up a B-spline basis of order 6 with knots at ages
knots  <- age
norder <- 6
nbasis <- length(knots) + norder - 2
hgtbasis <- create.bspline.basis(rng, nbasis, norder, knots)
#  Set up a functional parameter object for estimating
#  growth curves.  The 4th derivative is penalyzed to
#  ensure a smooth 2nd derivative or acceleration.
Lfdobj <- 4
lambda <- 10^(-0.5)   #  This value known in advance.
growfdPar <- fdPar(hgtbasis, Lfdobj, lambda)
#  Smooth the data.  The data for the boys and girls
#  are in matrices hgtm and hgtf, respectively.
hgtmfd <- smooth.basis(age, growth$hgtm, growfdPar)$fd
hgtffd <- smooth.basis(age, growth$hgtf, growfdPar)$fd
#  Compute the acceleration functions
accmfd <- deriv.fd(hgtmfd, 2)
accffd <- deriv.fd(hgtffd, 2)
#  Plot the two sets of curves
par(mfrow=c(2,1))
plot(accmfd)
plot(accffd)




graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx(); nameEx("df2lambda")
### * df2lambda

flush(stderr()); flush(stdout())

### Name: df2lambda
### Title: Convert Degrees of Freedom to a Smoothing Parameter Value
### Aliases: df2lambda
### Keywords: smooth

### ** Examples


#  Smooth growth curves using a specified value of
#  degrees of freedom.
#  Set up the ages of height measurements for Berkeley data
age <- c( seq(1, 2, 0.25), seq(3, 8, 1), seq(8.5, 18, 0.5))
#  Range of observations
rng <- c(1,18)
#  Set up a B-spline basis of order 6 with knots at ages
knots  <- age
norder <- 6
nbasis <- length(knots) + norder - 2
hgtbasis <- create.bspline.basis(rng, nbasis, norder, knots)
#  Find the smoothing parameter equivalent to 12
#  degrees of freedom
lambda <- df2lambda(age, hgtbasis, df=12)
#  Set up a functional parameter object for estimating
#  growth curves.  The 4th derivative is penalyzed to
#  ensure a smooth 2nd derivative or acceleration.
Lfdobj <- 4
growfdPar <- fdPar(hgtbasis, Lfdobj, lambda)
#  Smooth the data.  The data for the girls are in matrix
#  hgtf.
hgtffd <- smooth.basis(age, growth$hgtf, growfdPar)$fd
#  Plot the curves
plot(hgtffd)




cleanEx(); nameEx("dirs")
### * dirs

flush(stderr()); flush(stdout())

### Name: dirs
### Title: Get subdirectories
### Aliases: dirs dirs dirs
### Keywords: IO

### ** Examples

path2fdaM <- system.file('Matlab/fdaM', package='fda')
dirs(path2fdaM)
dirs(path2fdaM, full.names=TRUE)
dirs(path2fdaM, recursive=TRUE)
dirs(path2fdaM, exclude='^@|^private$', recursive=TRUE)

# Directories to add to Matlab path
# for R.matlab and fda
R.matExt <- system.file('externals', package='R.matlab')
fdaM <- dirs(path2fdaM, exclude='^@|^private$', full.names=TRUE,
              recursive=TRUE)  
add2MatlabPath <- c(R.matExt, path2fdaM, fdaM) 




cleanEx(); nameEx("eval.basis")
### * eval.basis

flush(stderr()); flush(stdout())

### Name: eval.basis
### Title: Values of Basis Functions or their Derivatives
### Aliases: eval.basis
### Keywords: smooth

### ** Examples

##
## 1.  B-splines
## 
# The simplest basis currently available:
# a single step function  
str(bspl1.1 <- create.bspline.basis(norder=1, breaks=0:1))
(eval.bspl1.1 <- eval.basis(seq(0, 1, .2), bspl1.1))

# The second simplest basis:
# 2 step functions, [0, .5], [.5, 1]
str(bspl1.2 <- create.bspline.basis(norder=1, breaks=c(0,.5, 1)))
(eval.bspl1.2 <- eval.basis(seq(0, 1, .2), bspl1.2))

# Second order B-splines (degree 1:  linear splines) 
str(bspl2.3 <- create.bspline.basis(norder=2, breaks=c(0,.5, 1)))
(eval.bspl2.3 <- eval.basis(seq(0, 1, .1), bspl2.3))
# 3 bases:  order 2 = degree 1 = linear 
# (1) line from (0,1) down to (0.5, 0), 0 after
# (2) line from (0,0) up to (0.5, 1), then down to (1,0)
# (3) 0 to (0.5, 0) then up to (1,1).

##
## 2.  Fourier 
## 
# The false Fourier series with 1 basis function
falseFourierBasis <- create.fourier.basis(nbasis=1)
(eval.fFB <- eval.basis(seq(0, 1, .2), falseFourierBasis))

# Simplest real Fourier basis with 3 basis functions
fourier3 <- create.fourier.basis()
(eval.fourier3 <- eval.basis(seq(0, 1, .2), fourier3))

# 3 basis functions on [0, 365]
fourier3.365 <- create.fourier.basis(c(0, 365))
eval.F3.365 <- eval.basis(day.5, fourier3.365)

matplot(eval.F3.365, type="l")

# The next simplest Fourier basis (5  basis functions)
fourier5 <- create.fourier.basis(nbasis=5)
(eval.F5 <- eval.basis(seq(0, 1, .1), fourier5))
matplot(eval.F5, type="l")

# A more complicated example
dayrng <- c(0, 365) 

nbasis <- 51
norder <- 6 

weatherBasis <- create.fourier.basis(dayrng, nbasis)
basisMat <- eval.basis(day.5, weatherBasis) 

matplot(basisMat[, 1:5], type="l")




cleanEx(); nameEx("eval.bifd")
### * eval.bifd

flush(stderr()); flush(stdout())

### Name: eval.bifd
### Title: Values a Two-argument Functional Data Object
### Aliases: eval.bifd
### Keywords: smooth

### ** Examples

daybasis   <- create.fourier.basis(c(0,365), 365)
harmLcoef  <- c(0,(2*pi/365)^2,0)
harmLfd    <- vec2Lfd(harmLcoef, c(0,365))
templambda <- 1.0
tempfdPar  <- fdPar(daybasis, harmLfd, lambda=1)
tempfd     <- smooth.basis(day.5,
          CanadianWeather$dailyAv[,,"Temperature.C"], tempfdPar)$fd
#    define the variance-covariance bivariate fd object
tempvarbifd <- var.fd(tempfd)
#    evaluate the variance-covariance surface and plot
weektime    <- seq(0,365,len=53)
tempvarmat  <- eval.bifd(weektime,weektime,tempvarbifd)
#    make a perspective plot of the variance function
persp(tempvarmat)



cleanEx(); nameEx("eval.fd")
### * eval.fd

flush(stderr()); flush(stdout())

### Name: eval.fd
### Title: Values of a Functional Data Object
### Aliases: eval.fd
### Keywords: smooth

### ** Examples


#    set up the fourier basis
daybasis <- create.fourier.basis(c(0, 365), nbasis=65)
#  Make temperature fd object
#  Temperature data are in 12 by 365 matrix tempav
#  See analyses of weather data.
#  Set up sampling points at mid days
#  Convert the data to a functional data object
tempfd <- data2fd(CanadianWeather$dailyAv[,,"Temperature.C"],
                   day.5, daybasis)
#   set up the harmonic acceleration operator
Lbasis  <- create.constant.basis(c(0, 365))
Lcoef   <- matrix(c(0,(2*pi/365)^2,0),1,3)
bfdobj  <- fd(Lcoef,Lbasis)
bwtlist <- fd2list(bfdobj)
harmaccelLfd <- Lfd(3, bwtlist)
#   evaluate the value of the harmonic acceleration
#   operator at the sampling points
Ltempmat <- eval.fd(day.5, tempfd, harmaccelLfd)
#  Plot the values of this operator
matplot(day.5, Ltempmat, type="l")




cleanEx(); nameEx("eval.monfd")
### * eval.monfd

flush(stderr()); flush(stdout())

### Name: eval.monfd
### Title: Values of a Monotone Functional Data Object
### Aliases: eval.monfd
### Keywords: smooth

### ** Examples


#  Estimate the acceleration functions for growth curves
#  See the analyses of the growth data.
#  Set up the ages of height measurements for Berkeley data
age <- c( seq(1, 2, 0.25), seq(3, 8, 1), seq(8.5, 18, 0.5))
#  Range of observations
rng <- c(1,18)
#  First set up a basis for monotone smooth
#  We use b-spline basis functions of order 6
#  Knots are positioned at the ages of observation.
norder <- 6
nage   <- 31
nbasis <- nage + norder - 2
wbasis <- create.bspline.basis(rng, nbasis, norder, age)
#  starting values for coefficient
cvec0 <- matrix(0,nbasis,1)
Wfd0  <- fd(cvec0, wbasis)
#  set up functional parameter object
Lfdobj    <- 3          #  penalize curvature of acceleration
lambda    <- 10^(-0.5)  #  smoothing parameter
growfdPar <- fdPar(Wfd0, Lfdobj, lambda)
#  Set up wgt vector
wgt   <- rep(1,nage)
#  Smooth the data for the first girl
hgt1 = growth$hgtf[,1]
result <- smooth.monotone(age, hgt1, growfdPar, wgt)
#  Extract the functional data object and regression
#  coefficients
Wfd  <- result$Wfdobj
beta <- result$beta
#  Evaluate the fitted height curve over a fine mesh
agefine <- seq(1,18,len=101)
hgtfine <- beta[1] + beta[2]*eval.monfd(agefine, Wfd)
#  Plot the data and the curve
plot(age, hgt1, type="p")
lines(agefine, hgtfine)
#  Evaluate the acceleration curve
accfine <- beta[2]*eval.monfd(agefine, Wfd, 2)
#  Plot the acceleration curve
plot(agefine, accfine, type="l")
lines(c(1,18),c(0,0),lty=4)




cleanEx(); nameEx("exponpen")
### * exponpen

flush(stderr()); flush(stdout())

### Name: exponpen
### Title: Exponential Penalty Matrix
### Aliases: exponpen
### Keywords: smooth

### ** Examples


#  set up an exponential basis with 3 basis functions
ratevec  <- c(0, -1, -5)
basisobj <- create.exponential.basis(c(0,1),3,ratevec)
#  compute the 3 by 3 matrix of inner products of
#  second derivatives
penmat <- exponpen(basisobj)




cleanEx(); nameEx("fRegress.CV")
### * fRegress.CV

flush(stderr()); flush(stdout())

### Name: fRegress.CV
### Title: Computes Cross-validated Error Sum of Squares for a Functional
###   Regression Model
### Aliases: fRegress.CV
### Keywords: smooth

### ** Examples

#See the analyses of the Canadian daily weather data.



cleanEx(); nameEx("fRegress")
### * fRegress

flush(stderr()); flush(stdout())

### Name: fRegress
### Title: A Functional Regression Analysis of the Concurrent Type
### Aliases: fRegress
### Keywords: smooth

### ** Examples

#See the Canadian daily weather data analyses in the file
# this-is-escaped-code{ for 
#examples of all the cases covered by this-is-escaped-codenormal-bracket48bracket-normal.



cleanEx(); nameEx("fRegress.stderr")
### * fRegress.stderr

flush(stderr()); flush(stdout())

### Name: fRegress.stderr
### Title: Compute Standard errors of Coefficient Functions Estimated by
###   Functional Regression Analysis
### Aliases: fRegress.stderr
### Keywords: smooth

### ** Examples

#See the weather data analyses in the file this-is-escaped-codenormal-bracket29bracket-normal for
#examples of the use of function this-is-escaped-codenormal-bracket30bracket-normal.



cleanEx(); nameEx("fd")
### * fd

flush(stderr()); flush(stdout())

### Name: fd
### Title: Define a Functional Data Object
### Aliases: fd
### Keywords: smooth internal

### ** Examples

##
## default
##
fd()

##
## The simplest b-spline basis:  order 1, degree 0, zero interior knots:
##       a single step function
##
bspl1.1 <- create.bspline.basis(norder=1, breaks=0:1)
fd.bspl1.1 <- fd(0, basisobj=bspl1.1)

fd.bspl1.1a <- fd(basisobj=bspl1.1)
## Don't show: 
 stopifnot( 
## End Don't show
all.equal(fd.bspl1.1, fd.bspl1.1a)
## Don't show: 
 ) 
## End Don't show
# TRUE

## Not run: 
##D fd.bspl1.1b <- fd(0)
##D Error in fd(0) :
##D   Number of coefficients does not match number of basis functions.
##D 
##D ... because fd by default wants to create a cubic spline
## End(Not run)
##
## Cubic spline:  4  basis functions
##
bspl4 <- create.bspline.basis(nbasis=4)
plot(bspl4)
parab4.5 <- fd(c(3, -1, -1, 3)/3, bspl4)
# = 4*(x-.5)^2
plot(parab4.5)




cleanEx(); nameEx("fdPar")
### * fdPar

flush(stderr()); flush(stdout())

### Name: fdPar
### Title: Define a Functional Parameter Object
### Aliases: fdPar
### Keywords: smooth

### ** Examples

##
## Simple example
##
#  set up range for density
rangeval <- c(-3,3)
#  set up some standard normal data
x <- rnorm(50)
#  make sure values within the range
x[x < -3] <- -2.99
x[x >  3] <-  2.99
#  set up basis for W(x)
basisobj <- create.bspline.basis(rangeval, 11)
#  set up initial value for Wfdobj
Wfd0 <- fd(matrix(0,11,1), basisobj)
WfdParobj <- fdPar(Wfd0)

WfdP3 <- fdPar(seq(-3, 3, length=11))

##
##  smooth the Canadian daily temperature data
##
#    set up the fourier basis
nbasis   <- 365
dayrange <- c(0,365)
daybasis <- create.fourier.basis(dayrange, nbasis)
dayperiod <- 365
harmaccelLfd <- vec2Lfd(c(0,(2*pi/365)^2,0), dayrange)
#  Make temperature fd object
#  Temperature data are in 12 by 365 matrix tempav
#    See analyses of weather data.
#  Set up sampling points at mid days
daytime  <- (1:365)-0.5
#  Convert the data to a functional data object
daybasis65 <- create.fourier.basis(dayrange, nbasis, dayperiod)
templambda <- 1e1
tempfdPar  <- fdPar(fdobj=daybasis65, Lfdobj=harmaccelLfd, lambda=templambda)

#FIXME
#tempfd <- smooth.basis(CanadianWeather$tempav, daytime, tempfdPar)
#  Set up the harmonic acceleration operator
Lbasis  <- create.constant.basis(dayrange);
Lcoef   <- matrix(c(0,(2*pi/365)^2,0),1,3)
bfdobj  <- fd(Lcoef,Lbasis)
bwtlist <- fd2list(bfdobj)
harmaccelLfd <- Lfd(3, bwtlist)
#  Define the functional parameter object for
#  smoothing the temperature data
lambda   <- 0.01  #  minimum GCV estimate
#tempPar <- fdPar(daybasis365, harmaccelLfd, lambda)
#  smooth the data
#tempfd <- smooth.basis(daytime, CanadialWeather$tempav, tempPar)$fd
#  plot the temperature curves
#plot(tempfd)




cleanEx(); nameEx("fda-package")
### * fda-package

flush(stderr()); flush(stdout())

### Name: fda-package
### Title: Functional Data Analysis in R
### Aliases: fda-package fda
### Keywords: smooth

### ** Examples

##
## Simple smoothing
##
girlGrowthSm <- with(growth, smooth.basisPar(argvals=age, y=hgtf))
plot(girlGrowthSm$fd, xlab="age", ylab="height (cm)",
         main="Girls in Berkeley Growth Study" )
plot(deriv(girlGrowthSm$fd), xlab="age", ylab="growth rate (cm / year)",
         main="Girls in Berkeley Growth Study" )
plot(deriv(girlGrowthSm$fd, 2), xlab="age",
        ylab="growth acceleration (cm / year^2)",
        main="Girls in Berkeley Growth Study" )
##
## Simple basis
##
bspl1.2 <- create.bspline.basis(norder=1, breaks=c(0,.5, 1))
plot(bspl1.2)
# 2 bases, order 1 = degree 0 = step functions:
# (1) constant 1 between 0 and 0.5 and 0 otherwise
# (2) constant 1 between 0.5 and 1 and 0 otherwise.

fd1.2 <- Data2fd(0:1, basisobj=bspl1.2)
op <- par(mfrow=c(2,1))
plot(bspl1.2, main='bases')
plot(fd1.2, main='fit')
par(op)
# A step function:  0 to time=0.5, then 1 after




graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx(); nameEx("fdaMatlabPath")
### * fdaMatlabPath

flush(stderr()); flush(stdout())

### Name: fdaMatlabPath
### Title: Add 'fdaM' to the Matlab path
### Aliases: fdaMatlabPath
### Keywords: programming

### ** Examples

# Modify the Matlab startup.m only when you really want to,
# typically once per installation ... certaintly not
# every time we test this package.
fdaMatlabPath()



cleanEx(); nameEx("file.copy2")
### * file.copy2

flush(stderr()); flush(stdout())

### Name: file.copy2
### Title: Copy a file with a default 'to' name
### Aliases: file.copy2
### Keywords: IO

### ** Examples

## Not run: 
##D file.copy2('startup.m')
##D # Used by 'fdaMatlabPath' so an existing 'startup.m' is not destroyed
## End(Not run)



cleanEx(); nameEx("fourier")
### * fourier

flush(stderr()); flush(stdout())

### Name: fourier
### Title: Fourier Basis Function Values
### Aliases: fourier
### Keywords: smooth

### ** Examples


#  set up a set of 11 argument values
x <- seq(0,1,0.1)
names(x) <- paste("x", 0:10, sep="")
#  compute values for five Fourier basis functions
#  with the default period (1) and derivative (0)
(basismat <- fourier(x, 5))

# Create a false Fourier basis, i.e., nbasis = 1
# = a constant function
fourier(x, 1)




cleanEx(); nameEx("fourierpen")
### * fourierpen

flush(stderr()); flush(stdout())

### Name: fourierpen
### Title: Fourier Penalty Matrix
### Aliases: fourierpen
### Keywords: smooth

### ** Examples


#  set up a Fourier basis with 13 basis functions
#  and and period 1.0.
basisobj <- create.fourier.basis(c(0,1),13)
#  compute the 13 by 13 matrix of inner products
#  of second derivatives
penmat <- fourierpen(basisobj)




cleanEx(); nameEx("gait")
### * gait

flush(stderr()); flush(stdout())

### Name: gait
### Title: Hip and knee angle while walking
### Aliases: gait
### Keywords: datasets

### ** Examples

plot(gait[,1, 1], gait[, 1, 2], type="b")



cleanEx(); nameEx("getbasismatrix")
### * getbasismatrix

flush(stderr()); flush(stdout())

### Name: getbasismatrix
### Title: Values of Basis Functions or their Derivatives
### Aliases: getbasismatrix
### Keywords: smooth

### ** Examples

# Minimal example:  a B-spline of order 1, i.e., a step function
# with 0 interior knots:
bspl1.1 <- create.bspline.basis(norder=1, breaks=0:1)
getbasismatrix(seq(0, 1, .2), bspl1.1)




cleanEx(); nameEx("getbasispenalty")
### * getbasispenalty

flush(stderr()); flush(stdout())

### Name: getbasispenalty
### Title: Evaluate a Roughness Penalty Matrix
### Aliases: getbasispenalty
### Keywords: smooth

### ** Examples


#  set up a B-spline basis of order 4 with 13 basis functions
#  and knots at 0.0, 0.1,..., 0.9, 1.0.
basisobj <- create.bspline.basis(c(0,1),13)
#  compute the 13 by 13 matrix of inner products of second derivatives
penmat <- getbasispenalty(basisobj)
#  set up a Fourier basis with 13 basis functions
#  and and period 1.0.
basisobj <- create.fourier.basis(c(0,1),13)
#  compute the 13 by 13 matrix of inner products of second derivatives
penmat <- getbasispenalty(basisobj)




cleanEx(); nameEx("growth")
### * growth

flush(stderr()); flush(stdout())

### Name: growth
### Title: Berkeley Growth Study data
### Aliases: growth
### Keywords: datasets

### ** Examples

with(growth, matplot(age, hgtf[, 1:10], type="b"))



cleanEx(); nameEx("handwrit")
### * handwrit

flush(stderr()); flush(stdout())

### Name: handwrit
### Title: Cursive handwriting samples
### Aliases: handwrit
### Keywords: datasets

### ** Examples

plot(handwrit[, 1, 1], handwrit[, 1, 2], type="l")



cleanEx(); nameEx("intensity.fd")
### * intensity.fd

flush(stderr()); flush(stdout())

### Name: intensity.fd
### Title: Intensity Function for Point Process
### Aliases: intensity.fd
### Keywords: smooth

### ** Examples


#  Generate 101 Poisson-distributed event times with
#  intensity or rate two events per unit time
N  <- 101
mu <- 2
#  generate 101 uniform deviates
uvec <- runif(rep(0,N))
#  convert to 101 exponential waiting times
wvec <- -log(1-uvec)/mu
#  accumulate to get event times
tvec <- cumsum(wvec)
tmax <- max(tvec)
#  set up an order 4 B-spline basis over [0,tmax] with
#  21 equally spaced knots
tbasis <- create.bspline.basis(c(0,tmax), 23)
#  set up a functional parameter object for W(t),
#  the log intensity function.  The first derivative
#  is penalized in order to smooth toward a constant
lambda <- 10
WfdParobj <- fdPar(tbasis, 1, lambda)
#  estimate the intensity function
Wfdobj <- intensity.fd(tvec, WfdParobj)$Wfdobj
#  get intensity function values at 0 and event times
events <- c(0,tvec)
intenvec <- exp(eval.fd(events,Wfdobj))
#  plot intensity function
plot(events, intenvec, type="b")
lines(c(0,tmax),c(mu,mu),lty=4)




cleanEx(); nameEx("knots.fd")
### * knots.fd

flush(stderr()); flush(stdout())

### Name: knots.fd
### Title: Extract the knots from a function basis or data object
### Aliases: knots.fd knots.fdSmooth knots.basisfd
### Keywords: smooth optimize

### ** Examples

x <- 0:24
y <- c(1.0,1.0,1.4,1.1,1.0,1.0,4.0,9.0,13.0,
       13.4,12.8,13.1,13.0,14.0,13.0,13.5,
       10.0,2.0,3.0,2.5,2.5,2.5,3.0,4.0,3.5)
if(require(DierckxSpline)){
   z1 <- curfit(x, y, method = "ss", s = 0, k = 3)
   knots1 <- knots(z1)
   knots1All <- knots(z1, interior=FALSE) # to see all knots
#
   fda1 <- dierckx2fd(z1)
   fdaKnots <- knots(fda1)
   fdaKnotsA <- knots(fda1, interior=FALSE)
   stopifnot(all.equal(knots1, fdaKnots))
   stopifnot(all.equal(knots1All, fdaKnotsA))
}

# knots.fdSmooth 
girlGrowthSm <- with(growth, smooth.basisPar(argvals=age, y=hgtf))

girlKnots.fdSm <- knots(girlGrowthSm) 
girlKnots.fdSmA <- knots(girlGrowthSm, interior=FALSE)
stopifnot(all.equal(girlKnots.fdSm, girlKnots.fdSmA[5:33]))

girlKnots.fd <- knots(girlGrowthSm$fd) 
girlKnots.fdA <- knots(girlGrowthSm$fd, interior=FALSE)

stopifnot(all.equal(girlKnots.fdSm, girlKnots.fd))
stopifnot(all.equal(girlKnots.fdSmA, girlKnots.fdA))




cleanEx(); nameEx("landmarkreg")
### * landmarkreg

flush(stderr()); flush(stdout())

### Name: landmarkreg
### Title: Landmark Registration of Functional Observations
### Aliases: landmarkreg
### Keywords: smooth

### ** Examples

#See the analysis for the lip data in the examples.



cleanEx(); nameEx("lines.fd")
### * lines.fd

flush(stderr()); flush(stdout())

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




cleanEx(); nameEx("linmod")
### * linmod

flush(stderr()); flush(stdout())

### Name: linmod
### Title: Fit Fully Functional Linear Model
### Aliases: linmod
### Keywords: smooth

### ** Examples

#See the prediction of precipitation using temperature as
#the independent variable in the analysis of the daily weather
#data.



cleanEx(); nameEx("lip")
### * lip

flush(stderr()); flush(stdout())

### Name: lip
### Title: Lip motion
### Aliases: lip lipmarks liptime
### Keywords: datasets

### ** Examples

#  See the this-is-escaped-codenormal-bracket21bracket-normal this-is-escaped-codenormal-bracket22bracket-normal.  



cleanEx(); nameEx("lmWinsor")
### * lmWinsor

flush(stderr()); flush(stdout())

### Name: lmWinsor
### Title: Winsorized Regression
### Aliases: lmWinsor
### Keywords: models

### ** Examples

# example from 'anscombe' 
lm.1 <- lmWinsor(y1~x1, data=anscombe)

# no leverage to estimate the slope 
lm.1.5 <- lmWinsor(y1~x1, data=anscombe, trim=0.5)

# test nonlinear optimization  
lm.1.25 <- lmWinsor(y1~x1, data=anscombe, trim=0.25)




cleanEx(); nameEx("lmeWinsor")
### * lmeWinsor

flush(stderr()); flush(stdout())

### Name: lmeWinsor
### Title: Winsorized Regression with mixed effects
### Aliases: lmeWinsor
### Keywords: models

### ** Examples

fm1w <- lmeWinsor(distance ~ age, data = Orthodont,
                 random=~age|Subject) 
fm1w.1 <- lmeWinsor(distance ~ age, data = Orthodont,
                 random=~age|Subject, trim=0.1) 



cleanEx(); nameEx("mean.fd")
### * mean.fd

flush(stderr()); flush(stdout())

### Name: mean.fd
### Title: Mean of Functional Data
### Aliases: mean.fd
### Keywords: smooth

### ** Examples

##
## 1.  univeriate:  lip motion
##
liptime  <- seq(0,1,.02)
liprange <- c(0,1)

#  -------------  create the fd object -----------------
#       use 31 order 6 splines so we can look at acceleration

nbasis <- 51
norder <- 6
lipbasis <- create.bspline.basis(liprange, nbasis, norder)

#  ------------  apply some light smoothing to this object  -------

lipLfdobj   <- int2Lfd(4)
lipLambda   <- 1e-12
lipfdPar <- fdPar(lipbasis, lipLfdobj, lipLambda)

lipfd <- smooth.basis(liptime, lip, lipfdPar)$fd
names(lipfd$fdnames) = c("Normalized time", "Replications", "mm")

lipmeanfd <- mean.fd(lipfd)
plot(lipmeanfd)

##
## 2.  Trivariate:  CanadianWeather
##
dayrng <- c(0, 365) 

nbasis <- 51
norder <- 6 

weatherBasis <- create.fourier.basis(dayrng, nbasis)

weather.fd <- smooth.basis(day.5, CanadianWeather$dailyAv,
            weatherBasis)

str(weather.fd.mean <- mean.fd(weather.fd$fd))




cleanEx(); nameEx("melanoma")
### * melanoma

flush(stderr()); flush(stdout())

### Name: melanoma
### Title: melanoma 1936-1972
### Aliases: melanoma
### Keywords: datasets

### ** Examples

plot(melanoma[, -1], type="b")



cleanEx(); nameEx("monomial")
### * monomial

flush(stderr()); flush(stdout())

### Name: monomial
### Title: Evaluate Monomial Basis
### Aliases: monomial
### Keywords: smooth

### ** Examples


# set up a monomial basis for the first five powers
nbasis   <- 5
basisobj <- create.monomial.basis(c(-1,1),nbasis)
#  evaluate the basis
tval <- seq(-1,1,0.1)
basismat <- monomial(tval, 1:basisobj$nbasis)




cleanEx(); nameEx("monomialpen")
### * monomialpen

flush(stderr()); flush(stdout())

### Name: monomialpen
### Title: Evaluate Monomial Roughness Penalty Matrix
### Aliases: monomialpen
### Keywords: smooth

### ** Examples


# set up a monomial basis for the first five powers
nbasis   <- 5
basisobj <- create.monomial.basis(c(-1,1),nbasis)
#  evaluate the rougness penalty matrix for the
#  second derivative.
penmat <- monomialpen(basisobj, 2)




cleanEx(); nameEx("nondurables")
### * nondurables

flush(stderr()); flush(stdout())

### Name: nondurables
### Title: Nondurable goods index
### Aliases: nondurables
### Keywords: datasets

### ** Examples

plot(nondurables, log="y")



cleanEx(); nameEx("norder")
### * norder

flush(stderr()); flush(stdout())

### Name: norder
### Title: Order of a B-spline
### Aliases: norder norder.fd norder.basisfd norder.default norder.bspline
### Keywords: smooth

### ** Examples

bspl1.1 <- create.bspline.basis(norder=1, breaks=0:1)

stopifnot(norder(bspl1.1)==1)

stopifnot(norder(fd(0, basisobj=bspl1.1))==1)

stopifnot(norder(fd(rep(0,4)))==4)

stopifnot(norder(list(fd(rep(0,4))))==4)
## Not run: 
##D norder(list(list(fd(rep(0,4)))))
##D Error in norder.default(list(list(fd(rep(0, 4))))) : 
##D   input is not a 'basisfd' object and does not have a 'basisfd'
##D component. 
## End(Not run)

stopifnot(norder(create.bspline.basis(norder=1, breaks=c(0,.5, 1))) == 1) 

stopifnot(norder(create.bspline.basis(norder=2, breaks=c(0,.5, 1))) == 2)

# Defaut B-spline basis:  Cubic spline:  degree 3, order 4,
# 21 breaks, 19 interior knots.  
stopifnot(norder(create.bspline.basis()) == 4)

## Not run: 
##D norder(create.fourier.basis(c(0,12) ))
##D Error in norder.bspline(x) : 
##D   object x is of type = fourier;  'norder' is only defined for type = 'bsline'
## End(Not run)




cleanEx(); nameEx("objAndNames")
### * objAndNames

flush(stderr()); flush(stdout())

### Name: objAndNames
### Title: Add names to an object
### Aliases: objAndNames
### Keywords: attribute

### ** Examples

# The following should NOT check 'anything' here
tst1 <- objAndNames(1:2, list(letters[1:2], LETTERS[1:2]), anything)
all.equal(tst1, c(a=1, b=2))

# The following should return 'object unchanged
tst2 <- objAndNames(1:2, NULL, list(letters))
all.equal(tst2, 1:2)

tst3 <- objAndNames(1:2, list("a", 2), list(letters[1:2]))
all.equal(tst3, c(a=1, b=2) )

# The following checks a matrix / array
tst4 <- array(1:6, dim=c(2,3))
tst4a <- tst4
dimnames(tst4a) <- list(letters[1:2], LETTERS[1:3])
tst4b <- objAndNames(tst4, 
       list(letters[1:2], LETTERS[1:3]), anything)
all.equal(tst4b, tst4a)

tst4c <- objAndNames(tst4, NULL,        
       list(letters[1:2], LETTERS[1:3]) )
all.equal(tst4c, tst4a)




cleanEx(); nameEx("odesolv")
### * odesolv

flush(stderr()); flush(stdout())

### Name: odesolv
### Title: Numerical Solution mth Order Differential Equation System
### Aliases: odesolv
### Keywords: smooth

### ** Examples

#See the analyses of the lip data.



cleanEx(); nameEx("onechild")
### * onechild

flush(stderr()); flush(stdout())

### Name: onechild
### Title: growth in height of one 10-year-old boy
### Aliases: onechild
### Keywords: datasets

### ** Examples

with(onechild, plot(day, height, type="b"))




cleanEx(); nameEx("pca.fd")
### * pca.fd

flush(stderr()); flush(stdout())

### Name: pca.fd
### Title: Functional Principal Components Analysis
### Aliases: pca.fd
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
daytemppcaVarmx <- varmx.pca.fd(daytemppcaobj)
#  plot harmonics
op <- par(mfrow=c(2,2))
plot.pca.fd(daytemppcaobj, cex.main=0.9)

plot.pca.fd(daytemppcaVarmx, cex.main=0.9)
par(op)

plot(daytemppcaobj$harmonics)
plot(daytemppcaVarmx$harmonics)



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx(); nameEx("pda.fd")
### * pda.fd

flush(stderr()); flush(stdout())

### Name: pda.fd
### Title: Principal Differential Analysis
### Aliases: pda.fd
### Keywords: smooth

### ** Examples

#See analyses of daily weather data for examples.
##
##  set up objects for examples
##
#  constant basis for estimating weight functions
cbasis = create.constant.basis(c(0,1))
#  monomial basis: {1,t}  for estimating weight functions
mbasis = create.monomial.basis(c(0,1),2)
#  quartic spline basis with 54 basis functions for
#    defining functions to be analyzed
xbasis = create.bspline.basis(c(0,1),24,5)
#  set up functional parameter objects for weight bases
cfdPar = fdPar(cbasis)
mfdPar = fdPar(mbasis)
#  sampling points over [0,1]
tvec = seq(0,1,len=101)
##
##  Example 1:  a single first order constant coefficient unforced equation
##     Dx = -4*x  for  x(t) = exp(-4t)
beta    = 4
xvec    = exp(-beta*tvec)
xfd     = smooth.basis(tvec, xvec, xbasis)$fd
xfdlist = list(xfd)
bwtlist = list(cfdPar)
#  perform the principal differential analysis
result = pda.fd(xfdlist, bwtlist)
#  display weight coefficient for variable
bwtlistout = result$bwtlist
bwtfd      = bwtlistout[[1]]$fd
par(mfrow=c(1,1))
plot(bwtfd)
title("Weight coefficient for variable")
print(round(bwtfd$coefs,3))
#  display residual functions
reslist    = result$resfdlist
plot(reslist[[1]])
title("Residual function")
##
##  Example 2:  a single first order varying coefficient unforced equation
##     Dx(t) = -t*x(t) or x(t) = exp(-t^2/2)
bvec    = tvec
xvec    = exp(-tvec^2/2)
xfd     = smooth.basis(tvec, xvec, xbasis)$fd
xfdlist = list(xfd)
bwtlist = list(mfdPar)
#  perform the principal differential analysis
result = pda.fd(xfdlist, bwtlist)
#  display weight coefficient for variable
bwtlistout = result$bwtlist
bwtfd      = bwtlistout[[1]]$fd
par(mfrow=c(1,1))
plot(bwtfd)
title("Weight coefficient for variable")
print(round(bwtfd$coefs,3))
#  display residual function
reslist    = result$resfdlist
plot(reslist[[1]])
title("Residual function")
##
##  Example 3:  a single second order constant coefficient unforced equation
##     Dx(t) = -(2*pi)^2*x(t) or x(t) = sin(2*pi*t)
##
xvec    = sin(2*pi*tvec)
xfd     = smooth.basis(tvec, xvec, xbasis)$fd
xfdlist = list(xfd)
bwtlist = list(cfdPar,cfdPar)
#  perform the principal differential analysis
result = pda.fd(xfdlist, bwtlist)
#  display weight coefficients
bwtlistout = result$bwtlist
bwtfd1     = bwtlistout[[1]]$fd
bwtfd2     = bwtlistout[[2]]$fd
par(mfrow=c(2,1))
plot(bwtfd1)
title("Weight coefficient for variable")
plot(bwtfd2)
title("Weight coefficient for derivative of variable")
print(round(c(bwtfd1$coefs, bwtfd2$coefs),3))
print(bwtfd2$coefs)
#  display residual function
reslist    = result$resfdlist
par(mfrow=c(1,1))
plot(reslist[[1]])
title("Residual function")
##
##  Example 4:  two first order constant coefficient unforced equations
##     Dx1(t) = x2(t) and Dx2(t) = -x1(t)  
##   equivalent to  x1(t) = sin(2*pi*t)
##
xvec1     = sin(2*pi*tvec)
xvec2     = 2*pi*cos(2*pi*tvec)
xfd1      = smooth.basis(tvec, xvec1, xbasis)$fd
xfd2      = smooth.basis(tvec, xvec2, xbasis)$fd
xfdlist   = list(xfd1,xfd2)
bwtlist   = list(
                 list(
                      list(cfdPar),
                      list(cfdPar)
                     ),
                 list(
                      list(cfdPar),
                      list(cfdPar)
                     )
                )
#  perform the principal differential analysis
result = pda.fd(xfdlist, bwtlist)
#  display weight coefficients
bwtlistout = result$bwtlist
bwtfd11    = bwtlistout[[1]][[1]][[1]]$fd
bwtfd21    = bwtlistout[[2]][[1]][[1]]$fd
bwtfd12    = bwtlistout[[1]][[2]][[1]]$fd
bwtfd22    = bwtlistout[[2]][[2]][[1]]$fd
par(mfrow=c(2,2))
plot(bwtfd11)
title("Weight for variable 1 in equation 1")
plot(bwtfd21)
title("Weight for variable 2 in equation 1")
plot(bwtfd12)
title("Weight for variable 1 in equation 2")
plot(bwtfd22)
title("Weight for variable 2 in equation 2")
print(round(bwtfd11$coefs,3))
print(round(bwtfd21$coefs,3))
print(round(bwtfd12$coefs,3))
print(round(bwtfd22$coefs,3))
#  display residual functions
reslist = result$resfdlist
par(mfrow=c(2,1))
plot(reslist[[1]])
title("Residual function for variable 1")
plot(reslist[[2]])
title("Residual function for variable 2")
##
##  Example 5:  a single first order constant coefficient equation
##     Dx = -4*x  for  x(t) = exp(-4t) forced by u(t) = 2
##
beta    = 4
alpha   = 2
xvec0   = exp(-beta*tvec)
intv    = (exp(beta*tvec) - 1)/beta
xvec    = xvec0*(1 + alpha*intv)
xfd     = smooth.basis(tvec, xvec, xbasis)$fd
xfdlist = list(xfd)
bwtlist = list(cfdPar)
awtlist = list(cfdPar)
ufdlist = list(fd(1,cbasis))
#  perform the principal differential analysis
result = pda.fd(xfdlist, bwtlist, awtlist, ufdlist)
#  display weight coefficients
bwtlistout = result$bwtlist
bwtfd      = bwtlistout[[1]]$fd
awtlistout = result$awtlist
awtfd      = awtlistout[[1]]$fd
par(mfrow=c(2,1))
plot(bwtfd)
title("Weight for variable")
plot(awtfd)
title("Weight for forcing function")
#  display residual function
reslist = result$resfdlist
par(mfrow=c(1,1))
plot(reslist[[1]], ylab="residual")
title("Residual function")
##
##  Example 6:  two first order constant coefficient equations
##     Dx = -4*x    for  x(t) = exp(-4t)     forced by u(t) =  2
##     Dx = -4*t*x  for  x(t) = exp(-4t^2/2) forced by u(t) = -1
##
beta    = 4
xvec10  = exp(-beta*tvec)
alpha1  = 2
alpha2  = -1
xvec1   = xvec0 + alpha1*(1-xvec10)/beta
xvec20  = exp(-beta*tvec^2/2)
vvec    = exp(beta*tvec^2/2);
intv    = 0.01*(cumsum(vvec) - 0.5*vvec)
xvec2   = xvec20*(1 + alpha2*intv)
xfd1    = smooth.basis(tvec, xvec1, xbasis)$fd
xfd2    = smooth.basis(tvec, xvec2, xbasis)$fd
xfdlist = list(xfd1, xfd2)
bwtlist    = list(
                 list(
                      list(cfdPar),
                      list(cfdPar)
                     ),
                 list(
                      list(cfdPar),
                      list(mfdPar)
                     )
                )
awtlist = list(list(cfdPar), list(cfdPar))
ufdlist = list(list(fd(1,cbasis)), list(fd(1,cbasis)))
#  perform the principal differential analysis
result = pda.fd(xfdlist, bwtlist, awtlist, ufdlist)
# display weight functions for variables
bwtlistout = result$bwtlist
bwtfd11    = bwtlistout[[1]][[1]][[1]]$fd
bwtfd21    = bwtlistout[[2]][[1]][[1]]$fd
bwtfd12    = bwtlistout[[1]][[2]][[1]]$fd
bwtfd22    = bwtlistout[[2]][[2]][[1]]$fd
par(mfrow=c(2,2))
plot(bwtfd11)
title("weight on variable 1 in equation 1")
plot(bwtfd21)
title("weight on variable 2 in equation 1")
plot(bwtfd12)
title("weight on variable 1 in equation 2")
plot(bwtfd22)
title("weight on variable 2 in equation 2")
print(round(bwtfd11$coefs,3))
print(round(bwtfd21$coefs,3))
print(round(bwtfd12$coefs,3))
print(round(bwtfd22$coefs,3))
#  display weight functions for forcing functions
awtlistout = result$awtlist
awtfd1     = awtlistout[[1]][[1]]$fd
awtfd2     = awtlistout[[2]][[1]]$fd
par(mfrow=c(2,1))
plot(awtfd1)
title("weight on forcing function in equation 1")
plot(awtfd2)
title("weight on forcing function in equation 2")
#  display residual functions
reslist    = result$resfdlist
par(mfrow=c(2,1))
plot(reslist[[1]])
title("residual function for equation 1")
plot(reslist[[2]])
title("residual function for equation 2")



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx(); nameEx("phaseplanePlot")
### * phaseplanePlot

flush(stderr()); flush(stdout())

### Name: phaseplanePlot
### Title: Phase-plane plot
### Aliases: phaseplanePlot
### Keywords: smooth hplot

### ** Examples

goodsbasis <- create.bspline.basis(rangeval=c(1919,2000),
                                   nbasis=979, norder=8)
LfdobjNonDur <- int2Lfd(4) 

library(zoo)
logNondurSm <- smooth.basisPar(argvals=index(nondurables),
                y=log10(coredata(nondurables)), fdobj=goodsbasis,
                Lfdobj=LfdobjNonDur, lambda=1e-11)
phaseplanePlot(1964, logNondurSm$fd)




cleanEx(); nameEx("pinch")
### * pinch

flush(stderr()); flush(stdout())

### Name: pinch
### Title: pinch force data
### Aliases: pinch pinchtime
### Keywords: datasets

### ** Examples

  plot(pinchtime, pinch[, 1], type="b")



cleanEx(); nameEx("plot.basisfd")
### * plot.basisfd

flush(stderr()); flush(stdout())

### Name: plot.basisfd
### Title: Plot a Basis Object
### Aliases: plot.basisfd
### Keywords: smooth

### ** Examples


# set up the b-spline basis for the lip data, using 23 basis functions,
#   order 4 (cubic), and equally spaced knots.
#  There will be 23 - 4 = 19 interior knots at 0.05, ..., 0.95
lipbasis <- create.bspline.basis(c(0,1), 23)
# plot the basis functions
plot(lipbasis)




cleanEx(); nameEx("plot.fd")
### * plot.fd

flush(stderr()); flush(stdout())

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




cleanEx(); nameEx("plot.lmWinsor")
### * plot.lmWinsor

flush(stderr()); flush(stdout())

### Name: plot.lmWinsor
### Title: lmWinsor plot
### Aliases: plot.lmWinsor
### Keywords: hplot

### ** Examples

lm.1 <- lmWinsor(y1~x1, data=anscombe)
plot(lm.1)
plot(lm.1, xlim=c(0, 15), main="other title")

# list example
lm.1. <- lmWinsor(y1~x1, data=anscombe, trim=c(0, 0.25, .4, .5)) 
plot(lm.1.)




cleanEx(); nameEx("plot.pca.fd")
### * plot.pca.fd

flush(stderr()); flush(stdout())

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




graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx(); nameEx("plotfit.fd")
### * plotfit.fd

flush(stderr()); flush(stdout())

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




graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx(); nameEx("polyg")
### * polyg

flush(stderr()); flush(stdout())

### Name: polyg
### Title: Polygonal Basis Function Values
### Aliases: polyg
### Keywords: smooth

### ** Examples


#  set up a set of 21 argument values
x <- seq(0,1,0.05)
#  set up a set of 11 argument values
argvals <- seq(0,1,0.1)
#  with the default period (1) and derivative (0)
basismat <- polyg(x, argvals)
#  plot the basis functions
matplot(x, basismat, type="l")




cleanEx(); nameEx("polygpen")
### * polygpen

flush(stderr()); flush(stdout())

### Name: polygpen
### Title: Polygonal Penalty Matrix
### Aliases: polygpen
### Keywords: smooth

### ** Examples


#  set up a sequence of 11 argument values
argvals <- seq(0,1,0.1)
#  set up the polygonal basis
basisobj <- create.polygonal.basis(argvals)
#  compute the 11 by 11 penalty matrix

penmat <- polygpen(basisobj)




cleanEx(); nameEx("powerbasis")
### * powerbasis

flush(stderr()); flush(stdout())

### Name: powerbasis
### Title: Power Basis Function Values
### Aliases: powerbasis
### Keywords: smooth

### ** Examples


#  set up a set of 10 positive argument values.
x <- seq(0.1,1,0.1)
#  compute values for three power basis functions
exponents <- c(0, 1, 2)
#  evaluate the basis matrix
basismat <- powerbasis(x, exponents)




cleanEx(); nameEx("powerpen")
### * powerpen

flush(stderr()); flush(stdout())

### Name: powerpen
### Title: Power Penalty Matrix
### Aliases: powerpen
### Keywords: smooth

### ** Examples


#  set up an power basis with 3 basis functions.
#  the powers are 0, 1, and 2.
basisobj <- create.power.basis(c(0,1),3,c(0,1,2))
#  compute the 3 by 3 matrix of inner products of second derivatives
#FIXME
#penmat <- powerpen(basisobj, 2)




cleanEx(); nameEx("predict.lmWinsor")
### * predict.lmWinsor

flush(stderr()); flush(stdout())

### Name: predict.lmWinsor
### Title: Predict method for Winsorized linear model fits
### Aliases: predict.lmWinsor
### Keywords: models

### ** Examples

# example from 'anscombe' 
lm.1 <- lmWinsor(y1~x1, data=anscombe)

newD <- data.frame(x1=seq(1, 22, .1))
predW <- predict(lm.1, newdata=newD) 
plot(y1~x1, anscombe, xlim=c(1, 22)) 
lines(newD[["x1"]], predW, col='blue')
abline(h=lm.1[['lower']]['y1'], col='red', lty='dashed') 
abline(h=lm.1[['upper']]['y1'], col='red', lty='dashed')
abline(v=lm.1[['lower']]['x1'], col='green', lty='dashed') 
abline(v=lm.1[['upper']]['x1'], col='green', lty='dashed') 




cleanEx(); nameEx("predict.lmeWinsor")
### * predict.lmeWinsor

flush(stderr()); flush(stdout())

### Name: predict.lmeWinsor
### Title: Predict method for Winsorized linear model fits with mixed
###   effects
### Aliases: predict.lmeWinsor
### Keywords: models

### ** Examples

fm1w <- lmeWinsor(distance ~ age, data = Orthodont,
                 random=~age|Subject)
# predict with newdata 
newDat <- data.frame(age=seq(0, 30, 2),
           Subject=factor(rep("na", 16)) )
pred1w <- predict(fm1w, newDat, level=0)

# fit with 10 percent Winsorization 
fm1w.1 <- lmeWinsor(distance ~ age, data = Orthodont,
                 random=~age|Subject, trim=0.1)
pred30 <- predict(fm1w.1)
stopifnot(all.equal(as.numeric(
              quantile(Orthodont$distance, c(.1, .9))),
          range(pred30)) )




cleanEx(); nameEx("quadset")
### * quadset

flush(stderr()); flush(stdout())

### Name: quadset
### Title: Quadrature points and weights for Simpson's rule
### Aliases: quadset
### Keywords: smooth

### ** Examples

(qs7.1 <- quadset(nquad=7, breaks=c(0, .3, 1)))
# cbind(quadpts=c(seq(0, 0.3, length=7),
#              seq(0.3, 1, length=7)), 
#    quadwts=c((0.3/18)*c(1, 4, 2, 4, 2, 4, 1),
#              (0.7/18)*c(1, 4, 2, 4, 2, 4, 1) ) )

# The simplest basis currently available with this function:
bspl2.2 <- create.bspline.basis(norder=2, breaks=c(0,.5, 1))

bspl2.2a <- quadset(basisobj=bspl2.2)
bspl2.2a$quadvals
# cbind(quadpts=c((0:4)/8, .5+(0:4)/8),
#       quadwts=rep(c(1,4,2,4,1)/24, 2) )
bspl2.2a$values
# a list of length 2
# [[1]] = matrix of dimension c(10, 3) with the 3 basis 
#      functions evaluated at the 10 quadrature points:
# values[[1]][, 1] = c(1, .75, .5, .25, rep(0, 6))
# values[[1]][, 2] = c(0, .25, .5, .75, 1, .75, .5, .25, 0)
# values[[1]][, 3] = values[10:1, 1]
#
# values[[2]] = matrix of dimension c(10, 3) with the
#     first derivative of values[[1]], being either 
#    -2, 0, or 2.  



cleanEx(); nameEx("refinery")
### * refinery

flush(stderr()); flush(stdout())

### Name: refinery
### Title: Reflux and tray level in a refinery
### Aliases: refinery
### Keywords: datasets

### ** Examples

    attach(refinery)
# allow space for an axis on the right 
    op <- par(mar=c(5, 4, 4, 5)+0.1)
# plot uval 
    plot(Time, Reflux, type="l", bty="n")
# add yval 
    y.u <- diff(range(Tray47))/diff(range(Reflux))
    u0 <- min(Reflux)
    y0 <- min(Tray47)

    lines(Time, u0+(Tray47-y0)/y.u, lty=3, lwd=1.5, col="red")
    y.tick <- pretty(range(Tray47))
    axis(4, at=u0+(y.tick)/y.u, labels=y.tick, col="red", lty=3,
            lwd=1.5)
# restore previous plot margins
    par(op)
    detach(refinery)



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx(); nameEx("register.fd")
### * register.fd

flush(stderr()); flush(stdout())

### Name: register.fd
### Title: Register Functional Data Objects Using a Continuous Criterion
### Aliases: register.fd
### Keywords: smooth

### ** Examples

#See the analyses of the growth data for examples.
##
## 1.  Simplest call
##
# Specify smoothing weight 
lambda.gr2.3 <- .03

# Specify what to smooth, namely the rate of change of curvature
Lfdobj.growth    <- 2 

# Establish a B-spline basis
nage <- length(growth$age)
norder.growth <- 6
nbasis.growth <- nage + norder.growth - 2
rng.growth <- range(growth$age)
# 1 18 
wbasis.growth <- create.bspline.basis(rangeval=rng.growth,
                   nbasis=nbasis.growth, norder=norder.growth,
                   breaks=growth$age)

# Smooth consistent with the analysis of these data
# in afda-ch06.R, and register to individual smooths:  
cvec0.growth <- matrix(0,nbasis.growth,1)
Wfd0.growth  <- fd(cvec0.growth, wbasis.growth)
growfdPar2.3 <- fdPar(Wfd0.growth, Lfdobj.growth, lambda.gr2.3)
# Create a functional data object for all the boys
hgtmfd.all <- with(growth, smooth.basis(age, hgtm, growfdPar2.3))

nBoys <- 2
# nBoys <- dim(growth[["hgtm"]])[2]
# register.fd takes time, so use only 2 curves as an illustration
# to minimize compute time in this example;  

#Alternative to subsetting later is to subset now:  
#hgtmfd.all<-with(growth,smooth.basis(age, hgtm[,1:nBoys],growfdPar2.3))

# Register the growth velocity rather than the
# growth curves directly 
smBv <- deriv(hgtmfd.all$fd, 1)

# This takes time, so limit the number of curves registered to nBoys

## Not run: 
##D smB.reg.0 <- register.fd(smBv[1:nBoys])
##D 
##D smB.reg.1 <- register.fd(smBv[1:nBoys],WfdParobj=c(Lfdobj=Lfdobj.growth, lambda=lambda.gr2.3))
##D 
##D ##
##D ## 2.  Call providing the target
##D ##
##D 
##D smBv.mean <- deriv(mean(hgtmfd.all$fd[1:nBoys]), 1)
##D smB.reg.2a <- register.fd(smBv.mean, smBv[1:nBoys],
##D                WfdParobj=c(Lfdobj=Lfdobj.growth, lambda=lambda.gr2.3))
##D 
##D smBv.mean <- mean(smBv[1:nBoys]) 
##D smB.reg.2 <- register.fd(smBv.mean, smBv[1:nBoys],
##D                WfdParobj=c(Lfdobj=Lfdobj.growth, lambda=lambda.gr2.3))
##D all.equal(smB.reg.1, smB.reg.2) 
##D 
##D ##
##D ## 3.  Call using WfdParobj
##D ##
##D 
##D # Create a dummy functional data object
##D # to hold the functional data objects for the
##D # time warping function
##D # ... start with a zero matrix (nbasis.growth, nBoys) 
##D smBc0 <- matrix(0, nbasis.growth, nBoys)
##D # ... convert to a functional data object 
##D smBwfd0 <- fd(smBc0, wbasis.growth)
##D # ... convert to a functional parameter object 
##D smB.wfdPar <- fdPar(smBwfd0, Lfdobj.growth, lambda.gr2.3)
##D 
##D smB.reg.3<- register.fd(smBv[1:nBoys], WfdParobj=smB.wfdPar)
##D all.equal(smB.reg.1, smB.reg.3)
## End(Not run)




cleanEx(); nameEx("sd")
### * sd

flush(stderr()); flush(stdout())

### Name: sd.fd
### Title: Standard Deviation of Functional Data
### Aliases: sd.fd std.fd stdev.fd stddev.fd
### Keywords: smooth

### ** Examples

liptime  <- seq(0,1,.02)
liprange <- c(0,1)

#  -------------  create the fd object -----------------
#       use 31 order 6 splines so we can look at acceleration

nbasis <- 51
norder <- 6
lipbasis <- create.bspline.basis(liprange, nbasis, norder)
lipbasis <- create.bspline.basis(liprange, nbasis, norder)

#  ------------  apply some light smoothing to this object  -------

Lfdobj   <- int2Lfd(4)
lambda   <- 1e-12
lipfdPar <- fdPar(lipbasis, Lfdobj, lambda)

lipfd <- smooth.basis(liptime, lip, lipfdPar)$fd
names(lipfd$fdnames) = c("Normalized time", "Replications", "mm")

lipstdfd <- sd.fd(lipfd)
plot(lipstdfd)

all.equal(lipstdfd, std.fd(lipfd))
all.equal(lipstdfd, stdev.fd(lipfd))
all.equal(lipstdfd, stddev.fd(lipfd))




cleanEx(); nameEx("smooth.basis")
### * smooth.basis

flush(stderr()); flush(stdout())

### Name: smooth.basis
### Title: Smooth Data with an Indirectly Specified Roughness Penalty
### Aliases: smooth.basis
### Keywords: smooth

### ** Examples

##
## Example 1:  Inappropriate smoothing
##
# A toy example that creates problems with
# data2fd:  (0,0) -> (0.5, -0.25) -> (1,1)
b2.3 <- create.bspline.basis(norder=2, breaks=c(0, .5, 1))
# 3 bases, order 2 = degree 1 =
# continuous, bounded, locally linear
fdPar2 <- fdPar(b2.3, Lfdobj=2, lambda=1)

## Not run: 
##D # Penalize excessive slope Lfdobj=1;
##D # second derivative Lfdobj=2 is discontinuous,
##D # so the following generates an error:
##D   fd2.3s0 <- smooth.basis(0:1, 0:1, fdPar2)
##D Derivative of order 2 cannot be taken for B-spline of order 2
##D Probable cause is a value of the nbasis argument
##D  in function create.basis.fd that is too small.
##D Error in bsplinepen(basisobj, Lfdobj, rng) :
## End(Not run)

##
## Example 2.  Better
##
b3.4 <- create.bspline.basis(norder=3, breaks=c(0, .5, 1))
# 4 bases, order 3 = degree 2 =
# continuous, bounded, locally quadratic
fdPar3 <- fdPar(b3.4, lambda=1)
# Penalize excessive slope Lfdobj=1;
# second derivative Lfdobj=2 is discontinuous.
fd3.4s0 <- smooth.basis(0:1, 0:1, fdPar3)

plot(fd3.4s0$fd)
# same plot via plot.fdSmooth
plot(fd3.4s0)

##
## Example 3.  lambda = 1, 0.0001, 0
##
#  Shows the effects of three levels of smoothing
#  where the size of the third derivative is penalized.
#  The null space contains quadratic functions.
x <- seq(-1,1,0.02)
y <- x + 3*exp(-6*x^2) + rnorm(rep(1,101))*0.2
#  set up a saturated B-spline basis
basisobj <- create.bspline.basis(c(-1,1), 101)

fdPar1 <- fdPar(basisobj, 2, lambda=1)
result1  <- smooth.basis(x, y, fdPar1)
with(result1, c(df, gcv, SSE))

##
## Example 4.  lambda = 0.0001
##
fdPar.0001 <- fdPar(basisobj, 2, lambda=0.0001)
result2  <- smooth.basis(x, y, fdPar.0001)
with(result2, c(df, gcv, SSE))
# less smoothing, more degrees of freedom,
# smaller gcv, smaller SSE

##
##  Example 5.  lambda = 0
##
fdPar0 <- fdPar(basisobj, 2, lambda=0)
result3  <- smooth.basis(x, y, fdPar0)
with(result3, c(df, gcv, SSE))
# Saturate fit:  number of observations = nbasis
# with no smoothing, so degrees of freedom = nbasis,
# gcv = Inf indicating overfitting;
# SSE = 0 (to within roundoff error)

plot(x,y)           # plot the data
lines(result1[['fd']], lty=2)  #  add heavily penalized smooth
lines(result2[['fd']], lty=1)  #  add reasonably penalized smooth
lines(result3[['fd']], lty=3)  #  add smooth without any penalty
legend(-1,3,c("1","0.0001","0"),lty=c(2,1,3))

plotfit.fd(y, x, result2[['fd']])  # plot data and smooth

##
## Example 6.  Supersaturated
##
basis104 <- create.bspline.basis(c(-1,1), 104)

fdPar104.0 <- fdPar(basis104, 2, lambda=0)
result104.0  <- smooth.basis(x, y, fdPar104.0)
with(result104.0, c(df, gcv, SSE))

plotfit.fd(y, x, result104.0[['fd']], nfine=501)
# perfect (over)fit
# Need lambda > 0.

##
## Example 7.  gait
##
gaittime  <- (1:20)/21
gaitrange <- c(0,1)
gaitbasis <- create.fourier.basis(gaitrange,21)
lambda    <- 10^(-11.5)
harmaccelLfd <- vec2Lfd(c(0, 0, (2*pi)^2, 0))

gaitfdPar <- fdPar(gaitbasis, harmaccelLfd, lambda)
gaitfd <- smooth.basis(gaittime, gait, gaitfdPar)$fd
## Not run: 
##D # by default creates multiple plots, asking for a click between plots
##D plotfit.fd(gait, gaittime, gaitfd)
## End(Not run)



cleanEx(); nameEx("smooth.basisPar")
### * smooth.basisPar

flush(stderr()); flush(stdout())

### Name: smooth.basisPar
### Title: Smooth Data Using a Directly Specified Roughness Penalty
### Aliases: smooth.basisPar
### Keywords: smooth

### ** Examples

##
## simplest call
##
girlGrowthSm <- with(growth, smooth.basisPar(argvals=age, y=hgtf))
plot(girlGrowthSm$fd, xlab="age", ylab="height (cm)",
         main="Girls in Berkeley Growth Study" )
plot(deriv(girlGrowthSm$fd), xlab="age", ylab="growth rate (cm / year)",
         main="Girls in Berkeley Growth Study" )
plot(deriv(girlGrowthSm$fd, 2), xlab="age",
        ylab="growth acceleration (cm / year^2)",
        main="Girls in Berkeley Growth Study" )
#  Shows the effects of smoothing
#  where the size of the third derivative is penalized.
#  The null space contains quadratic functions.

##
## Another simple call
##
lipSm <- smooth.basisPar(liptime, lip)
plot(lipSm)
# oversmoothing
plot(smooth.basisPar(liptime, lip, lambda=1e-9))
# more sensible 

##
## A third example 
##

x <- seq(-1,1,0.02)
y <- x + 3*exp(-6*x^2) + sin(1:101)/2
# sin not rnorm to make it easier to compare
# results across platforms 

#  set up a saturated B-spline basis
basisobj101 <- create.bspline.basis(x)
fdParobj101 <- fdPar(basisobj101, 2, lambda=1)
result101  <- smooth.basis(x, y, fdParobj101)

resultP <- smooth.basisPar(argvals=x, y=y, fdobj=basisobj101, lambda=1)
## Don't show: 
stopifnot(
## End Don't show
all.equal(result101, resultP)
## Don't show: 
)
## End Don't show
# TRUE 

result4 <- smooth.basisPar(argvals=x, y=y, fdobj=4, lambda=1)
## Don't show: 
stopifnot(
## End Don't show
all.equal(resultP, result4)
## Don't show: 
)
## End Don't show
# TRUE 

result4. <- smooth.basisPar(argvals=x, y=y, lambda=1)
## Don't show: 
stopifnot(
## End Don't show
all.equal(resultP, result4.)
## Don't show: 
)
## End Don't show
# TRUE

with(result4, c(df, gcv)) #  display df and gcv measures

result4.4 <- smooth.basisPar(argvals=x, y=y, lambda=1e-4)
with(result4.4, c(df, gcv)) #  display df and gcv measures
# less smoothing, more degrees of freedom, better fit 

plot(result4.4)
lines(result4, col='green')
lines(result4$fd, col='green') # same as lines(result4, ...)

result4.0 <- smooth.basisPar(x, y, basisobj101, lambda=0)

result4.0a <- smooth.basisPar(x, y, lambda=0)
## Don't show: 
stopifnot(
## End Don't show
all.equal(result4.0, result4.0a)
## Don't show: 
)
## End Don't show

with(result4.0, c(df, gcv)) #  display df and gcv measures
# no smoothing, degrees of freedom = number of points 
# but generalized cross validation = Inf
# suggesting overfitting.  

##
## fdnames?
##
girlGrow12 <- with(growth, smooth.basisPar(argvals=age, y=hgtf[, 1:2],
              fdnames=c('age', 'girl', 'height')) ) 
girlGrow12. <- with(growth, smooth.basisPar(argvals=age, y=hgtf[, 1:2],
    fdnames=list(age=age, girl=c('Carol', 'Sally'), value='height')) )




cleanEx(); nameEx("smooth.fd")
### * smooth.fd

flush(stderr()); flush(stdout())

### Name: smooth.fd
### Title: Smooth a Functional Data Object Using an Indirectly Specified
###   Roughness Penalty
### Aliases: smooth.fd
### Keywords: smooth

### ** Examples


#  Shows the effects of two levels of smoothing
#  where the size of the third derivative is penalized.
#  The null space contains quadratic functions.
x <- seq(-1,1,0.02)
y <- x + 3*exp(-6*x^2) + rnorm(rep(1,101))*0.2
#  set up a saturated B-spline basis
basisobj <- create.bspline.basis(c(-1,1),81)
#  convert to a functional data object that interpolates the data.
result <- smooth.basis(x, y, basisobj)
yfd  <- result$fd

#  set up a functional parameter object with smoothing
#  parameter 1e-6 and a penalty on the 3rd derivative.
yfdPar <- fdPar(yfd, 2, 1e-6)
yfd1 <- smooth.fd(yfd, yfdPar)

## Not run: 
##D # FIXME: using 3rd derivative here gave error?????
##D yfdPar3 <- fdPar(yfd, 3, 1e-6)
##D yfd1.3 <- smooth.fd(yfd, yfdPar3)
##D #Error in bsplinepen(basisobj, Lfdobj, rng) : 
##D #       Penalty matrix cannot be evaluated
##D #  for derivative of order 3 for B-splines of order 4
## End(Not run)

#  set up a functional parameter object with smoothing
#  parameter 1 and a penalty on the 3rd derivative.
yfdPar <- fdPar(yfd, 2, 1)
yfd2 <- smooth.fd(yfd, yfdPar)
#  plot the data and smooth
plot(x,y)           # plot the data
lines(yfd1, lty=1)  #  add moderately penalized smooth
lines(yfd2, lty=3)  #  add heavily  penalized smooth
legend(-1,3,c("0.000001","1"),lty=c(1,3))
#  plot the data and smoothing using function plotfit.fd
plotfit.fd(y, x, yfd1)  # plot data and smooth




cleanEx(); nameEx("smooth.fdPar")
### * smooth.fdPar

flush(stderr()); flush(stdout())

### Name: smooth.fdPar
### Title: Smooth a functional data object using a directly specified
###   roughness penalty
### Aliases: smooth.fdPar
### Keywords: smooth

### ** Examples

#  Shows the effects of two levels of smoothing
#  where the size of the third derivative is penalized.
#  The null space contains quadratic functions.
x <- seq(-1,1,0.02)
y <- x + 3*exp(-6*x^2) + rnorm(rep(1,101))*0.2
#  set up a saturated B-spline basis
basisobj <- create.bspline.basis(c(-1,1),81)
#  convert to a functional data object that interpolates the data.
result <- smooth.basis(x, y, basisobj)
yfd  <- result$fd
#  set up a functional parameter object with smoothing
#  parameter 1e-6 and a penalty on the 2nd derivative.
yfdPar <- fdPar(yfd, 2, 1e-6)
yfd1 <- smooth.fd(yfd, yfdPar)

yfd1. <- smooth.fdPar(yfd, 2, 1e-6)
all.equal(yfd1, yfd1.)
# TRUE

#  set up a functional parameter object with smoothing
#  parameter 1 and a penalty on the 2nd derivative.
yfd2 <- smooth.fdPar(yfd, 2, 1)

#  plot the data and smooth
plot(x,y)           # plot the data
lines(yfd1, lty=1)  #  add moderately penalized smooth
lines(yfd2, lty=3)  #  add heavily  penalized smooth
legend(-1,3,c("0.000001","1"),lty=c(1,3))
#  plot the data and smoothing using function plotfit.fd
plotfit.fd(y, x, yfd1)  # plot data and smooth




cleanEx(); nameEx("smooth.monotone")
### * smooth.monotone

flush(stderr()); flush(stdout())

### Name: smooth.monotone
### Title: Monotone Smoothing of Data
### Aliases: smooth.monotone
### Keywords: smooth

### ** Examples


#  Estimate the acceleration functions for growth curves
#  See the analyses of the growth data.
#  Set up the ages of height measurements for Berkeley data
age <- c( seq(1, 2, 0.25), seq(3, 8, 1), seq(8.5, 18, 0.5))
#  Range of observations
rng <- c(1,18)
#  First set up a basis for monotone smooth
#  We use b-spline basis functions of order 6
#  Knots are positioned at the ages of observation.
norder <- 6
nage   <- 31
nbasis <- nage + norder - 2
wbasis <- create.bspline.basis(rng, nbasis, norder, age)
#  starting values for coefficient
cvec0 <- matrix(0,nbasis,1)
Wfd0  <- fd(cvec0, wbasis)
#  set up functional parameter object
Lfdobj    <- 3          #  penalize curvature of acceleration
lambda    <- 10^(-0.5)  #  smoothing parameter
growfdPar <- fdPar(Wfd0, Lfdobj, lambda)
#  Set up wgt vector
wgt   <- rep(1,nage)
#  Smooth the data for the first girl
hgt1 = growth$hgtf[,1]
result <- smooth.monotone(age, hgt1, growfdPar, wgt)
#  Extract the functional data object and regression
#  coefficients
Wfd  <- result$Wfdobj
beta <- result$beta
#  Evaluate the fitted height curve over a fine mesh
agefine <- seq(1,18,len=101)
hgtfine <- beta[1] + beta[2]*eval.monfd(agefine, Wfd)
#  Plot the data and the curve
plot(age, hgt1, type="p")
lines(agefine, hgtfine)
#  Evaluate the acceleration curve
accfine <- beta[2]*eval.monfd(agefine, Wfd, 2)
#  Plot the acceleration curve
plot(agefine, accfine, type="l")
lines(c(1,18),c(0,0),lty=4)




cleanEx(); nameEx("smooth.pos")
### * smooth.pos

flush(stderr()); flush(stdout())

### Name: smooth.pos
### Title: Smooth Data with a Positive Function
### Aliases: smooth.pos
### Keywords: smooth

### ** Examples

#See the analyses of the daily weather data for examples.



cleanEx(); nameEx("tperm.fd")
### * tperm.fd

flush(stderr()); flush(stdout())

### Name: tperm.fd
### Title: Permutation t-test for two groups of functional data objects.
### Aliases: tperm.fd
### Keywords: smooth

### ** Examples
 
# This tests the difference between boys and girls heights in the Berkeley
# growth data.

# First set up a basis system to hold the smooths

knots  <- growth$age
norder <- 6
nbasis <- length(knots) + norder - 2
hgtbasis <- create.bspline.basis(range(knots), nbasis, norder, knots)

# Now smooth with a fourth-derivative penalty and a very small smoothing
# parameter

Lfdobj <- 4
lambda <- 1e-2
growfdPar <- fdPar(hgtbasis, Lfdobj, lambda)

hgtmfd <- smooth.basis(growth$age, growth$hgtm, growfdPar)$fd
hgtffd <- smooth.basis(growth$age, growth$hgtf, growfdPar)$fd

# Call tperm.fd

tres <- tperm.fd(hgtmfd,hgtffd)



cleanEx(); nameEx("var.fd")
### * var.fd

flush(stderr()); flush(stdout())

### Name: var.fd
### Title: Variance, Covariance, and Correlation Surfaces for Functional
###   Data Object(s)
### Aliases: var.fd
### Keywords: smooth

### ** Examples

##
## Example with 2 different bases 
##
daybasis3 <- create.fourier.basis(c(0, 365))
daybasis5 <- create.fourier.basis(c(0, 365), 5)
tempfd3 <- with(CanadianWeather, data2fd(dailyAv[,,"Temperature.C"], 
       day.5, daybasis3, argnames=list("Day", "Station", "Deg C")) )
precfd5 <- with(CanadianWeather, data2fd(dailyAv[,,"log10precip"],
       day.5, daybasis5, argnames=list("Day", "Station", "Deg C")) )

# Compare with structure described above under 'value':
str(tempPrecVar3.5 <- var.fd(tempfd3, precfd5))

##
## Example with 2 variables, same bases
##
gaitbasis3 <- create.fourier.basis(nbasis=3)
str(gaitfd3 <- data2fd(gait, basisobj=gaitbasis3))
str(gaitVar.fd3 <- var.fd(gaitfd3))

# Check the answers with manual computations 
all.equal(var(t(gaitfd3$coefs[,,1])), gaitVar.fd3$coefs[,,,1])
# TRUE
all.equal(var(t(gaitfd3$coefs[,,2])), gaitVar.fd3$coefs[,,,3])
# TRUE
all.equal(var(t(gaitfd3$coefs[,,2]), t(gaitfd3$coefs[,,1])),
          gaitVar.fd3$coefs[,,,2])
# TRUE

# NOTE:
dimnames(gaitVar.fd3$coefs)[[4]]
# [1] Hip-Hip
# [2] Knee-Hip 
# [3] Knee-Knee
# If [2] were "Hip-Knee", then
# gaitVar.fd3$coefs[,,,2] would match 
#var(t(gaitfd3$coefs[,,1]), t(gaitfd3$coefs[,,2]))
# *** It does NOT.  Instead, it matches:  
#var(t(gaitfd3$coefs[,,2]), t(gaitfd3$coefs[,,1])),

##
## The following produces contour and perspective plots
##
# Evaluate at a 53 by 53 grid for plotting

daybasis65 <- create.fourier.basis(rangeval=c(0, 365), nbasis=65)

daytempfd <- with(CanadianWeather, data2fd(dailyAv[,,"Temperature.C"],
       day.5, daybasis65, argnames=list("Day", "Station", "Deg C")) )
str(tempvarbifd <- var.fd(daytempfd))

str(tempvarmat  <- eval.bifd(weeks,weeks,tempvarbifd))
# dim(tempvarmat)= c(53, 53)

op <- par(mfrow=c(1,2), pty="s")
#contour(tempvarmat, xlab="Days", ylab="Days")
contour(weeks, weeks, tempvarmat, 
        xlab="Daily Average Temperature",
        ylab="Daily Average Temperature",
        main=paste("Variance function across locations\n",
          "for Canadian Anual Temperature Cycle"),
        cex.main=0.8, axes=FALSE)
axisIntervals(1, atTick1=seq(0, 365, length=5), atTick2=NA, 
            atLabels=seq(1/8, 1, 1/4)*365,
            labels=paste("Q", 1:4) )
axisIntervals(2, atTick1=seq(0, 365, length=5), atTick2=NA, 
            atLabels=seq(1/8, 1, 1/4)*365,
            labels=paste("Q", 1:4) )
persp(weeks, weeks, tempvarmat,
      xlab="Days", ylab="Days", zlab="Covariance")
mtext("Temperature Covariance", line=-4, outer=TRUE)
par(op)




graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx(); nameEx("vec2Lfd")
### * vec2Lfd

flush(stderr()); flush(stdout())

### Name: vec2Lfd
### Title: Make a Linear Differential Operator Object from a Vector
### Aliases: vec2Lfd
### Keywords: smooth

### ** Examples

#  define the harmonic acceleration operator used in the
#  analysis of the daily temperature data
harmaccelLfd <- vec2Lfd(c(0,(2*pi/365)^2,0), c(0,365))



cleanEx(); nameEx("zerofind")
### * zerofind

flush(stderr()); flush(stdout())

### Name: zerofind
### Title: Does the range of the input contain 0?
### Aliases: zerofind
### Keywords: logic

### ** Examples

zerofind(1:5)
# FALSE
zerofind(0:3)
# TRUE 



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
