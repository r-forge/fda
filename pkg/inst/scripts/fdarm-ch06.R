###
###
### Ramsey, Hooker & Graves (2009)
### Functional Data Analysis with R and Matlab (Springer)
###
### ch. 6.  Descriptions of Functional Data
###
library(fda)

##
## Section 6.1 Some Functional Descriptive Statistics
##
#  using 'logprec.fd' computed in fdarm-ch05.R as follows:
logprecav = CanadianWeather$dailyAv[
         dayOfYearShifted, , 'log10precip']
dayrange  = c(0,365)
daybasis  = create.fourier.basis(dayrange, 365)

Lcoef        = c(0,(2*pi/diff(dayrange))^2,0)
harmaccelLfd = vec2Lfd(Lcoef, dayrange)

lambda   = 1e6
fdParobj = fdPar(daybasis, harmaccelLfd, lambda)
logprec.fit = smooth.basis(day.5, logprecav, fdParobj)
logprec.fd = logprec.fit$fd

  meanlogprec   = mean(logprec.fd)
  stddevlogprec = std.fd(logprec.fd)

# Section 6.1.1 The Bivariate Covariance Function v(s; t)
  logprecvar.bifd = var.fd(logprec.fd)

  weektime        = seq(0,365,length=53)
  logprecvar_mat  = eval.bifd(weektime, weektime,
                              logprecvar.bifd)
# Figure 6.1
  persp(weektime, weektime, logprecvar_mat,
        theta=-45, phi=25, r=3, expand = 0.5,
        ticktype='detailed',
        xlab="Day (July 1 to June 30)",
        ylab="Day (July 1 to June 30)",
        zlab="variance(log10 precip)")

  contour(weektime, weektime, logprecvar_mat)

# Figure 6.2
day5time = seq(0,365,5)
logprec.varmat = eval.bifd(day5time, day5time,
                    logprecvar.bifd)
contour(day5time, day5time, logprec.varmat,
        xlab="Day (July 1 to June 30)",
        ylab="Day (July 1 to June 30)", lwd=2,
        labcex=1)

##
## Section 6.2 The Residual Variance-Covariance Matrix Se
##
#  (no computations in this section)

##
## Section 6.3 Functional Probes rho[xi]
##

# xifd, xfd = ??? ... need an example
probeval = inprod(xifd, xfd)
# see section 6.5 below?


##
## Section 6.4 Phase-plane Plots of Periodic Effects
##

# Fig. 6.3 The log nondurable goods index for 1964 to 1967

# copy from script for fda or afda


# section 6.4.1 Phase-plane Plots Show Energy Transfer
# Fig. 6.4 A phase-plane plot of the simple harmonic function sin(2pt).
# copy from script for fda or afda



# section 6.4.2 The Nondurable Goods Cycles
# Fig. 6.5 Phase-plane plot of the first and second
# derivatoves of the smoothed log nondurable goods index for 1964.

# copy from script for fda or afda



# sec. 6.4.3.  Phase-Plane Plotting the Growth of Girls
agefine  = linspace(1,18,101);
velffine = eval_fd(agefine, hgtfmonfd(1:10), 1);
accffine = eval_fd(agefine, hgtfmonfd(1:10), 2);
phdl = plot(velffine, accffine, 'k-', ...
            [1,18], [0,0], 'k:');
set(phdl, 'LineWidth', 1)
hold on
phdl = plot(velffine(:,6), accffine(:,6), ...
            'k--', [0,12], [0,0], 'k:');
set(phdl, 'LineWidth', 2)
phdl=plot(velffine(64,index), accffine(64,index), ...
          'ko');
set(phdl, 'LineWidth', 2)
hold off
xlabel('\fontsize{13} Velocity (cm/yr)')
ylabel('\fontsize{13} Acceleration (cm/yr^2)')
axis([0,12,-5,2])

##
## Section 6.5 Confidence Intervals for Curves and their Derivatives
##

# sec. 6.5.1.  Two Linear mappings Defining a Probe Value
dayvec  = seq(0,365,len=101)
xivec   = exp(20*cos(2*pi*(dayvec-197)/365))
xibasis = create.bspline.basis(c(0,365),13)
xifd    = smooth.basis(dayvec, xivec, xibasis)$fd

# tempbasis????

tempLmat = inprod(tempbasis, xifd)
precLmat = inprod(precbasis, xifd)

# sec. 6.5.3.  Confidence Limits for Prince Rupert's Log Precipitation
logprecav = CanadianWeather$dailyAv[
         dayOfYearShifted, , 'log10precip']

# as in section 5.3
lambda    = 1e6

dayrange  = c(0,365)
daybasis  = create.fourier.basis(dayrange, 365)
Lcoef        = c(0,(2*pi/diff(dayrange))^2,0)
harmaccelLfd = vec2Lfd(Lcoef, dayrange)

fdParobj  = fdPar(daybasis, harmaccelLfd, lambda)
logprecList= smooth.basis(day.5, logprecav, fdParobj)
logprec.fd = logprecList$fd
fdnames = list("Day (July 1 to June 30)",
               "Weather Station" = CanadianWeather$place,
               "Log10 Precipitation (mm)")
logprec.fd$fdnames = fdnames

logprecmat  = eval.fd(day.5, logprec.fd)
logprecres  = logprecav - logprecmat
logprecvar  = apply(logprecres^2, 1, sum)/(35-1)
lambda      = 1e8
resfdParobj = fdPar(daybasis, harmaccelLfd, lambda)
logvar.fit  = smooth.basis(day.5, log(logprecvar),
                           resfdParobj)
logvar.fd   = logvar.fit$fd
varvec      = exp(eval.fd(day.5, logvar.fd))
SigmaE      = diag(as.vector(varvec))

y2cMap = logprecList$y2cMap
c2rMap = eval.basis(day.5, daybasis)
Sigmayhat = c2rMap %*% y2cMap %*% SigmaE %*%
           t(y2cMap) %*% t(c2rMap)
logprec.stderr = sqrt(diag(Sigmayhat))
logprec29 = eval.fd(day.5, logprec.fd[29])
plot(logprec.fd[29], lwd=2, ylim=c(0.2, 1.3))
lines(day.5, logprec29 + 2*logprec.stderr,
        lty=2, lwd=2)
lines(day.5, logprec29 - 2*logprec.stderr,
        lty=2, lwd=2)
points(day.5, logprecav[,29])

##
## Section 6.6 Some Things to Try
##
# (exercises for the reader)
