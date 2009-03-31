###
###
### Ramsay, Hooker & Graves (2009)
### Functional Data Analysis with R and Matlab (Springer)
###
### ch. 8.  Registration: Aligning Features
###         for Samples of Curves
###

library(fda)

##
## Section 8.1 Amplitude and Phase Variation
##

#  set up ages of measurement and an age mesh

age     = growth$age
nage    = length(age)
ageRng  = range(age)
nfine   = 101
agefine = seq(ageRng[1], ageRng[2], length=nfine)

#  the data

hgtf   = growth$hgtf
ncasef = dim(hgtf)[2]

#  an order 6 bspline basis with knots at ages of measurement

norder = 6
nbasis = nage + norder - 2
wbasis = create.bspline.basis(ageRng, nbasis, norder, age)

#  define the roughness penalty for function W

Lfdobj    = 3          #  penalize curvature of acceleration
lambda    = 10^(-0.5)  #  smoothing parameter
cvecf     = matrix(0, nbasis, ncasef)
Wfd0      = fd(cvecf, wbasis)
growfdPar = fdPar(Wfd0, Lfdobj, lambda)

#  monotone smoothing

growthMon = smooth.monotone(age, hgtf, growfdPar)

# (wait for an iterative fit to each of 54 girls)

Wfd        = growthMon$Wfd
betaf      = growthMon$beta
hgtfhatfd  = growthMon$yhatfd

#  Set up functional data objects for the acceleration curves 
#  and their mean.  Suffix UN means "unregistered".

accelfdUN     = deriv.fd(hgtfhatfd, 2)
accelmeanfdUN = mean(accelfdUN)

#  plot unregistered curves

par(ask=F)
plot(accelfdUN, xlim=c(1,18), ylim=c(-4,3), lty=1, lwd=2,
     cex=2, xlab="Age", ylab="Acceleration (cm/yr/yr)")

#  This is a manual PGS spurt identification procedure requiring
#  a mouse click at the point where the acceleration curve
#  crosses the zero axis with a negative slope during puberty.
#  A second mouse click advances the plot to the next case.
#  Here we do this only for the first 10 children.

children = 1:10

PGSctr = rep(0,length(children))
par(mfrow=c(1,1), ask=T)
for (icase in children) {
    accveci = eval.fd(agefine, accelfdUN[icase])
    plot(agefine,accveci,"l", ylim=c(-6,4),
         xlab="Year", ylab="Height Accel.", 
         main=paste("Case",icase))
    lines(c(1,18),c(0,0),lty=2)
    PGSctr[icase] = locator(1)$x
}

#  This is an automatic PGS spurt identification procedure.
#  A mouse click advances the plot to the next case.
#  Compute PGS mid point for landmark registration.
#  Downward crossings are computed within the limits defined
#  by INDEX.  Each of the crossings within this interval 
#  are plotted.  The estimated PGS center is plotted as a vertical line.

#  The choice of range of argument values (6--18) to consider
#  for a potential mid PGS location is determined by previous
#  analyses, where they have a mean of about 12 and a s.d. of 1.

#  We compute landmarks for all 54 children

index  = 1:102  #  wide limits
nindex = length(index)
ageval = seq(8.5,15,len=nindex)
PGSctr = rep(0,ncasef)
par(mfrow=c(1,1), ask=T)
for (icase in 1:ncasef) {
    accveci = eval.fd(ageval, accelfdUN[icase])
    aup     = accveci[2:nindex]
    adn     = accveci[1:(nindex-1)]
    indx    = (1:102)[adn*aup < 0 & adn > 0]
    plot(ageval[2:nindex],aup,"p",
         xlim=c(7.9,18), ylim=c(-6,4))
    lines(c(8,18),c(0,0),lty=2)
    for (j in 1:length(indx)) {
        indxj = indx[j]
        aupj  = aup[indxj]
        adnj  = adn[indxj]
        agej  = ageval[indxj] + 0.1*(adnj/(adnj-aupj))
        if (j == length(indx)) {
            PGSctr[icase] = agej
            lines(c(agej,agej),c(-4,4),lty=1)
        } else {
            lines(c(agej,agej),c(-4,4),lty=3)
        }
    }
    title(paste('Case ',icase))
}

#  
# Landmark registration
#

#  We use the minimal basis function sufficient to fit 3 points
#  remember that the first coefficient is set to 0, so there
#  are three free coefficients, and the data are two boundary
#  values plus one interior knot.
#  Suffix LM means "Landmark-registered".

PGSctrmean = mean(PGSctr)

#  Define the basis for the function W(t).  

wbasisLM = create.bspline.basis(c(1,18), 4, 3, c(1,PGSctrmean,18))
WfdLM    = fd(matrix(0,4,1),wbasisLM)
WfdParLM = fdPar(WfdLM,1,1e-12)

#  Carry out landmark registration.  

registerlistLM = landmarkreg(accelfdUN, PGSctr, PGSctrmean, 
                             WfdParLM, TRUE)

accelfdLM     = registerlistLM$regfd 
accelmeanfdLM = mean(accelfdLM)

#  plot registered curves

par(ask=F)
plot(accelfdLM, xlim=c(1,18), ylim=c(-4,3), lty=1, lwd=1,
     cex=2, xlab="Age", ylab="Acceleration (cm/yr/yr)")
lines(accelmeanfdLM, col=1, lwd=2, lty=2)
lines(c(PGSctrmean,PGSctrmean), c(-4,3), lty=2, lwd=1.5)

# Figure 8.1

accelmeanfdUN10 = mean(accelfdUN[children])
accelmeanfdLM10 = mean(accelfdLM[children])

op = par(mfrow=c(2,1))
plot(accelfdUN[children], xlim=c(1,18), ylim=c(-3,1.5), lty=1, lwd=1,
     cex=2, xlab="", ylab="Acceleration (cm/yr/yr)")
lines(accelmeanfdUN10, col=1, lwd=2, lty=2)
lines(c(PGSctrmean,PGSctrmean), c(-3,1.5), lty=2, lwd=1.5)
plot(accelfdLM[children], xlim=c(1,18), ylim=c(-3,1.5), lty=1, lwd=1,
     cex=2, xlab="Age (Years)", ylab="Acceleration (cm/yr/yr)")
lines(accelmeanfdLM10, col=1, lwd=2, lty=2)
lines(c(PGSctrmean,PGSctrmean), c(-3,1.5), lty=2, lwd=1.5)
par(op)

# Figure 8.2 not computed here.

#  plot warping functions for cases 3 and 7

warpfdLM  = registerlistLM$warpfd
warpmatLM = eval.fd(agefine, warpfdLM)

op = par(mfrow=c(1,1))
matplot(agefine, warpmatLM[,c(3,7)], "l", lty=1, lwd=2, col=1, cex=1.2,
        xlab="Clock years", ylab="Growth years")
lines(agefine,  agefine, lty=2, lwd=1.5)
lines(c(PGSctrmean,PGSctrmean), c(1,18), lty=2, lwd=1.5)
text(c(PGSctrmean,PGSctrmean), warpmatLM[61,c(3,7)]+0.5, c("3","7"))
text(PGSctrmean, 6,"Early")
text(PGSctrmean,17,"Late")

# Figure 8.3

op = par(mfrow=c(2,2))
plot(accelfdUN[3], xlim=c(1,18), ylim=c(-3,1.5), lty=1, lwd=2,
     xlab="", ylab="")
lines(c(PGSctrmean,PGSctrmean), c(-3,1.5), lty=2, lwd=1.5)
plot(agefine, warpmatLM[,3], "l", lty=1, lwd=2, col=1, cex=1.2,
        xlab="", ylab="")
lines(agefine,  agefine, lty=2, lwd=1.5)
lines(c(PGSctrmean,PGSctrmean), c(1,18), lty=2, lwd=1.5)
text(PGSctrmean+0.1, warpmatLM[61,3]+0.3, "o", lwd=2)

plot(accelfdUN[7], xlim=c(1,18), ylim=c(-3,1.5), lty=1, lwd=2,
     xlab="", ylab="")
lines(c(PGSctrmean,PGSctrmean), c(-3,1.5), lty=2, lwd=1.5)
plot(agefine, warpmatLM[,7], "l", lty=1, lwd=2, col=1, cex=1.2,
        xlab="", ylab="")
lines(c(PGSctrmean,PGSctrmean), c(1,18), lty=2, lwd=1.5)
lines(agefine,  agefine, lty=2, lwd=1.5)
text(PGSctrmean+0.1, warpmatLM[61,7]+0.2, "o", lwd=2)
par(op)

##
## Section 8.5 A Decomposition into Amplitude and Phase Sums of Squares
##

AmpPhasList = AmpPhaseDecomp(accelfdUN, accelfdLM, warpfdLM)
RSQRLM      = AmpPhasList$RSQR
CLM         = AmpPhasList$C

print(paste("R-squared =", round(RSQRLM,3), ",  C =", round(CLM,3)))

#  
#  Continuous registration
#  

#  Set up a cubic spline basis for continuous registration

nwbasisCR = 15
norderCR  =  5
wbasisCR  = create.bspline.basis(c(1,18), nwbasisCR, norderCR)
Wfd0CR    = fd(matrix(0,nwbasisCR,ncasef),wbasisCR)
lambdaCR  = 1
WfdParCR  = fdPar(Wfd0CR, 1, lambdaCR)

#  carry out the registration

registerlistCR = register.fd(accelmeanfdLM, accelfdLM, WfdParCR)

accelfdCR = registerlistCR$regfd
warpfdCR  = registerlistCR$warpfd
WfdCR     = registerlistCR$Wfd

par(mfrow=c(1,1))
plot(warpfdCR)

AmpPhasList = AmpPhaseDecomp(accelfdLM, accelfdCR, warpfdCR)
RSQRCR      = AmpPhasList$RSQR
CCR         = AmpPhasList$C

print(paste("R-squared =", round(RSQRCR,3), ",  C =", round(CCR,3)))

#  plot landmark and continuously registered curves for the
#  first 10 children

accelmeanfdCR10 = mean(accelfdCR[children])

op = par(mfrow=c(2,1))
plot(accelfdLM[children], xlim=c(1,18), ylim=c(-3,1.5), lty=1, lwd=1,
     cex=2, xlab="Age (Years)", ylab="Acceleration (cm/yr/yr)")
lines(accelmeanfdLM10, col=1, lwd=2, lty=2)
lines(c(PGSctrmean,PGSctrmean), c(-3,1.5), lty=2, lwd=1.5)
plot(accelfdCR[children], xlim=c(1,18), ylim=c(-3,1.5), lty=1, lwd=1,
     cex=2, xlab="Age (Years)", ylab="Acceleration (cm/yr/yr)")
lines(accelmeanfdCR10, col=1, lwd=2, lty=2)
lines(c(PGSctrmean,PGSctrmean), c(-3,1.5), lty=2, lwd=1.5)
par(op)

#  plot all landmark and continuously registered curves 

accelmeanfdCR = mean(accelfdCR)

op = par(mfrow=c(2,1))
plot(accelfdLM, xlim=c(1,18), ylim=c(-4,3), lty=1, lwd=1,
     cex=2, xlab="Age (Years)", ylab="Acceleration (cm/yr/yr)")
lines(accelmeanfdLM, col=1, lwd=2, lty=2)
lines(c(PGSctrmean,PGSctrmean), c(-4,3), lty=2, lwd=1.5)
plot(accelfdCR, xlim=c(1,18), ylim=c(-4,3), lty=1, lwd=1,
     cex=2, xlab="Age (Years)", ylab="Acceleration (cm/yr/yr)")
lines(accelmeanfdCR, col=1, lwd=2, lty=2)
lines(c(PGSctrmean,PGSctrmean), c(-4,3), lty=2, lwd=1.5)
par(op)


# Figure 8.4

par(mfrow=c(1,1))
plot(accelfdCR[children], xlim=c(1,18), ylim=c(-3,1.5), lty=1, lwd=1,
     cex=2, xlab="Age (Years)", ylab="Acceleration (cm/yr/yr)")
lines(accelmeanfdCR10, col=1, lwd=2, lty=2)
lines(c(PGSctrmean,PGSctrmean), c(-3,1.5), lty=2, lwd=1.5)
par(op)

# Figure 8.5

accelmeanfdUN = mean(accelfdUN)
accelmeanfdLM = mean(accelfdLM)
accelmeanfdCR = mean(accelfdCR)
   
plot(accelmeanfdCR, xlim=c(1,18), ylim=c(-3,1.5), lty=1, lwd=2,
     cex=1.2, xlab="Years", ylab="Height Acceleration")
lines(accelmeanfdLM, lwd=1.5, lty=1)
lines(accelmeanfdUN, lwd=1.5, lty=2)

##
## 8.6 Registering the Chinese Handwriting Data
##

# Figure 8.6

##
## 8.7 Details for Functions landmarkreg and register.fd
##
help(landmarkreg)
help(register.fd)

##
## Section 8.8 Some Things to Try
##
# (exercises for the reader)

##
## Section 8.8  More to Read
##
