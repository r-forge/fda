###
###
### Ramsey, Hooker & Graves (2009)
### Functional Data Analysis with R and Matlab (Springer)
###
### ch. 11  Functional Models and Dynamics
###
library(fda)

##
## Section 11.1  Introduction to Dynamics
##

# section 11.1.2 Interpreting Second Order Linear Dynamics
#  Figure 11.1

x = seq(-1, 1, length=201)
x2 <- x^2
op = par(xpd=TRUE)
plot(x, x2, xlim=c(-1,1), axes=FALSE, lwd=2, type='l', xlab='', ylab='')
axis(1, 0)
axis(2, 0, las=1)
lines(c(-1, 1), c(0, 0), lty='dashed')
lines(c(0, 0), c(0, 1), lty='dashed')
text(-1.2, .5, expression(beta[0]), cex=2 )
text(0, -0.2, expression(beta[1]), cex=2)
text(-0.4, 0.8, "Increasing\nOscillations", cex=2)
text(0.4, 0.8, "Increasing\nOscillations", cex=2)
text(-0.7, 0.005, 'Exponential\nGrowth', cex=2)
text(0.7, 0.005, 'Exponential\nDecay', cex=2)
arrows(-0.3, 0.2, -0.4, 0.16, length=0.1, lwd=2)
text(-0.29, 0.2, "d=0", adj=0, cex=2)
par(op)

##
## Section 11.2 Principal Differential Analysis for Linear Dynamics
##
#  (no computations in this section)

##
## Section 11.3 Principal Differential Analysis of the Lip Data
##
# Figure 11.2
matplot(liptime, lip, type = 'l',
        xlab='Normalized Time', ylab='lip position (mm)')

lipbasis = create.bspline.basis(breaks=liptime)
lipfd    = Data2fd(liptime, lip, lipbasis)
bwtlist  = list(fdPar(lipbasis,2,0),
                fdPar(lipbasis,2,0) )
pdaList  = pda.fd(lipfd,bwtlist)

plot.pda.fd(pdaList)
# ????


## ***???


dfd <- 0.25*pdaList$bwtlist[[2]]$fd^2
             - pdaList$bwtlist[[1]]$fd
dfd$fdnames = list(’time’,’rep’,’discriminant’)

# Figure 11.3 ????



# Figure 11.4
pda.overlay(pdaList)



##
## Section 11.4 PDA of the Handwriting Data
##
xfdlist = list(fdafd[,1],fdafd[,2],fdafd[,3])

pdaPar = fdPar(fdabasis,2,0)
pdaParlist = list(pdaPar, pdaPar)
bwtlist = list( list(pdaParlist,pdaParlist),
    list(pdaParlist,pdaParlist) )
pdaList = pda.fd(xfdlist, bwtlist)


eigen.pda(pdaList)

# Figure 11.5









##
## Section 11.5 Registration and PDA
##
lipreglist = landmarkreg(lipfd, as.matrix(lipmarks),
    lipmeanmarks, WfdPar)
Dlipregfd = register.newfd(deriv.fd(lipfd,1),
    lipreglist$warpfd)
D2lipregfd = register.newfd(deriv.fd(lipfd,2),
    lipreglist$warpfd)
xfdlist = list(-Dlipregfd,-lipreglist$regfd)
lipregpda = fRegress( D2lipregfd, xfdlist, bwtlist)



##
## Section 11.6 Details for pda.fd, eigen.fd, pda.overlay
##              and register.newfd
##



##
## Section 11.7 Some Things to Try
##
# (exercises for the reader)

##
## Section 11.8  More to Read
##
