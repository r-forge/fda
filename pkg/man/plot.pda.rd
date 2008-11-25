\name{plot.pda}
\alias{plot.pda}
\title{
  Plot Prindiple Differential Analysis Components
}
\description{
  Plots the results of pda.fd, allows the user to group coefficient functions
  by variable, equation, derivative or combination of them.
}
\usage{
plot.pda(pdaList,whichdim=1,npts=501,...)
}
\arguments{
  \item{pdaList}{
    a list object returned by \code{pda.fd}.
  }
  \item{whichdim}{
    which dimension to use as grouping variables
    \item{1}{ coefficients of each variable differential equation}
    \item{2}{ coefficient functions for each equation}
    \item{3}{ coefficients of derivatives of each variable}
    \code{whichdim} should be an ordered vector of length between 1 and 3.
  }
  \item{npts}{
    number of points to use for plotting.
  }
  \item{\dots}{
    other arguments for 'plot'.  
  }
}
\details{
  Produces one plot for each coefficient function in a principle differential
  analysis.
}
\value{
  invisible(NULL) 
}
\seealso{
  \code{\link{pda.fd}}
  \code{\link{eigen.pda}}
}
\examples{

#  A pda analysis of the handwriting data

fdaarray = handwrit
fdatime  <- seq(0, 2.3, len=1401)

#  basis for coordinates

fdarange <- c(0, 2.3)
breaks = seq(0,2.3,length.out=501)
norder = 6
fdabasis = create.bspline.basis(fdarange,norder=norder,breaks=breaks)

#  parameter object for coordinates

fdaPar = fdPar(fdabasis,int2Lfd(4),1e-8)

#  coordinate functions and a list tontaining them

Xfd = smooth.basis(fdatime, fdaarray[,,1], fdaPar)$fd
Yfd = smooth.basis(fdatime, fdaarray[,,2], fdaPar)$fd

xfdlist = list(Xfd, Yfd)

#  basis and parameter object for weight functions

fdabasis2 = create.bspline.basis(fdarange,norder=norder,nbasis=51)
pdaPar = fdPar(fdabasis2,1,1e-8)

pdaParlist = list(pdaPar, pdaPar)

bwtlist = list( list(pdaParlist,pdaParlist), list(pdaParlist,pdaParlist) )

#  do the second order pda

pdaList = pda.fd(xfdlist, bwtlist)

# plot the results

plot.pda(pdaList,whichdim=1)
plot.pda(pdaList,whichdim=2)
plot.pda(pdaList,whichdim=3)

plot.pda(pdaList,whichdim=c(1,2))
plot.pda(pdaList,whichdim=c(1,3))
plot.pda(pdaList,whichdim=c(2,3))

plot.pda(pdaList,whichdim=1:3)
}

\keyword{smooth}