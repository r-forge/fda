\name{plotGCVRMSE.fd}
\alias{plotGCVRMSE.fd}
\title{
  Plot showing how df, gcv and RMSE depend on log10 smoothing
}
\description{
  A roughness penalized smoothing is quite flexible, allowing continuous
  control over smoothness and degrees of freedom, can be adapted to
  known features in the curve, and will generally provide better RMSE
  for given degrees of freedom.  It does require a bit more effort,
  though. First, we define a little display function for showing how df,
  gcv and RMSE depend on the log10 smoothing parameter.
}
\usage{
plotGCVRMSE.fd(lamlow, lamhi, lamdel, argvals, y,
            fdParobj, wtvec=NULL, fdnames=NULL, covariates=NULL, ...)
}
\arguments{
  \item{lamlow, lamhi, lamdel}{
    numeric scalars giving the low (lamlow) and high (lamhi) ends of the
    range for log10(smoothing) and the increment (lamdel, on the log
    scale).
  }
  \item{argvals, y, wtvec, fdnames, covariates}{
    arguments passed to smooth.basis
  }
  \item{fdParobj}{
    'fdParobj' object passed to smooth.basis after modifying the
    component 'lambda' with 10$^$loglambda (where loglambda =
    seq(lamlow, lamhi, lambdel)).
  }
  \item{\dots}{
    other arguments for 'plot'.
  }
}
\details{
  creates 3 plots:  equivalent degrees of freedom, RMSE and mean gcv.
}
\value{
  matrix loglamout
}
\seealso{
  \code{\link{smooth.basis}}
}
\examples{
# see examples for smooth.basis
}
% docclass is function
\keyword{smooth}
