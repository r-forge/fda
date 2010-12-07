\name{register.fd0}
\alias{register.fd0}
%\alias{plot.register.fd0}
\title{
  Estimate initial position error between functional data objects.
}
\description{
  Certain physical measurements differ only in their initial position.
  Correct for this by estimating x0 to minimize the following:

  integral( (yfd(x-x0)-y0fd(x))^2
  from max(x1, x1+x0) to min(x2, x2+x0)) / (x2-x1-abs(x0))
}
\usage{
register.fd0(y0fd, yfd=NULL, ...)
%\method{plot}{register.fd0}(x, ...)
}
\arguments{
  \item{y0fd}{
    a functional data object defining the target for registration.

    If \code{yfd} is NULL and y0fd is a multivariate data object, then
    y0fd is assigned to yfd and y0fd is replaced by its mean.
  }
  \item{yfd}{
    a multivariate functional data object defining the functions to be
    registered to target \code{y0fd}.  If it is NULL and \code{y0fd} is
    a multivariate functional data object, yfd takes the value of
    \code{y0fd}.
  }
  \item{\dots}{
    optional arguments passed to other methods of \code{\link{plot}}
  }
}
\details{
  Mikkelsen (2003) noted that depth positioning errors in measuring
  subsurface movement of soils can make it appear that soils are moving
  when they are not.  This function is designed to estimate and correct
  for errors in starting position like this.

  1.  eval.df y0fd and yfd at a grid of npts

  2.  n.5 <- ceiling(sqrt(npts))

  3.  compute the sum of squares of the differences between yfd and y0fd
  offset by i in seq(-n.5, n.5), divided by the number of points in each
  sum (npts with i = 0, npts-n.5 with i = abs(n.5)).


  4.  Fit a cubic spline to this sum of squares.


  5.  Find the minimum
}
\value{
  \code{register.fd0} returns an object of class \code{register.fd0},
  which is a list with the following components:

  \item{yfd}{
    the input argument \code{yfd}.
  }
  \item{offset}{
    the vector \code{x0} containing the offset for each function in
    \code{yfd}.
  }

}

\source{
  Ramsay, James O., and Silverman, Bernard W. (2006), \emph{Functional
    Data Analysis, 2nd ed.}, Springer, New York.

  Ramsay, James O., and Silverman, Bernard W. (2002), \emph{Applied
    Functional Data Analysis}, Springer, New York, ch. 6 & 7.
}
\seealso{
  \code{\link{smooth.monotone}},
  \code{\link{smooth.morph}}
}
\examples{
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

smB.reg.0 <- register.fd0(smBv[1:nBoys])

# plot(smB.reg.0)

}
% docclass is function
\keyword{smooth}
