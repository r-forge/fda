\name{plot.fdSmooth}
\alias{plot}
\alias{plot.fdSmooth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  plot an \code{fdSmooth} object
}
\description{
  Plot method for an \code{fdSmooth} object 
}
\usage{
\method{plot}{fdSmooth}(x, y, Lfdobj=0, href=TRUE, titles=NULL,
                    xlim=rangex, ylim=rangey, xlab=xlabel,
                    ylab=ylabel, ask=FALSE, nx=201, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y, Lfdojb, href, titles, xlim, ylim, xlab, ylab, ask, nx,
    ...}{
    as for \code{plot.fd}
  } 
}
\details{
  Plot the \code{fd} component of an \code{fdSmooth} object.  Objects of
  this class are returned by the functions \code{smooth.basis} and
  \code{smooth.basisPar}.  
}
\value{
  invisible(NULL)
}
\references{
  Ramsay and Silverman (2006) Functional Data Analysis, 2nd
  ed. (Springer) 
}
\author{Spencer Graves}
\seealso{\code{\link{plot}}, \code{\link{smooth.basis}},
  \code{\link{smooth.basisPar}}
}
\examples{
b3.4 <- create.bspline.basis(norder=3, breaks=c(0, .5, 1))
# 4 bases, order 3 = degree 2 =
# continuous, bounded, locally quadratic 
fdPar3 <- fdPar(b3.4, lambda=1)
# Penalize excessive slope Lfdobj=1;  
# second derivative Lfdobj=2 is discontinuous.
fd3.4s0 <- smooth.basis(0:1, 0:1, fdPar3)

# using plot.fd directly 
plot(fd3.4s0$fd)

# same plot via plot.fdSmooth 
plot(fd3.4s0)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{smooth}
\keyword{hplot}
