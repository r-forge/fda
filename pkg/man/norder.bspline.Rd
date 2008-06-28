\name{norder.bspline}
\alias{norder.bspline}
\title{
  Order of a B-spline
}
\description{
  norder = number of basis functions minus the number of interior
  knots.  
}
\usage{
norder.bspline(fdobj) 
}
\arguments{
  \item{fdobj}{ a functional data object of type 'bspline' }
}
\details{
  norder = with(dfobj, 
}
\value{
  An integer giving the order of the B-spline.  
}
%\references{}
\author{
  Spencer Graves
} 
\seealso{
  \code{\link{create.bspline.basis}}
}
\examples{
stopifnot(norder.bspline(create.bspline.basis(norder=1, breaks=0:1))==1) 
stopifnot(
  norder.bspline(create.bspline.basis(norder=1, breaks=c(0,.5, 1))) == 1) 

stopifnot(
  norder.bspline(create.bspline.basis(norder=2, breaks=c(0,.5, 1))) == 2)

# Defaut B-spline basis:  Cubic spline:  degree 3, order 4,
# 21 breaks, 19 interior knots.  
stopifnot(norder.bspline(create.bspline.basis()) == 4) 

}
\keyword{smooth}
