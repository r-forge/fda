\name{create.power.basis}
\alias{create.power.basis}
\title{
  Create a Power Basis Object
}
\description{
  The basis system is a set of powers of argument $x$.  That is, a basis
  function would be \code{x^exponent}, where \code{exponent}
  is a vector containing a set of powers or
  exponents.  The power basis would normally only be used for positive
  values of x, since the power of a negative number is only defined
  for nonnegative integers, and the exponents here can be any real
  numbers.
}
\usage{
create.power.basis(rangeval=c(0, 1),
            nbasis=length(exponents), exponents=1,
            dropind=NULL, quadvals=NULL, values=NULL)
}
\arguments{
  \item{rangeval}{
    a vector of length 2 with the first element being the lower limit of the
    range of argument values, and the second the upper limit.  Of course the
    lower limit must be less than the upper limit.
  }
  \item{nbasis}{
    the number of exponential functions.
  }
  \item{exponents}{
    a vector of length nbasis containing the powers.
  }
  \item{dropind}{
    a vector of integers specifiying the basis functions to
    be dropped, if any.  For example, if it is required that
    a function be zero at the left boundary, this is achieved
    by dropping the first basis function, the only one that
    is nonzero at that point. Default value NULL.
  }
  \item{quadvals}{
    a matrix with two columns and a number of rows equal to the number
    of quadrature points for numerical evaluation of the penalty
    integral.  The first column of \code{quadvals} contains the
    quadrature points, and the second column the quadrature weights.  A
    minimum of 5 values are required for each inter-knot interval, and
    that is often enough.  For Simpson's rule, these points are equally
    spaced, and the weights are proportional to 1, 4, 2, 4, ..., 2, 4,
    1.
  }
  \item{values}{
    a list containing the basis functions and their derivatives
    evaluated at the quadrature points contained in the first
    column of \code{ quadvals }.
  }
}
\value{
  a basis object of type \code{power}.
}
\details{
  The power basis differs from the monomial and polynomial
  bases in two ways.  First, the powers may be nonintegers.
  Secondly, they may be negative.  Consequently, a power
  basis is usually used with arguments that only take
  positive values, although a zero value can be tolerated
  if none of the powers are negative.
}
\seealso{
\code{\link{basisfd}},
\code{\link{create.bspline.basis}},
\code{\link{create.constant.basis}},
\code{\link{create.exponential.basis}},
\code{\link{create.fourier.basis}},
\code{\link{create.monomial.basis}},
\code{\link{create.polygonal.basis}},
\code{\link{create.polynomial.basis}}
}
\examples{

#  Create a power basis over the interval [1e-7,1]
#  with powers or exponents -1, -0.5, 0, 0.5 and 1
basisobj <- create.power.basis(c(1e-7,1), 5, seq(-1,1,0.5))
#  plot the basis
plot(basisobj)

}
% docclass is function
\keyword{smooth}
