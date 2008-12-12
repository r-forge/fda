\name{fRegress.CV}
\alias{fRegress.CV}
\title{
  Computes Cross-validated Error Sum of Integrated Squared Errors for a Functional
  Regression Model
}
\description{
  For a functional regression model, a cross-validated error sum of squares is
  computed.  For a functional dependent variable this is the sum of integrated
  squared errors. For a scalar response, this function has been superceded by
  the OCV and gcv elements returned by \code{fRegress}. This function aids
  the choice of smoothing parameters in this model using the
  cross-validated error sum of squares criterion.
}
\usage{
fRegress.CV(yfdPar, xfdlist, betalist, whichobs=NULL)
}
\arguments{
  \item{yfdPar}{
    the dependent variable object.  It may be an object of three
    possible classes:

    \itemize{
      \item{scalar}{ a vector if the dependent variable is scalar.}
      \item{fd}{
	functional data object if the dependent variable is functional.
      }
      \item{fdPar}{
	a functional parameter object
      }
    }
  }
  \item{xfdlist}{
    a list whose members are functional parameter objects specifying
    functional independent variables.  Some of these may also be vectors
    specifying scalar independent variables.
  }
  \item{betalist}{
    a list containing functional parameter objects specifying the
    regression functions and their level of smoothing.
  }
  \item{whichobs}{ a vector giving the indeces of which observations are to be
  left out in the cross validation. Defaults to all of them. }
}
\value{
  A list containing
  \item{SSE.CV}{ The sum of squared errors, or integrated squared errors}
  \item{errfd.cv}{ Either a vector or a functional data object giving the
    cross-validated errors }
}
\seealso{
\code{\link{fRegress}},
\code{\link{fRegress.stderr}}
}
\examples{
#See the analyses of the Canadian daily weather data.
}
% docclass is function
\keyword{smooth}
