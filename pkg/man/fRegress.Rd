\name{fRegress}
\alias{fRegress}
\alias{fRegress.fd}
\alias{fRegress.fdPar}
\alias{fRegress.numeric}
\title{
  A Functional Regression Analysis of the Concurrent Type
}
\description{
  This function carries out a functional regression analysis, where
  either the dependent variable or one or more independent variables are
  functional.  Non-functional variables may be included on either side
  of the equation.  In a concurrent functional linear model all function
  variables are all evaluated at a common time or argument value $t$.
  That is, the fit is defined in terms of the behavior of all variables
  at a fixed time, or in terms of "now" behavior.
}
\usage{
fRegress(y, ...)
\method{fRegress}{fd}(y, xfdlist, betalist, wt=NULL,
                     y2cMap=NULL, SigmaE=NULL, ...)
\method{fRegress}{fdPar}(y, xfdlist, betalist, wt=NULL,
                     y2cMap=NULL, SigmaE=NULL, ...)
\method{fRegress}{numeric}(y, xfdlist, betalist, wt=NULL,
                     y2cMap=NULL, SigmaE=NULL, ...)
}
\arguments{
  \item{y}{
    the dependent variable object.  It may be an object of three
    possible classes:

    \itemize{
      \item{scalar}{ a vector if the dependent variable is scalar.}
      \item{fd}{
	a functional data object if the dependent variable is
	functional.
      }
      \item{fdPar}{
	a functional parameter object if the dependent variable is
	functional, and if it is necessary to smooth the prediction
	of the dependent variable.
      }
    }
  }
  \item{xfdlist}{
    a list of length equal to the number of independent
    variables. Members of this list are the independent variables.  They
    be objects of either of these two classes:

    \itemize{
      \item{scalar}{
	a vector if the independent dependent variable is scalar.
      }
      \item{fd}{
	a functional data object if the dependent variable is
	functional.
      }
    }

    In either case, the object must have the same number of replications
    as the dependent variable object.  That is, if it is a scalar, it
    must be of the same length as the dependent variable, and if it is
    functional, it must have the same number of replications as the
    dependent variable.
  }
  \item{betalist}{
    a list of length equal to the number of independent
    variables. Members of this list define the regression functions to
    be estimated.  They are functional parameter objects (class
    \code{fdPar}).  Note that even if corresponding independent variable
    is scalar, its regression coefficient will be functional if the
    dependent variable is functional.  Each of these functional
    parameter objects defines a single functional data object, that is,
    with only one replication.
  }
  \item{wt}{
    weights for weighted least squares
  }
  \item{y2cMap}{
    the matrix mapping from the vector of observed values to the
    coefficients for the dependent variable.  This is output by function
    \code{smooth.basis}.  If this is supplied, confidence limits are
    computed, otherwise not.
  }
  \item{SigmaE}{
    Estimate of the covariances among the residuals.  This can only be
    estimated after a preliminary analysis with \code{fRegress}.
  }
  \item{\dots}{ optional arguments, not currently used }
}
\details{
  In the computation of regression function estimates, all independent
  variables are treated as if they are functional.  If argument
  \code{xfdlist} contains one or more vectors, these are converted to
  functional data objects having the constant basis with coefficients
  equal to the elements of the vector.

  Needless to say, if all the variables in the model are scalar, do NOT
  use this function.  Instead, use either \code{lm} or \code{lsfit}.
}
\value{
  For the default \code{method} = \code{fRegress},
  \code{fRegress.formula} returns a list of class \code{fRegress} with
  the following components:

  \item{y}{
    the first argument in the call to \code{fRegress} (coerced to
    \code{class} \code{fdPar})
  }
  \item{xfdlist}{
    the second argument in the call to \code{fRegress}.
  }
  \item{betalist}{
    the third argument in the call to \code{fRegress}.
  }
  \item{betaestlist}{
    a list of length equal to the number of independent variables and
    with members having the same functional parameter structure as the
    corresponding members of \code{betalist}.  These are the estimated
    regression coefficient functions.
  }
  \item{yhatfdobj}{
    a functional parameter object (class \code{fdPar}) if the dependent
    variable is functional or a vector if the dependent variable is
    scalar.  This is the set of predicted by the functional regression
    model for the dependent variable.
  }
  \item{Cmatinv}{
    a matrix containing the inverse of the coefficient matrix for the
    linear equations that define the solution to the regression problem.
    This matrix is required for function \code{\link{fRegress.stderr}}
    that estimates confidence regions for the regression coefficient
    function estimates.
  }
  \item{wt}{
    the vector of weights input or inferred
  }
  \item{df}{
    equivalent degrees of freedom for the fit;  only present if
    \code{class(y)} is \code{numeric}.
  }

  If \code{class(y)} is either \code{fd} or \code{fdPar}, the
  \code{fRegress} object returned also includes 5 other components:

  \item{y2cMap}{ an input \code{y2cMap} }
  \item{SigmaE}{ an input \code{SigmaE} }
  \item{betastderrlist}{
    an \code{fd} object estimating the standard errors of
    \code{betaestlist}
  }
  \item{bvar}{
    a covariance matrix
  }
  \item{c2bMap}{
    a map
  }

%  \item{OCV}{
%    the ordinary cross-validation score. This is returned only when \code{yfdPar}
%    is a vector.
%  }
%  \item{GCV}{
%    the generalized cross-validation score.  This is returned only when \code{yfdPar}
%    is a vector.
%  }
}
\seealso{
  \code{\link{fRegress.formula}},
  \code{\link{fRegress.stderr}},
  \code{\link{fRegress.CV}},
  \code{\link{linmod}}
}
\examples{
#See the following demos:

demo('canadian-weather', package='fda')
#demo('gait', package='fda')
#demo('refinery', package='fda')
#demo('weatherANOVA', package='fda')
#demo('weatherlm', package='fda')
}
% docclass is function
\keyword{smooth}
