\name{fRegress.formula}
\alias{fRegress.formula}
\title{
  Coming soon:  Formula interface for Functional Regression Analysis;
  not yet working.
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
fRegress.formula(formula, data=NULL, betalist=NULL, wts=NULL, ...)
% later 'fRegress' will be made generic
% with this being the formula method
%\method{fRegress}{formula}(formula, data=NULL, betalist=NULL,
%                           weights=NULL, ...)
}
\arguments{
  \item{formula}{
    an object of class \code{formula} (or one that can be coerced to
    that class): a symbolic description of the model to be fitted.

    All objects named on the right hand side must be either
    \code{numeric} or \code{fd} (functional data) or \code{fdPar}.  The
    number of replications of \code{fd} or \code{fdPar} object(s) must
    match each other and the number of observations of \code{numeric}
    objects named.
  }
  \item{data}{
    an optional list containing names of objects identified in
    \code{formula}.
  }
  \item{betalist}{
    Either \code{NULL}, a positive integer, functional data object
    (class \code{fd}) or a list of \code{fd} objects of length equal to
    the number of independent variables in \code{formula}.

    \itemize{
      \item{NULL}{
	Create \code{betalist} to match the \code{formula}.  In
	particular, \code{betalist} will be a list with one component
	for each object on the right hand side.  For \code{fd} objects,
	the corresponding \code{betalist} component will have the same
	\code{type}.  For \code{numeric} \code{vectors} or
	\code{arrays}, the corresponding \code{betalist} component will
	match the left hand side if it is \code{fd} or the most frequent
	\code{fd} type on the right hand side.
      }
      \item{integer}{
	Create \code{betalist} as with \code{betalist = NULL}, but
	adjust \code{nbasis} to this integer where feasible.  This is
	trivial for \code{fourier} and \code{polynomial} bases.  Adjust
	\code{bspline} bases to this number using evenly spaced knots.
	Ignore this integer for all other bases types.
      }
      \item{fd}{
	Use to match all components on the right hand side
      }
      \item{list}{
	Must have one component to match each independent variable in
	\code{formula}.
      }
    }
  }
  \item{wts}{
    weights for weighted least squares.
  }
  \item{\dots}{
    optional arguments currently not used.
  }
}
\details{
  1.  Parse \code{formula}.

  2.  Let \code{y} = the object on the left hand side of the formula.

  3.  Pass control to \code{fRegress.numeric} if \code{y} is
  \code{numeric} or \code{fRegress.fd} if \code{y} is \code{y} is a
  functional data object (class \code{fd}).  If \code{y} is anything
  else, give an error.
}
\value{
  a list of class \code{fRegress} with the following components:

  \item{yfdPar}{
    the left hand side of the \code{formula}.
  }
  \item{xfdlist}{
    a list of \code{fd} objects constructed from the right hand side of
    \code{formula}.
  }
  \item{betalist}{
    the list of \code{fd} objects of length matching \code{xfdlist}.
  }
  \item{betaestlist}{
    a list of length equal to the number of independent variables and
    with members having the same functional parameter structure as the
    corresponding members of \code{betalist}.  These are the estimated
    regression coefficient functions.
  }
  \item{yhatfdobj}{
    a functional data object if the dependent variable is functional or
    a vector if the dependent variable is scalar.  This is the set of
    predicted by the functional regression model for the dependent
    variable.
  }
  \item{Cmatinv}{
    a matrix containing the inverse of the coefficient matrix for the
    linear equations that define the solution to the regression problem.
    This matrix is required for function \code{\link{fRegress.stderr}}
    that estimates confidence regions for the regression coefficient
    function estimates.
  }
  \item{OCV}{
    the ordinary cross-validation score. This is returned only when \code{yfdPar}
    is a vector.
  }
  \item{GCV}{
    the generalized cross-validation score.  This is returned only when \code{yfdPar}
    is a vector.
  }
}
\seealso{
  \code{\link{fRegress}},
  \code{\link{fRegress.stderr}},
  \code{\link{fRegress.CV}},
  \code{\link{linmod}}
}
\examples{
daybasis65 <- create.fourier.basis(rangeval=c(0, 365), nbasis=65,
                  axes=list('axesIntervals'))
%harmaccelLfd365 <- vec2Lfd(c(0,(2*pi/365)^2,0), c(0, 365))
weatherfd <- with(CanadianWeather, smooth.basisPar(day.5, dailyAv,
                  daybasis65)$fd)

weatherRgn <- fRegress.formula(weatherfd ~ region, CanadianWeather)
}
% docclass is function
\keyword{smooth}
