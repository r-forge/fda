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
\method{fRegress}{formula}(y, data=NULL, betalist=NULL, wts=NULL,
                 y2cMap=NULL, SigmaE=NULL,
                 method=c('fRegress', 'model'), sep='.', ...)
% later 'fRegress' will be made generic
% with this being the formula method
%\method{fRegress}{formula}(formula, data=NULL, betalist=NULL,
%                           weights=NULL, ...)
}
\arguments{
  \item{y}{
    an object of class \code{formula} (or one that can be coerced to
    that class): a symbolic description of the model to be fitted.

    All objects named on the right hand side must be either
    \code{numeric} or \code{fd} (functional data) or \code{fdPar}.  The
    number of replications of \code{fd} or \code{fdPar} object(s) must
    match each other and the number of observations of \code{numeric}
    objects named.
  }
  \item{data}{
    an optional list containing names of objects identified in the
    \code{formula} \code{y}.
  }
  \item{betalist}{
    Either \code{NULL}, a positive integer, functional data object
    (class \code{fd}) or a list of \code{fd} objects of length equal to
    the number of independent variables created in the output
    \code{xfdlist}.

    \itemize{
      \item{NULL}{
	Create \code{betalist} to match the \code{formula} \code{y}.  In
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
	trivial for \code{fourier} and \code{polynomial} bases.  Not
	currently available for \code{bspline}, \code{polygonal} or
	\code{constant} bases.
      }
      \item{fd}{
	Use to match all components on the right hand side
      }
      \item{list}{
	Must have one component to match each independent variable in
	the \code{formula} \code{y}.  For a categorical independent
	variable with \code{k} levels, \code{betalist} must have
	\code{k-1} components, one for each \code{contrast} (selected
	according to \code{options('contrasts')}).
      }
    }
  }
  \item{wts}{
    weights for weighted least squares.
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
  \item{method}{
    a character string matching either \code{fRegress} for functional
    regression estimation or \code{mode} to create the argument lists
    for functional regression estimation without running it.
  }
  \item{sep}{
    separator for creating names for multiple variables for
    \code{fRegress.fdPar} or \code{fRegress.numeric} created from single
    variables on the right hand side of the \code{formula} \code{y}.
    This happens with multidimensional \code{fd} objects as well as with
    categorical variables.
  }
  \item{\dots}{
    optional arguments currently not used.
  }
}
\details{
  1.  Parse the \code{formula} \code{y}.

  2.  Let \code{y} = the object on the left hand side of the formula.

  3.  Create \code{xfdlist0} with one component for each variable on the
  right hand side of the formula.

  4.  Create \code{fdlist} from \code{xfdlist0}.

  5.  Create \code{betalist} to match \code{xfdlist}.

  6.  If \code{method} = \code{model}, return.  Else, pass control to
  \code{fRegress.fdPar} or \code{fRegress.numeric}.
}
\value{
  For the default \code{method} = \code{fRegress},
  \code{fRegress.formula} returns a list of class \code{fRegress} with
  the following components:

  \item{y}{
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

  For \code{method} = \code{model}, \code{fRegress.formula} returns a
  list with components \code{y}, \code{xfdlist}, \code{betalist},
  and \code{wt}, which can be subsequently be passed as arguments for
  calling either \code{fRegress.fdPar} or \code{fRegress.numeric}.  This
  list also includes other components for diagnostic purposes:

  \item{xfdlist0}{
    a list of the objects named on the right hand side of
    \code{formula}.  The will differ from from \code{xfdlist} for any
    categorical or multivariate right hand side object.
  }
  \item{type}{
    the \code{type} component of any \code{fd} object on the right hand
    side of \code{formula}.
  }
  \item{nbasis}{
    a vector containing the \code{nbasis} components of variables named
    in \code{formula} having such components
  }
  \item{xVars}{
    an integer vector with all the variable names on the right hand side
    of \code{formula} containing the corresponding number of variables
    in \code{xfdlist}.  This can exceed 1 for any multivariate object on
    the right hand side of class either \code{numeric} or \code{fd} as
    well as any categorical variable.
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
Temp.fd <- with(CanadianWeather, smooth.basisPar(day.5,
                dailyAv[,,'Temperature.C'], daybasis65)$fd)

TempRgn <- fRegress.formula(Temp.fd ~ region, CanadianWeather)
}
% docclass is function
\keyword{smooth}
