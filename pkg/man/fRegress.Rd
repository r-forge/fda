\name{fRegress}
\alias{fRegress}
\alias{fRegress.fd}
\alias{fRegress.fdPar}
\alias{fRegress.numeric}
\alias{fRegress.formula}
\alias{fRegress.character}
\title{
  Functional Regression Analysis
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
\method{fRegress}{formula}(y, data=NULL, betalist=NULL, wt=NULL,
                 y2cMap=NULL, SigmaE=NULL,
                 method=c('fRegress', 'model'), sep='.', ...)
\method{fRegress}{character}(y, data=NULL, betalist=NULL, wt=NULL,
                 y2cMap=NULL, SigmaE=NULL,
                 method=c('fRegress', 'model'), sep='.', ...)
\method{fRegress}{fd}(y, xfdlist, betalist, wt=NULL,
                     y2cMap=NULL, SigmaE=NULL, ...)
\method{fRegress}{fdPar}(y, xfdlist, betalist, wt=NULL,
                     y2cMap=NULL, SigmaE=NULL, ...)
\method{fRegress}{numeric}(y, xfdlist, betalist, wt=NULL,
                     y2cMap=NULL, SigmaE=NULL, ...)
}
\arguments{
  \item{y}{
    the dependent variable object.  It may be an object of four
    possible classes:

    \itemize{
      \item{character or formula}{
	a \code{formula} object or a \code{character} object that can be
	coerced into a \code{formula} providing a symbolic description
	of the model to be fitted satisfying the following rules:

	All objects named on the right hand side must be either
	\code{numeric} or \code{fd} (functional data) or \code{fdPar}.
	The number of replications of \code{fd} or \code{fdPar}
	object(s) must match each other and the number of observations
	of \code{numeric} objects named.

      }
      \item{scalar}{
	a vector if the dependent variable is scalar.

      }
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
  \item{data}{
    an optional \code{list} or \code{data.frame} containing names of
    objects identified in the \code{formula} or \code{character}
    \code{y}.

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
    For the \code{fd}, \code{fdPar}, and \code{numeric} methods,
    \code{betalist} must be a list of length equal to the number of
    independent variables.  Members of this list define the regression
    functions to be estimated.  They are functional parameter objects
    (class \code{fdPar}).  Even if a corresponding independent variable
    is scalar, its regression coefficient will be functional if the
    dependent variable is functional (though the intercept will be a
    scalar).  Each of these functional parameter objects defines a
    single functional data object, that is, with only one replication.

    For the \code{formula} and \code{character} methods, \code{betalist}
    can be a \code{list}, as for the other methods.  It can also be
    \code{NULL}, a positive integer, single functional data object
    (class \code{fd}) or functional parameter object that can be used to
    construct such a list:

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
	Coerce to a functional parameter object (\code{fdPar}) and use
	to match all components on the right hand side

      }
      \item{fdPar}{
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
  \item{\dots}{ optional arguments }
}
\details{
  Alternative forms of functional regression can be categorized with
  traditional least squares using the following 2 x 2 table:

  \tabular{lcccc}{
    \tab \tab explanatory \tab variable \tab \cr
    response \tab | \tab vector \tab | \tab function \cr
    \tab | \tab \tab | \tab \cr
    numeric vector \tab | \tab lm \tab | \tab fRegress.numeric \cr
    function \tab | \tab fRegress.fd or .fdPar \tab | \tab fRegress.fd
    or .fdPar or linmod \cr
  }

  For \code{fRegress.numeric}, the numeric response is assumed to be the
  sum of integrals of xfd * beta for all functional xfd terms.

  \code{fRegress.fd or .fdPar} produces a concurrent regression with
  each \code{beta} being also a (univariate) function.

  \code{linmod} predicts a functional response from a convolution
  integral, estimating a bivariate regression function.


  In the computation of regression function estimates in
  \code{fRegress}, all independent variables are treated as if they are
  functional.  If argument \code{xfdlist} contains one or more vectors,
  these are converted to functional data objects having the constant
  basis with coefficients equal to the elements of the vector.

  Needless to say, if all the variables in the model are scalar, do NOT
  use this function.  Instead, use either \code{lm} or \code{lsfit}.

  These functions provide a partial implementation of Ramsay and
  Silverman (2005, chapters 12-20).
}
\value{
  These functions return either a standard \code{fRegress} fit object or
  or a model specification:

  \item{fRegress fit}{
    a list of class \code{fRegress} with the following components:

    \itemize{
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
	a list of length equal to the number of independent variables
	and with members having the same functional parameter structure
	as the corresponding members of \code{betalist}.  These are the
	estimated regression coefficient functions.

      }
      \item{yhatfdobj}{
	a functional parameter object (class \code{fdPar}) if the
	dependent variable is functional or a vector if the dependent
	variable is scalar.  This is the set of predicted by the
	functional regression model for the dependent variable.

      }
      \item{Cmatinv}{
	a matrix containing the inverse of the coefficient matrix for
	the  linear equations that define the solution to the regression
	problem.  This matrix is required for function
	\code{\link{fRegress.stderr}} that estimates confidence regions
	for the regression coefficient function estimates.

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

      \item{y2cMap}{
	an input \code{y2cMap}

      }
      \item{SigmaE}{
	an input \code{SigmaE}

      }
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
    }
  }

  \item{model specification}{
    The \code{fRegress.formula} and \code{fRegress.character}
    functions translate the \code{formula} into the argument list
    required by \code{fRegress.fdPar} or \code{fRegress.numeric}.
    With the default value 'fRegress' for the argument \code{method},
    this list is then used to call the appropriate other
    \code{fRegress} function.


    Alternatively, to see how the \code{formula} is translated, use
    the alternative 'model' value for the argument \code{method}.  In
    that case, the function returns a list with the arguments
    otherwise passed to these other functions plus the following
    additional components:

    \itemize{
      \item{xfdlist0}{
	a list of the objects named on the right hand side of
	\code{formula}.  The will differ from from \code{xfdlist} for
	any categorical or multivariate right hand side object.

      }
      \item{type}{
	the \code{type} component of any \code{fd} object on the right
	hand side of \code{formula}.

      }
      \item{nbasis}{
	a vector containing the \code{nbasis} components of variables
	named in \code{formula} having such components

      }
      \item{xVars}{
	an integer vector with all the variable names on the right
	hand side of \code{formula} containing the corresponding
	number of variables in \code{xfdlist}.  This can exceed 1 for
	any multivariate object on the right hand side of class either
	\code{numeric} or \code{fd} as well as any categorical
	variable.

      }
    }
  }
}
\author{
  J. O. Ramsay, Giles Hooker, and Spencer Graves
}
\references{
  Ramsay, James O., and Silverman, Bernard W. (2006), \emph{Functional
    Data Analysis, 2nd ed.}, Springer, New York.
}
\seealso{
  \code{\link{fRegress.formula}},
  \code{\link{fRegress.stderr}},
  \code{\link{fRegress.CV}},
  \code{\link{linmod}}
}
\examples{
###
###
### vector response and explanatory variable
###   ... to compare fRegress and lm
###
###




###
###
### vector response with functional explanatory variable
###
###

##
## set up
##
annualprec <- log10(apply(CanadianWeather$dailyAv[,,"Precipitation.mm"],
                          2,sum))
# The simplest 'fRegress' call is singular with more bases
# than observations, so we use a small basis for this example
smallbasis  <- create.fourier.basis(c(0, 365), 25)
# There are other ways to handle this,
# but we will not discuss them here
tempfd <- smooth.basis(day.5,
          CanadianWeather$dailyAv[,,"Temperature.C"], smallbasis)$fd

##
## formula interface
##

precip.Temp.f <- fRegress(annualprec ~ tempfd)

##
## Get the default setup and modify it
##

precip.Temp.mdl <- fRegress(annualprec ~ tempfd, method='m')

#  set up a smaller basis than for temperature
nbetabasis  <- 21
betabasis2.  <- create.fourier.basis(c(0, 365), nbetabasis)
betafd2.     <- fd(rep(0, nbetabasis), betabasis2.)
# add smoothing
betafdPar2.  <- fdPar(betafd2., lambda=10)

# precip.Temp.mdl$betalist$tempfd <- betafdPar2.

# New do it.
precip.Temp.m <- do.call('fRegress', precip.Temp.mdl)

# With the change in betalist, the answers may be different
# Without that change, the answes will be the same.
\dontshow{stopifnot(}
all.equal(precip.Temp.m, precip.Temp.f)
\dontshow{)}

##
## Manual construction of xfdlist and betalist
##

xfdlist <- list(const=rep(1, 35), tempfd=tempfd)

# The intercept must be constant for a scalar response
betabasis1 <- create.constant.basis(c(0, 365))
betafd1    <- fd(0, betabasis1)
betafdPar1 <- fdPar(betafd1)

nbetabasis  <- 25
betabasis2  <- create.fourier.basis(c(0, 365), nbetabasis)
betafd2     <- fd(rep(0, nbetabasis), betabasis2)
# add smoothing
betafdPar2  <- fdPar(betafd2)

betalist <- list(const=betafdPar1, tempfd=betafdPar2)

precip.Temp <- fRegress(annualprec, xfdlist, betalist)
\dontshow{stopifnot(}
all.equal(precip.Temp[-3], precip.Temp.f[-3])
\dontshow{)}







###
###
### functional response with vector explanatory variables
###
###

##
## simplest:  formula interface
##
daybasis65 <- create.fourier.basis(rangeval=c(0, 365), nbasis=65,
                  axes=list('axesIntervals'))
Temp.fd <- with(CanadianWeather, smooth.basisPar(day.5,
                dailyAv[,,'Temperature.C'], daybasis65)$fd)
TempRgn.f <- fRegress(Temp.fd ~ region, CanadianWeather)

##
## Get the default setup and possibly modify it
##
TempRgn.mdl <- fRegress(Temp.fd ~ region, CanadianWeather, method='m')
%names(TempRgn.mdl)
# make desired modifications here
# then run
TempRgn.m <- do.call('fRegress', TempRgn.mdl)

# no change, so match the first run
\dontshow{stopifnot(}
all.equal(TempRgn.m, TempRgn.f)
\dontshow{)}

##
## More detailed set up
##
%str(TempRgn.mdl$xfdlist)
region.contrasts <- model.matrix(~factor(CanadianWeather$region))
rgnContr3 <- region.contrasts
dim(rgnContr3) <- c(1, 35, 4)
dimnames(rgnContr3) <- list('', CanadianWeather$place, c('const',
   paste('region', c('Atlantic', 'Continental', 'Pacific'), sep='.')) )

const365 <- create.constant.basis(c(0, 365))
region.fd.Atlantic <- fd(matrix(rgnContr3[,,2], 1), const365)
%str(region.fd.Atlantic)
region.fd.Continental <- fd(matrix(rgnContr3[,,3], 1), const365)
region.fd.Pacific <- fd(matrix(rgnContr3[,,4], 1), const365)
region.fdlist <- list(const=rep(1, 35),
     region.Atlantic=region.fd.Atlantic,
     region.Continental=region.fd.Continental,
     region.Pacific=region.fd.Pacific)
%str(TempRgn.mdl$betalist)

beta1 <- fdPar(Temp.fd)
Coefs <- coef(beta1)[, 1, drop=FALSE]
dimnames(Coefs)[[2]] <- NULL
Coefs[] <- 0
beta0 <- with(beta1, fdPar(fd(Coefs, fd$basis), Lfd,
                           lambda, estimate, penmat) )
betalist <- list(const=beta0, region.Atlantic=beta0,
             region.Continental=beta0, region.Pacific=beta0)

TempRgn <- fRegress(Temp.fd, region.fdlist, betalist)

\dontshow{stopifnot(}
all.equal(TempRgn, TempRgn.f)
\dontshow{)}

###
###
### functional response with (concurrent) functional explanatory variable
###
###

##
##  predict knee angle from hip angle;  from demo('gait', package='fda')
##
## formula interface
##
(gaittime <- as.numeric(dimnames(gait)[[1]])*20)
gaitrange <- c(0,20)
gaitbasis <- create.fourier.basis(gaitrange, nbasis=21)
harmaccelLfd <- vec2Lfd(c(0, (2*pi/20)^2, 0), rangeval=gaitrange)
gaitfd <- smooth.basisPar(gaittime, gait,
       gaitbasis, Lfdobj=harmaccelLfd, lambda=1e-2)$fd
hipfd  <- gaitfd[,1]
kneefd <- gaitfd[,2]

knee.hip.f <- fRegress(kneefd ~ hipfd)
%knee.hip.mdl <- fRegress(kneefd ~ hipfd, method='m')

##
## manual set-up
##
#  set up the list of covariate objects
conbasis <- create.constant.basis(c(0,20))
const  <- rep(1, dim(kneefd))
xfdlist  <- list(const=const, hipfd=hipfd)

betafdPar <- fdPar(gaitbasis, harmaccelLfd)

betalist  <- list(const=betafdPar, hipfd=betafdPar)

fRegressout <- fRegress(kneefd, xfdlist, betalist)

\dontshow{stopifnot(}
all.equal(fRegressout, knee.hip.f)
\dontshow{)}

#See also the following demos:

#demo('canadian-weather', package='fda')
#demo('gait', package='fda')
#demo('refinery', package='fda')
#demo('weatherANOVA', package='fda')
#demo('weatherlm', package='fda')
}
% docclass is function
\keyword{smooth}
