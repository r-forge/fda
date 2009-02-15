\name{predict.fRegress}
\alias{predict.fRegress}
\title{
  Predict method for Functional Regression
}
\description{
  Model predictions for object of class \code{fRegress}.
}
\usage{
\method{predict}{fRegress}(object, newdata=NULL, se.fit = FALSE,
     interval = c("none", "confidence", "prediction"),
     level = 0.95, ...)
}
\arguments{
  \item{ object }{
    Object of class inheriting from \code{lmWinsor}
  }
  \item{ newdata }{
    An optional vector of argument values for which predictions are
    desired.  If omitted, the fitted values are used.
  }
  \item{ se.fit}{
    a switch indicating if standard errors of predictions are required
  }
  \item{interval}{
    type of prediction (response or model term)
  }
  \item{ level }{ Tolerance/confidence level }
  \item{\dots}{
    additional arguments for other methods
  }
}
\details{
  1.  Without \code{newdata}, fit <- object\$yhatfdobj.

  2.  With \code{newdata}, first call \code{predict.fd} with each
  element of \code{object\$xfdlist} and \code{newdata}, then combine the
  results using \code{object\$betaestlist} to get \code{fit}.  Repeat
  for \code{object$betaestlist}.

  3.  If(se.fit | (interval != 'none')) compute \code{se.fit}, then
  return whatever is desired.
}
\value{
  \code{predict.fRegress} produces a vector of predictions or a matrix
  of predictions and bounds with column names \code{lwr} and
  \code{upr} if \code{interval} is not "none".

  If \code{se.fit} is \code{TRUE}, \code{predict.fRegress} returns a
  list with the following components:

  \item{fit}{
    vector or matrix depending on the value of \code{interval}.
  }
  \item{se.fit}{standard error of predicted means}
}
\author{ Spencer Graves }
\seealso{
  \code{\link{fRegress}}
  \code{\link[stats]{predict}}
}
\examples{
##
## example from help('lm')
##
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2,10,20, labels=c("Ctl","Trt"))
weight <- c(ctl, trt)
fRegress.D9 <- fRegress(weight ~ group)

pred.fR.D9 <- predict(fRegress.D9)
#  Now compare with 'lm'
lm.D9 <- lm(weight ~ group)
pred.lm.D9 <- predict(lm.D9)

\dontshow{stopifnot(}
all.equal(as.vector(pred.fR.D9), as.vector(pred.lm.D9))
\dontshow{)}
##
## Example using se.fit
##

# Coming soon


##
## Example using interval
##

# Coming soon


}
\keyword{ models }

