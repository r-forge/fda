\name{as.fd}
\alias{as.fd}
\alias{as.fd.dierckx}
\alias{as.fd.function}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Convert a spline object to class 'fd' 
}
\description{
  Translate a spline object of another class into the Functional Data
  (class \code{fd}) format.  
}
\usage{
as.fd(x, ...)
\method{as.fd}{dierckx}(x, ...) 
\method{as.fd}{function}(x, ...) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    an object to be converted to class \code{fd}.  
  }
  \item{\dots}{
    optional arguments passed to specific methods, currently unused.  
  }
}
\details{
  The behavior depends on the \code{class} and nature of \code{x}.

  \itemize{
    \item{as.fd.dierckx}{
      The 'fda' package (as of version 2.0.0) supports B-splines with
      coincident boundary knots.  For periodic phenomena, the
      \code{DierckxSpline} packages uses periodic spines, while
      \code{fda} recommends finite Fourier series.  Accordingly,
      \code{as.fd.dierckx} if x[["periodic"]] is TRUE.  

      The following describes how the components of a \code{dierckx}
      object are handled by as.dierckx(as.fd(x)):   
    
      \itemize{
	\item{x}{lost.  Restored from the knots.}
	\item{y}{
	  lost.  Restored from spline predictions at the restored values
	  of 'x'.  
	}
	\item{w}{lost.  Restored as rep(1, length(x)).}
	\item{from, to}{fd[["basis"]][["rangeval"]] }
	\item{k}{
	  coded indirectly as fd[["basis"]][["nbasis"]] -
	  length(fd[["basis"]][["params"]]) - 1.  
	}
	\item{s}{lost, restored as 0.}
	\item{nest}{lost, restored as length(x) + k + 1}
	\item{n}{
	  coded indirectly as 2*fd[["basis"]][["nbasis"]] -
	  length(fd[["basis"]][["params"]]).
	}
	\item{knots}{
	  The end knots are stored (unreplicated) in
	  fd[["basis"]][["rangeval"]], while the interior knots are
	  stored in fd[["basis"]][["params"]].
	}
	\item{fp}{lost.  Restored as 0.}
	\item{wrk, lwrk, iwrk}{
	  lost.  Restore by refitting to the knots.  
	}
	\item{ier}{lost.  Restored as 0.}
	\item{message}{lost.  Restored as character(0).}
	\item{g}{stored indirectly as
	  length(fd[["basis"]][["params"]]).} 
	\item{method}{lost.  Restored as "ss".}
	\item{periodic}{
	  'dierckx2fd' only translates 'dierckx' objects
	  with coincident boundary knots.  Therefore, 'periodic' is
	  restored as FALSE.
	} 
	\item{routine}{lost.  Restored as 'curfit.default'.}
	\item{xlab}{fd[["fdnames"]][["args"]]}
	\item{ylab}{fd[["fdnames"]][["funs"]]}
      }
    }

    \item{as.fd.function}{
      Create an \code{fd} object from a function of the form created by
      \code{splinefun}.  
    }
  }
}
\value{
  \code{as.fd.dierckx} converts an object of class 'dierckx' into one of
  class \code{fd}.
}
\references{
  Dierckx, P. (1991) \emph{Curve and Surface Fitting with Splines},
  Oxford Science Publications.

  Ramsay, James O., and Silverman, Bernard W. (2006), \emph{Functional 
    Data Analysis, 2nd ed.}, Springer, New York. 

  Ramsay, James O., and Silverman, Bernard W. (2002), \emph{Applied
    Functional Data Analysis}, Springer, New York. 
}
\author{Spencer Graves}
\seealso{
  \code{\link[DierckxSpline]{as.dierckx}}
  \code{\link[DierckxSpline]{curfit}}
  \code{\link{fd}}
}
\examples{
##
## as.fd.dierckx
##
x <- 0:24
y <- c(1.0,1.0,1.4,1.1,1.0,1.0,4.0,9.0,13.0,
       13.4,12.8,13.1,13.0,14.0,13.0,13.5,
       10.0,2.0,3.0,2.5,2.5,2.5,3.0,4.0,3.5)
library(DierckxSpline) 
curfit.xy <- curfit(x, y, s=0)

curfit.fd <- as.fd(curfit.xy)
plot(curfit.fd) # as an 'fd' object 
points(x, y) # Curve goes through the points.  

x. <- seq(0, 24, length=241)
pred.y <- predict(curfit.xy, x.) 
lines(x., pred.y, lty="dashed", lwd=3, col="blue")
# dierckx and fd objects match.

\dontshow{stopifnot(}
all.equal(knots(curfit.xy, FALSE), knots(curfit.fd, FALSE))
\dontshow{)}
\dontshow{stopifnot(}
all.equal(coef(curfit.xy), as.vector(coef(curfit.fd)))
\dontshow{)}


%curfit2 <- fd2dierckx(curfit.fd)
%pred.y2 <- predict(curfit2, x.)
%sum(abs(pred.y-pred.y2))/(sum(abs(pred.y)+abs(pred.y2))/2)
%# 2.7e-7 
%all.equal(knots(curfit.xy, FALSE), knots(curfit2, FALSE))
%# TRUE 
%all.equal(coef(curfit.xy), coef(curfit2)) 
%# "Mean relative  difference: 6.026638e-07"

%preserved <- c("from", "to", "n", "g", "periodic", "xlim","ylim")
%all.equal(curfit.xy[preserved], curfit2[preserved])
%# TRUE 
%# Other components are NOT preserved in translation
%# and so can NOT be restored.

##
## as.fd.function(splinefun(...), ...) 
## 
x <- 1:7
y <- sin((x-0.5)*pi)
f <- splinefun(x, y)
fd. <- as.fd(f)
x. <- seq(1, 7, .02)
fx. <- f(x.)
fdx. <- eval.fd(x., fd.) 
plot(range(x), range(y, fx., fdx.), type='n')
points(x, y)
lines(x., sin((x.-0.5)*pi), lty='dashed') 
lines(x., f(x.), col='blue')
lines(x., eval.fd(x., fd.), col='red')
# splinefun and as.fd(splineful(...)) are close
# but quite different from the actual function
# apart from the actual 7 points fitted,
# which are fitted exactly
# ... and there is no information in the data
# to support a better fit!

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{smooth}
\keyword{manip}
