as.fd <- function(x){
  UseMethod('as.fd')
}

as.fd.dierckx <- function(x){ 
# Translate an object of class dierckx to class fd
##
## 1.  check class
##
  if(!inherits(x, 'dierckx')) 
    stop("'x' is not of class 'dierckx', is ", class(x))
  objName <- deparse(substitute(x))
  {
    if(length(objName)>1)
      objName <- character(0)
    else
      if(nchar(objName)>33)
        objName <- substring(objName, 1, 33)
  }
  if(x$periodic)
    stop("object ", objName, " uses periodic B-splines.  ",
         "and dierckx2fd is programmed to translate only ",
         "B-splines with coincident boundary knots.")
##
## 2.  Create a basis 
##
  rngval <- with(x, c(from, to))
  nKnots <- x$n
# length(dierckx$knots) = nest = estimated number of knots
# number actually used = dierckx$n  
#  knots <- object$knots[1:n]
  Knots <- knots(x, interior=FALSE)
  k <- x$k
  nOrder <- k+1
  breaks <- Knots[nOrder:(nKnots-k)]
#
  xlab <- x$xlab
  if(!require(fda))
    stop("library(fda) required for function 'dierckx2fd'",
         ";  please install.") 
# 
  B.basis <- create.bspline.basis(rangeval=rngval, norder=nOrder,
                                breaks=breaks, names=xlab)
##
## 3.  Create fd object
##
  coef. <- coef(x)
#
  ylab <- x$ylab
  fdNms <- list(args=xlab, reps="reps 1", funs=ylab)
  fd(coef=coef., basisobj=B.basis, fdnames=fdNms)
}
