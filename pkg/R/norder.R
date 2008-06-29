norder <- function(x, ...)UseMethod("norder")

norder.fd <- function(x, ...)norder.bspline(x$basis)

norder.basisfd <- function(x, ...)norder.bspline(x)

norder.default <- function(x, ...){
#
  xB. <- sapply(x, function(x){
    inherits(x, 'basisfd') || inherits(x, 'fd')
  } )
  xB <- which(xB.)
#
  {    
    if(length(xB)<1)
      stop("input is not a 'basisfd' object and does not have ",
           "a 'basisfd' component.")
    else
      return(norder(x[[xB[1]]]))
  }
}

norder.bspline <- function(x, ...){
  with(x, nbasis - length(params)) 
}
