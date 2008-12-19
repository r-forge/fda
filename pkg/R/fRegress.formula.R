fRegress.formula <- function(formula, data=NULL, betalist=NULL,
                             wts=NULL, y2cMap=NULL, SigmaE=NULL,
                             ...){
##
## 1.  get y
##
  yName <- formula[[2]]
  yNm <- as.character(yName)
  if((class(yName) != 'name') || (length(yNm) > 1))
    stop('The left hand side of formula must be a simple object; ',
         ' instead, LHS = ', as.character(formula)[2],
         ', which has class = ', class(yName))
#
  dataNames <- names(data)
  y <- {
    if(yNm %in% dataNames)data[[yNm]]
    else get(yNm)
  }
  if(inherits(y, 'fd'))y <- fdPar(y)
#
  {
    if(inherits(y, 'fdPar')){
      ydim <- dim(y$fd$coefs)
      if(is.null(ydim) || (length(ydim)<2)) {
        y$fd$coefs <- as.matrix(y$fd$coefs)
        ydim <- dim(y$fd$coefs)
        ny <- ydim[2]
      }
    }
    else{
      if(inherits(y, 'numeric')){
        ydim <- dim(y)
        if(is.null(ydim))
          ny <- length(y)
        else
          ny <- ydim[1]
      }
      else
        stop('The left hand side of formula must have class ',
             'numeric, fd or fdPar;  instead is ', class(y))
    }
  }
##
## 2.  check the formula for excessive complexity
##
  allVars <- all.vars(formula)
  xNms <- allVars[allVars != yNm]
  Terms <- terms(formula)
  termLbls <- attr(Terms, 'term.labels')
  oops <- which(!(termLbls %in% xNms))
  if(length(oops)>1)
    stop('formula contains terms that fRegress can not handle; ',
         ' the first one = ', termLbls[oops[1]])
#
  k1 <- length(allVars)
  type <- rep(NA,k1)
  names(type) <- allVars
  nbasis <- type
  if(inherits(y, 'fdPar')){
    type[1] <- y$fd$basis$type
    nb <- y$fd$basis$nbasis
    if(!is.null(nb))nbasis[1] <- nb
  }
##
## 3.  Create xfdList
##
  k <- length(xNms)
  xfdList <- vector('list', k)
  names(xfdList) <- xNms
  betalist <- xfdList
  ki <- rep(NA, k)
  names(ki) <- xNms
  oops <- FALSE
  for(i in 1:k){
    xNm <- xNms
    xi <- {
      if(xNm %in% dataNames)data[[xNm]]
      else get(xNm)
    }
    {
      if(inherits(xi, 'fd')){
        xdim <- dim(xi$coefs)
        if(is.null(xdim) || (length(xdim)<2)){
          xi$coefs <- as.matrix(xi$coefs)
          xdim <- dim(xi$coefs)
          nxi <- xdim[2]
        }
        ki[i] <- prod(xdim[-1])
        type[i+1] <- xi$basis$type
        nb <- xi$basis$nbasis
        if(!is.null(nb))nbasis[i+1] <- nb
      }
      else {
        if(inherits(xi, 'numeric')) {
          xdim <- dim(xi)
          {
            if(is.null(xdim))
              nxi <- length(xi)
            else
              nxi <- xdim[1]
          }
          ki[i] <- prod(xdim)
        }
        else {
          stop('ERROR:  variable ', xNm, ' must be of class ',
              'fd or numeric;  is ', class(xi))
        }
      }
    }
    if(nxi != ny){
      cat('ERROR:  variable ', xNm, ' has only ',
          nxi, ' observations != ', ny,
          ' = the number of observations of y.')
      oops <- TRUE
    }
  }
  if(oops)stop('illegal variable on the right hand side.')
##
## 4.  Create betalist
##
  betatype <- type[1]
  if(is.na(betatype)){
    typeTbl <- table(type[-1])
    nType <- which(typeTbl==max(typeTbl))
    betatype <- names(typeTbl)[nType]
  }
  btype <- which(type==betatype)
  nb <- which((type==betatype) & (nbasis==min(nbasis)))
  {
    if(length(nb)>0) {
      beta1 <- {
        if(nb[1]==1) y
        else xfdList[[nb[1]-1]]
      }
    }
    else {
      beta1 <- {
        if(btype[1]==1) y
        else xfdList[[btype[1]-1]]
      }
    }
  }
#
  for(i in 1:k) betalist[[i]] <- beta1
##
## 5.  weight?
##
  {
    if(is.null(wts))
      wts <- 1:ny
    else {
      if(length(wts) != ny)
        stop('length(wts) must match y;  length(wts) = ',
             length(wts), ' != number of y observations = ',
             ny)
      if(any(wts<0))
        stop('Negative weights found;  not allowed.')
    }
  }
  xiEnd <- cumsum(ki)
  xiStart <- c(1, xiEnd[-1])
  fRegressList <- list(yfdPar=y, xfdlist=xfdList, betalist=betalist,
                       type=type, nbasis=nbasis, xVars=ki)
##
## 6.  class(y) == 'fd' or 'fdPar'
##
  if(inherits(y, 'fd'))y <- fdPar(y)
#
  {
    if(inherits(y, 'fdPar')) {
      return(fRegressList)





    }
    else{
##
## 7.  class(y) == 'numeric'
##
      return(fRegressList)





    }
  }
}

