fRegress.character <- function(y, data=NULL, betalist=NULL,
                             wt=NULL, y2cMap=NULL, SigmaE=NULL,
                             method=c('fRegress', 'model'),
                             sep='.', ...){
  fRegress.formula(y=y, data=data, betalist=betalist,
                             wt=wt, y2cMap=y2cMap, SigmaE=SigmaE,
                             method=method, sep=sep, ...)
}

fRegress.formula <- function(y, data=NULL, betalist=NULL,
                             wt=NULL, y2cMap=NULL, SigmaE=NULL,
                             method=c('fRegress', 'model'),
                             sep='.', ...){
##
## 1.  get y = left hand side of the formula
##
  Formula <- y
  yName <- Formula[[2]]
  yNm <- as.character(yName)
  if((class(yName) != 'name') || (length(yNm) > 1))
    stop('The left hand side of formula must be a simple object; ',
         ' instead, LHS = ', as.character(Formula)[2],
         ', which has class = ', class(yName))
#
  dataNames <- names(data)
  y <- {
    if(yNm %in% dataNames)data[[yNm]]
    else get(yNm)
  }
  if(inherits(y, 'fd'))y <- fdPar(y)
#
  trng <- NULL
  {
    if(inherits(y, 'fdPar')){
      ydim <- dim(y$fd$coefs)
      if(is.null(ydim) || (length(ydim)<2)) {
        y$fd$coefs <- as.matrix(y$fd$coefs)
        ydim <- dim(y$fd$coefs)
      }
      ny <- ydim[2]
      trng <- y$fd$basis$rangeval
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
  allVars <- all.vars(Formula)
  xNms <- allVars[allVars != yNm]
  Terms <- terms(Formula)
  termLbls <- attr(Terms, 'term.labels')
  oops <- which(!(termLbls %in% xNms))
  if(length(oops)>0)
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
## 3.  Inventory the right hand side
##
  k0 <- length(xNms)
  xfdList0 <- vector('list', k0)
  names(xfdList0) <- xNms
  xNames <- xfdList0
  nVars <- rep(NA, k0)
  names(nVars) <- xNms
  oops <- FALSE
  for(i in 1:k0){
    xNm <- xNms[i]
    xi <- {
      if(xNm %in% dataNames)data[[xNm]]
      else get(xNm)
    }
    {
      if(inherits(xi, 'fd')){
        xrng <- xi$basis$rangeval
        {
          if(is.null(trng))
            trng <- xrng
          else
            if(any(xrng != trng)){
              oops <- TRUE
              stop('incompatible rangeval found in ', xNm,
                   '$rangeval = ', paste(xrng, collapse=', '),
                   ' != previous = ', paste(trng, collapse=', ') )
            }
        }
        xdim <- dim(xi$coefs)
        {
          if(is.null(xdim) || (length(xdim)<2)){
            xi$coefs <- as.matrix(xi$coefs)
            xdim <- dim(xi$coefs)
            nxi <- xdim[2]
            nVars[i] <- 1
            xNames[[i]] <- xNm
          }
          else {
            if(length(xdim)<3){
              nxi <- xdim[2]
              nVars[i] <- 1
              xNames[[i]] <- xNm
            }
            else {
              if(length(xdim)<4){
                nxi <- xdim[2]
                nVars[i] <- xdim[3]
                xNmsi <- dimnames(xi$coefs)[[3]]
                {
                  if(is.null(xNmsi))
                    xNames[[i]] <- paste(xNm, 1:xdim[3], sep=sep)
                  else
                    xNames[[i]] <- paste(xNm, xNmsi, sep=sep)
                }
              }
              else
                stop(xNm, ' has too many levels:  dim(x$coefs) = ',
                     paste(xdim, collapse=', '))
            }
          }
        }
        xfdList0[[i]] <- xi
        type[i+1] <- xi$basis$type
        nb <- xi$basis$nbasis
        if(!is.null(nb))nbasis[i+1] <- nb
      }
      else {
        if(is.numeric(xi)) {
          xdim <- dim(xi)
          {
            if(is.null(xdim) || (length(xdim)<2)){
              nxi <- length(xi)
              nVars[i] <- 1
              xNames[[i]] <- xNm
            }
            else {
              nxi <- xdim[1]
              {
                if(length(xdim)<3){
                  nVars[i] <- xdim[2]
                  xNmsi <- dimnames(xi)[[2]]
                  {
                    if(is.null(xNmsi))
                      xNames[[i]] <- paste(xNm, 1:xdim[2], sep=sep)
                    else
                      xNames[[i]] <- paste(xNm, xNmsi, sep=sep)
                  }
                }
                else
                  stop(xNm, 'has too many levels:  dim(x) = ',
                       paste(xdim, collapse=', '))
              }
            }
          }
        }
        else {
          if(inherits(xi, 'character'))
            xi <- factor(xi)
          {
            if(inherits(xi, 'factor')) {
              f.i <- formula(paste('~', xNm))
              Xi.df <- data.frame(xi)
              names(Xi.df) <- xNm
              Xi <- (model.matrix(f.i, Xi.df)[, -1])
              nxi <- dim(Xi)[1]
              xiNms <- dimnames(Xi)[[2]]
              nVars[i] <- length(xiNms)
              xNmLen <- nchar(xNm)
              xiLvls <- substring(xiNms, xNmLen+1)
              xNames[[i]] <- paste(xNm, xiLvls, sep=sep)
              xfdList0[[i]] <- Xi
            }
            else
              stop('ERROR:  variable ', xNm, ' must be of class ',
                   'fd, numeric, character or factor;  is ', class(xi))
          }
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
## 5.  Create xfdList
##
  xNames. <- c('const', unlist(xNames))
  k <- 1+sum(nVars)
  xfdList <- vector('list', k)
  names(xfdList) <- xNames.
#  create constfd for the intercept
#  xfdList[[1]] <- create.constant.basis(trng)
  xfdList[[1]] <- rep(1, ny)
  i1 <- 1
  for(ix in 1:k0){
    i0 <- i1+1
    xNm <- xNms[ix]
    xi <- xfdList0[[ix]]
    {
      if(inherits(xi, 'fd')){
        if(nVars[ix]<2){
          i1 <- i0
          xfdList[[i0]] <- xi
        }
        else {
#          i1 <- (i1+nVars[ix])
          for(i in 1:nVars[ix]){
            i1 <- i1+1
            xii <- xi
            xii$coefs <- xi$coefs[,,i, drop=FALSE]
            xfdList[[i1]] <- xii
          }
        }
      }
      else {
        if(is.numeric(xi)) {
          if(nVars[ix]<2){
            i1 <- i0
            xfdList[[i0]] <- xi
          }
          else{
            for(i in 1:nVars[ix]){
              i1 <- i1+1
              xfdList[[i1]] <- xi[, i]
            }
          }
        }
      }
    }
  }
##
## 6.  betalist
##
  {
    if(inherits(betalist, 'list')){
      if(length(betalist) != k)
        stop('length(betalist) = ', length(betalist),
             ';  must be ', k, ' to match length(xfdlist).')
      betaclass <- sapply(betalist, class)
      oops <- which(betaclass != 'fdPar')
      if(length(oops)>0)
        stop('If betalist is a list, all components must have class ',
             'fdPar;  component ', oops[1], ' has class ',
             betaclass[oops[1]])
    }
    else {
      blist <- betalist
      betalist <- vector('list', k)
      names(betalist) <- xNames.
      beta1 <- blist
      if(!inherits(blist, 'fdPar')){
        betatype <- type[1]
        if(is.na(betatype)){
          typeTbl <- table(type[-1])
          nType <- which(typeTbl==max(typeTbl))[1]
          betatype <- names(typeTbl)[nType]
        }
        btype <- which(type==betatype)[1]
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
        if(is.numeric(blist)){
          if(length(blist)>1)
            stop('If betalist is numeric, it must have length',
                 ' 1;  is ', length(blist))
          if((blist %%1) > .Machine$double.eps)
            stop('If betalist is numeric, it must be an integer;',
                 '  blist = ', blist, ';  (blist %%1) = ',
                 blist %% 1)
          {
            if(betatype %in% c('fourier', 'polynomial'))
              beta1$nbasis <- blist
            else
              warning('betalist numeric ignored with betatype = ',
                      betatype)
          }
        }
      }
      Coefs <- coef(beta1)
      if(length(dim(Coefs))>2) Coefs <-  Coefs[,,1]
      Coefs <- {
        if(length(dim(Coefs))<2) matrix(Coefs)
        else Coefs[, 1, drop=FALSE]
      }
      if(!is.null(dimnames(Coefs))) dimnames(Coefs)[[2]] <- NULL
      Coefs[] <- 0
#      coef(beta1) <- coefs
      beta0 <- with(beta1, fdPar(fd(Coefs, fd$basis), Lfd,
                                 lambda, estimate, penmat) )
#      beta1$fd$coefs <- coefs
#      for(i in 1:k) betalist[[i]] <- beta1
      for(i in 1:k) betalist[[i]] <- beta0
    }
  }
##
## 7.  weight?
##
  {
    if(is.null(wt))
      wt <- rep(1, ny)
    else {
      if(length(wt) != ny)
        stop('length(wt) must match y;  length(wt) = ',
             length(wt), ' != number of y observations = ',
             ny)
      if(any(wt<0))
        stop('Negative weights found;  not allowed.')
    }
  }
  xiEnd <- cumsum(nVars)
  xiStart <- c(1, xiEnd[-1])
  fRegressList <- list(y=y, xfdlist=xfdList, betalist=betalist,
                       wt=wt, xfdlist0=xfdList0, type=type,
                       nbasis=nbasis, xVars=nVars)
##
## 8.  class(y) == 'fd' or 'fdPar'
##
  if(inherits(y, 'fd'))y <- fdPar(y)
#
  method <- match.arg(method)
  if(method=='model')
    return(fRegressList)
  {
    if(inherits(y, 'fdPar'))
      do.call('fRegress.fdPar', fRegressList)
    else
##
## 9.  class(y) == 'numeric'
##
      do.call('fRegress.numeric', fRegressList)
  }
}

