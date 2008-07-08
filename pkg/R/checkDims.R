checkDims <- function(x, y=NULL, xdim=1, ydim=1, defaultNames='x',
         subset=c('xiny', 'yinx', 'neither') ){
##
## 1.  length(xdim) == length(ydim)?
##  
  nDims <- length(xdim)
  if(length(ydim) != nDims)
    stop('length(xdim) = ', nDims, ' != ', length(ydim),
         ' = length(ydim)')
##
## 2.  check defaultnames
##  
  nDefault <- length(defaultNames)
  {
    if(nDefault<1){
      dNames0 <- NULL
      dNames <- rep(list(NULL), nDims)
    }
    else {
      dNames0 <- defaultNames[[nDefault]]
      dNames <- rep(as.list(defaultNames), length=nDims)
    }
  }
##
## 3.  loop
##
  for(id in 1:nDims){
    dNi <- list(dNames[[id]], dNames0)
    out <- checkDim(x, y, xdim[id], ydim[id], dNi, subset)
    x <- out$x
    y <- out$y
  }
##
## 4.  done
##  
  out
}

checkDim <- function(x, y=NULL, xdim=1, ydim=1, defaultNames='x',
         subset=c('xiny', 'yinx', 'neither') ){
##
## 1.  Check xdim, ydim 
##
  xname <- substring(deparse(substitute(x)), 1, 22)
  if(xdim>3)stop('Requested matching xdim = ', xdim,
                 ' > 3 = maximum current allowed.')
  {
    if(is.null(y)){ 
      y <- x
      yname <- xname
    }
    else 
      yname <- substring(deparse(substitute(y)), 1, 22)
  }
  if(ydim>3)stop('Requested matching ydim = ', ydim,
                 ' > 3 = maximum current allowed.')
##
## 2.  ixperm, iyperm 
##
  ixperm <- list(1:3, c(2, 1, 3), c(3, 2, 1))[[xdim]]
  iyperm <- list(1:3, c(2, 1, 3), c(3, 2, 1))[[ydim]]  
##
## 3.  x3 <- aperm, ... 
##
  x3 <- aperm(as.array3(x), ixperm);
  y3 <- aperm(as.array3(y), iyperm)   
##
## 4.  xNames, yNames 
##
  xNames <- dimnames(x3)
  yNames <- dimnames(y3)
  {
    if(is.null(defaultNames))
      dNames <- NULL
    else { 
      dNames <- defaultNames[[1]]
      if(is.null(dNames))
        dNames <- defaultNames[[2]]
    }
  }    
##
## 5.  Do it:  Subset & dimnames 
##
  sbst <- match.arg(subset) 
#
  if(sbst == 'xiny'){  
    if(!is.null(dNames)){
      if(length(dNames) < dim(x3)[1])
        dNames <- paste(dNames, 1:dim(x3)[1], sep='')
      else
        dNames <- dNames[1:dim(x3)[1]]
    }
    if(is.null(xNames)){
      if(dim(x3)[1]>dim(y3)[1])
        stop('Can NOT subset y because dim(x)[xdim=', xdim,
             '] = ', dim(x3)[1], ' > ', dim(y3)[1],
             ' = dim(y)[ydim=', ydim, ']') 
      y3 <- y3[1:dim(x3)[1],,, drop=FALSE]
      {
        if(is.null(yNames)){ 
          if(!is.null(dNames)){
            dimnames(x3) <- list(dNames, NULL, NULL)
            dimnames(y3) <- list(dNames, NULL, NULL)
          }
        }
        else
          dimnames(x3) <- list(dimnames(y3)[[1]], NULL, NULL)
      }
    }
    else {
      if(is.null(xNames[[1]])){
        if(dim(x3)[1]>dim(y3)[1])
          stop('Can NOT subset y because dim(x)[xdim=', xdim,
               '] = ', dim(x3)[1], ' > ', dim(y3)[1],
               ' = dim(y)[ydim=', ydim, ']') 
        y3 <- y3[1:dim(x3)[1],,, drop=FALSE]
        {
          if(is.null(yNames)){ 
            if(!is.null(dNames)){
              dNm <- rep(dNames, length=dim(x3)[1]) 
              dimnames(x3)[[1]] <- dNm
              dimnames(y3) <- list(dNm, NULL, NULL)
            }
          }
          else {
            if(is.null(yNames[[1]])){
              if(!is.null(dNames)){
                dNm <- rep(dNames, length=dim(x3)[1]) 
                dimnames(x3)[[1]] <- dNm
                dimnames(y3)[[1]] <- dNm
              }
            }
            else 
              dimnames(x3)[[1]] <- dimnames(y3)[[1]]              
          }
        }
      }
      else {
        xiny <- is.element(xNames[[1]], yNames[[1]])
        if(any(!xiny))
          stop('Can NOT subset y because some dimnames(x)[[xdim=',
               xdim, ']] are not found in dimnames(y)[[ydim=',
               ydim, ']];  the first one is ',
               xNames[[1]][!xiny][1]) 
        y3 <- y3[xNames[[1]],,, drop=FALSE]
      }
    }
  }
  else {
    if(sbst == 'yinx'){
      if(!is.null(dNames)){
        if(length(dNames) < dim(y3)[1])
          dNames <- paste(dNames, 1:dim(y3)[1], sep='')
        else
          dNames <- dNames[1:dim(y3)[1]]
      }
      if(is.null(yNames)){
        if(dim(y3)[1]>dim(x3)[1])
          stop('Can NOT subset x because dim(y)[ydim=', ydim,
               '] = ', dim(y3)[1], ' > ', dim(x3)[1],
               ' = dim(x)[xdim=', xdim, ']') 
        x3 <- x3[1:dim(y3)[1],,, drop=FALSE]
        {
          if(is.null(xNames)){ 
            if(!is.null(dNames)){
              dimnames(x3) <- list(dNames, NULL, NULL)
              dimnames(y3) <- list(dNames, NULL, NULL)
            }
          }
          else
            dimnames(y3) <- list(xNames[[1]], NULL, NULL)
        }
      }
      else {
        if(is.null(yNames[[1]])){
          if(dim(y3)[1]>dim(x3)[1]) 
            stop('Can NOT subset x because dim(y)[ydim=', ydim,
                 '] = ', dim(y3)[1], ' > ', dim(x3)[1],
                 ' = dim(x)[xdim=', xdim, ']') 
          x3 <- x3[1:dim(y3)[1],,, drop=FALSE]
          {
            if(is.null(xNames)){
              if(!is.null(dNames)){
                dNm <- rep(dNames, length=dim(y3)[1]) 
                dimnames(y3)[[1]] <- dNm
                dimnames(x3) <- list(dNm, NULL, NULL)
              }
            }
            else {
              if(is.null(xNames[[1]])){
                if(!is.null(dNames)){
                  dNm <- rep(dNames, length=dim(y3)[1]) 
                  dimnames(y3)[[1]] <- dNm
                  dimnames(x3)[[1]] <- dNm
                }
              }
              else
                dimnames(y3)[[1]] <- dimnames(x3)[[1]] 
            }
          }
        }
        else {
          yinx <- is.element(yNames[[1]], xNames[[1]]) 
          if(any(!yinx))
            stop('Can NOT subset x because some dimnames(y)[[ydim=',
                 ydim, ']] are not found in dimnames(x)[[xdim=',
                 xdim, ']];  the first one is ',
                 yNames[[1]][!yinx][1])
          x3 <- x3[yNames[[1]],,, drop=FALSE]
        }
      }
    }
    else
      if(sbsts == 'neither'){
        if(dim(x3)[1] != dim(y3)[1])
          stop('dim(x)[xdim=', xdim, '] = ', dim(x3)[1],
               ' != ', dim(y3)[1], ' = dim(y)[ydim=',
               ydim, ']')
        if(is.null(xNames)){
          if(!is.null(yNames) &&
             !is.null(yNames[[1]]))
            stop('is.null(dimnames(x)) but ',
                 '!is.null(dimnames(y)[[ydim=', ydim, ']]')
        }
        else{
          if(is.null(xNames[[1]])){ 
            if(!is.null(yNames) &&
               !is.null(yNames[[1]]))
              stop('is.null(dimnames(x)[[xdim=', xdim, ']]), but ', 
                   '!is.null(dimnames(y)[[ydim=', ydim, ']]')
          }
          else{
            if(is.null(yNames))
              stop('x has dimnames;  y has none.') 
            xiny <- (xNames[[1]] %in% yNames[[1]])
            if(any(!xiny))
              stop('Some dimnames(x)[[xdim=', xdim,
                   ']] are not in dimnames(y)[[ydim=',
                   ydim, ']]')
            yinx <- (yNames[[1]] %in% xNames[[1]])
            if(any(!yinx))
              stop('Some dimnames(y)[[ydim=', ydim,
                   ']] are not in dimnames(x)[[xdim=',
                   xdim, ']]')
          }
        }
      }
  }
##
## 6.  out = list(x=aperm, ... ) 
##   
  list(x=aperm(x3, ixperm), y=aperm(y3, iyperm) )
}
