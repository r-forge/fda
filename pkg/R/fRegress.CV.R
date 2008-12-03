fRegress.CV <- function(yfdPar, xfdlist, betalist,whichobs=NULL) {

# FREGRESS.CV computes cross-validated error sum of squares
# only for scalar dependent variable

#  Last modified 02 December 2008

#if (!inherits(yfdPar, "numeric")) stop("Dependent variable is not scalar.")
if (inherits(yfdPar, "fd")) yfdPar <- fdPar(yfdPar)

if (inherits(yfdPar, "numeric")){
  N <- length(yfdPar)
  p <- length(xfdlist)
  if(is.null(whichobs)) whichobs<-1:N
  N <- length(whichobs)  
  betafdPar <- betalist[[2]]
  SSE.CV  <- 0
  errfd = rep(0,N)
  for (m in 1:N) {
      i = whichobs[m]
      xfdlisti <- vector("list",p)
      for (j in 1:p) {
          xfdj  <- xfdlist[[j]]
          basisj <- xfdj$basis
          coefj <- xfdj$coefs
          if (dim(coefj)[1] == 1) coefj <- matrix(coefj[1,-i],1,N-1)
          else                    coefj <- as.matrix(coefj[,-i])
          xfdlisti[[j]] <- fd(coefj,basisj)
      }
      yfdPari <- yfdPar[-i]
      fRegressListi <- fRegress(yfdPari, xfdlisti, betalist)
      betaestlisti  <- fRegressListi$betaestlist
      yhati <- 0
      for (j in 1:p) {
          betafdParj <- betaestlisti[[j]]
          betafdj    <- betafdParj$fd
          xfdj    <- xfdlist[[j]]
          bbasisj <- betafdj$basis
          rangej  <- bbasisj$rangeval
          nfine   <- max(101, bbasisj$nbasis*10+1)
          tfine   <- seq(rangej[1], rangej[2], len=nfine)
          delta   <- tfine[2]-tfine[1]
          betavec <- eval.fd(tfine, betafdj)
          xveci   <- eval.fd(tfine, xfdj[i])
          yhati   <- yhati + delta*(sum(xveci*betavec) -
               0.5*( xveci[1]    *betavec[1] +
                     xveci[nfine]*betavec[nfine] ))
      }
      errfd[i] = yfdPar[i] - yhati;
      SSE.CV <- SSE.CV + errfd[i]^2
  }
}
else if (inherits(yfdPar,"fdPar")){
   yfd <- yfdPar$fd
   ycoef <- yfd$coefs
   N <- dim(ycoef)[2]
   if(is.null(whichobs)) whichobs<-1:N
   N <- length(whichobs)  
   p <- length(xfdlist)

   SSE.CV = 0
   errcoefs = c()

   for(m in 1:N){
      i =  whichobs[m]
      if(m == 2)
           print(paste('Estimated Computing time =',round(N*elapsed.time),'seconds.'))

      begin <- proc.time()
      txfdlist = xfdlist              # First of all, leave one out
  		for(k in 1:p){
  			txfdlist[[k]] = xfdlist[[k]][-i]
  		}
  		tres = fRegress(yfd[-i],txfdlist,betalist)

  		yhat = 0                        # Now we predict
  		for(k in 1:p){
  			yhat = yhat + xfdlist[[k]][i]*tres$betaestlist[[k]]$fd
  		}
  		err = yfd[i] - yhat

      errcoefs = cbind(errcoefs,err$coefs)

  		SSE.CV = SSE.CV + inprod(err,err)
  		elapsed.time <- max(proc.time()-begin,na.rm=TRUE)
  }
  errfd = fd(errcoefs,err$basis)
  names(errfd$fdnames)[[3]] = "Xval Errors"
}
else stop("Dependent variable is not scalar.")

return(list(SSE.CV=SSE.CV,errfd.cv=errfd))
}

