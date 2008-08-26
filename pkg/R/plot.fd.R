plot.fd <- function(x, y, Lfdobj=0, href=TRUE, 
                    xlim=rangex, ylim=rangey, 
                    xlab=xlabel, ylab=ylabel, 
                    ask=FALSE,   nx=nxdefault, ...)
{
#  -----------------------------------------------------------------------
#       plot for fd class
#  -----------------------------------------------------------------------

  #  Plot a functional data object fdobj.
  #  Arguments:
  #  fdobj     ... a functional data object
  #  Lfdobj    ... linear differental operator to be applied to fdobj before
  #             plotting
  #  HREF   ... If TRUE, a horizontal dotted line through 0 is plotted.
  #  The argument ASK, if TRUE, causes the curves to be displayed one at a time.
  #  NX     ... The number of sampling points to use for
  #             plotting.  (default 101)
  
  #  The remaining optional arguments are the same as those available
  #     in the regular "plot" function.

  #  Note that for multivariate fdobj, a suitable matrix of plots
  #    must be set up before calling plot by using something such as
  #    par(mfrow=c(1,nvar),pty="s")

  # last modified 25 August 2008 by Jim Ramsay
  # previously modified 3 May 2007 by Spencer Graves

  #  check fdobj

  fdobj <- x
  if (!(inherits(fdobj, "fd"))) stop(
		"First argument is not a functional data object.")

  #  check Lfdobj

  Lfdobj <- int2Lfd(Lfdobj)
  if (!inherits(Lfdobj, "Lfd")) stop(
      "Second argument is not a linear differential operator.")

  #  extract dimension information

  coef   <- fdobj$coefs
  coefd  <- dim(coef)
  ndim   <- length(coefd)
  # Number of basis functions   
  nbasis    <- coefd[1]
  nxdefault <- 10*nbasis + 1
  # Number of functional observations   
  nrep   <- coefd[2]
  if (ndim > 2) nvar <- coefd[3] else nvar <- 1

  #  get basis information

  basisobj <- fdobj$basis
  rangex   <- basisobj$rangeval

  #  set up a set of argument values for the plot

  if (missing(y)) {
    y <- nx
  } else {
    y <- as.vector(y)
  }

  if (length(y) == 1) {
    if (y >= 1) { 
      y <- seq(rangex[1],rangex[2],len=floor(y))
    } else {
      stop("'y' a single number less than one.")
    }
  }
  if (min(y) < rangex[1] || max(y) > rangex[2]) stop(
    "Values in Y are out of the basis range.")

  #  evaluate LFDOBJ(FDOBJ) at the argument values

  fdmat    <- eval.fd(y, fdobj, Lfdobj)
  rangey   <- range(c(fdmat))

  #  set up axis labels and, 
  #  optionally, caselabels and variable labels

  fdnames      = fdobj$fdnames
  fdlabelslist = fdlabels(fdnames, nrep, nvar)

  xlabel    = fdlabelslist$xlabel
  ylabel    = fdlabelslist$ylabel
  casenames = fdlabelslist$casenames
  varnames  = fdlabelslist$varnames

  # A single line?  
  if (ndim < 2) {
    plot (y, fdmat, type="l", xlim=xlim, ylim=ylim,
          xlab=xlab, ylab=ylab, ...)
    if (zerofind(fdmat) && href) abline(h=0,lty=2)
  }
  # Several copies of one function?    
  if (ndim ==2 ) {
    if (!ask) {
      matplot(y, fdmat, type="l", 
              xlim=xlim,   ylim=ylim,
              xlab=xlabel, ylab=ylabel, ...)
      if (zerofind(fdmat) && href) abline(h=0,lty=2)
    } else  {
      for (irep in 1:nrep) {
        plot (y, fdmat[,irep], type="l", 
              xlim=xlim, ylim=ylim,
              xlab=xlab, ylab=ylab, ...)
        if (!is.null(casenames)) title(casenames[irep])
        else                     title(paste("Case",irep))
        if (zerofind(fdmat[,irep]) && href) abline(h=0,lty=2)
        mtext("Click in graph to see next plot", side=3, outer=FALSE)
        text("",locator(1))
      }
    }
  }
  # Possibly multiple copies of different functions   
  if (ndim == 3) {
    if (!ask) {
      for (ivar in 1:nvar) {
        matplot (y, fdmat[,,ivar], type="l", 
                 xlim=xlim, ylim=ylim,
                 xlab=xlab, ylab=ylab, ask=FALSE, ...)
        if (!is.null(varnames)) title(varnames[ivar])
        else                    title(paste("Variable",ivar))
        if (zerofind(fdmat[,,ivar]) && href) abline(h=0,lty=2)
      }
    } else {
      for (irep in 1:nrep) {
        for (ivar in 1:nvar) {
          plot(y,fdmat[,irep,ivar],type="l", 
               xlim=xlim, ylim=ylim,
               xlab=xlab, ylab=ylab, ...)
          if (!is.null(casenames)) titlestr = casenames[irep]
          else                     titlestr = paste("Case",irep)
          if (!is.null(varnames)) { 
             titlestr = paste(titlestr,"  ",varnames[ivar])
          } else {                   
             titlestr = paste(titlestr,"  ","Variable",ivar)
          }
          title(titlestr)
          if (zerofind(fdmat[,irep,ivar]) && href) abline(h=0,lty=2)
          mtext("Click in graph to see next plot", side=3, outer=FALSE)
          text("",locator(1))
        }
      }
    }
  }
  invisible(NULL)
}

#  --------------------------------------------------------------------

zerofind <- function(fmat)
{
  frng <- range(fmat)
  if (frng[1] <= 0 && frng[2] >= 0) zeroin <- TRUE else zeroin <- FALSE
  return(zeroin)
}



