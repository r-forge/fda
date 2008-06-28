plot.fdSmooth <- function(x, y, Lfdobj=0, href=TRUE, titles=NULL,
                          xlim=rangex, ylim=rangey, xlab=xlabel,
                          ylab=ylabel, ask=FALSE, nx=201, ...){
  plot(x$fd, y, Lfdobj=0, href=TRUE, titles=NULL,
              xlim=xlim, ylim=ylim, xlab=xlab,
              ylab=ylab, ask=FALSE, nx=201, ...)
}
  
