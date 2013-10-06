boxplot.fdSmooth <- function(x, z=NULL, method = "MBD", depth = NULL,
         plot = TRUE, prob = 0.5, color = 6, outliercol = 2, barcol = 4,
	 fullout=FALSE, factor=1.5,xlim=c(1,nrow(x)),
	 ylim=c(min(x)-.5*diff(range(x)),max(x)+.5*diff(range(x))),
                          ...){
  fbplot(x$fd, z, method=method, depth=depth, plot=plot, prob=prob,
         color=color, outliercol=outliercol, barcol=barcol,
         fullout=fullout, factor=factor, xlim=xlim, ylim=ylim, ...)
}

boxplot.fdPar <- function(x, z=NULL, method = "MBD", depth = NULL,
         plot = TRUE, prob = 0.5, color = 6, outliercol = 2, barcol = 4,
	 fullout=FALSE, factor=1.5,xlim=c(1,nrow(x)),
	 ylim=c(min(x)-.5*diff(range(x)),max(x)+.5*diff(range(x))),
                          ...){
  fbplot(x$fd, z, method=method, depth=depth, plot=plot, prob=prob,
         color=color, outliercol=outliercol, barcol=barcol,
         fullout=fullout, factor=factor, xlim=xlim, ylim=ylim, ...)
}

boxplot.fd <- function(x, z=NULL, method = "MBD", depth = NULL,
         plot = TRUE, prob = 0.5, color = 6, outliercol = 2, barcol = 4,
	 fullout=FALSE, factor=1.5,xlim=c(1,nrow(x)),
	 ylim=c(min(x)-.5*diff(range(x)),max(x)+.5*diff(range(x))),
                          ...){
  fbplot(x, z, method=method, depth=depth, plot=plot, prob=prob,
         color=color, outliercol=outliercol, barcol=barcol,
         fullout=fullout, factor=factor, xlim=xlim, ylim=ylim, ...)
}
