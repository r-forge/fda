plotGCVRMSE.fd = function(lamlow, lamhi, lamdel, argvals, y, fdParobj, 
                          wtvec= NULL, covariates=NULL)  {
loglamvec = seq(lamlow, lamhi, lamdel)
loglamout = matrix(0,length(loglamvec),4)
m = 0
for (loglambda in loglamvec) {
  m = m + 1
  loglamout[m,1] = loglambda
  fdParobj = fdPar(basisobj, 2, lambda=10^(loglambda))
  smoothlist = smooth.basis(argvals, y, fdParobj, wtvec, covariates)
  xfd = smoothlist$fd   #  the curve smoothing the data
  loglamout[m,2] = smoothlist$df   #  degrees of freedom in the smoothing curve
  loglamout[m,3] = sqrt(mean((eval.fd(argvals, xfd) - x)^2))
  loglamout[m,4] = mean(smoothlist$gcv)  #  the mean of the N gcv values
}
cat("log10 lambda, deg. freedom, RMSE, GCV\n")
for (i in 1:m) {
  cat(format(round(loglamout[i,],3)))
  cat("\n")
}
par(mfrow=c(3,1))
plot(loglamvec, loglamout[,2], type="b")
title("Degrees of freedom")
plot(loglamvec, loglamout[,3], type="b")
title("RMSE")
plot(loglamvec, loglamout[,4], type="b")
title("Mean GCV")

return(loglamout)

}