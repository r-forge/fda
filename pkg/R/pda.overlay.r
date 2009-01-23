pda.overlay = function(pdaList,nfine=501,ncoarse=11,...){

fdrange = pdaList$bwtlist[[1]]$fd$basis$range

tfine = seq(fdrange[1],fdrange[2],len=nfine)
beta0vals = eval.fd(tfine,pdaList$bwtlist[[1]]$fd)
beta1vals = eval.fd(tfine,pdaList$bwtlist[[2]]$fd)

plot(beta1vals,beta0vals,type='l',col=4,xlab='beta 1',ylab='beta 0',...)
abline(h=0,col=2) 
abline(v=0,col=2)

bv = seq(min(beta1vals),max(beta1vals),len=nfine)
lines(bv,(bv/2)^2,col=2,lty=2,...)

tcoarse = seq(fdrange[1],fdrange[2],len=ncoarse)
beta0valsc = eval.fd(tcoarse,pdaList$bwtlist[[1]]$fd)
beta1valsc = eval.fd(tcoarse,pdaList$bwtlist[[2]]$fd)

text(x=beta1valsc,y=beta0valsc,labels=tcoarse,col=4)

}