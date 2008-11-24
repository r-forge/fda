### Name: predict.lmWinsor
### Title: Predict method for Winsorized linear model fits
### Aliases: predict.lmWinsor
### Keywords: models

### ** Examples

# example from 'anscombe' 
lm.1 <- lmWinsor(y1~x1, data=anscombe)

newD <- data.frame(x1=seq(1, 22, .1))
predW <- predict(lm.1, newdata=newD) 
plot(y1~x1, anscombe, xlim=c(1, 22)) 
lines(newD[["x1"]], predW, col='blue')
abline(h=lm.1[['lower']]['y1'], col='red', lty='dashed') 
abline(h=lm.1[['upper']]['y1'], col='red', lty='dashed')
abline(v=lm.1[['lower']]['x1'], col='green', lty='dashed') 
abline(v=lm.1[['upper']]['x1'], col='green', lty='dashed') 




