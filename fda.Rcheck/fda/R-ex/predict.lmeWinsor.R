### Name: predict.lmeWinsor
### Title: Predict method for Winsorized linear model fits with mixed
###   effects
### Aliases: predict.lmeWinsor
### Keywords: models

### ** Examples

fm1w <- lmeWinsor(distance ~ age, data = Orthodont,
                 random=~age|Subject)
# predict with newdata 
newDat <- data.frame(age=seq(0, 30, 2),
           Subject=factor(rep("na", 16)) )
pred1w <- predict(fm1w, newDat, level=0)

# fit with 10 percent Winsorization 
fm1w.1 <- lmeWinsor(distance ~ age, data = Orthodont,
                 random=~age|Subject, trim=0.1)
pred30 <- predict(fm1w.1)
stopifnot(all.equal(as.numeric(
              quantile(Orthodont$distance, c(.1, .9))),
          range(pred30)) )




