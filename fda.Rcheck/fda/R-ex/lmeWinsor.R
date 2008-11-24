### Name: lmeWinsor
### Title: Winsorized Regression with mixed effects
### Aliases: lmeWinsor
### Keywords: models

### ** Examples

fm1w <- lmeWinsor(distance ~ age, data = Orthodont,
                 random=~age|Subject) 
fm1w.1 <- lmeWinsor(distance ~ age, data = Orthodont,
                 random=~age|Subject, trim=0.1) 



