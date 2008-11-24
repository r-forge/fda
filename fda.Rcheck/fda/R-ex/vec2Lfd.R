### Name: vec2Lfd
### Title: Make a Linear Differential Operator Object from a Vector
### Aliases: vec2Lfd
### Keywords: smooth

### ** Examples

#  define the harmonic acceleration operator used in the
#  analysis of the daily temperature data
harmaccelLfd <- vec2Lfd(c(0,(2*pi/365)^2,0), c(0,365))



