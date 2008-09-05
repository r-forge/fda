### Name: create.fourier.basis
### Title: Create a Fourier Basis
### Aliases: create.fourier.basis
### Keywords: smooth

### ** Examples

# Create a minimal Fourier basis for the monthly temperature data, 
#  using 3 basis functions with period 12 months.
monthbasis3 <- create.fourier.basis(c(0,12) )
#  plot the basis
plot(monthbasis3)

# set up the Fourier basis for the monthly temperature data,
#  using 9 basis functions with period 12 months.
monthbasis <- create.fourier.basis(c(0,12), 9, 12.0)

#  plot the basis
plot(monthbasis)

# Create a false Fourier basis using 1 basis function.  
falseFourierBasis <- create.fourier.basis(nbasis=1)
#  plot the basis:  constant 
plot(falseFourierBasis)




