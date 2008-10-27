create.bspline.basis <- function (rangeval=c(0,1), nbasis=NULL, 
                                  norder=4,        breaks=NULL, 
                                  dropind=NULL,    quadvals=NULL,
                                  values=NULL,     basisvalues=NULL,
                                  names="bspl")
{
#  This function creates a bspline functional data basis.
#  Arguments
#  RANGEVAL ... an array of length 2 containing the lower and upper
#               boundaries for the rangeval of argument values, 
#               or a positive number, in which case command 
#               rangeval <- c(0, rangeval) is executed.
#               the default is c(0,1)
#  NBASIS   ... the number of basis functions.  This argument must be
#               supplied, and must be a positive integer.
#  NORDER   ... order of b-splines (one higher than their degree).  The
#                 default of 4 gives cubic splines.
#  BREAKS   ... also called knots, these are a non-decreasing sequence
#               of junction points between piecewise polynomial segments.
#               They must satisfy BREAKS[1] = RANGEVAL[1] and
#               BREAKS[NBREAKS] = RANGEVAL[2], where NBREAKS is the total
#               number of BREAKS.  There must be at least 2 BREAKS.
#  There is a potential for inconsistency among arguments NBASIS, NORDER,
#    and BREAKS since
#             NBASIS = NORDER + LENGTH(BREAKS) - 2
#  An error message is issued if this is the case.  Although previous
#  versions of this function attempted to resolve this inconsistency in
#  various ways, this is now considered to be too risky.
#  DROPIND  ... A vector of integers specifiying the basis functions to
#               be dropped, if any.  For example, if it is required that
#               a function be zero at the left boundary, this is achieved
#               by dropping the first basis function, the only one that
#               is nonzero at that point.
#  QUADVALS .. A NQUAD by 2 matrix.  The firs t column contains quadrature
#                points to be used in a fixed point quadrature.  The second
#                contains quadrature weights.  For example, for (Simpson"s
#                rule for (NQUAD = 7, the points are equally spaced and the
#                weights are delta.*[1, 4, 2, 4, 2, 4, 1]/3.  DELTA is the
#                spacing between quadrature points.  The default is 
#                matrix("numeric",0,0).
#  VALUES  ... A list, with entries containing the values of
#                the basis function derivatives starting with 0 and
#                going up to the highest derivative needed.  The values
#                correspond to quadrature points in QUADVALS and it is
#                up to the user to decide whether or not to multiply
#                the derivative values by the square roots of the
#                quadrature weights so as to make numerical integration
#                a simple matrix multiplication.
#                Values are checked against QUADVALS to ensure the correct
#                number of rows, and against NBASIS to ensure the correct
#                number of columns.
#                The default value of is VALUES is vector("list",0).
#                VALUES contains values of basis functions and derivatives at
#                quadrature points weighted by square root of quadrature weights.
#                These values are only generated as required, and only if slot
#                QUADVALS is not matrix("numeric",0,0).
#  BASISVALUES ... A vector of lists, allocated by code such as 
#                vector("list",1).
#                This field is designed to avoid evaluation of a
#                basis system repeatedly at a set of argument values.
#                Each list within the vector corresponds to a specific set 
#                of argument values, and must have at least two components, 
#                which may be tagged as you wish.  
#                The first component in an element of the list vector contains the 
#                argument values.
#                The second component in an element of the list vector 
#                contains a matrix of values of the basis functions evaluated
#                at the arguments in the first component. 
#                The third and subsequent components, if present, contain 
#                matrices of values their derivatives up to a maximum 
#                derivative order.
#                Whenever function getbasismatrix is called, it checks
#                the first list in each row to see, first, if the number of
#                argument values corresponds to the size of the first dimension,
#                and if this test succeeds, checks that all of the argument
#                values match.  This takes time, of course, but is much
#                faster than re-evaluation of the basis system.  Even this
#                time can be avoided by direct retrieval of the desired
#                array.
#                For example, you might set up a vector of argument values
#                called "evalargs" along with a matrix of basis function
#                values for these argument values called "basismat".
#                You might want too use tags like "args" and "values",
#                respectively for these.  You would then assign them
#                to BASISVALUES with code such as
#                  basisobj$basisvalues <- vector("list",1)
#                  basisobj$basisvalues[[1]] <- 
#                               list(args=evalargs, values=basismat)
#  NAMES  ...    Either a character vector of length NABASIS 
#                or a single character string to which NORDER, "." and
#                1:NBASIS are appended by the command
#                   paste(names, norder, ".", 1:nbreaks, sep="").  
#                For example, if norder = 4, this defaults to 
#                        'bspl4.1', 'bspl4.2', ... .   
#  Returns
#  BASISFD  ... a functional data basis object

#  Last modified       27 October   2008 by Jim Ramsay

#  Previously modified 28 September 2008 by Jim Ramsay
#  This modification eliminates the tolerance of previous versions for
#  inconsistencies between NBASIS, NORDER and BREAKS.

#  -----------------------------------------------------------------------------
#  Default basis for missing arguments:  A B-spline basis over [0,1] of 
#    order 2 (polygonal) with two basis functions.  The basis functions
#    are two right angle triangles, one left, the other right.  They are
#    a basis for straight lines over the unit interval, and are equivalent
#    to a monomial basis with two basis functions.  This B-spline system
#    can be explicitly created using the command
#                create.bspline.basis(c(0,1), 2, 2)
#  -----------------------------------------------------------------------------

if (nargs()==0) {

    type        <- "bspline"
    rangeval    <- c(0,1)
    nbasis      <- 2
    params      <- NULL
    dropind     <- NULL
    quadvals    <- NULL
    values      <- NULL
    basisvalues <- NULL

    basisobj  <- list(type=type,     rangeval=rangeval, nbasis=nbasis,
                      params=params, dropind=dropind,   quadvals=quadvals,
                      values=values, basisvalues=basisvalues)
    oldClass(basisobj) <- "basisfd"
    
    return(basisobj)
}

#  -----------------------------------------------------------------------------
#                     Set up non-default basis
#  -----------------------------------------------------------------------------

#  check RANGEVAL:  This argument is not optional

  if (!rangechk(rangeval)) stop(
      "Argument 'rangeval' is not a legitimate range.")
  #  Allow a range to be set up with a single positive number, in which case
  #    the lower limit is 0.
  if (length(rangeval) == 1){
      if(rangeval <= 0) stop(
          "Argument 'rangeval' is a single value that is not positive.")
      rangeval = c(0,rangeval)
  }

#  check NBASIS:  This argument is not optional
  print(nbasis)
  if (is.null(nbasis))         stop("Argument 'nbasis' is not supplied.")
  if (nbasis != floor(nbasis)) stop("Argument 'nbasis' is not an integer.")
  if (nbasis < 1)              stop("Argument 'nbasis' is not positive.")


#  check NORDER:  This argument is optional, and defaults to 4 (cubic splines)

  if (norder != floor(norder)) stop("Argument 'norder' is not an integer.")
  if (norder < 1)              stop("Argument 'norder' is not positive.")

#  check BREAKS:  This argument is optional, and defaults to NULL.  
#  if not NULL, it must contain at least two values, the first and last
#  being equal to the corresponding values of RANGEVAL.   The values
#  may not decrease, but there can be sequences of equal values.
#  the number of break values must be consistent with the values
#  of NBASIS and NORDER via the equation
#        NBASIS = NORDER + NBREAKS - 2

  nbreaks <- length(breaks)
  if (nbreaks > 0) {
    if (nbreaks < 2) stop(
        "Number of values in argument 'breaks' less than 2.")
    if (breaks[1] != rangeval[1] || breaks[nbreaks] != rangeval[2])
       stop(paste("Range of argument 'breaks' not identical to",
                  "that of argument 'rangeval'."))
    if (min(diff(breaks)) < 0) stop(
              "Values in argument 'breaks' are decreasing.")
    #  Check for consistency with NBASIS and NORDER
    if (nbasis != norder + nbreaks - 2) stop(
      paste("Relation NBASIS = NORDER + LENGTH(BREAKS) - 2",
            "does not hold."))
  } else {
    #  default to nbasis - norder + 2 equally spaced break values
    breaks = seq(rangeval[1], rangeval[2], length = nbasis - norder + 2)
  }

#  Set up the PARAMS vector, which contains only the interior knots.

  if (nbreaks > 2) {
    params <- breaks[2:(nbreaks-1)]
  } else {
    params <- NULL
  }

#  check DROPIND

  if (length(dropind) == 0 || missing(dropind)) dropind <- NULL
  if (length(dropind) > 0) {
    if (any(dropind != floor(dropind))) stop(
                         "Argument 'DROPIND' is not integer-valued.")
    if(length(dropind) >= nbasis)  
        stop("More than NBASIS index values in DROPIND.")
    dropind <- sort(dropind)
    if(length(dropind) > 1) {
      if(min(diff(dropind)) == 0) stop("Multiple index values in DROPIND.")
    }
    if (min(dropind) < 1 || max(dropind) > nbasis)
        stop("An index value of dropind is out of range.")
  }

#  set up basis object

  type        <- "bspline"

  basisobj <- basisfd(type=type,     rangeval=rangeval, nbasis=nbasis,
                      params=params, dropind=dropind,   quadvals=quadvals,
                      values=values, basisvalues=basisvalues)

  basisobj$names <- {
    if(length(names) == nbasis) names
    else {
      if(length(names)<1)
        paste("bspl", norder, '.', 1:nbasis, sep='')
      else {
        if(length(names)>1)
          stop('length(names) = ', length(names), ';  must be either ',
               '1 or nbasis = ', nbasis)
        paste(names, norder, ".", 1:nbasis, sep="")
      }
    }
  }

  basisobj

}
