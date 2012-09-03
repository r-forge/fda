CRAN <- function(x='R_CHECK_TIMINGS'){
    x. <- Sys.getenv(x)
    xl <- as.logical(x.)
    notCRAN <- is.na(xl) || xl
#
    return(!notCRAN)
}


