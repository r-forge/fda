\name{StatSciChinese}
\alias{StatSciChinese}
\docType{data}
\title{Statistical Science in Chinese}
\description{
  (x, y, z) coordinates of the location of the tip of a pen during fifty
  replications of writing 'Statistical Science' in simplified Chinese at
  2.5 milisecond intervals
}
\usage{data(StatSciChinese)}
\format{
  a 3-dimensional array of dimensions (601, 50, 3) containing 601
  observations of (x, y, z) coordinates of the tip of a pen at 2.5
  milisecond intervals for each of 50 repitions of writing 'Statistical
  Science' in simplified Chinese.
}
\details{
  Xiaochun Li wrote 'Statistical Science' in simplified Chinese 50
  times.  An infra-red detecting tablet was attached to the tip of the
  pen, and a wall-mounted set of three cameras recorded its position 400
  times per second with an error level of about 0.5 millimeters.  Each
  sample required about 6 seconds to produce, and for simplicity, time
  was normalized to this interval for all 50 records.  The script
  requires 50 strokes, with an average time of 120 milliseconds per
  stroke.  These raw data were transformed using principal components to
  produce this \code{StatSciChinese} data set.
}
%\source{}
\references{
  Ramsay, James O. (2000) Functional Components of Variation in
  Handwriting, \emph{Journal of the American Statistical Association},
  95, 9-15.
}
\examples{
data(StatSciChinese)

plot(StatSciChinese[, 1, 3], type='l')
sel1 <- (StatSciChinese[, 1, 3] <
         quantile(StatSciChinese[, 1, 3], .9) )
plot(StatSciChinese[sel1, 1, 1], StatSciChinese[sel1, 1, 2],
     type='l')
}
\keyword{datasets}
