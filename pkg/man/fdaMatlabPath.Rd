\name{fdaMatlabPath}
\alias{fdaMatlabPath}
\title{
  Add 'fdaM' to the Matlab path 
}
\description{
  Write a sequence of Matlab commands to \code{fdaMatlabPath.m} in the
  working directory containing commands to add \code{fdaM} to the path
  for Matlab.  Optionally, add these lines to \code{Matlab}
  \code{startup.m} if it can be found.  
}
\usage{
fdaMatlabPath(startupFile, R.matlab) 
}
\arguments{
  \item{startupFile}{
    character string giving the name of the Matlab \code{startup.m}.  
  }
  \item{R.matlab}{
    logical:  If TRUE, include '~R/library/R.matlab/externals' in the
    path.  
  }
}
\details{
  1.  path2fdaM = path to the \code{Matlab/fdaM} subdiretory of the
  \code{fda} installation directory.

  2.  Find all subdirectories of path2fdaM except those beginning in '@'
  or including 'private'.  

  3.  if(requires(R.matlab)) add the path to \code{MatlabServer.m} to
  \code{dirs2add} 

  4.  d2a <- paste("addpath('", dirs2add, "');", sep='')

  5.  writeLines(d2a, 'fdaMatlabPath.m')

  6.  if(exists(startupFile)) append \code{d2a} to it
}
\value{
  a character vector of Matlab \code{addpath} commands.  
}
\references{
  Matlab documentation for \code{addpath} and \code{startup.m}.  
}
\author{
  Spencer Graves with help from Jerome Besnard
} 
\seealso{
  \code{\link[R.matlab]{Matlab}},
  \code{\link{dirs}}
}
\examples{
# Modify the Matlab startup.m only when you really want to,
# typically once per installation ... certaintly not
# every time we test this package.
fdaMatlabPath()
}
\keyword{programming}% at least one, from doc/KEYWORDS

