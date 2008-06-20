fdaMatlabPath <- function(startupFile, R.matlab) {
##
## 1.  path2fdaM = path to ~R/library/fda/Matlab/fdaM
##
  path2fdaM <- system.file('Matlab/fdaM', package='fda')
##
## 2.  dirs2add = dirs(path2fdaM, ...)
##
  d2a.all <- dir(path2fdaM, full.names=TRUE, recursive=TRUE)
  d2a.at <- dirs(path2fdaM, full.names=TRUE, recursive=TRUE,
                 pattern = '!^@')
  d2a.private <- dirs(path2fdaM, full.names=TRUE, recursive=TRUE,
                      pattern='^private$')
  no.at <- !(d2a.all %in% d2a.at)
  no.private <- !(d2a.all %in% d2a.private)
  dirs2add <- d2a.all[no.at & no.private]
##
## 3.  requires(R.matlab)?
##
  missRmat <- missing(R.matlab)
  if(missRmat)R.matlab <- TRUE 
  if(R.matlab){
    if(require(R.matlab))
      dirs2add <- c(system.file('externals', package='R.matlab'),
                    dirs2add)
    else
      if(!missRmat)
        warning('Package R.matlab is not installed and can not be',
                ' included in the Matlab path.')
  }
##
## 4.  Create Matlab 'addpath' commands.
##
  d2a <- paste("addpath('", dirs2add, "');", sep='')
##
## 5.  write file
##
  writeLines(d2a, 'fdaMatlabPath.m')
##
## 6.  startupFile?  
##
  if(!missing(startupFile)){
    if(exists(startupFile))
      file.copy2(startupFile)
    cat(d2a, file=startupFile, sep='\n', append=TRUE)
  }
##
## 7.  Done
##
  d2a  
}
