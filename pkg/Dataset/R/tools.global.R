is.installed.pkg <- function(pkg) {
  return(is.element(pkg, installed.packages()[,1]))
}

exit.by.uninstalled.pkg <- function(pkg) {
  l <- length(pkg)
  
  this <- 'this'
  s <- ''
  
  if(l>1) {
    this <- 'these'
    s <- 's'
  }
  
  message(
  'To use this function you first need to install the following package',
  s,
  ':\n',
  paste(pkg, collapse=', '),
  '\n')
  message(
    'To install ',
    this,
    ' package',
    s,
    ' type\n',
    'install.packages(c(\'',
    paste(pkg, collapse="','"),
    '\'))'
  )
}
# exit.by.uninstalled.pkg('foreign')
# exit.by.uninstalled.pkg(c('foreign', 'xtable'))

all.is.numeric <- function (x) {
  warn.user <- options(warn = -1) # suppress warnings
  on.exit(options(warn.user)) # restore warnings
  x <- sub("[[:space:]]+$", "", x) # right space trim
  x <- sub("^[[:space:]]+", "", x) # left space trim
  xs <- x[(x %in% c("", ".", "NA"))==FALSE]
  out <- !any(is.na(as.numeric(xs)))
  return(out)
}