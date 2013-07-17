is.installed.pkg <- function(pkg) {
  return(is.element(pkg, installed.packages()[,1]))
}

exit.by.uninstalled.pkg <- function(pkg, repos) {
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
    ' type:'
  )
  
  
  if(!(missing(repos))) {
    stopifnot(inherits(repos, 'character'))
    stopifnot(length(repos) == l)
  } else {
    repos <- rep('', l)
  }  
  
  repos.unique <- unique(repos)
  p <- length(repos.unique)
#   repos.id <- vector(p, mode = 'list')
  for(i in 1:p) {
#     repos.id[[i]] <- which(repos == repos.unique[i])

    pkg.p <- which(repos == repos.unique[i])
    l.p <- length(pkg.p)
    
    repos.print <- ''
    repos.value <- ''
    if(nchar(repos.unique[i]) > 0){
      repos.print <- ', repos=\''
      repos.value <- paste(repos.unique[i], '\'', sep='')
    }
    
    message(
      'install.packages(c(\'',
      paste(pkg[pkg.p], collapse="','"),
      '\')',
      repos.print,
      repos.value,
      ')'
    )
  }
}
# exit.by.uninstalled.pkg('foreign')
# exit.by.uninstalled.pkg(c('foreign', 'xtable'))
# repos <- c('', 'http://r-forge.r-project.org')
# exit.by.uninstalled.pkg(c('foreign', 'CHAID'), repos)

all.is.numeric <- function (x) {
  warn.user <- options(warn = -1) # suppress warnings
  on.exit(options(warn.user)) # restore warnings
  x <- sub("[[:space:]]+$", "", x) # right space trim
  x <- sub("^[[:space:]]+", "", x) # left space trim
  xs <- x[(x %in% c("", ".", "NA"))==FALSE]
  out <- !any(is.na(as.numeric(xs)))
  return(out)
}

formula.expand <- function (formula, data) {
  return(as.formula(deparse(terms(formula, data = data))))
}

formula.variables <- function(formula) { # expanded.formula
  form <- deparse(attr(terms(formula), 'variables'))
  vars <- strsplit(substr(form, 6,nchar(form)-1), ', ')[[1]]
  return(vars)
}