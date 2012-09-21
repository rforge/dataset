#df0 <- data.frame('chi2' = c(23.664518,48.69871987,19.71,29.65419,34.7531), 'p-values' = c(0.0265432, 0.007555, 1.065789316, NA, NaN), 't' = c(23.664518,48.69871987,19.71,29.65419,34.7531), 'p-values' = c(0.0265432, 0.007555, 0.065789316, NA, NaN))
#sdf0 <- statdf(df0)
#sdf0 <- statdf(df0, pvalues = 'even')
#df1 <- data.frame('chi2' = c(23.664518,48.69871987,19.71,29.65419,34.7531), 'p-values' = c(0.0265432, 0.007555, 0.065789316, NA, NaN), 't' = c(23.664518,48.69871987,19.71,29.65419,34.7531), 'p-values' = c(-0.0265432, 0.007555, 0.065789316, NA, NaN))
#sdf1 <- statdf(df1, pvalues = 'even')
#df2 <- data.frame('chi2' = c(23.664518,48.69871987,19.71,NA,34.7531), 'p-values' = c(0.0265432, 0.007555, 0.065789316, NA, NaN), 't' = c(23.664518,48.69871987,19.71,29.65419,34.7531), 'p-values' = c(0.0265432, 0.007555, 0.065789316, NA, NaN))
#sdf2 <- statdf(df2)
#sdf2 <- statdf(df2, name = "Bonjour table", pvalues = 'even')
#sdf2
#sdf(sdf2)
#ssdf2 <- summary(sdf2, merge = 'left')
#ssdf2
# summaryToPDF(sdf2, merge = 'left')
#df3 <- data.frame('chi2' = c(23.664518,48.69871987), 'p-values' = c(NA, NA), 't' = c(23.664518,48.69871987), 'p-values' = c(0.0265432, 0.007555))
#sdf3 <- statdf(df3)
#sdf3
#as.data.frame(sdf3)
#ssdf3 <- summary(sdf3)
#ssdf3
#df4 <- data.frame('chi2' = c(NA,NaN), 'p-values' = c(NA, NA), 't' = c(23.664518,48.69871987), 'p-values' = c(0.0265432, 0.007555))
#sdf4 <- statdf(df4)
#sdf4
#as.data.frame(sdf4)
#ssdf4 <- summary(sdf4)
#ssdf4
#df5 <- data.frame('p-values' = c(NA, NA), 'p-values' = c(0.0265432, 0.007555))
#sdf5 <- statdf(df5)
#sdf5
#as.data.frame(sdf5)
#ssdf5 <- summary(sdf5)
#ssdf5
#sdf5 <- statdf(df5, pvalues = 'all')
#sdf5
#as.data.frame(sdf5)
#ssdf5 <- summary(sdf5)
#ssdf5


#---------------------------------------------------------------------------
#        summary.Statdf class specifications
#---------------------------------------------------------------------------

setClass(
  'summary.Statdf',
  representation(
    name = 'character',
    thresholds = 'character'
  ),
  contains = c('data.frame'),
  validity = function(object) {
    flag = TRUE
    
    # only one name
    if(flag && length(name(object)) > 1) {
      print('name argument should have a length of 1')
      flag <- FALSE
    }
    
    # only one thresholds legend
    if(flag && length(thresholds(object)) > 1) {
      print('thresholds argument should have a length of 1')
      flag <- FALSE
    }
    
    return(flag)
  }
)

setMethod('sdf', 'summary.Statdf', 
          definition = function (object) {
            out <- data.frame(slot(object, '.Data'))
            names(out) <- slot(object, 'names')
            row.names(out) <- slot(object, 'row.names')
            return(out)
          }
)
setReplaceMethod(
  f = 'sdf' ,
  signature = c('summary.Statdf', 'data.frame') ,
  definition = function(object, value){
    object@sdf <- value
    object@names <- names(value)
    object@row.names <- row.names(value)
    validObject(object)
    return(object)
  }
)
setMethod('as.data.frame', 'summary.Statdf', 
          definition = function (x) {
            return(sdf(x))
          }
)

setMethod('name', 'summary.Statdf', 
          definition = function (object) { 
            return(slot(object, 'name'))
          }
)
setReplaceMethod(
  f = 'name' ,
  signature = 'summary.Statdf' ,
  definition = function(object, value){
    object@name <- value
    validObject(object)
    return(object)
  }
)

setMethod('thresholds', 'summary.Statdf', 
          definition = function (object) { 
            return(slot(object, 'thresholds'))
          }
)
setReplaceMethod(
  f = 'thresholds' ,
  signature = 'summary.Statdf' ,
  definition = function(object, value){
    object@thresholds <- value
    validObject(object)
    return(object)
  }
)


setMethod(
  f = 'print',
  signature = c('summary.Statdf'),
  definition = function(x, ...) {
    message(name(x))
    print(sdf(x))
    message(thresholds(x))
  }
)

setMethod(
  f = 'show',
  signature = c('summary.Statdf'),
  definition = function(object) {
    print(object)
  }
)


#---------------------------------------------------------------------------
#        Statdf class specifications
#---------------------------------------------------------------------------

setClass(
  'Statdf',
  representation(
    name = 'character',
    pvalues = 'numeric',
    thresholds = 'numeric',
    na = 'character',
    nan = 'character',
    formatc = 'list'
  ),
  contains = c('data.frame'),
  validity = function(object) {
    flag = TRUE
    
    # only one name
    if(flag && length(name(object)) > 1) {
      print('name argument should have a length of 1')
      flag <- FALSE
    }
    
    # check if all pvalues id are in the data frame bounds
    if(flag && !all(pvalues(object) %in% 1:ncol(object))) {
      print('One or more pvalues column id is out of the data frame bounds, please check your pvalues argument')
      flag <- FALSE
    }
    
    # p-values >= 0
    if(flag && length(pvalues(object)) > 0) {
      for (i in pvalues(object)) {
        if (length(which(object[,i] < 0)) > 0){
          print(paste('One (or more) p-value is negative in column',i))
          print(object[,i])
          flag <- FALSE
          break
        }
      }
    }
    # p-values <= 1
    if(flag && length(pvalues(object)) > 0) {
      for (i in pvalues(object)) {
        if (length(which(object[,i] > 1)) > 0){
          print(paste('One (or more) p-value is > 1 in column',i))
          print(object[,i])
          flag <- FALSE
          break
        }
      }
    }
    
    # only one NA symbol
    if(flag && length(na(object)) > 1) {
      print('na argument should have a length of 1')
      flag <- FALSE
    }
    
    # only one NaN symbol
    if(flag && length(nan(object)) > 1) {
      print('nan argument should have a length of 1')
      flag <- FALSE
    }
    
    thresholds <- thresholds(object)
    # no NAs in thresholds
    if(flag && any(is.na(thresholds))) {
      print('thresholds argument must not contain NA')
      flag <- FALSE
    }
    
    # thresholds > 0
    if(flag && (length(which(thresholds <= 0)) > 0)) {
      print('thresholds have to be greater than 0')
      flag <- FALSE
    }
    # thresholds < 1
    if(flag && (length(which(thresholds >= 1)) > 0)) {
      print('thresholds have to be lower than 1')
      flag <- FALSE
    }
    # thresholds unique
    if(flag && (length(unique(thresholds)) != length(thresholds))) {
      print('thresholds have to be unique')
      flag <- FALSE
    }
    # thresholds sorted
    if(flag && !(all(sort(thresholds) == thresholds))) {
      print('thresholds have to be in an increasing sorting')
      flag <- FALSE
    }
    # thresholds termine par 1 ?
    
    return(flag)
  }
)

# builder
statdf <- function(
  sdf,
  name = 'Untitled table',
  pvalues,
  thresholds = c('***' = 0.001, '**' = 0.01, '*' = 0.05, '+' = 0.1),
  na = '?',
  nan = '',
  formatc = list('digits' = 2, 'format' = 'f')
) {
  if(missing(sdf)) sdf <- data.frame()
  stopifnot(inherits(sdf, 'data.frame'))  
  
  if(missing(pvalues)) pvalues <- numeric(0)
  
  if(inherits(pvalues, 'character')) {
    stopifnot(pvalues %in% c('even', 'odd', 'all'))
    stopifnot(length(pvalues) <= 1)
    
    if(pvalues == 'even') {
      if(ncol(sdf) >= 2) {
        newpvalues <- seq(2,ncol(sdf), 2)
      } else {
        newpvalues <- numeric(0)
      }
    }
    if(pvalues == 'odd') {
      if(ncol(sdf) >= 2) {
        newpvalues <- seq(1,ncol(sdf), 2)
      } else {
        newpvalues <- numeric(0)
      }
    }
    if(pvalues == 'all') {
      newpvalues <- 1:ncol(sdf)
    }
    pvalues <- newpvalues
  }
  
  out <- new('Statdf',
    .Data = sdf,
    row.names = row.names(sdf),
    names = names(sdf),
    name = name,
    pvalues = pvalues,
    thresholds = thresholds,
    na = na,
    nan = nan,
    formatc = formatc
  )
}

setMethod('sdf', 'Statdf', 
  definition = function (object) {
    out <- data.frame(slot(object, '.Data'))
    names(out) <- slot(object, 'names')
    row.names(out) <- slot(object, 'row.names')
    return(out)
  }
)
setReplaceMethod(
  f = 'sdf' ,
  signature = c('Statdf', 'data.frame') ,
  definition = function(object, value){
    object@sdf <- value
    object@names <- names(value)
    object@row.names <- row.names(value)
    validObject(object)
    return(object)
  }
)

setMethod('name', 'Statdf', 
          definition = function (object) { 
            return(slot(object, 'name'))
          }
)
setReplaceMethod(
  f = 'name' ,
  signature = 'Statdf' ,
  definition = function(object, value){
    object@name <- value
    validObject(object)
    return(object)
  }
)

setMethod('pvalues', 'Statdf', 
          definition = function (object) { 
            return(slot(object, 'pvalues'))
          }
)
setReplaceMethod(
  f = 'pvalues' ,
  signature = 'Statdf' ,
  definition = function(object, value){
    object@pvalues <- value
    validObject(object)
    return(object)
  }
)

setMethod('thresholds', 'Statdf', 
  definition = function (object) { 
    return(slot(object, 'thresholds'))
  }
)
setReplaceMethod(
  f = 'thresholds' ,
  signature = 'Statdf' ,
  definition = function(object, value){
    object@thresholds <- value
    validObject(object)
    return(object)
  }
)

setMethod('na', 'Statdf', 
          definition = function (object) { 
            return(slot(object, 'na'))
          }
)
setReplaceMethod(
  f = 'na' ,
  signature = 'Statdf' ,
  definition = function(object, value){
    object@na <- value
    validObject(object)
    return(object)
  }
)

setMethod('nan', 'Statdf', 
          definition = function (object) { 
            return(slot(object, 'nan'))
          }
)
setReplaceMethod(
  f = 'nan' ,
  signature = 'Statdf' ,
  definition = function(object, value){
    object@nan <- value
    validObject(object)
    return(object)
  }
)

setMethod('formatc', 'Statdf', 
  definition = function (object) { 
    return(slot(object, 'formatc'))
  }
)
setReplaceMethod(
  f = 'formatc' ,
  signature = 'Statdf' ,
  definition = function(object, value){
    object@formatc <- value
    validObject(object)
    return(object)
  }
)

setMethod(
  f = 'print',
  signature = c('Statdf'),
  definition = function(x, ...) {
    print(sdf(x))
  }
)

setMethod(
  f = 'show',
  signature = c('Statdf'),
  definition = function(object) {
    print(object)
  }
)

is.nan.in.pvalues <- function(x) { # x : a statdf object
  flag <- FALSE
  for (i in pvalues(x)) {
    if (any(is.nan(x[,i]))){
      flag <- TRUE
      break
    }
  }
  return(flag)
}

is.na.in.pvalues <- function(x) {
  flag <- FALSE
  for (i in pvalues(x)) {
    if (any(is.na(x[,i]))){
      flag <- TRUE
      break
    }
  }
  return(flag)
}

is.nan.in.statistics <- function(x) {
  flag <- FALSE
  for (i in setdiff(1:ncol(x), pvalues(x))) {
    if (any(is.nan(x[,i]))){
      flag <- TRUE
      break
    }
  }
  return(flag)
}

is.na.in.statistics <- function(x) {
  flag <- FALSE
  for (i in setdiff(1:ncol(x), pvalues(x))) {
    if (any(is.na(x[,i]))){
      flag <- TRUE
      break
    }
  }
  return(flag)
}

# FIXME PREVOIR LES DEGRES DE LIBERTES ?
giveStars <- function(pvalues, thresholds, na = '?', nan = '#') {
  stars <- c(names(thresholds), '')
  maxnc <- max(nchar(stars))
  
  out <- pvalues
  
  nas <- which(is.na(pvalues))
  
  # first we check if we have only NAs value
  if (length(nas) == length(pvalues)) {
    out <- rep(na, length(pvalues))
  } else {
  
    if(length(nas) > 0) {
      lna <- paste(rep(' ', maxnc-nchar(na)+1), collapse = '') #FIXME pourquoi +1 ?
      for (i in nas)
        out[i] <-  paste(na, lna, sep = '')
    }
    
    nans <- which(is.nan(pvalues))
    if(length(nans) > 0) {
      lna <- paste(rep(' ', maxnc-nchar(nan)+1), collapse = '') #FIXME pourquoi +1 ?
      for (i in nans)
        out[i] <-  paste(nan, lna, sep = '')
    }
    
    
    for (i in 1:length(stars)) {
      stars[i] <- paste(stars[i], paste(rep(' ', maxnc-nchar(stars[i])), collapse = ''))
    }
    
    # we set stars but not for NAs (NaN included)
    # first : case with no NAs
    if(length(nas) == 0) {
      out <- stars[mapply(findInterval, pvalues, list(thresholds), rightmost.closed = T) + 1]
    } else {
      out[-nas] <- stars[mapply(findInterval, pvalues[-nas], list(thresholds), rightmost.closed = T) + 1]
    }
  }
  
  return(out)
}
#th <- c('***' = 0.001, '**' = 0.01, '*' = 0.05, '+' = 0.1)
#giveStars(c(0.02, 0.005, 0.06), th)
#giveStars(c(0.02, 0.005, 0.06, NA), th)
#giveStars(c(0.02, 0.005, 0.06, NA, 0.2, NaN), th, '!')
#giveStars(c(NA,NA,NA), th)


#sdf4
#sdf(sdf4)
#sdf4[2,1]
#ssdf4 <- summary(sdf4)

setMethod(
  f = 'summary',
  signature = c('Statdf'),
  definition = function(object, merge = 'no', ...) {
    
    stopifnot(inherits(merge, 'character') && length(merge) <= 1)
    stopifnot(merge %in% c('no', 'left', 'right'))
    
    ncol <- ncol(object)
    out <- data.frame(matrix(rep(0, ncol*nrow(object)), ncol = ncol))
    
    id.pval <- pvalues(object)
    id.stat <- setdiff(1:ncol, pvalues(object))
    
    # we format values
    for(i in id.stat){
      out[,i] <- object[,i]
      
      the.nas <- which(is.na(out[,i]))
      the.nans <- which(is.nan(out[,i]))
      # we replace NAs
      if(length(the.nas) > 0) out[the.nas,i] <- na(object)
      # we replace NAs
      if(length(the.nans) > 0) out[the.nans,i] <- nan(object)
      # then we format values
      out[,i] <- do.call(formatC, c(list("x" = out[,i]), formatc(object)))
    }
    
    # we give stars
    for(i in id.pval) {
      out[,i] <- giveStars(object[,i], thresholds = thresholds(object), na = na(object), nan = nan(object))
    }
    
    row.names(out) <- row.names(object)
    if(merge == 'no') {
      names(out) <- names(object)
    }
    if(merge == 'left') {
      colToMerge <- pvalues(object)[which(pvalues(object) >= 2)]
      colToKeep <- setdiff(1:ncol, colToMerge)
      out2 <- out
      for (i in colToMerge) {
        out2[,i-1] <- paste(out2[,i-1], out2[,i])
      }
      out2 <- out2[colToKeep]
      names(out2) <- names(object)[colToKeep]
      out <- out2
    }
    if(merge == 'right') {
      colToMerge <- pvalues(object)[which(pvalues(object) < ncol)]
      colToKeep <- setdiff(1:ncol, colToMerge)
      out2 <- out
      for (i in colToMerge) {
        out2[,i] <- paste(out2[,i], out2[,i-1])
      }
      out2 <- out2[colToKeep]
      names(out2) <- names(object)[colToKeep]
      out <- out2
    }
    
    # we create the legend
    legend <- paste(names(thresholds(object)), thresholds(object), collapse = ', ', sep = ' < ')
    if(is.na.in.pvalues(object) || is.na.in.statistics(object)) {
      legend <- paste(legend, ", '", na(object), "' = NA", sep = '')
    }
    if(is.nan.in.pvalues(object) || is.nan.in.statistics(object)) {
      legend <- paste(legend, ", '", nan(object), "' = NaN", sep = '')
    }
    
    
    out <- new('summary.Statdf',
       .Data = out,
       row.names = row.names(out),
       names = names(out),
       name = name(object),
       thresholds = legend
    )
    return(out)
  }
)



setMethod(
  f = 'summaryToPDF',
  signature = c('Statdf'),
  definition = function(object, pdfSavingName, graphics = FALSE, description.chlength = 120, values.chlength = 6, dateformat, latexPackages = NULL, keepTex = FALSE, openPDF, merge = 'no') {
    
    if(!is.installed.pkg('xtable')) {
      exit.by.uninstalled.pkg('xtable')
    } else {
      
      require(xtable)
      
      s <- summary(object, merge = merge)
      
      outName <- name(object)
      
      outName.pdf <- make.names(outName) # no spaces for Unix/Texlive compilation ?
      
      if(missing(pdfSavingName)) {		
        pdfSavingName <- paste("Summary-", outName.pdf, sep = "") # no spaces for Unix/Texlive compilation ?
      }
      
      latexFile <- paste(pdfSavingName, ".tex", sep="")
      
      outFileCon <- file(latexFile, "w", encoding="UTF-8")
      
      latex.head(title = paste("Summary of the", totex(outName)), latexPackages, outFileCon)
      
      #cat("\\section*{Overview} \n", file = outFileCon, append = T)
      
      object.xtable <- xtable(
        sdf(s),
        #label='validCasesSummary',
        #caption='Number of variables by percent of valid cases',
        caption=thresholds(s),
        #digits = 3,
        align = c("l", rep('c', ncol(sdf(s)))),
        #display = c("d","d","d")
      )
      
      cat("\\begin{center} \n", file = outFileCon, append = T)
      print(object.xtable, file=outFileCon , append=T,
        #tabular.environment='longtable',
        table.placement = "htb",
        floating=F
      )
      
      cat("\\newline ", " \n", file = outFileCon, append = T)
      cat(thresholds(s), " \n", file = outFileCon, append = T)
      
      cat("\\end{center} \n", file = outFileCon, append = T)
  
      
      close.and.clean(outFileCon, pdfSavingName, keepTex, openPDF)
    }
  }
)
