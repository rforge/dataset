#df1 <- data.frame('chi2' = c(23.664518,48.69871987,19.71,29.65419,34.7531), 'p-values' = c(0.0265432, 0.007555, 0.065789316, NA, NaN), 't' = c(23.664518,48.69871987,19.71,29.65419,34.7531), 'p-values' = c(0.0265432, 0.007555, 0.065789316, NA, NaN))
#sdf1 <- statdf(df1)
#sdf1
#df(sdf1)
#ssdf1 <- summary(sdf1)
#ssdf1

#---------------------------------------------------------------------------
#        summary.Statdf class specifications
#---------------------------------------------------------------------------

setClass(
  'summary.Statdf',
  representation(
    legend = 'character'
  ),
  contains = c('data.frame'),
  validity = function(object) {
    flag = TRUE
    
    # only one NA symbol
    if(flag && length(legend(object)) > 1) {
      print('legend argument should have a length of 1')
      flag <- FALSE
    }
    
    return(flag)
  }
)

setMethod('df', 'summary.Statdf', 
          definition = function (object) {
            out <- data.frame(slot(object, '.Data'))
            names(out) <- slot(object, 'names')
            row.names(out) <- slot(object, 'row.names')
            return(out)
          }
)
setReplaceMethod(
  f = 'df' ,
  signature = c('summary.Statdf', 'data.frame') ,
  definition = function(object, value){
    object@df <- value
    object@names <- names(value)
    object@row.names <- row.names(value)
    validObject(object)
    return(object)
  }
)

setMethod('legend', 'summary.Statdf', 
          definition = function (object) { 
            return(slot(object, 'legend'))
          }
)
setReplaceMethod(
  f = 'legend' ,
  signature = 'summary.Statdf' ,
  definition = function(object, value){
    object@legend <- value
    validObject(object)
    return(object)
  }
)


setMethod(
  f = 'print',
  signature = c('summary.Statdf'),
  definition = function(x, ...) {
    print(df(x))
    print(legend(x))
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
    thresholds = 'numeric',
    na = 'character',
    nan = 'character',
    formatc = 'list'
  ),
  contains = c('data.frame'),
  validity = function(object) {
    flag = TRUE
    
    # p-values > 0
    if(flag) {
      ncol <- ncol(object)/2
      ids <- seq.int(1, ncol, by = 2) + 1
      for (i in ids) {
        if (length(which(object[,i] < 0)) > 0){
          print(paste('One (or more) p-value is negative in column',i))
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
  df, 
  thresholds = c('***' = 0.001, '**' = 0.01, '*' = 0.05, '+' = 0.1),
  na = '?',
  nan = '',
  formatc = list('digits' = 2, 'format' = 'f')
) {
  stopifnot(inherits(df, 'data.frame'))
  if(ncol(df) %% 2 == 1) {
    stop('statdf: ncol(df) should be even')
  }
  out <- new('Statdf',
    .Data = df,
    row.names = row.names(df),
    names = names(df),
    thresholds = thresholds,
    na = na,
    nan = nan,
    formatc = formatc
  )
}

setMethod('df', 'Statdf', 
  definition = function (object) {
    out <- data.frame(slot(object, '.Data'))
    names(out) <- slot(object, 'names')
    row.names(out) <- slot(object, 'row.names')
    return(out)
  }
)
setReplaceMethod(
  f = 'df' ,
  signature = c('Statdf', 'data.frame') ,
  definition = function(object, value){
    object@df <- value
    object@names <- names(value)
    object@row.names <- row.names(value)
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
    print(df(x))
  }
)

setMethod(
  f = 'show',
  signature = c('Statdf'),
  definition = function(object) {
    print(object)
  }
)

is.nan.in.pvalues <- function(x) {
  ncol <- ncol(x)/2
  ids <- seq.int(1, ncol, by = 2) + 1
  flag <- FALSE
  for (i in ids) {
    if (any(is.nan(x[,i]))){
      flag <- TRUE
      break
    }
  }
  return(flag)
}

is.na.in.pvalues <- function(x) {
  ncol <- ncol(x)/2
  ids <- seq.int(1, ncol, by = 2) + 1
  flag <- FALSE
  for (i in ids) {
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
  if(length(nas) > 0) {
    lna <- paste(rep(' ', maxnc-nchar(na)+1), collapse = '') #FIXME pourquoi +1 ?
    #lna <- paste(rep(' ', maxnc-nchar('?')+1), collapse = '') #FIXME pourquoi +1 ?
    for (i in nas)
      out[i] <-  paste(na, lna, sep = '')
    #out[i] <-  paste('?', lna, sep = '')
  }
  
  nans <- which(is.nan(pvalues))
  if(length(nans) > 0) {
    lna <- paste(rep(' ', maxnc-nchar(nan)+1), collapse = '') #FIXME pourquoi +1 ?
    #lna <- paste(rep(' ', maxnc-nchar('?')+1), collapse = '') #FIXME pourquoi +1 ?
    for (i in nans)
      out[i] <-  paste(nan, lna, sep = '')
    #out[i] <-  paste('?', lna, sep = '')
  }
  
  
  for (i in 1:length(stars)) {
    stars[i] <- paste(stars[i], paste(rep(' ', maxnc-nchar(stars[i])), collapse = ''))
  }
  
  # we set stars but not for NAs (NaN included)
  out[-nas] <- stars[mapply(findInterval, pvalues[-nas], list(thresholds), rightmost.closed = T) + 1]
  
  return(out)
}
#th <- c('***' = 0.001, '**' = 0.01, '*' = 0.05, '+' = 0.1)
#giveStars(c(0.02, 0.005, 0.06), th)
#giveStars(c(0.02, 0.005, 0.06, NA), th)
#giveStars(c(0.02, 0.005, 0.06, NA, 0.2, NaN), th, '!')



setMethod(
  f = 'summary',
  signature = c('Statdf'),
  definition = function(object, ...) {
    ncol <- ncol(object)/2
    ids <- seq.int(1, ncol(object), by = 2)
    out <- data.frame(matrix(rep(0, ncol*nrow(object)), ncol = ncol))
    names(out) <- names(object)[ids]
    
    # we format values
    for(i in 1:ncol){
      out[,i] <- do.call(formatC, c(list("x" = object[,ids[i]]), formatc(sdf1))) 
    }
    
    # we give stars
    for(i in 1:ncol) {
      temp <- giveStars(object[,ids[i]+1], thresholds = thresholds(object), na = na(object), nan = nan(object))
      out[,i] <- mapply(paste, out[,i], temp, sep = '')
    }
    
    legend <- paste(names(thresholds(object)), thresholds(object), collapse = ', ', sep = ' < ')
    if(is.na.in.pvalues(object)) legend <- paste(legend, ', ', na(object), ' = NA', sep = '')

    out <- new('summary.Statdf',
       .Data = out,
       row.names = row.names(out),
       names = names(out),
      "legend" = legend
    )
    return(out)
  }
)

























giveStars.variable <- function(x, legend = FALSE) {
  return(sapply(x, giveStars))
}
# giveStars.variable(c(0.000002, 0.002, 0.02, 0.2))

giveStars.df <- function(df, legend = FALSE) {
 for (i in 1:ncol(df)) {
   df[[i]] <- giveStars.variable(df[[i]])
 }
 return(df)
}
# df <- data.frame(x = c(0.01,0.2,0.0001), y = c(0.02,0.05,0.00001))
# giveStars.df(df)

even <- function(x) {
  return(x[which((x %% 2) == 0)])
}
# even(1:6)

giveStars.df.even <- function(df, legend = FALSE) {
 for (i in even(1:ncol(df))) {
   df[[i]] <- giveStars.variable(df[[i]])
 }
 return(df)
}
# df <- data.frame(x = c(0.01,0.2,0.0001), y = c(0.02,0.05,0.00001))
# giveStars.df.even(df)

addEvenNames <- function(char.vector, word = ' ', use.colnames = FALSE) {
  out <- character(0)
  for (i in 1:length(char.vector)) {
    out <- c(out, char.vector[i])
    if (use.colnames)
      out <- c(out, paste(char.vector[i], word))
    if (!use.colnames)
      out <- c(out,word)
  }
  return(out)
}

# addEvenNames(c("Model 1", "Model 2"))
# addEvenNames(c("Model 1", "Model 2"), word = "signif.")
# addEvenNames(c("Model 1", "Model 2"), word = "signif.", use.colnames = T)

addSignif <- function(char.vector, word = "signif.") {
  out <- character(0)
  for (i in 1:length(char.vector)) {
    out <- c(out, char.vector[i])
    out <- c(out, paste(char.vector[i], word))
  }
  return(out)
}
