

setClass(
  Class = "QuantitativeVariable",
  contains = c("Variable"),
  representation = c("VIRTUAL"),
  validity = function(object) {
    if(Dataset.globalenv$print.io) cat (" =>       QuantitativeVariable: object validity check \n")
  	flag = TRUE
    
		return(flag)
	}
)


quantitativeVariable <- function(x, values, missings, description) {
  if(Dataset.globalenv$print.io) cat(" => (in)  QuantitativeVariable: virtual builder \n")
  
  if(inherits(x, 'factor')) stop("x can't be a factor")
  
  # we apply special treatment for variable
  variable <- variable(
    x = x,
    missings = missings,
    values = values,
    description = description
  )
  
  # then we apply special treatment for a quantitative variable
  # (nothing)

  
  out <- list(
    x = variable$x,
    missings = variable$missings,
    values = variable$values,
    description = variable$description,
    Variable.version = variable$Variable.version
  )
  #print(out)
  if(Dataset.globalenv$print.io) cat(" => (out) QuantitativeVariable: virtual builder \n")
  return(out)
}

is.quantitative <- function(x){
  if(inherits(x, "QuantitativeVariable")){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

setMethod("plot", "QuantitativeVariable", 
  definition = function (x, ...) {
    boxplot(as.vector(x), ...)
  }
)

setMethod(
  f = "cut", 
  signature = "QuantitativeVariable", 
  definition = function (x, ...) {
    args <- list(...)
    if('include.lowest' %in% names(args)) {
      out <- cut(as.vector(x), ... = ...)
    } else {
      out <- cut(as.vector(x), include.lowest=T, ... = ...)
    }
    
    valids.names <- levels(out)
    min.valid.code <- max(max(missings(x)),0) + 1 
    # we want first valid case start at 1
    valids <- min.valid.code:(min.valid.code+nlevels(out)-1)
#     valids <- 1:nlevels(out)
    names(valids) <- valids.names
    
    out <- as.numeric(out) ## FIXME pb missing collision ?
    
    diff.min.code <- min(out, na.rm=T) - min.valid.code
#     print(diff.min.code)
#     print(out[1:20])
    out <- out - diff.min.code
#     print(out[1:20])
#     print(valids)
    
    for (i in missings(x)){
      out[which(codes(x) == i)] <- i
    }
    
    if (nlevels(out) == 2) out <- bvar(out)
    else out <- ovar(out, missings=missings(x), values = valids, description = paste(description(x),'- cutted'))
    
    if(is.null(args$silent) || (args$silent == FALSE))
      print(table(v(x), v(out)))
    
    nmissings.before <- nmissings(x)
    nmissings.after <- nmissings(out)
    if(nmissings.before != nmissings.after) {
      message('Sorry, a problem occurs, data consistency lost. Please report this bug to the package maintainer.')
      message(paste('nmissings.before', nmissings.before))
      message(paste('nmissings.after', nmissings.after))
      stop()
    }
    return(out)
  }
)

setMethod("Ops", signature(e1="QuantitativeVariable", e2="QuantitativeVariable"),
          function(e1, e2) {
            e1@codes=callGeneric(codes(e1), codes(e2))
            validObject(e1)
            return(e1)
          }
)
setMethod("Ops", signature(e1="QuantitativeVariable", e2="numeric"),
          function(e1, e2) {
            e1@codes=callGeneric(codes(e1), e2)
            validObject(e1)
            return(e1)
          }
)
setMethod("Ops", signature(e1="numeric", e2="QuantitativeVariable"),
          function(e1, e2) {
            e2@codes=callGeneric(e1, codes(e2))
            validObject(e2)
            return(e2)
          }
)

setMethod(
  f ="sum",
  signature =c("QuantitativeVariable"),
  definition = function(x, na.rm=FALSE){
    return(sum(as.numeric(x), na.rm=na.rm))
  }
)