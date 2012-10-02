

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
    
    if(hasArg('breaks')) {
      args$breaks <- unique(c(min(x, na.rm=T), args$breaks, max(x, na.rm=T)))
    }
    if('include.lowest' %in% names(args)) {
      out <- do.call(cut, c(list('x'=as.vector(x)), args))
    } else {
#       out <- cut(as.vector(x), include.lowest=T, ... = args)
      out <- do.call(cut, c(list('x'=as.vector(x), 'include.lowest'=T), args))
    }
    
    valids.names <- levels(out)
    min.valid.code <- 1 
    # we want first valid case start at least at 1
    if(nmissings(x) > 0) {
      min.valid.code <- max(max(missings(x)),min.valid.code)
    }
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
    
    if (length(na.omit(unique(out))) == 2) {
      for (i in missings(x)){ # we refill missing values
        out[which(codes(x) == i)] <- i
      }
      out <- bvar(out, missings=missings(x), values = valids, description = paste(description(x),'- cutted'))
    } else {
      for (i in missings(x)){ # we refill missing values
        out[which(codes(x) == i)] <- i
      }
      out <- ovar(out, missings=missings(x), values = valids, description = paste(description(x),'- cutted'))
    }
    
    if(is.null(args$silent) || (args$silent == FALSE))
      print(table(v(x), v(out)))
    
    nmissings.before <- nmissings(x)
    nmissings.after <- nmissings(out)
    if(nmissings.before != nmissings.after) {
#       message('Sorry, a problem occurs, data consistency lost. Please report this bug to the package maintainer.')
      message('Sorry, a problem occurs, data consistency lost. Please check bounds.')
      message(paste('nmissings.before', nmissings.before))
      message(paste('nmissings.after', nmissings.after))
      stop()
    }
    return(out)
  }
)

setMethod("Ops", signature(e1="QuantitativeVariable", e2="QuantitativeVariable"),
          function(e1, e2) {
            
            # we have to perform the operation only on valid cases, not on missings
            # then we have to test that we did't create collisions with missings
            
#             old.codes <- e1@codes
#             mis <- which(old.codes %in% missings(e1))
            
            new.codes=callGeneric(codes(e1), codes(e2))
            
#             mis.new <- which(new.codes %in% missings(e2))
            
#             if(
            e1@new.codes
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