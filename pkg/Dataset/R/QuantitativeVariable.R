

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
    out <- cut(as.vector(x), ... = ...)
    
    # print(levels(out))
    
    if (nlevels(out) == 2) out <- bvar(out)
    else out <- ovar(out)
    
    description(out) <- paste(description(x),'- cutted')
    missings(out) <- missings(x)

    if(is.null(args$silent) || (args$silent == FALSE))
      print(table(v(x), v(out)))
    
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