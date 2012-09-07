

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

    if(is.null(args$silent) || (args$silent == FALSE))
      print(table(v(x), v(out)))
    
    return(out)
  }
)
