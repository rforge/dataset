#=================================================================================================
# Class definition
#=================================================================================================


setClass(
  Class = "OrderedVariable",
  contains = c("CategoricalVariable"),
  representation = c("VIRTUAL"),
  validity = function(object) {
    if(Dataset.globalenv$print.io) cat (" =>       OrderedVariable: object validity check \n")
  	flag <- TRUE
    
    #codes <- unique(object@codes)
    #missings <- object@missings
    #values <- object@values
    #description <- object@description
      
		return(flag)
	}
)



#=================================================================================================
# Class initializer
#=================================================================================================

  
#=================================================================================================
# Class standard constructeur
#=================================================================================================

#OrderedVariable <- function(
#  x,
#  missings,
#  values,
#  description
#) {
#  if(Dataset.globalenv$print.io) cat(" => (in)  OrderedVariable: virtual builder\n")
#  if(Dataset.globalenv$print.io) cat(" => (out) OrderedVariable: virtual builder\n")
#}


# as.factor
setMethod(
  f = "as.factor",
  signature = "OrderedVariable", 
  definition = function (x) {
    m <- getMethod("as.factor", "CategoricalVariable")
    out <- do.call(m, list(x))
    #out <- ordered(out) # FIXED if we use ordered(), unused levels are dropped! So keep in mind we always have to specify levels!
    out <- ordered(out, levels = levels(out))
    return(out)
  }
)

setMethod(
  f = "as.vector",
  signature = "OrderedVariable", 
  definition = function (x) {
    return(as.factor(x))
  }
)
setMethod(
  f = "v",
  signature = "OrderedVariable", 
  definition = function (x) {
    return(as.vector(x))
  }
)

setMethod(
  f = "is.ordered",
  signature = "OrderedVariable", 
  definition = function (x) {
    if(inherits(x, "OrderedVariable")){
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
)

#is.ordered <- function(x){
#  if(inherits(x, "OrderedVariable")){
#    return(TRUE)
#  } else {
#    return(FALSE)
#  }
#}

setMethod("summary", "OrderedVariable", 
  definition = function (object, ...) {
    summary(as.vector(object))
  }
)