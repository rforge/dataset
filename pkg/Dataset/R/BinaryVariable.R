#=================================================================================================
# Class definition
#=================================================================================================


setClass(
  Class = "BinaryVariable",
  contains = c("CategoricalVariable", "OrderedVariable"),
  validity = function(object) {
    if(Dataset.globalenv$print.io) cat (" =>       BinaryVariable: object validity check \n")
  	flag = TRUE
  
    if (nvalues(object) != 2) {
      message("The variable must have exactly two values")
      flag <- FALSE
    }
    if (!all(values(object) %in% c(0, 1))) {
      message("Codes have to be 0 and 1 for binary variables")
      message(paste("Codes are:", values(object, type = "values")))
      flag <- FALSE
    }
     
		return(flag)
	}
)


#=================================================================================================
# Class initializer
#=================================================================================================

  
#=================================================================================================
# Class standard constructeur
#=================================================================================================

binaryVariable <- function(
  x,
  missings,
  values,
  description,
  weights
){
  if(Dataset.globalenv$print.io) cat(" => (in)  BinaryVariable: builder \n")
    
    # then we apply special treatment for a binary variable
    #x <- variable$x
    #values <- variable$values
    m <- min(values)
    M <- max(values)
    
    if (m != 0) {
      x[which(x == m)] <- 0
      values[which(values == m)] <- 0
      message("the lower value must be 0 in a BinaryVariable object, a recodage has been performed")
    }
    if (M != 1) {
      x[which(x == M)] <- 1
      values[which(values == M)] <- 1
      message("the higher value must be 1 in a BinaryVariable object, a recodage has been performed")
    }

    out <- list(
      x = x,
      missings = missings,
      values = values,
      description = description,
      weights = weights
    )
    if(Dataset.globalenv$print.io) cat(" => (out) BinaryVariable: builder \n")
    return(out)
}

bvar <- function(
  x,
  missings,
  values,
  description,
  weights
) {
  if(missing(missings)) missings <- numeric(0)
  if(missing(values)) values <- numeric(0)
  if(missing(description)) description <- character(0)
  if(missing(x)) x <- numeric(0)
  if(missing(weights)) weights <- numeric(0)
  
  # we apply special treatment for categorical variable
  variable <- categoricalVariable(
    x = x,
    missings = missings,
    values = values,
    description = description,
    weights = weights
  )
  # we apply special treatment for qualitative variable
  variable <- binaryVariable(
    x = variable$x,
    missings = variable$missings,
    values = variable$values,
    description = variable$description,
    weights = variable$weights
  )
  
  out <- new(
    Class = "BinaryVariable",
    codes = variable$x,
    missings = variable$missings,
    values = variable$values,
    description = variable$description,
    weights = variable$weights
  )
  message(paste('number of missings:',nmissings(out), '(', round(nmissings(out)/length(out)*100,2), '%)'))
  return(out)
}

is.binary <- function(x){
  if(inherits(x, "BinaryVariable")){
    return(TRUE)
  } else {
    return(FALSE)
  }
}


setMethod("summary", "BinaryVariable", 
  definition = function (object, ...) {
    summary(as.vector(object))
  }
)