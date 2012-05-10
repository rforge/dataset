#=================================================================================================
# Class definition
#=================================================================================================


setClass(
  Class = "OrdinalVariable",
  contains = c("NominalVariable", "OrderedVariable"),
  validity = function(object) {
    if(Dataset.globalenv$print.io) cat (" =>       OrdinalVariable: object validity check \n")
    flag = TRUE
     
		return(flag)
	}
)


#=================================================================================================
# Class initializer
#=================================================================================================

  
#=================================================================================================
# Class standard constructeur
#=================================================================================================

ovar <- function(
  x,
  missings,
  values,
  description,
  weights
) {
  if(Dataset.globalenv$print.io) cat(" => (in)  OrdinalVariable: builder \n")
  if(missing(missings)) missings <- numeric(0)
  if(missing(values)) values <- numeric(0)
  if(missing(description)) description <- character(0)
  if(missing(x)) x <- numeric(0)
  if(missing(weights)) weights <- numeric(0)
  
  if (inherits(x, 'NominalVariable')) {
    out <- new(
      Class = "OrdinalVariable",
      codes = codes(x),
      missings = missings(x),
      values = values(x),
      description = description(x),
      weights = slot(x, 'weights')
    )
  } else {
  
    # we apply special treatment for categorical variable
    variable <- categoricalVariable(
      x = x,
      missings = missings,
      values = values,
      description = description,
      weights = weights
    )
    
    # we apply special treatment for nominal variable
    variable <- nominalVariable(
      x = variable$x,
      missings = variable$missings,
      values = variable$values,
      description = variable$description,
      weights = variable$weights
    )
    
    if(Dataset.globalenv$print.io) cat(" => (out) OrdinalVariable: builder \n")
    out <- new(
      Class = "OrdinalVariable",
      codes = variable$x,
      missings = variable$missings,
      values = variable$values,
      description = variable$description,
      weights = variable$weights
    )
  }
  message(paste('number of missings:',nmissings(out)))
  return(out)
}

is.ordinal <- function(x){
  if(inherits(x, "OrdinalVariable")){
    return(TRUE)
  } else {
    return(FALSE)
  }
}