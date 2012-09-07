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
  description
) {
  if(Dataset.globalenv$print.io) cat(" => (in)  OrdinalVariable: builder \n")
  if(missing(missings)) missings <- numeric(0)
  if(missing(values)) values <- numeric(0)
  if(missing(description)) description <- Dataset.globalenv$Variable.description.default
  if(missing(x)) x <- numeric(0)
  
  if (inherits(x, 'NominalVariable')) {
    out <- new(
      Class = "OrdinalVariable",
      codes = codes(x),
      missings = missings(x),
      values = values(x),
      description = description(x),
      Variable.version = slot(x, 'Variable.version')
    )
  } else {
  
    # we apply special treatment for categorical variable
    variable <- categoricalVariable(
      x = x,
      missings = missings,
      values = values,
      description = description
    )
    
    # we apply special treatment for nominal variable
    nvariable <- nominalVariable(
      x = variable$x,
      missings = variable$missings,
      values = variable$values,
      description = variable$description
    )
    
    if(Dataset.globalenv$print.io) cat(" => (out) OrdinalVariable: builder \n")
    out <- new(
      Class = "OrdinalVariable",
      codes = nvariable$x,
      missings = nvariable$missings,
      values = nvariable$values,
      description = nvariable$description,
      Variable.version = variable$Variable.version
    )
  }
  message(paste('number of missings:',nmissings(out), '(', round(nmissings(out)/length(out)*100,2), '%)'))
  return(out)
}

is.ordinal <- function(x){
  if(inherits(x, "OrdinalVariable")){
    return(TRUE)
  } else {
    return(FALSE)
  }
}