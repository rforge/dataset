#=================================================================================================
# Class definition
#=================================================================================================


setClass(
  Class = "NominalVariable",
	contains = c("CategoricalVariable"),
  validity = function(object) {
    if(Dataset.globalenv$print.io) cat (" =>       NominalVariable: object validity check \n")
  	flag = TRUE
        
    if (nvalues(object) <= 2) {
      message("The variable must have at least three values")
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

nominalVariable <- function(
  x,
  missings,
  values,
  description
){
  if(Dataset.globalenv$print.io) cat(" => (in)  NominalVariable: builder \n")
  
  out <- list(
    x = x,
    missings = missings,
    values = values,
    description = description
  )
  
  if(Dataset.globalenv$print.io) cat(" => (out) NominalVariable: builder \n")
  return(out)
}

nvar <- function(
  x,
  missings,
  values,
  description
) {
  
  if(missing(missings)) missings <- numeric(0)
  if(missing(values)) values <- numeric(0)
  if(missing(description)) description <- Dataset.globalenv$Variable.description.default
  if(missing(x)) x <- numeric(0)
  
  if (inherits(x, 'OrdinalVariable')) {
    out <- new(
      Class = "NominalVariable",
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
    
    out <- new(
      Class = "NominalVariable",
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

is.nominal <- function(x){
  if(inherits(x, "NominalVariable")){
    return(TRUE)
  } else {
    return(FALSE)
  }
}
is.nominal.root <- function(x){
  if(is.scale(x) && !inherits(x, "OrdinalVariable")){
    return(TRUE)
  } else {
    return(FALSE)
  }
}
# data(iris)
# ir <- dataset(iris)
# t <- ir$Species

setMethod("summary", "NominalVariable", 
  definition = function (object, ...) {
    summary(as.vector(object))
  }
)

