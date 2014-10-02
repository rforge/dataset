#=================================================================================================
# Class definition
#=================================================================================================


setClass(
  Class = "OrdinalVariable",
  contains = c("NominalVariable", "OrderedVariable"),
  validity = function(object) {
    if(Rsocialdata.globalenv$print.io) cat (" =>       OrdinalVariable: object validity check \n")
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
  if(Rsocialdata.globalenv$print.io) cat(" => (in)  OrdinalVariable: builder \n")
  
  if (inherits(x, 'NominalVariable')) {
    if(missing(missings)) {
      missings <- Rsocialdata::missings(x) 
      
    }
    if(missing(values)) {
      valids <- Rsocialdata::valids(x) 
    }
    if(missing(description)) {
      description <- Rsocialdata::description(x) 
    }
    out <- new(
      Class = "OrdinalVariable",
      codes = codes(x),
      missings = missings,
      values = valids,
      description = description,
      Variable.version = slot(x, 'Variable.version')
    )
  } else {
  
    if(missing(missings)) missings <- numeric(0)
    if(missing(values)) values <- numeric(0)
    if(missing(description)) description <- Rsocialdata.globalenv$Variable.description.default
    if(missing(x)) x <- numeric(0)
    
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
    
    if(Rsocialdata.globalenv$print.io) cat(" => (out) OrdinalVariable: builder \n")
    out <- new(
      Class = "OrdinalVariable",
      codes = nvariable$x,
      missings = nvariable$missings,
      values = nvariable$values,
      description = nvariable$description,
      Variable.version = variable$Variable.version
    )
  }
  
  if(Rsocialdata.globalenv$print.comments <= Rsocialdata.globalenv$important){
    message(paste(
      'number of missings:',
      nmissings(out),
      '(',
      round(nmissings(out)/length(out)*100,2),
      '%)'
    ))
  }
  
  return(out)
}

is.ordinal <- function(x){
  if(inherits(x, "OrdinalVariable")){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

valids.ordering <- function(x) {
  if(!inherits(x, 'OrdinalVariable')) {
    message("The object is not an ordinal variable.")
    message("You can use 'ovar' to coerce your object.")
#     stop("Class of 'x' parameter ")
  } else {
    out <- paste(names(valids(x)), collapse = ' < ')
    return(out)
  }
}