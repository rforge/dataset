#####################################################################
## WeightingVariable class
#####################################################################


setClass(
  Class = "WeightingVariable",
  contains = c("ScaleVariable"),
  validity = function(object) {
    if(Dataset.globalenv$print.io) cat (" =>       WeightingVariable: object validity check \n")
    flag <- TRUE
    if(length(slot(object, 'Weighting')) > 0)
      stop("A WeightingVariable can't have a non-empty Weighting slot")
    
    #FIXME: no NAs
    
  	return(flag)
	}
)


#=================================================================================================
# Class initializer
#=================================================================================================

  
#=================================================================================================
# Class standard constructeur
#=================================================================================================

# x <- c(10,53,24,96,85,77,12,NA)
# y <- svar(x)
wvar <- function(
  x,
  missings,
  values,
  description,
  Weighting
) {
  if(Dataset.globalenv$print.io) cat(" => (in)  WeightingVariable: builder \n")
  if(missing(missings)) missings <- numeric(0)
  if(missing(values)) values <- numeric(0)
  if(missing(description)) description <- character(0)
  if(missing(x)) x <- numeric(0)
  Weighting <- numeric(0)
  
  # we apply special treatment for scale variable
  variable <- quantitativeVariable( #FIXME: SHOULD BE SCALE
    x = x,
    missings = missings,
    values = values,
    description = description,
    Weighting = Weighting
  )
  
  # then we apply special treatment for a Weighting variable
  # (nothing)
  
  if(Dataset.globalenv$print.io) cat(" => (out) WeightingVariable: builder \n")
  out <- new(
    Class = "WeightingVariable",
    codes = variable$x,
    missings = variable$missings,
    values = variable$values,
    description = variable$description,
    Weighting = Weighting
  )
  message(paste('number of missings:',nmissings(out))) # FIXME: REMOVE
  return(out)
}

# ww <- wvar(c(1,2,3))
# str.typevar(ww, parenthesis = T)

setMethod(
  f = "show",
  signature = "WeightingVariable", 
  definition = function (object) {
    txt.desc <- 'Description: no'
    if(length(description(object)) > 0)
      txt.desc <- paste('Description:', description(object))
      
    message(txt.desc)
	  print(as.numeric(object))
  }
)

# print
setMethod("print", "WeightingVariable", 
  definition = function (x, ...) {
	show(x)
  }
)

is.weighting <- function(x){
  if(inherits(x, "WeightingVariable")){
    return(TRUE)
  } else {
    return(FALSE)
  }
}