#####################################################################
## CategoricalFeature class
#####################################################################


setClass(
  Class = "ScaleVariable",
  contains = c("QuantitativeVariable"),
  validity = function(object) {
    if(Dataset.globalenv$print.io) cat (" =>       ScaleVariable: object validity check \n")
    flag <- TRUE
       
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
# t <- svar(1:10)
# u <- wvar(t)
# v <- svar(u)
svar <- function(
  x,
  missings,
  values,
  description
) {
  if(Dataset.globalenv$print.io) cat(" => (in)  ScaleVariable: builder \n")
  if(missing(missings)) missings <- numeric(0)
  if(missing(values)) values <- numeric(0)
  if(missing(description)) description <- Dataset.globalenv$Variable.description.default
  if(missing(x)) x <- numeric(0)
  
  if(inherits(x, "WeightingVariable")) {
    return(new(
      Class = "ScaleVariable",
      codes = codes(x),
      missings = missings(x),
      values = valids(x),
      description = description(x),
      Variable.version = slot(x, 'Variable.version')
    )
    )
  }
  
  # we apply special treatment for quantitative variable
  variable <- quantitativeVariable(
    x = x,
    missings = missings,
    values = values,
    description = description
  )
  
  # then we apply special treatment for a scale variable
  # (nothing), but warning, if a add something I have to add it in weightingVariable
  
  if(Dataset.globalenv$print.io) cat(" => (out) ScaleVariable: builder \n")
  out <- new(
    Class = "ScaleVariable",
    codes = variable$x,
    missings = variable$missings,
    values = variable$values,
    description = variable$description,
    Variable.version = variable$Variable.version
  )
  message(paste('number of missings:',nmissings(out), '(', round(nmissings(out)/length(out)*100,2), '%)'))
  return(out)
}


setMethod(
  f = "as.numeric",
  signature = "ScaleVariable", 
  definition = function (x) {
    out <- codes(x)
    for (i in missings(x)) {
      temp <- which(out == i)
      out[temp] <- NA
    }
    return(out)
  }
)
setMethod(
  f = "as.vector",
  signature = "ScaleVariable", 
  definition = function (x) {
    return(as.numeric(x))
  }
)
setMethod(
  f = "v",
  signature = "ScaleVariable", 
  definition = function (x) {
    return(as.vector(x))
  }
)

setMethod(
  f = "as.data.frame",
  signature = "ScaleVariable", 
  definition = function (x) {
    return(data.frame(as.numeric(x)))
  }
)

# show
setMethod(
  f = "show",
  signature = "ScaleVariable", 
  definition = function (object) {
    do.call(getMethod('show', 'Variable'), list(object))
	  print(as.numeric(object))
  }
)

# print
setMethod("print", "ScaleVariable", 
  definition = function (x, ...) {
	show(x)
  }
)

# as.data.frame
setMethod("as.data.frame", "ScaleVariable", 
  definition = function (x) {
		as.data.frame(as.numeric(x))
  }
)


#=================================================================================================
# Stats
#=================================================================================================


setMethod("min", "ScaleVariable", 
  definition = function (x, na.rm) {
	  return(min(as.numeric(x), na.rm = na.rm))
  }
)

setMethod("max", "ScaleVariable", 
  definition = function (x, na.rm) {
    return(max(as.numeric(x), na.rm = na.rm))
  }
)

setMethod("mean", "ScaleVariable", 
  definition = function (x, na.rm) {
    return(mean(as.numeric(x), na.rm = na.rm))
  }
)

setMethod("sd", "ScaleVariable", 
  definition = function (x, na.rm) {
    return(sd(as.numeric(x), na.rm = na.rm))
  }
)

setMethod("var", "ScaleVariable", 
  definition = function (x, na.rm) {
    return(sd(x, na.rm = na.rm)^2)
  }
)

is.scale <- function(x){
  if(inherits(x, "ScaleVariable")){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

is.scale.exact <- function(x){
  if(is.scale(x) && !inherits(x, "WeightingVariable")){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

setMethod("nmissings", "ScaleVariable", 
  definition = function (object) {
    return(length(which(is.na(as.vector(object)))))
  }
)

setMethod("summary", "ScaleVariable", 
  definition = function (object, ...) {
    return(summary(as.vector(object)))
  }
)