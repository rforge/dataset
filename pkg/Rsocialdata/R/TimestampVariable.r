#####################################################################
## TimestampVariable class
#####################################################################


setClass(
  Class = "TimestampVariable",
  contains = c("QuantitativeVariable"),
  representation(
  	origin = "character",
		format = "character"
	),
  validity = function(object) {
    if(Rsocialdata.globalenv$print.io) cat (" =>       TimestampVariable: object validity check \n")
    flag = TRUE
      
    #missingss <- object@missings
    #values <- object@values
    #description <- object@description
    
    # origine ne doit pas être vide
    # format ne doit pas être vide
    # origine doit être valide (de type dddd-dd-dd)
    if(!grepl('[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}', origin(object))){
      message("Invalid origin, please use a 'yyyy-mm-dd' format")
      flat <- FALSE
    }
    # format doit être valide
 
		return(flag)
	}
)


#=================================================================================================
# Class initializer
#=================================================================================================

  
#=================================================================================================
# Class standard constructeur
#=================================================================================================

tvar <- function(
  x,
  missings,
  values,
  description,
  origin = "1970-01-01",
  format = "%Y/%m/%d-%H:%M:%S"
) {
  if(Rsocialdata.globalenv$print.io) cat(" => (in)  TimestampVariable: builder \n")
  if(missing(missings)) missings <- numeric(0)
  if(missing(values)) values <- numeric(0)
  if(missing(description)) description <- Rsocialdata.globalenv$Variable.description.default
  if(missing(x)) x <- numeric(0)
  
  # we apply special treatment for quantitative variable
  variable <- quantitativeVariable(
    x = x,
    missings = missings,
    values = values,
    description = description
  )
  
  # then we apply special treatment for a timestamp variable
  # (nothing)
  
  if(Rsocialdata.globalenv$print.io) cat(" => (out) TimestampVariable: builder \n")
  out <- new(
    Class = "TimestampVariable",
    codes = variable$x,
    missings = variable$missings,
    values = variable$values,
    description = variable$description,
    Variable.version = variable$Variable.version,
    origin = origin,
    format = format
  )
  
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

# origin
setMethod("origin", "TimestampVariable", 
  definition = function (object) {
  	slot(object, "origin")
  }
)
setReplaceMethod(
  f = "origin" ,
	signature = "TimestampVariable" ,
	definition = function(object, value){
		object@origin <- value
    validObject(object)
		return(object)
	}
)

# format
setMethod("format", "TimestampVariable", 
  definition = function (x) { # need to be x as in base::format
		slot(x, "format")
  }
)
setReplaceMethod(
  f = "format" ,
	signature = "TimestampVariable" ,
	definition = function(object, value){
		object@format <- value
    validObject(object)
		return(object)
	}
)

setMethod(
  f = "as.numeric",
  signature = "TimestampVariable", 
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
  f = "as.character",
  signature = "TimestampVariable", 
  definition = function (x) {
    out <- format.POSIXct(as.POSIXct(as.numeric(x), origin=origin(x), tz="GMT"), format(x))
    for (i in missings(x)) {
      temp <- which(out == i)
      out[temp] <- NA
    }
    return(out)
  }
)

#setMethod(#FIXME a faire
#  f = "as.Date",
#  signature = "TimestampVariable", 
#  definition = function (x) {
#    out <- codes(x)
#    for (i in missings(x)) {
#      temp <- which(out == i)
#      out[temp] <- NA
#    }
#    return(out)
#  }
#)

setMethod(
  f = "as.data.frame",
  signature = "TimestampVariable", 
  definition = function (x) {
    return(data.frame(as.character(x)))
  }
)

setMethod(
  f = "as.vector",
  signature = "TimestampVariable", 
  definition = function (x) {
    return(as.character(x)) # as.Date instead
  }
)
setMethod(
  f = "v",
  signature = "TimestampVariable", 
  definition = function (x) {
    return(as.vector(x)) # as.Date instead
  }
)

# as.data.frame
#setMethod("as.data.frame", "TimestampVariable", 
#  definition = function (x) {
#  	as.data.frame(as.Date(x))
#  }
#)

# show
setMethod(
  f = "show",
  signature = "TimestampVariable", 
  definition = function (object) {
    do.call(getMethod('show', 'Variable'), list(object))
	  print(as.character(object))
  }
)

# print
setMethod("print", "TimestampVariable", 
  definition = function (x, ...) {
	show(x)
  }
)

is.time <- function(x){
  if(inherits(x, "TimestampVariable")){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

setMethod("nmissings", "TimestampVariable", 
  definition = function (object) {
    return(length(which(is.na(as.vector(object)))))
  }
)

setMethod("summary", "TimestampVariable", 
  definition = function (object, ...) {
    summary(as.vector(object))
  }
)


#=================================================================================================
# Stats
#=================================================================================================

setMethod("min", "TimestampVariable", 
  definition = function (x, na.rm) {
    return(format.POSIXct(as.POSIXct(min(as.numeric(x), na.rm=na.rm), origin=origin(x), tz="GMT"), format(x)))
  }
)

setMethod("max", "TimestampVariable", 
  definition = function (x, na.rm) {
    return(format.POSIXct(as.POSIXct(max(as.numeric(x), na.rm=na.rm), origin=origin(x), tz="GMT"), format(x)))
  }
)

setMethod("median", "TimestampVariable", 
  definition = function (x, na.rm) {
    return(format.POSIXct(as.POSIXct(median(as.numeric(x), na.rm=na.rm), origin=origin(x), tz="GMT"), format(x)))
  }
)

setMethod("mean", "TimestampVariable", 
  definition = function (x, na.rm) {
    return(format.POSIXct(as.POSIXct(mean(as.numeric(x), na.rm=na.rm), origin=origin(x), tz="GMT"), format(x)))
  }
)
