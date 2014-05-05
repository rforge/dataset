#=================================================================================================
# Class definition
#=================================================================================================


setClass(
  Class = "BinaryVariable",
  contains = c("CategoricalVariable", "OrderedVariable"),
  validity = function(object) {
    if(Dataset.globalenv$print.io) cat (" =>       BinaryVariable: object validity check \n")
  	flag = TRUE
  
    if (nvalids(object) != 2) {
      message("The variable must have exactly two values")
      flag <- FALSE
    }
    if (!all(valids(object) %in% c(0, 1))) {
      message("Codes have to be 0 and 1 for binary variables")
      message(paste("Codes are:", valids(object, type = "values")))
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
  description
){
  if(Dataset.globalenv$print.io) cat(" => (in)  BinaryVariable: builder \n")
    
    # then we apply special treatment for a binary variable
    #x <- variable$x
    #values <- variable$values
    m <- min(values)
    M <- max(values)
  
  # as we recode it may happen collision if a missing take value 0 or 1
  if (0 %in% missings) {
    mis <- which(missings == 0)
    codemis <- missings[mis]
    
    mintotal <- min(values, missings, 0)
    missings[mis] <- mintotal - 1
    
    x[which(x == 0)] <- mintotal - 1
  }
  if (1 %in% missings) {
    mis <- which(missings == 1)
    codemis <- missings[mis]
    
    mintotal <- min(values, missings, 0)
    missings[mis] <- mintotal - 1
    
    x[which(x == 1)] <- mintotal - 1
  }
    
    if (m != 0) {
      x[which(x == m)] <- 0
      values[which(values == m)] <- 0
#       message("the lower value must be 0 in a BinaryVariable object, a recodage has been performed")
    }
    if (M != 1) {
      x[which(x == M)] <- 1
      values[which(values == M)] <- 1
#       message("the higher value must be 1 in a BinaryVariable object, a recodage has been performed")
    }

  
#   data(iris)
#   ir <- dataset(iris)
#   a <- ir$Species
#   a1 <- as.missing('setosa', a)
#   a1 <- as.missing('versicolor', a)
  
    out <- list(
      x = x,
      missings = missings,
      values = values,
      description = description
    )
    if(Dataset.globalenv$print.io) cat(" => (out) BinaryVariable: builder \n")
    return(out)
}

bvar <- function(
  x,
  missings,
  values,
  description
) {
  if(missing(missings)) missings <- numeric(0)
  if(missing(values)) values <- numeric(0)
  if(missing(description)) description <- Dataset.globalenv$Variable.description.default
  if(missing(x)) x <- numeric(0)
  
  # we apply special treatment for categorical variable
  variable <- categoricalVariable(
    x = x,
    missings = missings,
    values = values,
    description = description
  )
  # we apply special treatment for qualitative variable
  bvariable <- binaryVariable(
    x = variable$x,
    missings = variable$missings,
    values = variable$values,
    description = variable$description
  )
  
  out <- new(
    Class = "BinaryVariable",
    codes = bvariable$x,
    missings = bvariable$missings,
    values = bvariable$values,
    description = bvariable$description,
    Variable.version = variable$Variable.version
  )
  
  if(Dataset.globalenv$print.comments <= Dataset.globalenv$important){
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