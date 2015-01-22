#' Get/Set the networks matrix of an object containing network data
#' 
#' This generic method intends to extract the networks object.
#' 
#' @param object the \code{NetworkVariable} object for which we want to get the networks matrix.
#' @export
setGeneric("networks",
  function(object)
    standardGeneric("networks")
)

#' Set the networks matrix of an object containing network data
#' 
#' This generic method intends to set the networks object.
#' 
#' @param object the \code{NetworkVariable} object for which we want to set the networks matrix.
#' @param value a list of network \code{matrix}
#' @export
setGeneric("networks<-",
  function(object, value)
    standardGeneric("networks<-")
)