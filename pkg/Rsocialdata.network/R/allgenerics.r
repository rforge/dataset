#' Get/Set the networks matrix of an object containing network data
#' 
#' This generic method intends to extract the networks object.
#' 
#' @param object the object for which we want to get the networks matrix.
#' @param value a list of network \code{matrix}
#' @export
setGeneric("networks",
  function(object)
    standardGeneric("networks")
)

#' Set the networks matrix of an object containing network data
#' 
#' This generic method intends to set the networks in a compatible object.
#' 
#' @param object the object for which we want to set the networks matrix.
#' @param value a list of network \code{matrix}
#' @export
setGeneric("networks<-",
  function(object, value)
    standardGeneric("networks<-")
)

#' Compute network density measure
#' 
#' This generic method computes the density of a network object
#' @param x the object for which we want to compute the density
#' @param target a \code{character} the label of the valid case to use for attesting of a link (see \code{valids(your_network)} or listing valid cases).
#' @param quiet a \code{logical} if \code{TRUE} messages are disabled.
#' @return A \code{ScaleVariable} object.
#' @export
setGeneric("net.density",
  function(x, target, quiet = FALSE)
    standardGeneric("net.density")
)


#' Compute network degree centrality measure
#' 
#' This generic method computes the centrality degree of a network object
#' @param x the object for which we want to compute the centrality
#' @param which a \code{character} if set to a node name, the centrality degree of this node is computed. If set to the value 'network', the network centrality degree is computed.
#' @param direction a \code{character} if set to 'in' the in-degree centrality is computed, if set to 'out' the out-degree centrality is computed.
#' @param target a \code{character} the label of the valid case to use for attesting of a link (see \code{valids(your_network)} or listing valid cases).
#' @param quiet a \code{logical} if \code{TRUE} messages are disabled.
#' @return A \code{ScaleVariable} object.
#' @export
setGeneric("net.centrality.degree",
  function(x, target, which = "ego", direction = "in", quiet = FALSE)
    standardGeneric("net.centrality.degree")
)

# 
# #' Compute network betweenness centrality measure
# #' 
# #' This generic method computes the betweeness centrality of a network object
# #' @param x the object for which we want to compute the centrality
# #' @return A \code{ScaleVariable} object.
# #' @export
# setGeneric("net.centrality.betweenness",
#   function(x)
#     standardGeneric("net.centrality.betweenness")
# )
# 
