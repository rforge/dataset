#' @describeIn net.density method for \code{NetworkVariable} objects. The density is computed as the ratio of the number of existing connections over the total number of possible connections. The number of existing connections is computed as the number of occurences of the value '1' in the network. The total number of possible connections is computed as (number of row)*(number of rows - 1).
setMethod(
  f = 'net.density',
  signature = 'NetworkVariable',
  definition = function(x, target, quiet) {
    if(!quiet) {
      message(paste("Target:", target))
    }
    
    stopifnot(target %in% names(valids(x)))
    target_num = valids(x)[which(names(valids(x)) == target)]
    
    n <- length(x@networks)
    density <- numeric(0)
    for (i in 1:n) {
      net <- x@networks[[i]]
      if(is.null(dim(net))) {
        density <- c(density, NA)
      } else {
        con.possible <- dim(net)[1] * (dim(net)[1] - 1)
        if (con.possible == 0) {
          density <- c(density, 0)
        } else {
          con.exist <- length(which(as.vector(net) == target_num))
          density <- c(density, con.exist/con.possible)
        }
      }
    }
    out <- svar(
      x = density,
      description = paste0("Network density of the network variable: ", description(x))
    )
    return(out)
  }
)



#' @describeIn net.centrality.degree for \code{NetworkVariable} objects. 
setMethod(
  f = 'net.centrality.degree',
  signature = 'NetworkVariable',
  definition = function(x, target, which, direction, quiet) {
    if(!quiet) {
      message(paste("Which:", which))
      message(paste("Direction:", direction))
      message(paste("Target:", target))
    }
    
    stopifnot(which %in% c('ego', 'network'))
    stopifnot(direction %in% c('in','out'))
    stopifnot(target %in% names(valids(x)))
    target_num = valids(x)[which(names(valids(x)) == target)]
    
    n <- length(x@networks)
    out <- rep(NaN,n)
    
    nets <- x@networks
    if(direction == 'out') nets <- lapply(nets, t)
    
    for (i in 1:n) {
      net <- nets[[i]]
      if(is.null(dim(net))) {
        out[[i]] <- NA
      } else {
        # if 'ego'
        j <- which(dimnames(net)[[1]] == "ego")
        indiv <- length(which(net[j,] == target_num))
        total <- dim(net)[1]-1
        out[[i]] <- indiv/total
        # if 'network'
      }
    }
    out <- svar(
      x = out,
      description = paste0("Centrality ", direction, "-degree on ", which, " of the network variable: ", description(x))
    )
    return(out)
  }
)