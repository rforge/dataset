

setClass(
  Class = "QuantitativeVariable",
  contains = c("Variable"),
  representation = c("VIRTUAL"),
  validity = function(object) {
    if(Dataset.globalenv$print.io) cat (" =>       QuantitativeVariable: object validity check \n")
  	flag = TRUE
    
		return(flag)
	}
)


quantitativeVariable <- function(x, values, missings, description) {
  if(Dataset.globalenv$print.io) cat(" => (in)  QuantitativeVariable: virtual builder \n")
  
  if(inherits(x, 'factor')) stop("x can't be a factor")
  
  # we apply special treatment for variable
  variable <- variable(
    x = x,
    missings = missings,
    values = values,
    description = description
  )
  
  # then we apply special treatment for a quantitative variable
  # (nothing)

  
  out <- list(
    x = variable$x,
    missings = variable$missings,
    values = variable$values,
    description = variable$description,
    Variable.version = variable$Variable.version
  )
  #print(out)
  if(Dataset.globalenv$print.io) cat(" => (out) QuantitativeVariable: virtual builder \n")
  return(out)
}

is.quantitative <- function(x){
  if(inherits(x, "QuantitativeVariable")){
    return(TRUE)
  } else {
    return(FALSE)
  }
}




setMethod(
  f = "cut", 
  signature = "QuantitativeVariable", 
  definition = function (x, ...) {
    dots <- list(...)
    
    quiet = F
    if('quiet' %in% names(dots)) {
      if(dots$quiet) quiet <- T
    }
    
    if(hasArg('breaks')) {
      dots$breaks <- unique(c(min(x, na.rm=T), dots$breaks, max(x, na.rm=T)))
    }
    if('include.lowest' %in% names(dots)) {
      out <- do.call(cut, c(list('x'=as.vector(x)), dots))
    } else {
#       out <- cut(as.vector(x), include.lowest=T, ... = dots)
      out <- do.call(cut, c(list('x'=as.vector(x), 'include.lowest'=T), dots))
    }
    
    valids.names <- levels(out)
    min.valid.code <- 1 
    # we want first valid case start at least at 1
    if(nmissings(x) > 0) {
      min.valid.code <- max(max(missings(x)),min.valid.code)
    }
    valids <- min.valid.code:(min.valid.code+nlevels(out)-1)
#     valids <- 1:nlevels(out)
    names(valids) <- valids.names
    
    out <- as.numeric(out) ## FIXME pb missing collision ?
    
    
    diff.min.code <- min(out, na.rm=T) - min.valid.code
#     print(diff.min.code)
#     print(out[1:20])
    out <- out - diff.min.code
#     print(out[1:20])
#     print(valids)

    if (length(valids) == 2) {
      for (i in missings(x)){ # we refill missing values
        out[which(codes(x) == i)] <- i
      }
      out <- bvar(out, missings=missings(x), values = valids, description = paste(description(x),'- cutted'))
    } else {
      for (i in missings(x)){ # we refill missing values
        out[which(codes(x) == i)] <- i
      }
      out <- ovar(out, missings=missings(x), values = valids, description = paste(description(x),'- cutted'))
    }
    
    if(!quiet)
      print(base::table(v(x), v(out)))
    
    nmissings.before <- nmissings(x)
    nmissings.after <- nmissings(out)
    if(nmissings.before != nmissings.after) {
#       message('Sorry, a problem occurs, data consistency lost. Please report this bug to the package maintainer.')
      message('Sorry, a problem occurs, data consistency lost. Please check bounds.')
      message(paste('nmissings.before', nmissings.before))
      message(paste('nmissings.after', nmissings.after))
      stop()
    }
    return(out)
  }
)


setMethod("Compare", signature(e1="QuantitativeVariable", e2="character"),
          function(e1, e2) {
            return(callGeneric(as.vector(e1), e2))
          }
)
setMethod("Compare", signature(e1="character", e2="QuantitativeVariable"),
          function(e1, e2) {
            return(callGeneric(e1, as.vector(e2)))
          }
)

setMethod("Arith", signature(e1="QuantitativeVariable", e2="QuantitativeVariable"),
  function(e1, e2) {
  
  # we have to perform the operation only on valid cases, not on missings
  # then we have to test that we did't create collisions with missings
  
    nmissings.before <- nmissings(e2)
    codes <- e2@codes
    missings.id <- which(is.na(as.vector(e2)))
    if(length(missings.id) == 0) {
      e2@codes=callGeneric(e1, codes(e2))
    } else {
      codes[-missings.id] <- callGeneric(codes(e2)[-missings.id], e1)
      e2@codes <- codes
    }
    validObject(e2)
    nmissings.after <- nmissings(e2)
    
    if(nmissings.before != nmissings.after) {
      message(Dataset.globalenv$message.missing.collision)
      message(paste('nmissings.before', nmissings.before))
      message(paste('nmissings.after', nmissings.after))
      stop("Unable to secure data.")
    }
    
    return(e2)
  }
)
setMethod("Arith", signature(e1="QuantitativeVariable", e2="numeric"),
  function(e1, e2) {
    nmissings.before <- nmissings(e2)
    codes <- e2@codes
    missings.id <- which(is.na(as.vector(e2)))
    if(length(missings.id) == 0) {
      e2@codes=callGeneric(e1, codes(e2))
    } else {
      codes[-missings.id] <- callGeneric(codes(e2)[-missings.id], e1)
      e2@codes <- codes
    }
    validObject(e2)
    nmissings.after <- nmissings(e2)
    
    if(nmissings.before != nmissings.after) {
      message(Dataset.globalenv$message.missing.collision)
      message(paste('nmissings.before', nmissings.before))
      message(paste('nmissings.after', nmissings.after))
      stop("Unable to secure data.")
    }
    
    return(e2)
  }
)

setMethod("Arith", signature(e1="numeric", e2="QuantitativeVariable"),
  function(e1, e2) {
    nmissings.before <- nmissings(e2)
    codes <- e2@codes
    missings.id <- which(is.na(as.vector(e2)))
    if(length(missings.id) == 0) {
      e2@codes=callGeneric(e1, codes(e2))
    } else {
      codes[-missings.id] <- callGeneric(e1, codes(e2)[-missings.id])
      e2@codes <- codes
    }
    validObject(e2)
    nmissings.after <- nmissings(e2)
    
    if(nmissings.before != nmissings.after) {
      message(Dataset.globalenv$message.missing.collision)
      message(paste('nmissings.before', nmissings.before))
      message(paste('nmissings.after', nmissings.after))
      stop("Unable to secure data.")
    }
    
    return(e2)
  }
)

setMethod(
  f ="sum",
  signature =c("QuantitativeVariable"),
  definition = function(x, na.rm=FALSE){
    return(sum(as.numeric(x), na.rm=na.rm))
  }
)

setMethod(
  f = "frequencies",
  signature = "QuantitativeVariable", 
  definition = function (x, data, sort, sort.ordinal, ...) {
    
    unique.val <- na.omit(unique(as.numeric(x)))
    n.unique <- length(unique.val)
    
    dots <- list(...)
    if('breaks.max' %in% names(dots)) {
      n.cut.max <- breaks.max
    } else {
      n.cut.max <- 10 # to have 2^3 range
    }
    
    n.cut <- min(n.unique-1, n.cut.max)
    
    if(n.unique == 0) {
      out <- cvar( # FIXME doesn't work
        x = codes(x),
        missings = missings(x),
        values = unique.val,
        description = description(x)
      )
    }
    if(n.unique == 1) {
      names(unique.val) <- paste('{', unique.val, '}', sep='')
      out <- cvar(
        x = codes(x),
        missings = missings(x),
        values = unique.val,
        description = description(x)
      )
    }
    if(n.unique == 2) {
      names(unique.val) <- c(
#         paste('[', unique.val[1], ',', unique.val[1], ']', sep=''),
#         paste('(', unique.val[1], ',', unique.val[2], ']', sep='')
          paste('{', unique.val[1], '}', sep=''),
          paste('{', unique.val[2], '}', sep='')
      )
      out <- cvar(
        x = codes(x),
        missings = missings(x),
        values = unique.val,
        description = description(x)
      )
    }
    if(n.unique > 2) {
#       breaks <- unique.val[-c(min(unique.val), max(unique.val))]
      breaks <- seq(min(unique.val), max(unique.val), length.out = n.cut+1)
#       if(svar.is.integer(x)){
#         breaks <- unique(round(breaks))
#       }
#       print(breaks)
#       unique.val <- sort(unique.val, decreasing = FALSE)
      out <- cut(
        x,
        breaks = breaks,
        quiet = T
#         breaks = unique.val[-c(min(unique.val), max(unique.val))]
      )
    }
    
    l <- list(x = out)
    if('weights' %in% names(dots)) {
      l$weights <- dots$weights
    }
    
    out <- do.call(getMethod('frequencies', 'CategoricalVariable'), l)
    return(out)
  }
)

svar.is.integer <- function(x){
  unique.val <- na.omit(unique(as.numeric(x)))
  unique.val.rounded <- round(unique.val)
  union <- union(unique.val, unique.val.rounded)
  return(length(union) == length(unique.val.rounded))
}
# svar.is.integer(svar(c(1,2,3.1)))
# b <- svar(c(NA,NA,NA)); frequencies(b)
# b <- svar(c(1,1,1)); frequencies(b)
# b <- svar(c(1,1,1,NA)); frequencies(b)
# b <- svar(c(1,1,1,2,2,2,2,NA)); frequencies(b)
# b <- svar(c(1,1,1,2,2,2,2,3,NA)); frequencies(b)
# b <- svar(c(1,1,1,2,2,2,2,3,4,NA)); frequencies(b)
# b <- svar(c(1,1,1,2,2,2,2,3,4,5,NA)); frequencies(b)
# b <- svar(c(1,2,3,4,5,NA)); frequencies(b)
# b <- svar(c(1,2,3.2,4,5,NA)); frequencies(b)
# b <- svar(c(1,2,3,4,5,6,7,8,9,10,NA)); frequencies(b)
# b <- svar(c(1,2,3,4,5,6.2,7,8,9,10,NA)); frequencies(b)
# b <- svar(c(1,2,3,4,5,6.2,7,8,9,10,11,12,13,14,15,16,17,18,19,20,NA)); frequencies(b)
# b <- svar(c(1.01,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,NA)); frequencies(b)
# b <- svar(c(10,10,10,80,80,90,90,100,100,100,NA)); frequencies(b)
# b <- svar(c(10,10,10,80,80.4,90,90,NA)); frequencies(b)
# b <- svar(c(1,1,1,23,23,NA,31,31,31,31,31.2,4,4,5,6,7,8,9)); frequencies(b)