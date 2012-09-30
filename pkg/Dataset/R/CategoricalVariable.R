#=================================================================================================
# Class definition
#=================================================================================================


setClass(
  Class = "CategoricalVariable",
  contains = c("Variable"),
  representation = c("VIRTUAL"),
  validity = function(object) {
    if(Dataset.globalenv$print.io) cat (" =>       CategoricalVariable: object validity check \n")
  	flag = TRUE
    
    uniqueCodes <- unique(object@codes)
    missings <- object@missings
    values <- object@values
    description <- object@description
    Variable.version <- object@Variable.version

    
    if(!all(is.element(uniqueCodes, union(values, missings)))){
      print("For a categorical variable, all codes have to exist in values")
      flag <- FALSE
    }    
		return(flag)
	}
)

categoricalVariable <- function(x, values, missings, description) {
  if(Dataset.globalenv$print.io) cat(" => (in)  CategoricalVariable: virtual builder \n")
  
  matched <- FALSE
  
  if(inherits(x, 'character')) {
     x <- as.factor(x)
     matched <- TRUE
  }
     
  if(inherits(x, 'factor')) {
    matched <- TRUE
    codes <- rep.int(NA, length(x))
    allvalues <- numeric()
    
    if (length(values) > 0) { # the user specified values
      if(is.null(names(values))) names(values) <- values
    } else {
      values <- 1:nlevels(x)
      names(values) <- levels(x)
    }
    allvalues <- c(allvalues, values)
      
    if (length(missings) > 0) # the user specified missings
      if(is.null(names(missings))) names(missings) <- missings
    allvalues <- c(allvalues, missings)
      
    names <- names(allvalues)
    if(!all(levels(x) %in% names)) {
      print(levels(x))
      print(names)
      stop("Some values in 'x' don't appears in 'values'")
    } else {
      for (i in 1:length(names)){
       codes[which(x == names[i])] <- values[i]
      }
    }
  }
  
  if(inherits(x, 'numeric') || inherits(x, 'integer')) {
    matched <- TRUE
    codes <- x
  }
  
  stopifnot(matched)
  
  # we apply special treatment for variable
  variable <- variable(
    x = codes,
    missings = missings,
    values = values,
    description = description
  )
  
  # then we apply special treatment for a qualitative variable
  # (nothing)

  out <- list(
    x = variable$x,
    missings = variable$missings,
    values = variable$values,
    description = variable$description,
    Variable.version = variable$Variable.version
  )
  #print(out)
  if(Dataset.globalenv$print.io) cat(" => (out) CategoricalVariable: virtual builder \n")
  return(out)
}

cvar <- function(
  x,
  missings,
  values,
  description
) {
  
  if(missing(missings)) missings <- numeric(0)
  if(missing(values)) values <- numeric(0)
  if(missing(description)) description <- Dataset.globalenv$Variable.description.default
  if(missing(x)) x <- numeric(0)
  
  # we apply special treatment for qualitative variable
  variable <- categoricalVariable(
    x = x,
    missings = missings,
    values = values,
    description = description
  )
  
  if (length(variable$values) != 2) {
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
  } else {
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
  }
}

is.qualitative <- function(x){
  if(inherits(x, "CategoricalVariable")){
    return(TRUE)
  } else {
    return(FALSE)
  }
}


setMethod(
  f = "as.factor",
  signature = "CategoricalVariable", 
  definition = function (x) {
    out <- factor(codes(x), exclude = missings(x)) #factor with codes
    codelevels <- levels(out)
    valuelevels <- names(valids(x))[na.omit(match(codelevels, valids(x)))] #we match levels with values
    levels(out) <- valuelevels
    out <- as.character(out) # we get a character vector using the values
    
    # then we create the factor with all values as levels in the same order as given in the values slot (without missing values)
    #out <- factor(out, levels = names(validsx)), labels = names(validsx)))
    out <- factor(out, levels = names(valids(x)))
    return(out)
  }
)


setMethod(
  f = "as.vector",
  signature = "CategoricalVariable", 
  definition = function (x) {
    return(as.factor(x))
  }
)
setMethod(
  f = "v",
  signature = "CategoricalVariable", 
  definition = function (x) {
    return(as.vector(x))
  }
)

setMethod("nmissings", "CategoricalVariable", 
  definition = function (object) {
    return(length(which(is.na(as.vector(object)))))
  }
)

setMethod(
  f = "as.data.frame",
  signature = "CategoricalVariable", 
  definition = function (x) {
    return(data.frame(as.factor(x)))
  }
)

#setMethod(
#  f = "table",
#  signature = "CategoricalVariable", 
#  definition = function (object, percent) {
#    return(table(as.factor(object)))
#  }
#)

setMethod(
  f = "distrib",
  signature = "CategoricalVariable", 
  definition = function (object, weights, missings.omit, percent, sorting, format, digits, chlength, sep, cut, cut.percent) {
    out <- as.factor(object)
    
    stopifnot(length(out) == length(weights))
    
    if(missing(sep)){
      if (is.ordinal(object) && missing(sorting)) sep <- " < "
      else sep <- ", "
    }
    
    lev <- levels(out)
    out1 <- numeric(0)

    for (i in 1:nlevels(out)){
      which.in.level <- which(out == lev[i])
      out1 <- c(out1, sum(weights[which.in.level]))
    }
    out2 <- as.table(out1/sum(weights))
    names(out2) <- levels(out)
    out <- out2
    
#     if (missings.omit)
#       out <- table(out)/(length(out) - nmissings(object))
#     else
#       out <- table(out)/length(out)
#     
#     names(attr(out, "dimnames")) <- ""
#     print(out)
    
    if(!missing(cut.percent)){
      cut.1 <- cut.percent / 100
      which.cutted <- which(out < cut.1)
      if(length(which.cutted) > 0) {
        out <- out[-which.cutted]
      }
    }
    
    if(!missing(sorting)) {
      if(!is.element(sorting, c("increasing", "decreasing"))){
        stop("sorting argument must be either 'increasing' or 'decreasing'")
      } else {
        if (sorting == "increasing") out <- sort(out)
        if (sorting == "decreasing") out <- sort(out, decreasing = T)
      }
    }
    
    if(percent) out <- out * 100
    if(format) {
      if(length(out)==0) {
        if(length(which.cutted) > 0) {
          out <- '...'
        } else {  # the vector has only missings
          out <- ''
        }
      } else {
        out1 <- formatC(out, digits = digits, format = "f")
        out1 <- paste("(", out1, ")", sep = "")
        
        nam <- names(out)
        #print('nam')
        #print(nam)
        out <- mapply(substr, nam, 0, chlength)
        toolong <- which(!nam == out)
        for (i in toolong) {
          #out[i] <- paste(substr(out[i], chlength-2, chlength-1),"-", sep = "")
          substr(out[i], nchar(out[i]), nchar(out[i])) <- "-"
        }
        out <- paste(out, out1, sep = " ", collapse = sep)
        
        if(length(which.cutted) > 0) {
          out <- paste(out, ', ...', sep='')
        }
#         if(nchar(out) > cut) {
#           csum <- cumsum(as.vector(sapply(strsplit(out, sep), nchar)))
#           pos <- findInterval(cut,csum)
#           endsubstr <- csum[pos] 
#           if (length(endsubstr) == 0) out <- "..."
#           else {
#             endsubstr <- endsubstr + length(sep) * (pos - 1)
#             out <- paste(substr(out, 0, endsubstr), "...", sep = sep)
#           }
#         }
      }
    }
    return(out)
  }
)

# show
setMethod(
  f = "show",
  signature = "CategoricalVariable", 
  definition = function (object) {
    do.call(getMethod('show', 'Variable'), list(object))
    print(as.factor(object))
  }
)

# print
setMethod("print", "CategoricalVariable", 
  definition = function (x, ...) {
	show(x)
  }
)

setMethod("plot", "CategoricalVariable", 
  definition = function (x, ...) {
  plot(as.vector(x), ...)
  }
)


# as.data.frame
setMethod("as.data.frame", "CategoricalVariable", 
  definition = function (x) {
		as.data.frame(as.factor(x))
  }
)

# levels
#setMethod("levels", "CategoricalVariable", 
#  definition = function (x) {
#		levels(as.factor(x))
#  }
#)
setMethod("nvalues", "CategoricalVariable", 
  definition = function (object) {
  	length(valids(object))
  }
)


setMethod(
  f = "recode",
  signature = c("CategoricalVariable"),
  definition = function (object, ...) {

    recoding <- list(...)
    newcat <- names(recoding)
    stopifnot(length(newcat) != length(unique(newcat))) #new cat has to be unique
    
    args <- list(...)
    object.init <- object
    
    val <- valids(object)
    names <- names(recoding)
    
    if (all.is.numeric(names)) { #user gives codes
      if(!(is.null(args$keep) || (args$keep == FALSE))) {
        names <- as.numeric(names)
        if (!all(names %in% val)) stop("[Dataset::recode] all codes have to appear in values")
        else {
          names.temp <- character(0)
          for(i in 1:length(names)) {
            names.temp[i] <- value(names[i], object)
          }
          names <- names.temp
        }
      }
    }
  
    for (i in 1:length(recoding)) {
      r <- recoding[[i]]

      if (all.is.numeric(r)) { #user gives codes
        r <- as.numeric(r)
        r.temp <- character(0)
        for(j in 1:length(r)) {
          #print(r[j])
          r.temp[j] <- value(r[j], object)
        }
        r <- r.temp
      }
      
      #print(r)
      #print(names(val))
      
      #print(r %in% names(val))
      if (!all(r %in% names(val))) stop("[Dataset::recode] some recoding names doesn't exist in the variable")
      v <- match(r, names(val))
      #print(v)
      code <- val[v[1]]
      #print(code)
      # we change codes
      wcodes <- which(codes(object) %in% val[v])
      codes(object)[wcodes] <- code
      # then we change values
      names(code) <- names[i]
      val <- val[-v]
      val <- c(val,code)
      
    }
    if(length(val) < 3) {
      object <- bvar(
        x = codes(object),
        missings = missings(object),
        values = val,
        description = description(object),
        Variable.version = Variable.version(object)
      )
    } else {
      valids(object) <- val
    }
    
    if(is.null(args$silent) || (args$silent == FALSE))
      print(table(v(object.init), v(object)))
      
    return(object)
  }
)

#setMethod(
#  f = "table",
#  signature = c("CategoricalVariable"),
#  definition = function (..., exclude, useNA, dnn, deparse.level) {
#    args <- list(...)
#    # Function: table (package base)
#    # ...="ANY"
#    # exclude="CategoricalVariable"
#    # exclude="missing"
#    # (inherited from: ...="ANY")
#  }
#)

