# gestion des missings
# on ne devrait pas affectuer un nombre, comme -1 ou autre
# car même si cette valeur n'apparait pas dans les données
# elle peut apparaitre par la suite, et transformer alors
# des valeurs de l'utilisateurs par des missings
# il faudrait utiliser d'autres symboles, par ex #1, #2, ...
# on interdirait alors à l'utilisateur d'utiliser # dans ses données
# (pour les variables numérique c'est naturel, mais pour le 
# catégoriel il faudrait l'interdire
# seulement dans ce cas le vecteur de codes numériques est
# transformé en vecteur de caractère, et la taille est exponentiellement
# plus importante, exemple ci dessous :
#s1 <- s2 <- c()
#for (i in 1:6) {
#  t <- 1:10^i
#  s1 <- c(s1, object.size(t))
#  s2 <- c(s2, object.size(as.character(t)))
#}
#plot(s2, type = 'l')
#lines(s1)
# il serait donc nécessaire que ces symboles soient des numériques
# comme le sont NaN ou Inf, mais pour cela il faut recompiler R
# je conserve donc la solution de mettre des nombres négatifs
# pour identifier les missings, mais dès que l'on modifie un
# vecteur de type Variable, si on met des valeurs qui continnent
# un code de missing, je print un message indiquant combien
# de missings ont été appellé.

#=================================================================================================
# CLASS DEFINITION FILE: Variable
#=================================================================================================


#=================================================================================================
# Class specification
#=================================================================================================
setClass(
  Class = "Variable",
	representation(
		codes = "numeric",
		missings = "numeric",
    values = "numeric",
		description = "character",
    Variable.version = "character",
		"VIRTUAL"
	),
	validity = function(object) {
    if(Dataset.globalenv$print.io) cat (" =>       Variable: object validity check \n")
		flag = TRUE
    
    codes <- object@codes
    values <- object@values
    missings <- object@missings
    description <- object@description
    Variable.version <- object@Variable.version
      
    allvalues <- c(values, missings)
    names(allvalues) <- c(names(values), names(missings))
    # print(allvalues)
    
    if(flag && length(description)==0){
      print("The description of a Variable object can't be empty")
      flag <- FALSE
    }
    if(flag && any(is.na(codes))){
      print("codes can't contain NA")
      print(allvalues)
      flag <- FALSE # unique allvalues codes
    }
    if(flag && any(is.na(allvalues))){
      print("values and missings can't contain NA")
      flag <- FALSE # unique allvalues codes
    }
    
    if(flag && length(unique(allvalues)) != length(allvalues)){
      print("values and missings have to be unique")
      print(allvalues)
      flag <- FALSE # unique allvalues codes
    }
    
    if(flag && length(allvalues) > 0){
      tmp <- names(allvalues)
      if(length(na.omit(tmp)) != length(tmp)){
        print("values and missings labels can't have NA(s)")
        print(allvalues)
        flag <- FALSE # names have to be unique
      }
      tmp <- sapply(na.omit(names(allvalues)), nchar)
      if(is.element(0, tmp)){
        print("values and missings labels can't be empty")
        print(allvalues)
        flag <- FALSE # names have to be unique
      }
    }
    
    lth <- length(Variable.version)
    if(lth > 0) {
     if(any(is.na(Variable.version)) > 0){
       stop("Variable.version can't contain NAs")
     }
     if(lth > 1) {
       stop("Variable.version length must be one")
     }
    } else {
      stop("Variable.version can't be empty")
    }
		#if (is.null(object@numericCodes)) { ## ATTENTION : il ne faut pas tester si le slot est null, car si on ne met rien dans ce slot, c'est un numeric(0) : is.null() renvoie FALSE
		return(flag)
	}
)

#=================================================================================================
# Class initializer
#=================================================================================================

# setMethod(
#   f = "initialize",
# 	signature = "Column",
# 	definition = function(.Object, codes = NULL, missings = NULL, values = NULL, label = NULL){
# 		cat(" ~~~~~ Column: initialize ~~~~~\n")
#     if(!missing(codes)) {
#   		.Object@codes <- codes
#     } else {
#       .Object@codes <- numeric(0)
#     }
#     if(!missing(missings)) {
#     	.Object@missings <- missings
#     } else {
#       .Object@missings <- numeric(0)
#     }
#     if(!missing(values)) {
#     	.Object@values <- values
#     } else {
#       .Object@values <- numeric(0)
#     }
#     if(!missing(label)) {
#     	.Object@label <- label
#     } else {
#       .Object@label <- character(0)
#     }
#     validObject(.Object)
# 	  return(.Object)
# 	}
# )
  
#=================================================================================================
# Class standard virtual builder
#=================================================================================================

variable <- function(x, values, missings, description) {
  if(Dataset.globalenv$print.io) cat(" => (in)  Variable: virtual builder \n")
  
  #print(x)
  nas <- which(sapply(x, is.na))
  if(length(nas) > 0) {
    m <- min(x, na.rm = T) # we take the minimal value appearing in codes (for scales)
    if (length(missings)>0) m <- min(min(missings, na.rm = T), m) # then we take the min value defined in missings (and not appearing in codes)
    if (length(values)>0) m <- min(min(values, na.rm = T), m) # then we take the min value defined in values (and not appearing in codes)
    m <- min(m - 1, -1) # then we set the new value at the min -1, and at least -1
    
    x[nas] <- m
    names <- names(missings)
    missings <- c(missings, m)
    missinglabel <- 'Unspecified missing'
    names(missings) <- c(names, missinglabel)
    warning(paste("One or more NA value(s) found, a missing value has been created, with code", m, "and label", missinglabel))
  }
  
#   print(values)
  if(length(values) > 0) {
    misvalidlabs <- which(nchar(names(values)) == 0)
    if (length(misvalidlabs) > 0) {
      valnames <- names(values)
      valnames[misvalidlabs] <- as.character(values[misvalidlabs])
      names(values) <- valnames
      warning("one or more value label missing, the code will be used as label")
    }
  }
#   print(values)
  
  if(length(missings) > 0 && any(is.null(names(missings)))) names(missings) <- make.names(1:length(missings))
  
  out <- list(
    x = x,
    missings = missings,
    values = values,
    description = description,
    Variable.version = Dataset.globalenv$Variable.version
  )
  #print(out)
  if(Dataset.globalenv$print.io) cat(" => (out) Variable: virtual builder \n")
  return(out)
}

#=================================================================================================
# Class getters and setters
#=================================================================================================

setMethod("codes", "Variable", 
  definition = function (object) { 
  return(slot(object, "codes"))
  }
)

setReplaceMethod(
	f = "codes" ,
	signature = "Variable" ,
	definition = function(object, value){
		object@codes <- value
    validObject(object)
		return(object)
	}
)

setMethod(
  f = "missings",
  signature = "Variable", 
  definition = function (object) { 
  return(slot(object, "missings"))
  }
)

setReplaceMethod(
	f = "missings" ,
	signature = "Variable" ,
	definition = function(object, value){
		object@missings <- value
    validObject(object)
		return(object)
	}
)

setMethod(
  f = "nmissings",
  signature = "Variable", 
  definition = function (object) { 
    return(length(slot(object, "missings")))
  }
)
setMethod(
  f = "nmissingsw",
  signature = "Variable", 
  definition = function (object, weights) {
    stopifnot(length(object) == length(weights))
    out <- which(codes(object) %in% missings(object))
    return(sum(weights[out]))
  }
)

setMethod(
  f = "valids",
  signature = "Variable", 
  definition = function (object) { 
    return(c(slot(object, "values")))
  }
)

setReplaceMethod(
  f = "valids" ,
  signature = "Variable" ,
  definition = function(object, value){
    object@values <- value
    validObject(object)
    return(object)
  }
)

setMethod(
  f = "nvalids",
  signature = "Variable", 
  definition = function (object) { 
    return(length(slot(object, "values")))
  }
)

setMethod(
  f = "values",
  signature = "Variable", 
  definition = function (object) { 
  return(c(slot(object, "missings"),slot(object, "values")))
  }
)

setMethod(
  f = "nvalues",
  signature = "Variable", 
  definition = function (object) { 
    return(length(c(slot(object, "missings"),slot(object, "values"))))
  }
)

#setMethod(
#  f = "values",
#  signature = "Variable", 
#  definition = function (object, type) {
#    if(!(type %in% c("all", "classes", "missings"))) {
#      stop("type shoud be either 'all', 'classes' or 'missings'")
#    } else {
#      if (type == 'all') return(slot(object, "values"))
#      if (type == 'classes') {
#        values <- slot(object, "values")
#        classes <- setdiff(values, missings(object))
#        return(values[match(classes, values)])
#      }
#      if (type == 'missings') {
#        values <- slot(object, "values")
#        return(values[match(missings(object), values)])
#      }
#    }
#  }
#)

setMethod(
  f = "value",
  signature = c("character", "Variable"), 
  definition = function (value, object) {
    stopifnot(length(value) == 1)
    out <- values(object)[value]
    names(out) <- NULL
    return(out)
  }
)

setMethod(
  f = "value",
  signature = c("numeric", "Variable"), 
  definition = function (value, object) {
    stopifnot(length(value) == 1)
    return(names(values(object))[which(values(object) == value)])
  }
)

setMethod(
  f = "valids.reverse",
  signature = c("Variable"), 
  definition = function (object) {
    #names <- names(values(object))
    new <- rev(valids(object))
    #names(new) <- names
    valids(object) <- new
    return(object)
  }
)

setMethod(
  f = "valids.permut",
  signature = c("Variable", "numeric", "numeric"), 
  definition = function (object, i, j) {
    val <- valids(object)
    temp <- val[i]
    temp.name <- names(val)[i]
    val[i] <- val[j]
    names(val)[i] <- names(val)[j]
    val[j] <- temp
    names(val)[j] <- temp.name
    valids(object) <- val
    return(object)
  }
)
setMethod(
  f = "valids.permut",
  signature = c("Variable", "character", "character"), 
  definition = function (object, i, j) {
    val <- valids(object)
    val.i <- val[which(names(val) == i)]
    val.j <- val[which(names(val) == j)]
    
    out <- do.call(findMethods('valids.permut', classes = c("numeric"))[[1]], list(object, val.i, val.j))
    return(out)
  }
)



setMethod(
  f = "description",
  signature = "Variable", 
  definition = function (object) { 
	return(slot(object, "description"))
  }
)

setReplaceMethod(
	f = "description" ,
	signature = "Variable" ,
	definition = function(object, value){
		object@description <- value
    validObject(object)
		return(object)
	}
)

# setMethod("weights", "Variable", 
#   definition = function (object, ...) {
#     w <- slot(object, "Variable.version")
#     l <- length(w)
#     if(l == 0) {
#       message("warning: no Variable.version defined, equiponderation is used")
#       return(rep(1, length(object)))
#     } else {
#       return(slot(object, "Variable.version"))
#     }
#   }
# )

#  
# setMethod(
#   f = "is.weighted",
#   signature = "Variable", 
#   definition = function (object) { 
#     if(length(slot(object, 'weights')) > 0) return(TRUE)
#     else return(FALSE)
#   }
# )

setMethod(
  f = "Variable.version",
  signature = "Variable", 
  definition = function (object, ...) { 
    return(slot(object, "Variable.version"))
  }
)

setMethod(
  f = "contains",
  signature = c('character', 'Variable'), 
  definition = function (keywords, data, ignore.case, and) {
    nkeys <- length(keywords)
    stopifnot(nkeys > 0)
    
    l <- list()
    for (i in 1:nkeys) {
      l[[i]] <- grepl(keywords[i], description(data), ignore.case = ignore.case)
    }
    
    l <- as.logical(l)
    if(and){
      out <- all(l)
    } else {
      out <- any(l)
    }
    return(out)
  }
)
# data(iris)
# diris <- dataset(iris)
# description(diris$Sepal.Length) <- "hello, good bye"
# description(diris$Sepal.Width) <- "hello!"
# description(diris$Species) <- "hello!"
# contains('hello', diris$Sepal.Length)
# contains(c('hello', 'good'), diris$Sepal.Length)
# contains(c('hello', 'good'), diris$Sepal.Length, and = T)
# contains(c('hello', 'good'), diris$Sepal.Width)
# contains(c('hello', 'good'), diris$Sepal.Width, and = T)

setMethod(
  f = "valid",
  signature = c("Variable"), 
  definition = function (object, percent) {
    stopifnot((percent >= 0) && (percent <= 100))
    validcases <- (length(object) - nmissings(object))/length(object) * 100
    return(validcases >= percent)
  }
)

#####################################################################
## TOOLS for editing variables objects
#####################################################################

.missing.gen.candidate.vector <- function(valids, missings){
  stopifnot(inherits(valids, 'numeric'))
  stopifnot(inherits(missings, 'numeric'))
  val <- union(valids, missings)
  
  if(length(val) == 0) {
    return(-1)
  } else {
    return(min(-1, min(val)-1))
  }
}

missing.gen.candidate <- function(x){
  stopifnot(inherits(x, 'Variable'))
  return(.missing.gen.candidate.vector(valids(x), missings(x)))
}
valid.gen.candidate <- function(x){
  stopifnot(inherits(x, 'Variable'))
  val <- values(x)
  if(length(val) == 0) {
    return(1)
  } else {
    return(max(1, max(val)+1))
  }
}

missing.add <- function(x, label, code) {
  stopifnot(inherits(x, 'Variable'))
  stopifnot(inherits(label, "character"))
  stopifnot(nchar(label)>0)
  
  if (missing(code)) {
    code <- missing.gen.candidate(x)
  } else {
    stopifnot(inherits(code, "numeric"))
    if(code %in% missings(x)) {
      stop("The 'code' supplied already exists in the current 'missing' codes")
    }
  }
  new.miss <- code
  names(new.miss) <- label
  missings(x) <- c(missings(x), new.miss)
  return(x)
}

# a <- cvar(c("a", "b"), missings = c("no answer" = 0))
# values(a)
# a <- missing.add(a, "I don't know")
# values(a)
#####################################################################
## Column LENGTH
#####################################################################

setMethod("length", "Variable", 
  definition = function (x) {
	return(length(slot(x, "codes")))
  }
)

setMethod(
  f ="[",
	signature ="Variable",
		definition = function(x,i){
      codes(x) <- codes(x)[i]
      return(x)
	}
)
setReplaceMethod(
  f ="[",
	signature =c("Variable"),
		definition = function(x,i,value){
      codes <- codes(x)
      codes[i] <- value
      codes(x) <- codes
      return(x)
	}
)

# show
setMethod(
  f = "show",
  signature = "Variable", 
  definition = function (object) {
    
#     txt.weighted <- 'Weighted: no'
#     if(is.weighted(object)) txt.weighted <- 'Weighted: yes'
    
    txt.desc <- 'Description: no'
    if(length(description(object)) > 0)
      txt.desc <- paste('Description:', description(object))
      
    message(txt.desc)
#     message(txt.weighted)
  }
)

setMethod("rename", "Variable", 
  definition = function (x, ...) {
    newnames <- list(...)
    if (length(names(newnames)) == 0) {
      stop("You have to specify names...")
    } else {
      for (i in names(newnames)) {
        temp <- newnames[[i]]
        stopifnot(inherits(temp, 'character'))
        if(length(temp) > 1) {
          stop(paste("In", temp, "you give more than one name..."))
        } else {
          if(!(i %in% names(values(x)))) {
            stop(paste(i, 'is not in the values of x'))
          } else {
            if(i %in% names(missings(x))){
              new <- missings(x)
              id <- which(names(new) == i)
              names(new)[id] <- temp
              missings(x) <- new
            } else {
              new <- valids(x)
              id <- which(names(new) == i)
              names(new)[id] <- temp
              valids(x) <- new
            }
          }
        }
      }
      return(x)
    }
  }
)



setMethod(
  "plot", 
  signature = c("Variable"), 
  definition = function (x, ...) {
    message("A Variable have to be plot inside a Dataset object.")
    message("Please use the following syntax:")
    message("plot(varname, data)")
    stop("Bad use of plot method for Variable object")
  }
)

setMethod(
  "table", 
  definition = function (..., exclude, useNA, dnn, deparse.level) {
    
    args <- list(...)
#     if(inherits(args[[1]], "Variable")) {
    
    message("the table function for Variable objects has been replaced by the two following commands:")
    message("- if you want to compute frequencies, use the frequencies() method")
    message("- if you want a contigency table, use the bivan() method")
    message("")
    message("table() has been desactivated by the Dataset package.")
    message("Please use base::table() if you want to use table on a classical R object")
    message("")
    message("If you really need to reactivate table(), type")
    message("table <- base::table")
    message("")
    stop("table() has been desactivated by the Dataset package.")
    
#     } else {
      
#     base::table(... = ..., exclude = exclude, useNA = useNA, dnn= dnn, deparse.level = deparse.level)
#     }
  }
)


setMethod(
  "plot", 
  signature = c("character"), 
  definition = function (x, data,...) {
    
    if(missing(data)){
      message("You have to provide a 'data' argument")
      stop("No Dataset provided")
    }
    
    args <- list(...)
    
    if(is.null(args$x.srt)) args$x.srt <- 0
    if(is.null(args$x.xmv)) args$x.xmv <- 0
    if(is.null(args$x.ymv)) args$x.ymv <- -2.5

    flag <- F
    
    if(inherits(data[[x]], "CategoricalVariable")) {
      flag <- T
      vali <- names(valids(data[[x]]))
      bar <- numeric(0)
      
      for (i in 1:length(vali)) {
        bar <- c(bar, sum(weights(data)[which(data[[x]] == vali[i])]))
      }
      #     original.parameters<-par()
      #     par(xaxt="n")
      b <- barplot(bar, xaxt="n")
      #     axis(1, at=seq(1, 1:length(vali), by=1), labels = FALSE)
      text(cex=1, x=b+args$x.xmv, y=args$x.ymv, labels = vali, xpd=TRUE, srt=args$x.srt)
    }
    
    if(inherits(data[[x]], "QuantitativeVariable")) {
      flag <- T
#       boxplot(v(data[[x]])*v(weights(data)))
      message("unweighted box plot (weighted version forthcoming)")
      boxplot(v(data[[x]]))
    }
    
    stopifnot(flag)
  }
)

