#=================================================================================================
# CLASS DEFINITION FILE: Rsocialdata
#=================================================================================================


setClass(
  'Rsocialdata',
	representation(
		name = 'character',
		description = 'character',
		variables = 'list',
		row.names = 'character',
    weights = 'character',
    checkvars = 'character',
    spatial = 'list',
    Rsocialdata.version = 'character',
    infos = 'list'
	),
	validity = function(object) {
		if(Rsocialdata.globalenv$print.io) cat (" =>       Rsocialdata: object validity check \n")
		flag = TRUE
    n <- length(row.names(object))
    
		if(flag && any(is.na(row.names(object)))) {
		  message("row.names argument can't contain NA")
      print(row.names(object))
		  flag <- FALSE
		}

		if(flag && (n != length(unique(row.names(object))))) {
		  message("row.names have to be unique")
		  print(row.names(object))
		  flag <- FALSE
		}
    
    lw <- length(slot(object, 'weights'))
    if(flag && lw > 1){
      message("Only one weighting variable name is expected")
      flag <- FALSE
    }
    if(flag && lw == 1){
      if(!is.element(weighting(object), names(object))){
        message("The weighting variable name given is not in the dataset")
        flag <- FALSE
      }
      if(!is.weighting(object[[varid(weighting(object), object)]])){
        message("The weighting variable must be a WeightingVariable object")
        flag <- FALSE
      }
    }
    
    if (flag && length(checkvars(object)) > 0) {
      if(flag && !all(checkvars(object) %in% names(object))) {
        message("All checkvars variables have to exist in the Rsocialdata")
        flag <- FALSE
      }
#       if(flag && !all(mapply('inherits', variables(object[,checkvars(object)]), 'CategoricalVariable'))) {
#         message("All checkvars variables have to be categorical")
#         flag <- FALSE
#       }
      if(flag) {
        for (i in checkvars(object)){ # don't use a mapply here, loop in subscript with checkvars
          if(!inherits(object[[i]], 'CategoricalVariable')) {
            message("All checkvars variables have to be categorical")
            flag <- FALSE
            break
          }
        }
      }
    }
    
		version <- object@Rsocialdata.version
		lth <- length(version)
		if(flag && lth > 0) {
		  if(any(is.na(version)) > 0){
		    message("Rsocialdata.version can't contain NAs")
		    flag <- FALSE
		  }
		  if(lth > 1) {
		    message("Rsocialdata.version length must be one")
		    flag <- FALSE
		  }
		} else {
		  message("Rsocialdata.version can't be empty")
		  flag <- FALSE
		}
    
		for (v in variables(object)) {
			if (flag && !inherits(v, "Variable")) {
				message(paste("One or more variable is a", class(v), "object. It should be a Variable object"))
				flag <- FALSE
        break
			}
			# tester que toutes les variables ont le même nombre d'individus
      if (n != length(v)){
        message(paste("All variables must have the same length"))
        message(paste("length of row.names:", n))
        message(paste("length of variable ", description(v), ":", length(v)))
        flag <- FALSE
        break
      }

      
  		if (flag && length(unique(names(object@variables))) != length(names(object@variables))) {
  			message("names of column have to be unique")
  			message(paste("names are : ", paste(names(object@variables), collapse=', '), sep=""))
  			flag = FALSE
        break
  		}
  		
  	}
	return(flag)
  }
)

dataset <- function(
  x,
  name,
  description,
  row.names,
  weights,
  checkvars,
  spatial,
  infos = list(),
  db.author = "",
  db.manager = "",
  db.contact.email = "",
  db.license = "",
  db.release.date = "",
  db.citation = "",
  db.website = "",
  db.details = "",
  check.rows = FALSE # not used
) {
  if(Rsocialdata.globalenv$print.io) cat(" => (in)  Rsocialdata: builder \n")
  
  ptm <- Sys.time()
  
  warn.user <- options(warn = -1) # suppress warnings
  on.exit(options(warn.user)) # restore warnings
  
  print.comments.user <- Rsocialdata.globalenv$print.comments
  if(print.comments.user < Rsocialdata.globalenv$monitoring) {
    Rsocialdata.globalenv$print.comments <- Rsocialdata.globalenv$monitoring
  }
  
  if (missing(x)) x <- list()
  if (missing(name)) name <- character()
  if (missing(description)) description <- character()
  #if (missing(row.names)) row.names <- character()
  if (missing(weights)) weights <- character()
  if (missing(checkvars)) checkvars <- character()
  if (missing(spatial)) spatial <- list()
  if (missing(infos)) infos <- list()
  
  infos[["db.author"]] <- db.author
  infos[["db.manager"]] <- db.manager
  infos[["db.contact.email"]] <- db.contact.email
  infos[["db.license"]] <- db.license
  infos[["db.release.date"]] <- db.release.date
  infos[["db.citation"]] <- db.citation
  infos[["db.website"]] <- db.website
  infos[["db.details"]] <- db.details
  
  if (inherits(x, 'data.frame')) {
    cons.counter <- cons.counter.new(
      txt.before = 'Importing variables... ',
      txt.after = paste('/', ncol(x), sep='')
    )
    variables <- list()
    for (i in 1:ncol(x)) {
      
      if(Rsocialdata.globalenv$print.comments <= Rsocialdata.globalenv$monitoring){
        cons.counter <- cons.counter.add.one(cons.counter)
        cons.counter.print(cons.counter)
        
        #   	    message(v, ' : ', appendLF=F)
        #   	    flag <- F
        #   	    
        #   	    message(paste("Loading", v))
      }
      
      match <- FALSE
      if(inherits(x[[i]], 'character')) {
        variables[[i]] <- cvar(x[[i]])
        match <- TRUE
      }
      if(inherits(x[[i]], 'factor')){
        variables[[i]] <- cvar(x[[i]])
        match <- TRUE
        if(is.ordered(x[[i]]) && is.nominal(variables[[i]])) { # could be binary
          variables[[i]] <- ovar(variables[[i]])
        }
      }
      if(inherits(x[[i]], 'numeric') || inherits(x[[i]], 'integer')) {
        variables[[i]] <- svar(x[[i]])
        match <- TRUE
      }
      if(!match) {
        stop(paste("Rsocialdata::dataset - variable", i, "didn't match"))
      }
      
      if(Rsocialdata.globalenv$print.comments <= Rsocialdata.globalenv$monitoring){
        cons.counter.print.finish(cons.counter)
      }
      
    }
    if(Rsocialdata.globalenv$print.comments <= Rsocialdata.globalenv$monitoring){
      duration <- Sys.time() - ptm
      message(paste("Duration:", format(duration)))
    }
    names(variables) <- names(x)
  } else {
    variables <- x #FIXME: more tests to do
  }
  # return(variables)
  
  if (missing(row.names)){
    if (length(variables) > 0) {
      row.names <- as.character(1:length(variables[[1]]))
    } else {
      row.names <- character(0)
    }
  }
  if(is.null(names(variables))) {
    if(length(variables) > 0) {
      names(variables) <- 1:length(variables)
    } 
  }
    
#   nas <- which(is.na(names(variables)))
#   print(nas)
#   if(length(nas) > 0) {
#     print("warning: NA values was found in variable names. They was ignored")
#     names(variables) <- na.omit(names(variables))
#   }
#   names(variables) <- unique(names(variables))
  
#   names(variables) <- unique(names(variables))
  
  ## FIXME with shp.merge of AnCat, nothing works but this
  names(variables) <- make.names(names(variables), unique = T)
  
  
  Rsocialdata.globalenv$print.comments <- print.comments.user
  
  
  
  if(Rsocialdata.globalenv$print.io) cat(" => (out) Rsocialdata: builder \n")
  
  # on nvague1to3 NULL appear...
  var.null <- which(as.logical(lapply(variables, is.null)))
  if(length(var.null) > 0) {
    message("Sorry, but importing variables failed... Please find in output a list containing all the variables imported. Some of them are NULL. Try to fix in your data.frame what could be the problem, then run this function again.")
    return(variables)
  } else {
  
    return(new(
      Class = "Rsocialdata",
      name = name,
      variables = variables,
      description = description,
      row.names = row.names,
      weights = weights,
      checkvars = checkvars,
      spatial = spatial,
      Rsocialdata.version = Rsocialdata.globalenv$Rsocialdata.version,
      infos = infos
    ))
  }
  
}


setMethod("name", "Rsocialdata", 
  definition = function (object) { 
  return(slot(object, "name"))
  }
)
setReplaceMethod(
	f = "name" ,
	signature = "Rsocialdata" ,
	definition = function(object, value){
		object@name <- value
    validObject(object)
		return(object)
	}
)

setMethod("description", "Rsocialdata", 
  definition = function (object) { 
  return(slot(object, "description"))
  }
)
setReplaceMethod(
	f = "description" ,
	signature = "Rsocialdata" ,
	definition = function(object, value){
		object@description <- value
    validObject(object)
		return(object)
	}
)

setMethod("db.author", "Rsocialdata", 
          definition = function (object) { 
            return(slot(object, "infos")[["db.author"]])
          }
)
setReplaceMethod(
  f = "db.author" ,
  signature = "Rsocialdata" ,
  definition = function(object, value){
    object@infos[["db.author"]] <- value
    validObject(object)
    return(object)
  }
)

setMethod("db.manager", "Rsocialdata", 
          definition = function (object) { 
            return(slot(object, "infos")[["db.manager"]])
          }
)
setReplaceMethod(
  f = "db.manager" ,
  signature = "Rsocialdata" ,
  definition = function(object, value){
    object@infos[["db.manager"]] <- value
    validObject(object)
    return(object)
  }
)

setMethod("db.contact.email", "Rsocialdata", 
          definition = function (object) { 
            return(slot(object, "infos")[["db.contact.email"]])
          }
)
setReplaceMethod(
  f = "db.contact.email" ,
  signature = "Rsocialdata" ,
  definition = function(object, value){
    object@infos[["db.contact.email"]] <- value
    validObject(object)
    return(object)
  }
)

setMethod("db.license", "Rsocialdata", 
          definition = function (object) { 
            return(slot(object, "infos")[["db.license"]])
          }
)
setReplaceMethod(
  f = "db.license" ,
  signature = "Rsocialdata" ,
  definition = function(object, value){
    object@infos[["db.license"]] <- value
    validObject(object)
    return(object)
  }
)

setMethod("db.release.date", "Rsocialdata", 
          definition = function (object) { 
            return(slot(object, "infos")[["db.release.date"]])
          }
)
setReplaceMethod(
  f = "db.release.date" ,
  signature = "Rsocialdata" ,
  definition = function(object, value){
    object@infos[["db.release.date"]] <- value
    validObject(object)
    return(object)
  }
)

setMethod("db.citation", "Rsocialdata", 
          definition = function (object) { 
            return(slot(object, "infos")[["db.citation"]])
          }
)
setReplaceMethod(
  f = "db.citation" ,
  signature = "Rsocialdata" ,
  definition = function(object, value){
    object@infos[["db.citation"]] <- value
    validObject(object)
    return(object)
  }
)

setMethod("db.website", "Rsocialdata", 
          definition = function (object) { 
            return(slot(object, "infos")[["db.website"]])
          }
)
setReplaceMethod(
  f = "db.website" ,
  signature = "Rsocialdata" ,
  definition = function(object, value){
    object@infos[["db.website"]] <- value
    validObject(object)
    return(object)
  }
)

setMethod("db.details", "Rsocialdata", 
          definition = function (object) { 
            return(slot(object, "infos")[["db.details"]])
          }
)
setReplaceMethod(
  f = "db.details" ,
  signature = "Rsocialdata" ,
  definition = function(object, value){
    object@infos[["db.details"]] <- value
    validObject(object)
    return(object)
  }
)


setMethod("nroww", "Rsocialdata", 
          definition = function (object) { 
            return(sum(weights(object)))
          }
)
setMethod("nindividual", "Rsocialdata", 
          definition = function (object) { 
            return(sum(weights(object)))
          }
)

setMethod("checkvars", "Rsocialdata", 
  definition = function (object) { 
    #if(length(slot(object,'checkvars')) == 0) {
    #  message("no checkvars variables are defined, so all are used")
    #  return(svar(rep(1, nrow(object))))
    #} else {
      #return(variables(object)[slot(object, 'checkvars')])
      return(slot(object, 'checkvars'))
    #}
  }
)
setReplaceMethod(
  f = "checkvars" ,
	signature = "Rsocialdata" ,
	definition = function(object, value){
		object@checkvars <- value
    validObject(object)
		return(object)
	}
)

setMethod("spatial", "Rsocialdata", 
          definition = function (object) { 
#             return(slot(object, "spatial"))
            return("") # change in sumtopdf
          }
)

setMethod("spatial.country", "Rsocialdata", 
          definition = function (object) { 
            return(slot(object, "spatial")[["country"]])
          }
)
setReplaceMethod(
  f = "spatial.country" ,
  signature = "Rsocialdata" ,
  definition = function(object, value){
    object@spatial[["country"]] <- value
    validObject(object)
    return(object)
  }
)
setMethod("spatial.variable", "Rsocialdata", 
          definition = function (object) { 
            return(slot(object, "spatial")[["variable"]])
          }
)
setReplaceMethod(
  f = "spatial.variable" ,
  signature = "Rsocialdata" ,
  definition = function(object, value){
    object@spatial[["variable"]] <- value
    validObject(object)
    return(object)
  }
)

setMethod(
  f = "Rsocialdata.version",
  signature = "Rsocialdata", 
  definition = function (object, ...) { 
    return(slot(object, "Rsocialdata.version"))
  }
)

setMethod("alldescriptions", "Rsocialdata", 
  definition = function (object) {
    out <- mapply(description, variables(object))
    # if one (or more or all) description is empty, a list is returned, else named character vector, because it is impossible in R to store a named vector with '' value
    out <- data.frame(out)
    if(ncol(out) > 0) {
      row.names(out) <- names(object)
      names(out) <- 'Description'
    }
    return(out)
  }
)

setMethod("allvalues", "Rsocialdata", 
  definition = function (object) {
    valids <- mapply(valids, variables(object))
    missings <- mapply(missings, variables(object))
    
    nvar <- length(valids)
    
    out <- vector("list", nvar)
    for(i in 1:nvar) {
      out[[i]] <- list(
        'valids' = valids[[i]],
        'missings' = missings[[i]]
      )            
    }
    names(out) <- names(object)
    return(out)
  }
)

allvalids <- function(x) {
  stopifnot(inherits(x, 'Rsocialdata')) 
  valids <- mapply(valids, variables(x))
  return(valids)
}
allmissings <- function(x) {
  stopifnot(inherits(x, 'Rsocialdata')) 
  valids <- mapply(missings, variables(x))
  return(valids)
}

setMethod("variables", "Rsocialdata", 
  definition = function (object) {
    return(slot(object, "variables"))
  }
)
setReplaceMethod(
  f = "variables" ,
	signature = "Rsocialdata" ,
	definition = function(object, value){
		object@variables <- value
    validObject(object)
		return(object)
	}
)

setMethod("row.names", "Rsocialdata", 
  definition = function (x) { 
  return(slot(x, "row.names"))
  }
)
setReplaceMethod(
  f = "row.names" ,
  signature = "Rsocialdata" ,
	definition = function(x, value){
		x@row.names <- value
    validObject(x)
		return(x)
	}
)

setMethod("weights", "Rsocialdata", 
  definition = function (object, ...) {
    if(!is.weighted(object)) {
      message("warning: no weights defined, equiponderation is used")
      return(svar(rep(1, nrow(object))))
    } else {
      return(variables(object)[[slot(object, 'weights')]])
    }
  }
)


#' Get the non-response weights of a \code{Rsocialdata} database
#' 
#' This generic method intends to extract the non-response weights of a \code{Rsocialdata} database.
#' 
#' @param object the \code{Rsocialdata} object for which we want to get the map.
#' @export
setGeneric("weighting", function(object){ standardGeneric("weighting" ) })

#' @describeIn weighting method for \code{Rsocialdata} objects.
setMethod("weighting", "Rsocialdata", 
          definition = function (object) {
    return(slot(object, 'weights'))
  }
)

#' Set the non-response weights of a \code{Rsocialdata} database
#' 
#' This generic method intends to set or replace the non-response weights of a \code{Rsocialdata} database. Weights has to be defined a \code{\link{WeightingVariable}} object.
#' 
#' @param object the \code{Rsocialdata} object for which we want to set the map.
#' @param value a character, the name of the variable to use as non-response weights.
#' @export
setGeneric("weighting<-", function(object, value){ standardGeneric("weighting<-" ) })

#' @describeIn weighting method for \code{Rsocialdata} objects.
setMethod(
  f = "weighting<-" ,
  signature = c("Rsocialdata", "character") ,
  definition = function(object, value){
    if(length(value) == 0) {
      object@weights <- value
    } else {
      if(is.null(value) || nchar(value) == 0) {
        object@weights <- character(0)
      } else {
        if(is.weighting(object[[value]])) {
          object@weights <- value
        } else {
          message("The argument given isn't a WeightingVariable object")
    #       message("I'll try to perform a conversion...", appendLF=F)
    #       print(deparse(substitute(object)))
    #       .GlobalEnv$object[[value]] <- wvar(object[[value]])
    #       message("success!")
        }
      }
    }
    validObject(object)
		return(object)
	}
)
# weighting(ir) <- 'Sepal.Length'
# head(ir)

setReplaceMethod(
  f = "weighting" ,
  signature = c("Rsocialdata", "WeightingVariable") ,
  definition = function(object, value){
  	object@weights <- value
    validObject(object)
		return(object)
	}
)

setMethod(
  f = "is.weighted",
  signature = "Rsocialdata", 
  definition = function (object) { 
    if(length(slot(object, 'weights')) > 0) return(TRUE)
    else return(FALSE)
  }
)

setMethod("infos", "Rsocialdata", 
  definition = function (object) { 
  return(slot(object, "infos"))
  }
)
setReplaceMethod(
  f = "infos" ,
  signature = "Rsocialdata" ,
	definition = function(object, value){
		object@infos <- value
    validObject(object)
		return(object)
	}
)

setMethod(
  "names",
	"Rsocialdata",
	function (x) {
		return(names(variables(x)))
	}
)

# names : getter

setReplaceMethod(
	f = "names" ,
	signature = "Rsocialdata" ,
	definition = function(x, value){
		listData <- variables(x)
		oldnames <- names(listData)
    if(length(oldnames) != length(value)) {
      stop(paste0("Bad length of the vector supplied. Length: ", length(value), " for ", length(oldnames), " variables."))
    }
    if(length(weighting(x))>0) {
      x@weights <- value[varid(weighting(x),x)]
    }
    if(length(checkvars(x))>0) {
      x@checkvars <- value[varid(checkvars(x),x)]
    }
    names(listData) <- value
    variables(x) <- listData
    return(x)
	}
)


setMethod("ncol", "Rsocialdata", 
  definition = function (x) {
    return(length(names(x)))
	}
)

setMethod("nvariable", "Rsocialdata", 
          definition = function (x) {
            return(length(names(x)))
          }
)

setMethod(
  f = "varid",
  signature = c("character", "Rsocialdata"), 
  definition = function (names, object) { 
    return(which(names(object) %in% names))
#     maybe match(names, names(object)) is more efficient ?
  }
)
# promptMethods('varid', filename = 'method-varid.Rd')

setMethod(
  f ="[",
	signature ="Rsocialdata",
	definition = function(x,i,j){
#     if(!missing(i)) {print('i');print(i)}
#     if(!missing(j)) {print('j');print(j)}
#     if(missing(i) || missing(j)) {
#      stop("You have to specify rows and columns index. Don't forget the ','") 
#     }
		listData <- variables(x)
    row.names <- row.names(x)
		if (!missing(j)) {
      wv <- weighting(x)
      cv <- checkvars(x)
      # if wv or cv are defined, we keep them in the subscript
      if(inherits(j, 'character')) {
        j <- varid(j, x)
      }
      if(length(cv) > 0) {j <- c(varid(cv,x),j)}
      if(length(wv) > 0) {j <- c(varid(wv,x),j)}
      j <- unique(j) # to avoid doulons with weighting and checkvars
			listData <- listData[j]
		}
		if (!missing(i)){ 
      # ask i to be unique? no for data.frame, I do either
      if(inherits(i, 'character')) {
        # data.frame do a make.names on row.names ?
        row.id <- match(i, row.names)
      } else {
        row.id <- i
      }
      
      row.id.na <- which(is.na(row.id))
#       print(row.id.na)
      if(length(row.id.na) > 0) {
        warning(paste(
          "Following row.names wasn't found in data",
          paste(i[row.id.na], collapse=', ')
        ))
      }
      row.id <- na.omit(row.id)
#       print(row.id)
		  row.names <- make.unique(row.names[row.id])
#       if (inherits(i, 'character')) {
#         i <- which(row.names(x) %in% i)
        if(length(row.id) == 0) {
          message("Your 'i' argument doesn't match any row name") 
          return(dataset())
        }
#     }
			for (k in 1:length(listData)) {
				listData[[k]] <- listData[[k]][row.id]
			}
#       print(listData[[weighting(x)]])
      #representativity to checkvars variables check
      lc <- length(checkvars(x))
      if(lc > 0) {
       for (k in 1:lc) {
          var <- variables(x)[[checkvars(x)[k]]]
          if (inherits(var, 'CategoricalVariable')) {
            #a <- table(v(weights(x)) * v(listData[[checkvars(x)[k]]]))
            o <- base::table(v(listData[[checkvars(x)[k]]]))
#             print(listData[[checkvars(x)[k]]])
#             print(weights(x)[row.id])
            a <- distrib(listData[[checkvars(x)[k]]], weights = weights(x)[row.id])
            a <- a * sum(weights(x)[row.id])
            b <- distrib(var, weights = weights(x)) #initial
#             print(a)
#             print(o)
#             print(b)
            #print(a)
            #print(a/sum(a))
            #print(b)
            cs <- chisq.test(x = a, p = b)
            if(cs$p.value >= 0.05) {
              message(paste("=> control on ", checkvars(x)[k], ': ok', sep = ''))
            } else {
              res <- cs$stdres
              res <- res[which(abs(res) > 1.96)]
              over <- which(res > 0)
              under <- which(res < 0)
              message(paste("=> control on ", checkvars(x)[k], ': warning, p-value < 0.05', sep = ''))
             #message(paste("=> control on ", 'sexe', ': warning, p-value < 0.05', sep = ''))
              if(length(over) > 0)
                message(paste(paste(names(over), collapse=', '), 'are overrepresented'))
              if(length(under) > 0)
                message(paste(paste(names(under), collapse=', '), 'are underrepresented'))
            }
          }
        }
      }
		}
# 		print(row.names.new)
		return(new("Rsocialdata", 
        name = name(x),
        description = description(x),
				variables = listData,
        row.names = row.names,
        weights = weighting(x),
        checkvars = checkvars(x),
        Rsocialdata.version = Rsocialdata.version(x),
        infos = infos(x)
			)
		)
	}
)

setReplaceMethod(
	f ="[",
	signature ="Rsocialdata",
		definition = function(x, i, j, value){
			if(missing(i)) {
        if(missing(j)) {
          stop("error: you have to specifie i or j")
        } else {
          if (!inherits(value, "Variable")) {
				    stop("Rsocialdata::[<-  value should inherit of Variable")
			    } else {
				    temp <- variables(x)
				    temp[j] <- value
            variables(x) <- temp
				    return(x)
			    }
        }
			} else {
        if (length(i) == 1) {
        } else {
        }
			}
	}
)

setMethod(
  f ="[[",
	signature ="Rsocialdata",
		definition = function(x,i){
      if(length(i) == 1){
        return(variables(x)[[i]])
      } else {
        stop("you can extract only one variable")
      }
	}
)

setReplaceMethod(
	f ="[[",
	signature ="Rsocialdata",
		definition = function(x, j, value){
			# vérifier si j existe, sinon renvoyer un message d'erreur (plutot que null)
			if (!is(value, "Column")) {
				stop("Rsocialdata::[<-  value should inherit of Column")
			} else {
				temp <- slot(x, "data")
				temp[j] <- value
				return(new(
          "Rsocialdata",
          data = temp
        ))
			}
	}
)











# $
setMethod(
  "$",
	"Rsocialdata",
	function (x, name) {
		return(variables(x)[[name]])
	}
)
setReplaceMethod(
  "$",
  "Rsocialdata",
	function (x, name, value) {
    y <- variables(x)
    y[[name]] <- value
    slot(x, 'variables') <- y
		return(x)
	}
)

setMethod("nrow", "Rsocialdata", 
  definition = function (x) {
  	if (length(names(x)) == 0) {
			#stop("Rsocialdata::nrow    object is an empty Rsocialdata")
			return(0)
		} else {
			return(length(variables(x)[[1]]))
		}
	}
)

  
setMethod(
  "missings",
  "Rsocialdata",
	function (object) {
    nmiss <- sum(unlist(lapply(variables(object), nmissings)))
    nmisspercent <- nmiss / (ncol(object)*nrow(object)) * 100
    out <- c(nmiss, nmisspercent, formatC(nmisspercent, digits = 2, format = "f"))
    names(out) <- c("nmissings", "nmissingspercent.num", "nmissingspercent.cha")
		return(out)
	}
)

setMethod("as.data.frame", "Rsocialdata", 
  definition = function (x) {
  names <- names(x)
	out <- as.data.frame(lapply(variables(x), as.data.frame))
	names(out) <- names
	return(out)
  }
)




# show
setMethod(
  f = "show",
  signature = "Rsocialdata", 
  definition = function (object) {
    
    txt.weighted <- 'Weighted: no'
    if(is.weighted(object)) txt.weighted <- 'Weighted: yes'
    
    txt.desc <- 'Description: no'
    if(length(description(object)) > 0)
      txt.desc <- paste('Description:', description(object))
      
    message(txt.desc)
    message(txt.weighted)
    
    if(ncol(object) == 0) {
      message('Rsocialdata with 0 columns and 0 rows')
    } else {
      out <- as.data.frame(object)
      names(out) <- str.names(object, parenthesis = T)
      print(out)
    }
  }
)

# print
setMethod("print", "Rsocialdata", 
  definition = function (x, ...) {
	show(x)
  }
)



setMethod(
  "head",
  "Rsocialdata",
  function (x) {
		return(x[1:6,])
	}
)
setMethod(
  "tail",
  "Rsocialdata",
  function (x) {
    N <- nrow(x)
  	return(x[(N-6):N,])
	}
)
setMethod(
  "quantitatives",
  "Rsocialdata",
  function (object) {
    checkvars(object) <- character(0)
    weighting(object) <- character(0)
    subsetvar <- names(which(unlist(lapply(variables(object), is.quantitative))))
    if(length(subsetvar) > 0) {
      variables(object) <- variables(object)[subsetvar]
      return(object)
    } else {
      return(dataset())
    }
  }
)
setMethod(
  "nquantitatives",
  "Rsocialdata",
  function (object) {
    return(ncol(quantitatives(object)))
  }
)
setMethod(
  "scales",
  "Rsocialdata",
  function (object) {
    checkvars(object) <- character(0)
    weighting(object) <- character(0)
    subsetvar <- names(which(unlist(lapply(variables(object), is.scale))))
    if(length(subsetvar) > 0) {
      variables(object) <- variables(object)[subsetvar]
      return(object)
    } else {
      return(dataset())
    }
  }
)
setMethod(
  "scales.exact",
  "Rsocialdata",
  function (object) {
    checkvars(object) <- character(0)
    weighting(object) <- character(0)
    subsetvar <- names(which(unlist(lapply(variables(object), is.scale.exact))))
    if(length(subsetvar) > 0) {
      variables(object) <- variables(object)[subsetvar]
      return(object)
    } else {
      return(dataset())
    }
	}
)
setMethod(
  "nscales",
  "Rsocialdata",
  function (object) {
    return(ncol(scales(object)))
	}
)
setMethod(
  "nscales.exact",
  "Rsocialdata",
  function (object) {
    return(ncol(scales.exact(object)))
  }
)
setMethod(
  "qualitatives",
  "Rsocialdata",
  function (object) {
    checkvars(object) <- character(0)
    weighting(object) <- character(0)
    subsetvar <- names(which(unlist(lapply(variables(object), is.qualitative))))
    if(length(subsetvar) > 0) {
      variables(object) <- variables(object)[subsetvar]
      return(object)
    } else {
      return(dataset())
    }
  }
)
setMethod(
  "nqualitatives",
  "Rsocialdata",
  function (object) {
    return(ncol(qualitatives(object)))
	}
)
setMethod(
  "nominals",
  "Rsocialdata",
  function (object) {
    checkvars(object) <- character(0)
    weighting(object) <- character(0)
    subsetvar <- names(which(unlist(lapply(variables(object), is.nominal))))
    if(length(subsetvar) > 0) {
      variables(object) <- variables(object)[subsetvar]
      return(object)
    } else {
      return(dataset())
    }
	}
)
setMethod(
  "nominals.exact",
  "Rsocialdata",
  function (object) {
    checkvars(object) <- character(0)
    weighting(object) <- character(0)
    subsetvar <- names(which(unlist(lapply(variables(object), is.nominal.exact))))
    if(length(subsetvar) > 0) {
      variables(object) <- variables(object)[subsetvar]
      return(object)
    } else {
      return(dataset())
    }
  }
)
setMethod(
  "nnominals",
  "Rsocialdata",
  function (object) {
    return(ncol(nominals(object)))
	}
)
setMethod(
  "nnominals.exact",
  "Rsocialdata",
  function (object) {
    return(ncol(nominals.exact(object)))
  }
)
setMethod(
  "ordinals",
  "Rsocialdata",
  function (object) {
    checkvars(object) <- character(0)
    weighting(object) <- character(0)
    subsetvar <- names(which(unlist(lapply(variables(object), is.ordinal))))
    if(length(subsetvar) > 0) {
      variables(object) <- variables(object)[subsetvar]
      return(object)
    } else {
      return(dataset())
    }
	}
)
setMethod(
  "nordinals",
  "Rsocialdata",
  function (object) {
    return(ncol(ordinals(object)))
	}
)
setMethod(
  "weightings",
  "Rsocialdata",
  function (object) {
    checkvars(object) <- character(0)
    weighting(object) <- character(0)
    subsetvar <- names(which(unlist(lapply(variables(object), is.weighting))))
    if(length(subsetvar) > 0) {
      variables(object) <- variables(object)[subsetvar]
      return(object)
    } else {
      return(dataset())
    }
  }
)
setMethod(
  "nweightings",
  "Rsocialdata",
  function (object) {
    return(ncol(weightings(object)))
	}
)
setMethod(
  "times",
  "Rsocialdata",
  function (object) {
    checkvars(object) <- character(0)
    weighting(object) <- character(0)
    subsetvar <- names(which(unlist(lapply(variables(object), is.time))))
    if(length(subsetvar) > 0) {
      variables(object) <- variables(object)[subsetvar]
      return(object)
    } else {
      return(dataset())
    }
	}
)
setMethod(
  "ntimes",
  "Rsocialdata",
  function (object) {
    return(ncol(times(object)))
	}
)
setMethod(
  "binaries",
  "Rsocialdata",
  function (object) {
    checkvars(object) <- character(0)
    weighting(object) <- character(0)
    subsetvar <- names(which(unlist(lapply(variables(object), is.binary))))
    if(length(subsetvar) > 0) {
      variables(object) <- variables(object)[subsetvar]
      return(object)
    } else {
      return(dataset())
    }
  }
)
setMethod(
  "nbinaries",
  "Rsocialdata",
  function (object) {
    return(ncol(binaries(object)))
	}
)


setMethod(
  f = "contains",
  signature = c('character', 'Rsocialdata'), 
  definition = function (keywords, data, ignore.case, and) {
    nkeys <- length(keywords)
    stopifnot(nkeys > 0)
    stopifnot(ncol(data) > 0)
    
    checkvars(data) <- character(0)
    weighting(data) <- character(0)
    
    l <- which(mapply(contains, list(keywords), variables(data), ignore.case, and))
    if(length(l) == 0) {
      out <- NULL
    } else {
      out <- data[,l]
      print(alldescriptions(out))
    }
    return(out)

  }
)
# data(iris)
# diris <- dataset(iris)
# description(diris$Sepal.Length) <- "hello, good bye"
# description(diris$Sepal.Width) <- "hello!"
# description(diris$Species) <- "hello!"
# names(contains('hello', diris))
# names(contains(c('hello','good'), diris))
# names(contains(c('good'), diris))
# names(contains(c('hello','good', 'bye'), diris, and = T))
# names(contains(c('hqsdfqsdfello','good', 'bye'), diris, and = T))

    
setMethod(
  f = "valid",
  signature = c("Rsocialdata"), 
  definition = function (object, percent) {
    subsetvar <- names(which(unlist(mapply(valid, variables(object), percent))))
    variables(object) <- variables(object)[subsetvar]
    return(object)
  }
)

# setMethod(
#   "index",
#   c("Rsocialdata", "character"),
#   function (object, names) {
#     return(match(names, names(object)))
#   }
# )

setMethod("summary", "Rsocialdata", 
  definition = function (object, ...) {
    lapply(variables(object), summary)
  }
)





# as.data.frame
setMethod("as.data.frame", "Rsocialdata", 
  definition = function (x) {
  names <- names(x)
	out <- as.data.frame(lapply(variables(x), as.data.frame))
	names(out) <- names
  row.names(out) <- row.names(x)
	return(out)
  }
)

setMethod("v", "Rsocialdata", 
  function(x) {
	  return(as.data.frame(x))
  }
)

setAs("Rsocialdata", "data.frame", 
  function(from) {
		return(as.data.frame(from))
	}
)

setMethod(
  f = "distrib",
  signature = "Rsocialdata", 
  definition = function (object, weights, missings.omit, percent, sorting, format, digits, chlength, sep, cut) {
    out <- qualitatives(object)
    names <- names(out)
    
    weights <- weights(object)
    
    m <- getMethod("distrib", "CategoricalVariable")
    if (missing(sorting)) {
      out <- mapply(
        m,
        variables(out),
        weights = weights,
        missings.omit = missings.omit,
        percent = percent,
        format = format,
        digits = digits,
        chlength = chlength,
        sep = sep,
        cut = cut
      )
    } else {
      out <- mapply(
        m,
        variables(out),
        weights = weights,
        missings.omit = missings.omit,
        percent = percent,
        sorting = sorting,
        format = format,
        digits = digits,
        chlength = chlength,
        sep = sep,
        cut = cut
      )
    }
    
    if (format) {
      out <- data.frame(out)
      row.names(out) <- names
      names(out) <- "Distribution"
    }
    
    return(out)
  }
)
  
setMethod(
  "export",
  "Rsocialdata",
  function (
    object,
    name,
    ...
  ) {
    
    #dir.create(name)
    if(!missing(name)) {
      outname <- make.names(name)
    } else {
      if(length(name(object)) > 0) {
        outname <- make.names(name(object))
      } else {
        outname <- 'untitled.bdd'
      }
    }
    
    message('Your file will be save in ', getwd())
    message('Name of your file: ', outname, '.RData')
    
    assign(outname, object)
    c <- call('save', outname, file = paste(name, ".RData", sep = ''))
    eval(c)
#     save(object, file = paste(name, ".RData", sep = ''))
    exportPDF(object, name, ...)
  }
)

setMethod(
  "subset",
  "Rsocialdata",
  function (x, ...) {
    
    rnames.ini <- row.names(x)
    y <- v(x)
    s <- subset(y, ... = ...)
    rnames.sub <- row.names(s)
    out <- x[rnames.sub,]
#     message("Select option isn't implemented yet.")
    return(out)
  }
)
# data(iris)
# ir <- dataset(iris)
# t <- ir[1:3,]
# t <- ir[as.character(1:3),]
# t <- subset(ir, Species == 'versicolor')


setMethod(
  f = "frequencies",
  signature = c("character", "Rsocialdata"), 
  definition = function (x, data, ...) {
    
    out <- frequencies(data[[x]], weights=weights(data))
    
    return(out)
  }
)

setMethod("rename", "Rsocialdata", 
  definition = function (x, ...) {
    newnames <- list(...)
    oldnames <- names(newnames)
    
    problem <- setdiff(oldnames, names(x))
    if(length(problem) > 0 ) {
      stop(paste(
        "Some variables don't exist in data:\n ", paste(problem, collapse = ', ')))
    }
    
    if (length(names(newnames)) == 0) {
      stop("You have to specify names...")
    } else {
      for (i in names(newnames)) {
        temp <- newnames[[i]]
        stopifnot(inherits(temp, 'character'))
        if(length(temp) > 1) {
          stop(paste("In", temp, "you give more than one name..."))
        } else {
          if(all.is.numeric(i)) {
            id <- as.numeric(i)
          } else {
            id <- varid(i, x)
          }
          names(x)[id] <- temp
        }
      }
      return(x)
    }
  }
)

setMethod(
  f = "only.complete",
  signature = c("character", "Rsocialdata"),
  definition = function (variables, data, ...) {
    
    if(ncol(data) == 0) {
      return(data) # if empty dataset, we return it
    }
    
    if(length(variables) == 0) { # if variables no provided, we take all
      vars <- names(data)
    } else {
      vars <- variables
    }
    
    to.keep <- row.names(data)
    
    for (i in vars) {
      mis <- which(is.na(as.vector(data[[i]])))
      if(length(mis) > 0) { # if there are missings
        to.keep.temp <- setdiff(row.names(data), row.names(data)[mis])
        to.keep <- intersect(to.keep, to.keep.temp)
      }
    }
    
    if(length(to.keep) > 0) {
      out <- data[to.keep,]
    } else {
      return(dataset(
        description = description(data),
        infos = infos(data)
      ))
    }
    
    return(out)
  }
)


#' @describeIn exportPDF method for \code{Rsocialdata} objects. Generate a codebook of the database.
setMethod("exportPDF", "Rsocialdata", 
          definition = function (
            object,
            pdfSavingName,
            dateformat,
            page.orientation = "landscape",
            latexPackages = NULL,
            keepTex = F,
            openPDF = T,
            description.chlength = 300,
            valids.chlength = 40,
            valids.cut.percent = 0.5,
            sorting = "decreasing",
            width.id = 0.5,
            width.varname = 1.5,
            width.description = 10.5,
            width.n = 0.8,
            width.na = 1.2,
            width.valids = 5,
            width.valids.nao.inc = 5,
            width.min = 1.25,
            width.max = 1.25,
            width.mean = 1.25,
            width.stddev = 1.25
          ) {
            
            check.tex()
            
            if(!is.installed.pkg('xtable')) {
              exit.by.uninstalled.pkg('xtable')
            } else {
              require(xtable)
              
              
              #TAILLE IDEALE 19.5cm
              align.common <-  c(
                paste("p{",width.id,"cm}",sep=''),
                paste("p{",width.varname,"cm}",sep=''),
                paste("p{",width.description,"cm}",sep=''),
                paste("p{",width.n,"cm}",sep=''),
                paste("p{",width.na,"cm}",sep='')
              ) # 14.5cm
              align.bin    <-  c(
                align.common,
                paste("p{",width.valids+0.5,"cm}",sep='')
              )
              align.nom    <-  c(
                align.common,
                paste("p{",0.6,"cm}",sep=''),
                paste("p{",width.valids-0.6,"cm}",sep='')
              )
              align.nom[3] <- paste("p{",width.description - width.valids.nao.inc,"cm}",sep='')
              align.nom[7] <- paste("p{",width.valids + width.valids.nao.inc,"cm}",sep='')
              align.ord    <-  align.nom
              align.scale  <-  c(
                align.common,
                paste("p{",width.min,"cm}",sep=''),
                paste("p{",width.max,"cm}",sep=''),
                paste("p{",width.mean,"cm}",sep=''),
                paste("p{",width.stddev,"cm}",sep='')
              ) 
              align.wvar   <-  align.scale
              align.ts   <-  align.scale
              #     align.ts     <-  c(
              #       align.common,
              #       paste("p{"width.min"cm}",sep=''),
              #       paste("p{"width.min"cm}",sep=''),
              #       paste("p{"width.min"cm}",sep=''),
              #       paste("p{"1.5"cm}",sep='')
              #     )
              
              nTuples <- nroww(object)
              weights <- weights(object)
              
              if (length(name(object)) == 0) { outName <- "Untitled Rsocialdata" } else { outName <- name(object) }
              outName <- make.names(outName) # no spaces for Unix/Texlive compilation ?
              
              if(missing(pdfSavingName)) {    
                pdfSavingName <- paste("Summary-", outName, sep = "") # no spaces for Unix/Texlive compilation ?
              }
              
              
              latexFile <- paste(pdfSavingName, ".tex", sep="")
              
              is.writable(pdfSavingName, path = getwd()) 	
              
              outFileCon <- file(latexFile, "w", encoding="UTF-8")
              
              #     print('hello-beforelatexhead')
              #     print(class(getMethod("show", "Variable")))
              
              latex.head(title = paste("Summary of the", totex(name(object)), "dataset"),
                         page.orientation, latexPackages, outFileCon)
              
              #     print('hello-afterlatexhead')
              #     print(class(getMethod("show", "Variable")))
              
              cat("\\section*{Overview} \n", file = outFileCon, append = T)
              cat("\\begin{minipage}[t]{.46\\linewidth} \n", file = outFileCon, append = T)
              cat("\\begin{itemize*} \n", file = outFileCon, append = T)
              cat("\\item \\textbf{Name:}", totex(name(object)), "\n", file = outFileCon, append = T)
              cat("\\item \\textbf{Description:}", description(object), "\n", file = outFileCon, append = T)
              #cat("\\item Object version:", oversion(object), "\n", file = outFileCon, append = T)
              #cat("\\item Created by Rsocialdata version:", pversion(object), "\n", file = outFileCon, append = T)
              cat("\\item \\textbf{Number of variables:} ", ncol(object), " (",
                  nbinaries(object), " binaries, ",
                  nordinals(object), " ordinals, ",
                  nnominals.exact(object), " nominals, ",
                  nscales.exact(object), " scales, ",
                  ntimes(object), " timestamps, ",
                  nweightings(object), " weightings",
                  ")", "\n", sep = "", file = outFileCon, append = T)
              cat("\\item \\textbf{Number of individuals:}", nTuples, "(for ", nrow(object), " rows)","\n", file = outFileCon, append = T)
              
              
              
              cat("\\item \\textbf{Percent of missing values:}", missings(object)["nmissingspercent.cha"], "\\%", "\n", file = outFileCon, append = T)
              if(length(weighting(object)) > 0) {
                cat("\\item \\textbf{Weighting variable:} ", totex(weighting(object)), ', ', totex(description(object[[weighting(object)]])), ".", "\n", file = outFileCon, append = T, sep='')
              } else {
                cat("\\item \\textbf{Weighting variable:} none.", "\n", file = outFileCon, append = T, sep='')
              }
              if(length(checkvars(object)) > 0) {
                cat("\\item \\textbf{Control variable(s):} ", file = outFileCon, append = T, sep='')
                cv.temp <- checkvars(object)
                count <- 0
                count.last <- length(cv.temp)
                for (cv in cv.temp) {
                  count <- count + 1
                  cat(totex(cv), ' (', totex(description(object[[cv]])), ")", file = outFileCon, append = T, sep='')
                  if (count == count.last) {
                    cat('. ', "\n",  file = outFileCon, append = T, sep='')
                  } else {
                    cat(', ', file = outFileCon, append = T, sep='')
                  }
                }
              } else {
                cat("\\item \\textbf{Control variable(s):} none.", "\n", file = outFileCon, append = T, sep='')
              }
              if(nchar(spatial(object)) > 0) {
                cat("\\item \\textbf{Spatial variable:} ", totex(spatial(object)), ', ', totex(description(spatial[[weighting(object)]])), ".", "\n", file = outFileCon, append = T, sep='')
              } else {
                cat("\\item \\textbf{Spatial variable:} none.", "\n", file = outFileCon, append = T, sep='')
              }
              cat("\\end{itemize*} \n", file = outFileCon, append = T)
              
              cat("\\end{minipage} \\hfill \n", file = outFileCon, append = T)
              cat("\\begin{minipage}[t]{.46\\linewidth} \n", file = outFileCon, append = T)
              cat("\\begin{itemize*} \n", file = outFileCon, append = T)
              cat("\\item \\textbf{Author(s):} ", totex(db.author(object)), "\n", file = outFileCon, append = T, sep='')
              cat("\\item \\textbf{Database manager(s):} ", totex(db.manager(object)), "\n", file = outFileCon, append = T, sep='')
              cat("\\item \\textbf{Contact e-mail(s):} ", totex(db.contact.email(object)), "\n", file = outFileCon, append = T, sep='')
              cat("\\item \\textbf{License:} ", totex(db.license(object)), "\n", file = outFileCon, append = T, sep='')
              cat("\\item \\textbf{Release date:} ", totex(db.release.date(object)), "\n", file = outFileCon, append = T, sep='')
              cat("\\item \\textbf{Citation:} ", totex(db.citation(object)), "\n", file = outFileCon, append = T, sep='')
              cat("\\item \\textbf{Website:} ", totex(db.website(object)), "\n", file = outFileCon, append = T, sep='')
              #     cat("\\item \\textbf{Population:} ", totex(db.details(object)), "\n", file = outFileCon, append = T, sep='')
              cat("\\end{itemize*} \n", file = outFileCon, append = T)
              cat("\\end{minipage} \\hfill \n", file = outFileCon, append = T)
              
              cat("\\newpage \n", file = outFileCon, append = T)
              cat("\\section*{Detailed description} \n", totex(db.details(object)), "\n", file = outFileCon, append = T, sep='')
              
              cat("\\newpage \n", file = outFileCon, append = T)
              cat("\\section*{Distribution of variables by percent of valid cases} \n", file = outFileCon, append = T)
              
              percents <- seq(from = 0, to = 100, by = 10)
              val <- c()
              for (i in percents) {
                val <- c(val, ncol(valid(object, percent = i)))
              }
              valdf <- data.frame(percents,val)
              names(valdf) <- c("Percents of valid cases", "Number of variables")
              
              object.xtable <- xtable(
                valdf,
                label='validCasesSummary',
                caption='Number of variables by percent of valid cases',
                digits = 3,
                #     		align = c("l","p{4cm}","l"),
                display = c("d","d","d")
              )
              
              plot.filename <- paste(pdfSavingName, ".validcasesRplot", sep = "")
              plot.filename <- gsub("\\.", "-", plot.filename)
              plot.filename.pdf <- paste(plot.filename, ".pdf", sep = "")
              pdf(file = plot.filename.pdf)
              plot(
                percents,
                val,
                type = "l",
                main = "Number of variables by percent of valid cases",
                xlab = "Percentage of valid cases",
                ylab = "Number of variables"
              )
              polygon(c(0,percents,100),c(0,val,0), col="gray")
              dev.off()
              
              cat("\\begin{center} \n", file = outFileCon, append = T)
              cat("\\begin{minipage}[c]{.35\\linewidth} \n", file = outFileCon, append = T)
              print(object.xtable, file=outFileCon , append=T, include.rownames = F, table.placement = "htb", floating=F) 
              cat("\\end{minipage} \n", file = outFileCon, append = T)
              cat("\\begin{minipage}[c]{.40\\linewidth} \n", file = outFileCon, append = T)
              cat("\\includegraphics[scale=0.40]{", plot.filename.pdf, "} \n", file = outFileCon, append = T, sep = "")
              cat("\\end{minipage} \n", file = outFileCon, append = T)
              cat("\\end{center} \n", file = outFileCon, append = T)
              
              
              cat("\\newpage \n", file = outFileCon, append = T)
              cat("\\section*{Configuration} \n", file = outFileCon, append = T)
              cat("\\begin{itemize} \n", file = outFileCon, append = T)
              cat("\\item Variable descriptions are cut above", description.chlength, "characters. \n", file = outFileCon, append = T)
              cat("\\item Valid case descriptions are cut above", valids.chlength, "characters. \n", file = outFileCon, append = T)
              if (valids.cut.percent > 0) {
                cat("\\item For categorical variables, valid cases are cut when under", valids.cut.percent, "\\% of representativity. \n", file = outFileCon, append = T)
              }
              cat("\\item For nominal variables, valid cases are listed in", sorting, "order. \n", file = outFileCon, append = T)
              cat("\\item For ordinal variables, valid cases are listed in", sorting, "order. \n", file = outFileCon, append = T)
              cat("\\end{itemize} \n", file = outFileCon, append = T)
              cat("\\newpage \n", file = outFileCon, append = T)
              flag.newpage <- FALSE
              cat("\\section*{Variable summary} \n", file = outFileCon, append = T)
              
              
              if(nweightings(object) > 0 ) {
                if (flag.newpage) {
                  #         cat("\\newpage \n", file = outFileCon, append = T)
                  cat("\\vspace*{2cm} \n", file = outFileCon, append = T)
                }
                cat("\\subsection*{Weighting variable(s)} \n", file = outFileCon, append = T)
                scvar <- weightings(object)
                scvar.names <- names(scvar)
                
                
                descriptions <- c()
                nbNA <- c()
                theMin <- c()
                theMax <- c()
                theMean <- c()
                theSD <- c()
                
                for (i in scvar.names ) {
                  vtemp <- scvar[[i]]
                  
                  desc.temp <- description(vtemp)
                  if (nchar(desc.temp) > description.chlength)
                    desc.temp <- paste(substr(desc.temp, 0, description.chlength - 3), "...", sep = "")
                  
                  descriptions <- c(descriptions, desc.temp)
                  nbNA <- c(nbNA, nmissingsw(vtemp, weights))
                  theMin <- c(theMin, min(vtemp, na.rm = TRUE))
                  theMax <- c(theMax, max(vtemp, na.rm = TRUE))
                  theMean <- c(theMean, mean(vtemp, na.rm = TRUE))
                  theSD <- c(theSD, sd(vtemp, na.rm = TRUE))
                }
                
                nbNA[which(is.na(nbNA))] <- 0
                N <- rep(nTuples, nweightings(object)) - nbNA
                NApourcent <- nbNA / nTuples * 100
                
                df <- data.frame(scvar.names, descriptions,N, NApourcent, theMin, theMax, theMean, theSD)
                
                names(df) <- c("Variable", "Description", "N", "NA (%)", "Min", "Max", "Mean", "Std dev.")
                # row.names(df) <- 1:nrow(df)
                
                
                row.names(df) <- varid(scvar.names, object)
                cat("{\\footnotesize \n", file = outFileCon, append = T)
                
                object.xtable <- xtable(
                  df,
                  label='featureSummary',
                  caption='Weighting variable(s) summary',
                  digits = 3,
                  align = align.wvar,
                  display = c("d","s","s","d","fg","fg","fg","fg","fg")
                )
                
                print(object.xtable, file=outFileCon , append=T, tabular.environment='longtable', table.placement = "htb", floating=F) 
                cat("} \n", file = outFileCon, append = T)
                flag.newpage <- TRUE
              }
              
              
              if(nbinaries(object) > 0 ) {
                if (flag.newpage) {
                  #         cat("\\newpage \n", file = outFileCon, append = T)
                  cat("\\vspace*{2cm} \n", file = outFileCon, append = T)
                }
                cat("\\subsection*{Binary variables} \n", file = outFileCon, append = T)
                scvar <- binaries(object)
                scvar.names <- names(scvar)
                
                descriptions <- c()
                nbNA <- c()
                theDistrib <- c()   
                
                for (i in scvar.names ) {
                  vtemp <- scvar[[i]]
                  
                  desc.temp <- description(vtemp)
                  if (nchar(desc.temp) > description.chlength)
                    desc.temp <- paste(substr(desc.temp, 0, description.chlength - 3), "...", sep = "")
                  
                  descriptions <- c(descriptions, desc.temp)
                  nbNA <- c(nbNA, nmissingsw(vtemp, weights))
                  theDistrib <- c(theDistrib, distrib(vtemp, weights, percent = T, format = T, chlength = valids.chlength, sorting=sorting, cut.percent=valids.cut.percent))
                  #theSD <- c(theSD, sd(vtemp, na.rm = TRUE))
                }
                
                nbNA[which(is.na(nbNA))] <- 0
                N <- rep(nTuples, nbinaries(object)) - nbNA
                NApourcent <- nbNA / nTuples * 100
                
                df <- data.frame(scvar.names, descriptions,N, NApourcent, theDistrib)
                names(df) <- c("Variable", "Description", "N", "NA (%)", "Distribution (%)")
                row.names(df) <- varid(scvar.names, object)
                cat("{\\footnotesize \n", file = outFileCon, append = T)
                
                object.xtable <- xtable(
                  df,
                  label='featureSummary',
                  caption='Binary variables summary',
                  digits = 3,
                  align = align.bin,
                  display = c("d","s","s","d","fg","s")
                )
                
                print(object.xtable, file=outFileCon , append=T, tabular.environment='longtable', table.placement = "htb", floating=F) 
                cat("} \n", file = outFileCon, append = T)
                flag.newpage <- TRUE
              }
              
              if(nordinals(object) > 0 ) {
                if (flag.newpage) {
                  #         cat("\\newpage \n", file = outFileCon, append = T)
                  cat("\\vspace*{2cm} \n", file = outFileCon, append = T)
                }
                cat("\\subsection*{Ordinal variables} \n", file = outFileCon, append = T)
                scvar <- ordinals(object)
                scvar.names <- names(scvar)
                
                descriptions <- c()
                nbNA <- c()
                theNlevels <- c()
                theDistrib <- c()   
                
                for (i in scvar.names ) {
                  vtemp <- scvar[[i]]
                  
                  desc.temp <- description(vtemp)
                  if (nchar(desc.temp) > description.chlength)
                    desc.temp <- paste(substr(desc.temp, 0, description.chlength - 3), "...", sep = "")
                  
                  descriptions <- c(descriptions, desc.temp)
                  nbNA <- c(nbNA, nmissingsw(vtemp, weights))
                  theNlevels <- c(theNlevels, nvalids(vtemp))
                  theDistrib <- c(theDistrib, distrib(vtemp, weights, percent = T, format = T, chlength = valids.chlength, sorting=sorting, cut.percent=valids.cut.percent))
                  #theSD <- c(theSD, sd(vtemp, na.rm = TRUE))
                }
                
                nbNA[which(is.na(nbNA))] <- 0
                N <- rep(nTuples, nordinals(object)) - nbNA
                NApourcent <- nbNA / nTuples * 100
                
                df <- data.frame(scvar.names, descriptions,N, NApourcent, theNlevels, theDistrib)
                names(df) <- c("Variable", "Description", "N", "NA (%)", "Classes", "Distribution (%)")
                row.names(df) <- varid(scvar.names, object)
                cat("{\\footnotesize \n", file = outFileCon, append = T)
                
                object.xtable <- xtable(
                  df,
                  label='featureSummary',
                  caption='Ordinal variables summary',
                  digits = 3,
                  align = align.ord,
                  display = c("d","s","s","d","fg","d","s")
                )
                
                print(object.xtable, file=outFileCon , append=T, tabular.environment='longtable', table.placement = "htb", floating=F) 
                cat("} \n", file = outFileCon, append = T)
                flag.newpage <- TRUE
              }
              
              if(nnominals.exact(object) > 0 ) {
                if (flag.newpage) {
                  #         cat("\\newpage \n", file = outFileCon, append = T)
                  cat("\\vspace*{2cm} \n", file = outFileCon, append = T)
                }
                cat("\\subsection*{Nominal variables} \n", file = outFileCon, append = T)
                scvar <- nominals.exact(object)
                scvar.names <- names(scvar)
                
                descriptions <- c()
                nbNA <- c()
                theNlevels <- c()
                theDistrib <- c()   
                
                for (i in scvar.names ) {
                  #print(i)
                  vtemp <- scvar[[i]]
                  
                  desc.temp <- description(vtemp)
                  #print(desc.temp)
                  if (nchar(desc.temp) > description.chlength)
                    desc.temp <- paste(substr(desc.temp, 0, description.chlength - 3), "...", sep = "")
                  
                  descriptions <- c(descriptions, desc.temp)
                  nbNA <- c(nbNA, nmissingsw(vtemp, weights))
                  theNlevels <- c(theNlevels, nvalids(vtemp))
                  theDistrib <- c(theDistrib, distrib(vtemp, weights, percent = T, format = T, chlength = valids.chlength, sorting=sorting, cut.percent=valids.cut.percent))
                  #theSD <- c(theSD, sd(vtemp, na.rm = TRUE))
                }
                
                nbNA[which(is.na(nbNA))] <- 0
                N <- rep(nTuples, nnominals.exact(object)) - nbNA
                NApourcent <- nbNA / nTuples * 100
                
                df <- data.frame(scvar.names, descriptions,N, NApourcent, theNlevels, theDistrib)
                names(df) <- c("Variable", "Description", "N", "NA (%)", "Classes", "Distribution (%)")
                row.names(df) <- varid(scvar.names, object)
                cat("{\\footnotesize \n", file = outFileCon, append = T)
                
                object.xtable <- xtable(
                  df,
                  label='featureSummary',
                  caption='Nominal variables summary',
                  digits = 3,
                  align = align.nom,
                  display = c("d","s","s","d","fg","d","s")
                )
                
                print(object.xtable, file=outFileCon , append=T, tabular.environment='longtable', table.placement = "htb", floating=F) 
                cat("} \n", file = outFileCon, append = T)
                flag.newpage <- TRUE
              }
              
              if(nscales.exact(object) > 0 ) {
                if (flag.newpage) {
                  #         cat("\\newpage \n", file = outFileCon, append = T)
                  cat("\\vspace*{2cm} \n", file = outFileCon, append = T)
                }
                cat("\\subsection*{Scale variables} \n", file = outFileCon, append = T)
                scvar <- scales.exact(object)
                scvar.names <- names(scvar)
                
                descriptions <- c()
                nbNA <- c()
                theMin <- c()
                theMax <- c()
                theMean <- c()
                theSD <- c()
                
                for (i in scvar.names ) {
                  vtemp <- scvar[[i]]
                  
                  desc.temp <- description(vtemp)
                  if (nchar(desc.temp) > description.chlength)
                    desc.temp <- paste(substr(desc.temp, 0, description.chlength - 3), "...", sep = "")
                  
                  descriptions <- c(descriptions, desc.temp)
                  nbNA <- c(nbNA, nmissingsw(vtemp, weights))
                  theMin <- c(theMin, min(vtemp, na.rm = TRUE))
                  theMax <- c(theMax, max(vtemp, na.rm = TRUE))
                  theMean <- c(theMean, mean(vtemp, na.rm = TRUE))
                  theSD <- c(theSD, sd(vtemp, na.rm = TRUE))
                }
                
                nbNA[which(is.na(nbNA))] <- 0
                N <- rep(nTuples, nscales.exact(object)) - nbNA
                NApourcent <- nbNA / nTuples * 100
                
                df <- data.frame(scvar.names, descriptions,N, NApourcent, theMin, theMax, theMean, theSD)
                names(df) <- c("Variable", "Description", "N", "NA (%)", "Min", "Max", "Mean", "Std dev.")
                # row.names(df) <- 1:nrow(df)
                row.names(df) <- varid(scvar.names, object)
                cat("{\\footnotesize \n", file = outFileCon, append = T)
                
                object.xtable <- xtable(
                  df,
                  label='featureSummary',
                  caption='Scale variables summary',
                  digits = 3,
                  align = align.scale,
                  display = c("d","s","s","d","fg","fg","fg","fg","fg")
                )
                
                print(object.xtable, file=outFileCon , append=T, tabular.environment='longtable', table.placement = "htb", floating=F) 
                cat("} \n", file = outFileCon, append = T)
                flag.newpage <- TRUE
              }
              
              if(ntimes(object) > 0 ) {
                if (flag.newpage) {
                  #         cat("\\newpage \n", file = outFileCon, append = T)
                  cat("\\vspace*{2cm} \n", file = outFileCon, append = T)
                }
                cat("\\subsection*{Timestamp variables} \n", file = outFileCon, append = T)
                scvar <- times(object)
                scvar.names <- names(scvar)
                
                descriptions <- c()
                nbNA <- c()
                theMin <- c()
                theMax <- c()
                theMed <- c()
                theMean <- c()  	
                
                for (i in scvar.names ) {
                  vtemp <- scvar[[i]]
                  if(!missing(dateformat)){
                    format(vtemp) <- dateformat
                  }
                  
                  desc.temp <- description(vtemp)
                  if (nchar(desc.temp) > description.chlength)
                    desc.temp <- paste(substr(desc.temp, 0, description.chlength - 3), "...", sep = "")
                  
                  descriptions <- c(descriptions, desc.temp)
                  nbNA <- c(nbNA, nmissingsw(vtemp, weights))
                  theMin <- c(theMin, min(vtemp, na.rm = TRUE))
                  theMax <- c(theMax, max(vtemp, na.rm = TRUE))
                  theMed <- c(theMed, median(vtemp, na.rm = TRUE))
                  theMean <- c(theMean, mean(vtemp, na.rm = TRUE))
                  #theSD <- c(theSD, sd(vtemp, na.rm = TRUE))
                }
                
                nbNA[which(is.na(nbNA))] <- 0
                N <- rep(nTuples, ntimes(object)) - nbNA
                NApourcent <- nbNA / nTuples * 100
                
                df <- data.frame(scvar.names, descriptions,N, NApourcent, theMin, theMax, theMed, theMean)
                names(df) <- c("Variable", "Description", "N", "NA (%)", "Min", "Max", "Median", "Mean")
                row.names(df) <- varid(scvar.names, object)
                cat("{\\footnotesize \n", file = outFileCon, append = T)
                
                object.xtable <- xtable(
                  df,
                  label='featureSummary',
                  caption='Timestamp variables summary',
                  digits = 3,
                  align = align.ts,
                  display = c("d","s","s","d","fg","fg","fg","fg","fg")
                )
                
                print(object.xtable, file=outFileCon , append=T, tabular.environment='longtable', table.placement = "htb", floating=F) 
                cat("} \n", file = outFileCon, append = T)
                flag.newpage <- TRUE
              }
              
              
              close.and.clean(outFileCon, pdfSavingName, keepTex, openPDF)
              
              if(!keepTex) {
                unlink(plot.filename.pdf)
              }
            }
          }
)