#=================================================================================================
# CLASS DEFINITION FILE: Dataset
#=================================================================================================


setClass(
  'Dataset',
	representation(
		name = 'character',
		description = 'character',
		variables = 'list',
		row.names = 'character',
    weights = 'character',
    checkvars = 'character',
    Dataset.version = 'character',
    infos = 'list'
	),
	validity = function(object) {
		if(Dataset.globalenv$print.io) cat (" =>       Dataset: object validity check \n")
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
        message("All checkvars variables have to exist in the Dataset")
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
    
		version <- object@Dataset.version
		lth <- length(version)
		if(flag && lth > 0) {
		  if(any(is.na(version)) > 0){
		    message("Dataset.version can't contain NAs")
		    flag <- FALSE
		  }
		  if(lth > 1) {
		    message("Dataset.version length must be one")
		    flag <- FALSE
		  }
		} else {
		  message("Dataset.version can't be empty")
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
  infos,
  check.rows = FALSE # not used
) {
  if(Dataset.globalenv$print.io) cat(" => (in)  Dataset: builder \n")
  if (missing(x)) x <- list()
  if (missing(name)) name <- character()
  if (missing(description)) description <- character()
  #if (missing(row.names)) row.names <- character()
  if (missing(weights)) weights <- character()
  if (missing(checkvars)) checkvars <- character()
  if (missing(infos)) infos <- list()
  
  if (inherits(x, 'data.frame')) {
    variables <- list()
    for (i in 1:ncol(x)) {
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
        stop(paste("Dataset::dataset - variable", i, "didn't match"))
      }
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
  names(variables) <- make.names(names(variables), unique = T)
      
  if(Dataset.globalenv$print.io) cat(" => (out) Dataset: builder \n")
  return(new(
    Class = "Dataset",
    name = name,
    variables = variables,
    row.names = row.names,
    weights = weights,
    checkvars = checkvars,
    Dataset.version = Dataset.globalenv$Dataset.version,
    infos = infos
  ))
}


setMethod("name", "Dataset", 
  definition = function (object) { 
  return(slot(object, "name"))
  }
)
setReplaceMethod(
	f = "name" ,
	signature = "Dataset" ,
	definition = function(object, value){
		object@name <- value
    validObject(object)
		return(object)
	}
)

setMethod("description", "Dataset", 
  definition = function (object) { 
  return(slot(object, "description"))
  }
)
setReplaceMethod(
	f = "description" ,
	signature = "Dataset" ,
	definition = function(object, value){
		object@description <- value
    validObject(object)
		return(object)
	}
)

setMethod("checkvars", "Dataset", 
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
	signature = "Dataset" ,
	definition = function(object, value){
		object@checkvars <- value
    validObject(object)
		return(object)
	}
)

setMethod(
  f = "Dataset.version",
  signature = "Dataset", 
  definition = function (object, ...) { 
    return(slot(object, "Dataset.version"))
  }
)

setMethod("alldescriptions", "Dataset", 
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

setMethod("allvalues", "Dataset", 
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

setMethod("variables", "Dataset", 
  definition = function (object) {
    return(slot(object, "variables"))
  }
)
setReplaceMethod(
  f = "variables" ,
	signature = "Dataset" ,
	definition = function(object, value){
		object@variables <- value
    validObject(object)
		return(object)
	}
)

setMethod("row.names", "Dataset", 
  definition = function (x) { 
  return(slot(x, "row.names"))
  }
)
setReplaceMethod(
  f = "row.names" ,
  signature = "Dataset" ,
	definition = function(x, value){
		x@row.names <- value
    validObject(x)
		return(x)
	}
)

setMethod("weights", "Dataset", 
  definition = function (object, ...) {
    if(!is.weighted(object)) {
      message("warning: no weights defined, equiponderation is used")
      return(svar(rep(1, nrow(object))))
    } else {
      return(variables(object)[[slot(object, 'weights')]])
    }
  }
)

setMethod("weighting", "Dataset", 
  definition = function (object, ...) {
    return(slot(object, 'weights'))
  }
)

setReplaceMethod(
  f = "weighting" ,
  signature = c("Dataset", "character") ,
  definition = function(object, value){
    if(length(value) == 0) {
      object@weights <- value
    } else {
      if(is.weighting(object[[value]])) {
        object@weights <- value
      } else {
  #       message("The argument given isn't a WeightingVariable object")
  #       message("I'll try to perform a conversion...", appendLF=F)
  #       print(deparse(substitute(object)))
  #       .GlobalEnv$object[[value]] <- wvar(object[[value]])
  #       message("success!")
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
  signature = c("Dataset", "WeightingVariable") ,
  definition = function(object, value){
  	object@weights <- value
    validObject(object)
		return(object)
	}
)

setMethod(
  f = "is.weighted",
  signature = "Dataset", 
  definition = function (object) { 
    if(length(slot(object, 'weights')) > 0) return(TRUE)
    else return(FALSE)
  }
)

setMethod("infos", "Dataset", 
  definition = function (object) { 
  return(slot(object, "infos"))
  }
)
setReplaceMethod(
  f = "infos" ,
  signature = "Dataset" ,
	definition = function(object, value){
		object@infos <- value
    validObject(object)
		return(object)
	}
)

setMethod(
  "names",
	"Dataset",
	function (x) {
		return(names(variables(x)))
	}
)

# names : getter

setReplaceMethod(
	f = "names" ,
	signature = "Dataset" ,
	definition = function(x, value){
		listData <- variables(x)
		oldnames <- names(listData)
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


setMethod("ncol", "Dataset", 
  definition = function (x) {
    return(length(names(x)))
	}
)


setMethod(
  f = "varid",
  signature = c("character", "Dataset"), 
  definition = function (names, object) { 
    return(which(names(object) %in% names))
  }
)
# promptMethods('varid', filename = 'method-varid.Rd')

setMethod(
  f ="[",
	signature ="Dataset",
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
		if (!missing(i)){ # i have to be understood as row.names
      # ask i to be unique? data.frame do a make.names, I do either
      row.id <- match(i, row.names)
		  row.names <- make.unique(row.names[row.id])
#       if (inherits(i, 'character')) {
#         i <- which(row.names(x) %in% i)
        if(length(row.id) == 0) {
          message("Your 'i' argument doesn't match any row name") 
          return(dataset())
        }
#       }
			for (k in 1:length(listData)) {
				listData[[k]] <- listData[[k]][row.id]
			}
      #representativity to checkvars variables check
      lc <- length(checkvars(x))
      if(lc > 0) {
       for (k in 1:lc) {
          var <- variables(x)[[checkvars(x)[k]]]
          if (inherits(var, 'CategoricalVariable')) {
            #a <- table(v(weights(x)) * v(listData[[checkvars(x)[k]]]))
            a <- table(v(listData[[checkvars(x)[k]]]))
            b <- distrib(var)
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
                message(paste(paste(names(over), collapse=', '), 'is/are oversampled'))
              if(length(under) > 0)
                message(paste(paste(names(under), collapse=', '), 'is/are undersampled'))
            }
          }
        }
      }
		}
# 		print(row.names.new)
		return(new("Dataset", 
        name = name(x),
        description = description(x),
				variables = listData,
        row.names = row.names,
        weights = weighting(x),
        checkvars = checkvars(x),
        Dataset.version = Dataset.version(x),
        infos = infos(x)
			)
		)
	}
)

setReplaceMethod(
	f ="[",
	signature ="Dataset",
		definition = function(x, i, j, value){
			if(missing(i)) {
        if(missing(j)) {
          stop("error: you have to specifie i or j")
        } else {
          if (!inherits(value, "Variable")) {
				    stop("Dataset::[<-  value should inherit of Variable")
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
	signature ="Dataset",
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
	signature ="Dataset",
		definition = function(x, j, value){
			# vérifier si j existe, sinon renvoyer un message d'erreur (plutot que null)
			if (!is(value, "Column")) {
				stop("Dataset::[<-  value should inherit of Column")
			} else {
				temp <- slot(x, "data")
				temp[j] <- value
				return(new(
          "Dataset",
          data = temp
        ))
			}
	}
)











# $
setMethod(
  "$",
	"Dataset",
	function (x, name) {
		return(variables(x)[[name]])
	}
)
setReplaceMethod(
  "$",
  "Dataset",
	function (x, name, value) {
    y <- variables(x)
    y[[name]] <- value
    slot(x, 'variables') <- y
		return(x)
	}
)

setMethod("nrow", "Dataset", 
  definition = function (x) {
  	if (length(names(x)) == 0) {
			#stop("Dataset::nrow    object is an empty Dataset")
			return(0)
		} else {
			return(length(variables(x)[[1]]))
		}
	}
)

  
setMethod(
  "missings",
  "Dataset",
	function (object) {
    nmiss <- sum(unlist(lapply(variables(object), nmissings)))
    nmisspercent <- nmiss / (ncol(object)*nrow(object)) * 100
    out <- c(nmiss, nmisspercent, formatC(nmisspercent, digits = 2, format = "f"))
    names(out) <- c("nmissings", "nmissingspercent.num", "nmissingspercent.cha")
		return(out)
	}
)

setMethod("as.data.frame", "Dataset", 
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
  signature = "Dataset", 
  definition = function (object) {
    
    txt.weighted <- 'Weighted: no'
    if(is.weighted(object)) txt.weighted <- 'Weighted: yes'
    
    txt.desc <- 'Description: no'
    if(length(description(object)) > 0)
      txt.desc <- paste('Description:', description(object))
      
    message(txt.desc)
    message(txt.weighted)
    
    if(ncol(object) == 0) {
      message('Dataset with 0 columns and 0 rows')
    } else {
      out <- as.data.frame(object)
      names(out) <- str.names(object, parenthesis = T)
      print(out)
    }
  }
)

# print
setMethod("print", "Dataset", 
  definition = function (x, ...) {
	show(x)
  }
)



setMethod(
  "head",
  "Dataset",
  function (x) {
		return(x[1:6,])
	}
)
setMethod(
  "tail",
  "Dataset",
  function (x) {
    N <- nrow(x)
  	return(x[(N-6):N,])
	}
)
setMethod(
  "quantitatives",
  "Dataset",
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
  "Dataset",
  function (object) {
    return(ncol(quantitatives(object)))
  }
)
setMethod(
  "scales",
  "Dataset",
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
  "Dataset",
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
  "Dataset",
  function (object) {
    return(ncol(scales(object)))
	}
)
setMethod(
  "nscales.exact",
  "Dataset",
  function (object) {
    return(ncol(scales.exact(object)))
  }
)
setMethod(
  "qualitatives",
  "Dataset",
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
  "Dataset",
  function (object) {
    return(ncol(qualitatives(object)))
	}
)
setMethod(
  "nominals",
  "Dataset",
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
  "Dataset",
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
  "Dataset",
  function (object) {
    return(ncol(nominals(object)))
	}
)
setMethod(
  "nnominals.exact",
  "Dataset",
  function (object) {
    return(ncol(nominals.exact(object)))
  }
)
setMethod(
  "ordinals",
  "Dataset",
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
  "Dataset",
  function (object) {
    return(ncol(ordinals(object)))
	}
)
setMethod(
  "weightings",
  "Dataset",
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
  "Dataset",
  function (object) {
    return(ncol(weightings(object)))
	}
)
setMethod(
  "times",
  "Dataset",
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
  "Dataset",
  function (object) {
    return(ncol(times(object)))
	}
)
setMethod(
  "binaries",
  "Dataset",
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
  "Dataset",
  function (object) {
    return(ncol(binaries(object)))
	}
)


setMethod(
  f = "contains",
  signature = c('character', 'Dataset'), 
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
  signature = c("Dataset"), 
  definition = function (object, percent) {
    subsetvar <- names(which(unlist(mapply(valid, variables(object), percent))))
    variables(object) <- variables(object)[subsetvar]
    return(object)
  }
)

setMethod(
  "index",
  c("Dataset", "character"),
  function (object, names) {
    return(match(names, names(object)))
  }
)

setMethod("summary", "Dataset", 
  definition = function (object, ...) {
    lapply(variables(object), summary)
  }
)



setMethod("summaryToPDF", "Dataset", 
  definition = function (object, pdfSavingName, graphics, description.chlength, values.chlength, dateformat, latexPackages, keepTex, openPDF) {
  
  if(!is.installed.pkg('xtable')) {
    exit.by.uninstalled.pkg('xtable')
  } else {
    require(xtable)
    nTuples <- nrow(object)
  
  	if (length(name(object)) == 0) { outName <- "Untitled Dataset" } else { outName <- name(object) }
  	outName <- make.names(outName) # no spaces for Unix/Texlive compilation ?
  	
  	if(missing(pdfSavingName)) {		
  		pdfSavingName <- paste("Summary-", outName, sep = "") # no spaces for Unix/Texlive compilation ?
  	}
  	
  	latexFile <- paste(pdfSavingName, ".tex", sep="")
  	
    outFileCon <- file(latexFile, "w", encoding="UTF-8")
    
  	latex.head(title = paste("Summary of the", totex(name(object)), "dataset"), latexPackages, outFileCon)
                           
  	cat("\\section*{Overview} \n", file = outFileCon, append = T)
  	cat("\\begin{itemize} \n", file = outFileCon, append = T)
  	cat("\\item Name:", totex(name(object)), "\n", file = outFileCon, append = T)
  	cat("\\item Description:", description(object), "\n", file = outFileCon, append = T)
  	#cat("\\item Object version:", oversion(object), "\n", file = outFileCon, append = T)
  	#cat("\\item Created by Dataset version:", pversion(object), "\n", file = outFileCon, append = T)
  	cat("\\item Number of variables: ", ncol(object), " (",
        nbinaries(object), " binaries, ",
        nordinals(object), " ordinals, ",
        nnominals.exact(object), " nominals, ",
        nscales.exact(object), " scales, ",
        ntimes(object), " timestamps, ",
  	    nweightings(object), " weightings",
        ")", "\n", sep = "", file = outFileCon, append = T)
    cat("\\item Number of rows:", nTuples, "\n", file = outFileCon, append = T)
    cat("\\item Percent of missing values:", missings(object)["nmissingspercent.cha"], "\\%", "\n", file = outFileCon, append = T)
  	cat("\\end{itemize} \n", file = outFileCon, append = T)
    
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
    		#align = c("l","l","l","c","c"),
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
    flag.newpage <- FALSE
  	cat("\\section*{Feature summary} \n", file = outFileCon, append = T)
    
    if(nbinaries(object) > 0 ) {
      if (flag.newpage) {
        cat("\\newpage \n", file = outFileCon, append = T)
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
    		nbNA <- c(nbNA, nmissings(vtemp))
    		theDistrib <- c(theDistrib, distrib(vtemp, percent = T, format = T, chlength = values.chlength))
    		#theSD <- c(theSD, sd(vtemp, na.rm = TRUE))
  	  }
    
      nbNA[which(is.na(nbNA))] <- 0
      N <- rep(nTuples, nbinaries(object)) - nbNA
      NApourcent <- nbNA / nTuples * 100
    
    	df <- data.frame(scvar.names, descriptions,N, NApourcent, theDistrib)
    	names(df) <- c("Variable", "Description", "N", "NA (%)", "Distribution (%)")
      row.names(df) <- index(object, scvar.names)
      cat("{\\footnotesize \n", file = outFileCon, append = T)
  
    	object.xtable <- xtable(
    		df,
    		label='featureSummary',
    		caption='Binary variables summary',
    		digits = 3,
    		#align = c("l","l","l","c","c"),
    		display = c("d","s","s","d","fg","s")
    	)
  
  	  print(object.xtable, file=outFileCon , append=T, tabular.environment='longtable', table.placement = "htb", floating=F) 
      cat("} \n", file = outFileCon, append = T)
      flag.newpage <- TRUE
    }
    
    if(nordinals(object) > 0 ) {
      if (flag.newpage) {
        cat("\\newpage \n", file = outFileCon, append = T)
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
    		nbNA <- c(nbNA, nmissings(vtemp))
    		theNlevels <- c(theNlevels, nvalids(vtemp))
    		theDistrib <- c(theDistrib, distrib(vtemp, percent = T, format = T, chlength = values.chlength))
    		#theSD <- c(theSD, sd(vtemp, na.rm = TRUE))
  	  }
    
      nbNA[which(is.na(nbNA))] <- 0
      N <- rep(nTuples, nordinals(object)) - nbNA
      NApourcent <- nbNA / nTuples * 100
    
    	df <- data.frame(scvar.names, descriptions,N, NApourcent, theNlevels, theDistrib)
    	names(df) <- c("Variable", "Description", "N", "NA (%)", "Classes", "Distribution (%)")
      row.names(df) <- index(object, scvar.names)
      cat("{\\footnotesize \n", file = outFileCon, append = T)
  
    	object.xtable <- xtable(
    		df,
    		label='featureSummary',
    		caption='Ordinal variables summary',
    		digits = 3,
    		#align = c("l","l","l","c","c"),
    		display = c("d","s","s","d","fg","d","s")
    	)
  
  	  print(object.xtable, file=outFileCon , append=T, tabular.environment='longtable', table.placement = "htb", floating=F) 
      cat("} \n", file = outFileCon, append = T)
      flag.newpage <- TRUE
    }
    
    if(nnominals(object) > 0 ) {
      if (flag.newpage) {
        cat("\\newpage \n", file = outFileCon, append = T)
      }
      cat("\\subsection*{Nominal variables} \n", file = outFileCon, append = T)
      scvar <- nominals(object)
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
    		nbNA <- c(nbNA, nmissings(vtemp))
    		theNlevels <- c(theNlevels, nvalids(vtemp))
    		theDistrib <- c(theDistrib, distrib(vtemp, percent = T, format = T, chlength = values.chlength))
    		#theSD <- c(theSD, sd(vtemp, na.rm = TRUE))
  	  }
    
      nbNA[which(is.na(nbNA))] <- 0
      N <- rep(nTuples, nnominals(object)) - nbNA
      NApourcent <- nbNA / nTuples * 100
    
    	df <- data.frame(scvar.names, descriptions,N, NApourcent, theNlevels, theDistrib)
    	names(df) <- c("Variable", "Description", "N", "NA (%)", "Classes", "Distribution (%)")
      row.names(df) <- index(object, scvar.names)
      cat("{\\footnotesize \n", file = outFileCon, append = T)
  
    	object.xtable <- xtable(
    		df,
    		label='featureSummary',
    		caption='Nominal variables summary',
    		digits = 3,
    		#align = c("l","l","l","c","c"),
    		display = c("d","s","s","d","fg","d","s")
    	)
  
  	  print(object.xtable, file=outFileCon , append=T, tabular.environment='longtable', table.placement = "htb", floating=F) 
      cat("} \n", file = outFileCon, append = T)
      flag.newpage <- TRUE
    }
    
    if(nscales(object) > 0 ) {
      if (flag.newpage) {
        cat("\\newpage \n", file = outFileCon, append = T)
      }
      cat("\\subsection*{Scale variables} \n", file = outFileCon, append = T)
      scvar <- scales(object)
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
    		nbNA <- c(nbNA, nmissings(vtemp))
    		theMin <- c(theMin, min(vtemp, na.rm = TRUE))
    		theMax <- c(theMax, max(vtemp, na.rm = TRUE))
    		theMean <- c(theMean, mean(vtemp, na.rm = TRUE))
    		#theSD <- c(theSD, sd(vtemp, na.rm = TRUE))
  	  }
    
      nbNA[which(is.na(nbNA))] <- 0
      N <- rep(nTuples, nscales(object)) - nbNA
      NApourcent <- nbNA / nTuples * 100
    
    	df <- data.frame(scvar.names, descriptions,N, NApourcent, theMin, theMax, theMean)
    	names(df) <- c("Variable", "Description", "N", "NA (%)", "Min", "Max", "Mean")
      # row.names(df) <- 1:nrow(df)
      row.names(df) <- index(object, scvar.names)
      cat("{\\footnotesize \n", file = outFileCon, append = T)
  
    	object.xtable <- xtable(
    		df,
    		label='featureSummary',
    		caption='Scale variables summary',
    		digits = 3,
    		#align = c("l","l","l","c","c"),
    		display = c("d","s","s","d","fg","fg","fg","fg")
    	)
  
  	  print(object.xtable, file=outFileCon , append=T, tabular.environment='longtable', table.placement = "htb", floating=F) 
      cat("} \n", file = outFileCon, append = T)
      flag.newpage <- TRUE
    }
       
    if(ntimes(object) > 0 ) {
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
    		nbNA <- c(nbNA, nmissings(vtemp))
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
      row.names(df) <- index(object, scvar.names)
      cat("{\\footnotesize \n", file = outFileCon, append = T)
  
    	object.xtable <- xtable(
    		df,
    		label='featureSummary',
    		caption='Timestamp variables summary',
    		digits = 3,
    		#align = c("l","l","l","c","c"),
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


# as.data.frame
setMethod("as.data.frame", "Dataset", 
  definition = function (x) {
  names <- names(x)
	out <- as.data.frame(lapply(variables(x), as.data.frame))
	names(out) <- names
  row.names(out) <- row.names(x)
	return(out)
  }
)

setMethod("v", "Dataset", 
  function(x) {
	  return(as.data.frame(x))
  }
)

setAs("Dataset", "data.frame", 
  function(from) {
		return(as.data.frame(from))
	}
)

setMethod(
  f = "distrib",
  signature = "Dataset", 
  definition = function (object, missings.omit, percent, sorting, format, digits, chlength, sep, cut) {
    out <- qualitatives(object)
    names <- names(out)
    
    m <- getMethod("distrib", "CategoricalVariable")
    if (missing(sorting)) {
      out <- mapply(
        m,
        variables(out),
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
  "Dataset",
  function (object, name) {
    #dir.create(name)
    assign(make.names(name(object)), object)
    c <- call('save', name(object), file = paste(name, ".RData", sep = ''))
    eval(c)
#     save(object, file = paste(name, ".RData", sep = ''))
    summaryToPDF(object, name)
  }
)

setMethod(
  "subset",
  "Dataset",
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