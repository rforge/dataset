removeEmptyValueLabels <- function(values) {
  empty <- which(names(values) == "")
  if (length(empty) > 0 ) {
    values <- values[-empty]
  }
  return(values)
}
# vl1 <- c(1,2,3,4)
# names(vl1) <- c("a", "b", "c", "d")
# removeEmptyValueLabels(vl1)
# vl2 <- c(1,2,3,4)
# names(vl2) <- c("a", "", "c", "d")
# removeEmptyValueLabels(vl2)
# vl3 <- c(1,2,3,4)
# names(vl3) <- c("", "", "", "")
# removeEmptyValueLabels(vl3)


#' Import a SPSS database into a \code{Rsocialdata} database.
#' 
#' The \code{get.spss.file} is the standard function to import a SPSS database (in a \code{.sav} file) into a \code{Rsocialdata} database. The function is based on the \code{\link[foreign]{read.spss}} function. The \code{\link[foreign]{read.spss}} function is run two times to get both coding and labels of both variables and values. The measure of each imported variable (categorical or scale) is automatically detected. If a variable has all its values (valid cases and missing values) labelised, the variable will be set as a categorical variable. If a variable has at least one of its values (valid cases or missing values) with no label, the variable will be set as a scale variable.
#' 
#' @param file the full name of the file you want to import
#' @param datadir the directory where your SPSS file is. If missing the working directory is used.
#' @param variables the list of variables you want to import. If missing all variables in the SPSS file are imported.
#' @param tsvar a vector containing the names of variables having to be converted in the \code{TimestampVariable} type. The SPSS timestamp origin "1582-10-14" is used to set the origin of the time variable.
#' @param ovara vector containing the names of variables having to be converted in the \code{OrdinalVariable} type.
#' @param wvara vector containing the names of variables having to be converted in the \code{WeightingVariable} type.
#' @param use.coding.order a character, either \code{"increasing"}, \code{"decreasing"}, \code{"default"}. If \code{"increasing"} the coding order is used to sort the order in the values of the Variable. If \code{"decreasing"} the order is in a decreasing way. If \code{"default"} no sorting is performed. Default is \code{"increasing"}.
#' @param max.value.labels depreciated. Will be soon removed.
#' @param savingName a character. By setting this argument you produce an export in .Rdata of the SPSS file with the name given.
#' @param lowernames a logical. If \code{TRUE} all names are set in lower case.
#' @param name a character. The name of the data base. If \code{NULL} the name of the file is used.
#' @param description a character. A description of the database.
#' @param exportPDF a logical. If \code{TRUE} the summary in PDF of the data base is created.
#' @param reencode a logical: should character strings be re-encoded to the current locale. Default is \code{TRUE}. Alternatively character, specifying an encoding to assume.
#' @return a \code{\link{Rsocialdata-class}} object.
#' @export
get.spss.file <- function(
  file,
  datadir, 
	variables = NULL,
	tsvar = character(0),
  ovar = character(0),
  wvar = character(0),
  use.coding.order = "increasing",
	max.value.labels = Inf,
	savingName = NULL,
	lowernames = TRUE,
	name = NULL,
	description = character(0),
	exportPDF = TRUE,
  reencode = "latin1"
) {
	ptm <- Sys.time()
# 	warn.user <- options("warn")
# 	options(warn = -1) # suppress warnings
# 	on.exit(options(warn.user)) # restore warnings
	warn.user <- options(warn = -1) # suppress warnings
	on.exit(options(warn.user)) # restore warnings
  
  print.comments.user <- Rsocialdata.globalenv$print.comments
  if(print.comments.user < Rsocialdata.globalenv$monitoring) {
	  Rsocialdata.globalenv$print.comments <- Rsocialdata.globalenv$monitoring
  }
  
  if(!is.installed.pkg('foreign')) {
    exit.by.uninstalled.pkg('foreign')
  } else {
  	require('foreign')
  	#require('R.utils')
    
    if (missing(tsvar)) tsvar <- character(0)
    if (missing(ovar)) ovar <- character(0)
  	
    #sub(paste(.Platform$file.sep,'$', sep=''), '', 'cc//')
    if(missing(datadir)) datadir <- getwd()
    path <- file.path(datadir,file)
    path <- sub(paste(.Platform$file.sep,.Platform$file.sep, sep=''), .Platform$file.sep, path)
  	spssdata <- read.spss(
  		path,
  		use.value.labels = FALSE,
  		to.data.frame = TRUE,
  		use.missings = FALSE,
  		reencode = reencode
  	)
  	
  	variable.labels <- attr(spssdata, "variable.labels")
  	
  	#on converti les noms de variable en noms de variables valides pour R (remplacement des $$ en particulier) (déjà fait par read.spss ?)
  	if(!is.null(variables)) {
  		variablesExist <- variables %in% tolower(names(spssdata))
  		if(FALSE %in% variablesExist)
  			stop(paste("Rsocialdata::getSPSSfile    ", variables[which(variablesExist == FALSE)], "contain(s) some names not in the file"));
  	}
  	names(spssdata) <- make.names(names(spssdata))
  	names(variable.labels) <- make.names(names(variable.labels))
  	if (lowernames) {
  		names(spssdata) <- tolower(names(spssdata))
  		names(variable.labels) <- tolower(names(variable.labels))
  	}
  	
  	## liste qui construira l'objet Rsocialdata
  	l <- list();
  	
  	
  	
  	if(missing(variables)) variables <- names(spssdata)
    
  	counter <- 0
    
  	cons.counter <- cons.counter.new(
  	  txt.before = 'Importing variables... ',
  	  txt.after = paste('/', length(variables), sep='')
  	)
#   	cons.counter <- cons.counter.new(
#   	  txt.before = 'a',
#   	  txt.after = paste('/', length(variables), sep='')
#   	)
    
  	
  	for (v in variables) {
  	  
  	  counter <- counter + 1;
      
  	  if(Rsocialdata.globalenv$print.comments <= Rsocialdata.globalenv$monitoring){
  	    cons.counter <- cons.counter.add.one(cons.counter)
  	    cons.counter.print(cons.counter)
        
#   	    message(v, ' : ', appendLF=F)
#   	    flag <- F
#   	    
#   	    message(paste("Loading", v))
  	  }
  	  
  	
  		vtemp <- spssdata[[v]]
      codes <- vtemp
      attributes(codes) <- NULL # we remove all attributes: codes must contains only numerics codes
      var.description <- variable.labels[v]
  		value.labels.all <- as.numeric(attr(vtemp,'value.labels'))
      names(value.labels.all) <- names(attr(vtemp,'value.labels'))
      value.labels.all <- removeEmptyValueLabels(value.labels.all)
      value.labels <- value.labels.all[which(value.labels.all >= 0)] # positive codes
      if(use.coding.order == "increasing" ) value.labels <- sort(value.labels)
      if(use.coding.order == "decreasing" ) value.labels <- sort(value.labels, decreasing = T)
      uval <- unique(vtemp)
  		#missings <- union(
      #  uval[which(uval < 0)], # negative codes appearing in data
      #  value.labels[which(value.labels < 0)] # negative codes appearing in values
      #)
      missings <- value.labels.all[which(value.labels.all < 0)] # negative codes appearing in values
      #HERE I ADD THE UNSPECIFIED MISSINGS ???
      
      if (is.element(v, tsvar)) {
  			l[[counter]] <- tvar(
  				x = codes,
  				description = var.description,
  				values = value.labels,
  				missings = missings,
  				origin = "1582-10-14"
        )
        flag <- T
  		} else {
      
        if (is.element(v, wvar)) {
          l[[counter]] <- wvar(
            x = codes,
            description = var.description,
            values = value.labels,
            missings = missings
          )
          flag <- T
        } else {    
      		if (length(uval) > max.value.labels) { # then scale
            
      			l[[counter]] <- svar(
      				x = codes,
      				description = var.description,
      				values = value.labels,
      				missings = missings)
      			flag <- T
            
      		} else {
      			if (all(uval %in% union(value.labels, missings))) {# then qualitative
              if (length(value.labels) == 2) { #then binary
        				l[[counter]] <- bvar(
        					x = codes,
        					description = var.description,
        					values = value.labels,
        					missings = missings)
        				flag <- T
              } else { # then not binary
                if (is.element(v, ovar)) { # then ordinal
                  l[[counter]] <- ovar( 
            			x = codes,
          				description = var.description,
          				values = value.labels,
          				missings = missings)
                  flag <- T
                } else { # then nominal
                  l[[counter]] <- nvar( 
              		x = codes,
          				description = var.description,
          				values = value.labels,
          				missings = missings)
                  flag <- T
                }
              }
      			} else { #nothing matched, we try scale
      			l[[counter]] <- svar(
      				x = codes,
      				description = var.description,
      				values = value.labels,
      				missings = missings)
      			flag <- T
      		  }
      		}
        }
  		}
      stopifnot(flag)
  	}
  
  	if(Rsocialdata.globalenv$print.comments <= Rsocialdata.globalenv$monitoring){
  	  cons.counter.print.finish(cons.counter)
  	}
  	
    
    names(l) <- variables
  	
  	if (!is.null(name)){
  		outName <- name
  	} else {
  		outName <- file
  	}
  		
  	out <- dataset(
  		x = l,
  		name = outName,
      description = description
  	)
  	
  	
  	# sauvegarde dans un fichier si demandé
  	if (!is.null(savingName)) {
  		fileName <- paste(savingName, ".RData", sep="")
  		.localStuff <- new.env()
  		assign(savingName, out, envir = .localStuff)
  		eval(call("save", savingName, file = fileName, envir = .localStuff))
  		#print(ls(envir = .localStuff))
  		#assign(savingName, out, envir = .GlobalEnv)
  		#save(get(savingName, envir = .GlobalEnv), file = fileName)
  		#save(get(savingName, envir = .localStuff), file = fileName)
  		#saveObject(out, file = fileName)
  		#save(out, list = savingName, file = fileName)
  		message(paste("Data set saved in ", getwd(), "/", fileName, sep=""))
  	}
  	
  	if(exportPDF) {
  		if (!missing(savingName)) {
  			#exportPDF(out, pdfSavingName = savingName)
  		} else {
  			#exportPDF(out)
  		}
  	}
  	
  	if(Rsocialdata.globalenv$print.comments <= Rsocialdata.globalenv$monitoring){
  	  duration <- Sys.time() - ptm
  	  message(paste("Duration:", format(duration)))
  	}
    
  	Rsocialdata.globalenv$print.comments <- print.comments.user
  	return(out)
  }
}
