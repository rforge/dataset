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

get.spss.file <- function(
  datadir,
	file, 
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
	summaryToPDF = TRUE,
  reencode = "latin1"
) {
	ptm <- proc.time()

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
  			stop(paste("Dataset::getSPSSfile    ", variables[which(variablesExist == FALSE)], "contain(s) some names not in the file"));
  	}
  	names(spssdata) <- make.names(names(spssdata))
  	names(variable.labels) <- make.names(names(variable.labels))
  	if (lowernames) {
  		names(spssdata) <- tolower(names(spssdata))
  		names(variable.labels) <- tolower(names(variable.labels))
  	}
  	
  	## liste qui construira l'objet Dataset
  	l <- list();
  	
  	counter <- 0;
  	
  	if(missing(variables)) variables <- names(spssdata)
  	for (v in variables) {
      flag <- F
      
      message(paste("Loading", v))
      
  		counter <- counter + 1;
  	
  		vtemp <- spssdata[[v]]
      codes <- vtemp
      attributes(codes) <- NULL # we remove all attributes: codes must contains only numerics codes
      description <- variable.labels[v]
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
      
      if (is.element(v, tsvar)) {
  			l[[counter]] <- tvar(
  				x = codes,
  				description = description,
  				values = value.labels,
  				missings = missings,
  				origin = "1582-10-14"
        )
        flag <- T
  		}
      
      if (is.element(v, wvar)) {
        l[[counter]] <- wvar(
          x = codes,
          description = description,
          values = value.labels,
          missings = missings
        )
        flag <- T
      } else {    
    		if (length(uval) > max.value.labels) { # then scale
          
    			l[[counter]] <- svar(
    				x = codes,
    				description = description,
    				values = value.labels,
    				missings = missings)
    			flag <- T
          
    		} else {
    			if (all(uval %in% union(value.labels, missings))) {# then qualitative
            if (length(value.labels) == 2) { #then binary
      				l[[counter]] <- bvar(
      					x = codes,
      					description = description,
      					values = value.labels,
      					missings = missings)
      				flag <- T
            } else { # then not binary
              if (is.element(v, ovar)) { # then ordinal
                l[[counter]] <- ovar( 
          			x = codes,
        				description = description,
        				values = value.labels,
        				missings = missings)
                flag <- T
              } else { # then nominal
                l[[counter]] <- nvar( 
            		x = codes,
        				description = description,
        				values = value.labels,
        				missings = missings)
                flag <- T
              }
            }
    			} else { #nothing matched, we try scale
    			l[[counter]] <- svar(
    				x = codes,
    				description = description,
    				values = value.labels,
    				missings = missings)
    			flag <- T
    		  }
    		}
      }
      stopifnot(flag)
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
  	
  	if(summaryToPDF) {
  		if (!missing(savingName)) {
  			#summaryToPDF(out, pdfSavingName = savingName)
  		} else {
  			#summaryToPDF(out)
  		}
  	}
  	
  	duration <- proc.time() - ptm
  	message(paste("Duration:", duration[1]))
  	return(out)
  }
}
