#=================================================================================================
# CLASS DEFINITION FILE: Dataset
#=================================================================================================


setClass(
  "Dataset",
	representation(
		name = "character",
		description = "character",
		variables = "list",
		row.names = "character",
    weights = "character",
    control = "character",
    infos = "list"
	),
	validity = function(object) {
		if(Dataset.globalenv$print.io) cat (" =>       Dataset: object validity check \n")
		flag = TRUE
    n <- length(row.names(object))
    
    lw <- length(slot(object, 'weights'))
    if(lw > 1){
      message("Only one weights variable name is expected")
      flag <- FALSE
    }
    if(lw == 1){
      if(!is.element(weighting(object), names(object))){
        message("The weights variable name given is not in the dataset")
        flag <- FALSE
      }
    }
    
    if (length(control(object)) > 0) {
      if(!all(control(object) %in% names(object))) {
        message("All control variables have to exist in the Dataset")
        flag <- FALSE
      }
    }
    #FIXME control variables have to be categorical?
    #FIXME control variables can't be weighting variables?
    
		for (v in variables(object)) {
			if (!inherits(v, "Variable")) {
				message(paste("One or more variable is a", class(v), "object. It should be a Variable object"))
				flag = FALSE
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

      
  		if (length(unique(names(object@variables))) != length(names(object@variables))) {
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
  infos,
  check.rows = FALSE # not used
) {
  if(Dataset.globalenv$print.io) cat(" => (in)  Dataset: builder \n")
  if (missing(x)) x <- list()
  if (missing(name)) name <- character()
  if (missing(description)) description <- character()
  #if (missing(row.names)) row.names <- character()
  if (missing(weights)) weights <- character()
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
  if(is.null(names(variables))) names(variables) <- 1:length(variables)
  names(variables) <- make.names(names(variables), unique = T)
      
  if(Dataset.globalenv$print.io) cat(" => (out) Dataset: builder \n")
  return(new(
    Class = "Dataset",
    name = name,
    variables = variables,
    row.names = row.names,
    weights = weights,
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

setMethod("control", "Dataset", 
  definition = function (object) { 
    #if(length(slot(object,'control')) == 0) {
    #  message("no control variables are defined, so all are used")
    #  return(svar(rep(1, nrow(object))))
    #} else {
      #return(variables(object)[slot(object, 'control')])
      return(slot(object, 'control'))
    #}
  }
)
setReplaceMethod(
  f = "control" ,
	signature = "Dataset" ,
	definition = function(object, value){
		object@control <- value
    validObject(object)
		return(object)
	}
)

setMethod("alldescriptions", "Dataset", 
  definition = function (object) {
    out <- mapply(description, variables(object))
    out <- data.frame(out)
    if(ncol(out) > 0) {
      row.names(out) <- names(object)
      names(out) <- "Description"
    }
    return(out)
  }
)

setMethod("variables", "Dataset", 
  definition = function (object, weighting) {
    if (weighting) {
      return(slot(object, "variables"))
    } else {
      
    }
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
		object@weights <- value
    validObject(object)
		return(object)
	}
)

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
  f ="[",
	signature ="Dataset",
	definition = function(x,i,j){
    #print('i');print(i)
    #print('j');print(j)
		listData <- variables(x)
    row.names <- row.names(x)
		if (!missing(j)) {
			listData <- listData[j]
		}
		if (!missing(i)){
      row.names <- row.names[i]
			for (k in 1:length(listData)) {
				listData[[k]] <- listData[[k]][i]
			}
      #representativity to control variables check
      lc <- length(control(x))
      if(lc > 0) {
       for (k in 1:lc) {
          var <- variables(x)[[control(x)[k]]]
          if (inherits(var, 'CategoricalVariable')) {
            #a <- table(v(weights(x)) * v(listData[[control(x)[k]]]))
            a <- table(v(listData[[control(x)[k]]]))
            b <- distrib(var)
            #print(a)
            #print(a/sum(a))
            #print(b)
            cs <- chisq.test(x = a, p = b)
            if(cs$p.value >= 0.05) {
              message(paste("=> control on ", control(x)[k], ': ok', sep = ''))
            } else {
              res <- cs$stdres
              res <- res[which(abs(res) > 1.96)]
              over <- which(res > 0)
              under <- which(res < 0)
              message(paste("=> control on ", control(x)[k], ': warning, p-value < 0.05', sep = ''))
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
		# print(listData)
		return(new("Dataset", 
        name = name(x),
        description = description(x),
				variables = listData,
        row.names = row.names,
        weights = weighting(x),
        control = control(x),
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
				return(new("Dataset", data = temp))
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
    out <- as.data.frame(object)
    names(out) <- str.names(object, parenthesis = T)
    print(out)
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
    control(object) <- character(0)
    weighting(object) <- character(0)
    subsetvar <- names(which(unlist(lapply(variables(object), is.quantitative))))
    variables(object) <- variables(object)[subsetvar]
    return(object)
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
    control(object) <- character(0)
    weighting(object) <- character(0)
    subsetvar <- names(which(unlist(lapply(variables(object), is.scale))))
    variables(object) <- variables(object)[subsetvar]
    return(object)
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
  "qualitatives",
  "Dataset",
  function (object) {
    control(object) <- character(0)
    weighting(object) <- character(0)
    subsetvar <- names(which(unlist(lapply(variables(object), is.qualitative))))
    variables(object) <- variables(object)[subsetvar]
    return(object)
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
    control(object) <- character(0)
    weighting(object) <- character(0)
    subsetvar <- names(which(unlist(lapply(variables(object), is.nominal))))
    variables(object) <- variables(object)[subsetvar]
    return(object)
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
  "ordinals",
  "Dataset",
  function (object) {
    control(object) <- character(0)
    weighting(object) <- character(0)
    subsetvar <- names(which(unlist(lapply(variables(object), is.ordinal))))
    variables(object) <- variables(object)[subsetvar]
    return(object)
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
    control(object) <- character(0)
    weighting(object) <- character(0)
    subsetvar <- names(which(unlist(lapply(variables(object), is.weighting))))
    variables(object) <- variables(object)[subsetvar]
    return(object)
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
    control(object) <- character(0)
    weighting(object) <- character(0)
    subsetvar <- names(which(unlist(lapply(variables(object), is.time))))
    variables(object) <- variables(object)[subsetvar]
    return(object)
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
    control(object) <- character(0)
    weighting(object) <- character(0)
    subsetvar <- names(which(unlist(lapply(variables(object), is.binary))))
    variables(object) <- variables(object)[subsetvar]
    return(object)
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
  signature = c("Dataset", "character"), 
  definition = function (object, ch) {
    subsetvar <- names(which(unlist(mapply(contains, variables(object), ch))))
    variables(object) <- variables(object)[subsetvar]
    return(object)
  }
)


    
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
  definition = function (object, pdfSavingName, graphics = FALSE, description.chlength, values.chlength, dateformat, keepTex = FALSE) {
  require(xtable)
  nTuples <- nrow(object)

	if (length(name(object)) == 0) { outName <- "Untitled Dataset" } else { outName <- name(object) }
	outName <- make.names(outName) # no spaces for Unix/Texlive compilation ?
	
	if(missing(pdfSavingName)) {		
		pdfSavingName <- paste("Summary-", outName, sep = "") # no spaces for Unix/Texlive compilation ?
	}
	
	latexFile <- paste(pdfSavingName, ".tex", sep="")
	
	cat("\\documentclass[landscape]{article} \n" , file = latexFile, append = F)
	latex.head(latexFile)
	cat("\\author{Generated by the R Dataset package} \n", file = latexFile, append = T)
	cat("\\title{Summary of the", totex(name(object)), "dataset} \n", file = latexFile, append = T)
	cat("\\begin{document} \n", file = latexFile, append = T)
	cat("\\maketitle \n", file = latexFile, append = T)
	cat("\\section*{Overview} \n", file = latexFile, append = T)
	cat("\\begin{itemize} \n", file = latexFile, append = T)
	cat("\\item Name:", totex(name(object)), "\n", file = latexFile, append = T)
	cat("\\item Description:", description(object), "\n", file = latexFile, append = T)
	#cat("\\item Object version:", oversion(object), "\n", file = latexFile, append = T)
	#cat("\\item Created by Dataset version:", pversion(object), "\n", file = latexFile, append = T)
	cat("\\item Number of variables: ", ncol(object), " (",
      nbinaries(object), " binaries, ",
      nordinals(object), " ordinals, ",
      nnominals(object), " nominals, ",
      nscales(object), " scales, ",
      ntimes(object), " timestamps",
      ")", "\n", sep = "", file = latexFile, append = T)
  cat("\\item Number of rows:", nTuples, "\n", file = latexFile, append = T)
  cat("\\item Percent of missing values:", missings(object)["nmissingspercent.cha"], "\\%", "\n", file = latexFile, append = T)
	cat("\\end{itemize} \n", file = latexFile, append = T)
  
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
       
  cat("\\begin{center} \n", file = latexFile, append = T)
  cat("\\begin{minipage}[c]{.35\\linewidth} \n", file = latexFile, append = T)
  print(object.xtable, file=latexFile , append=T, include.rownames = F, table.placement = "htb", floating=F) 
  cat("\\end{minipage} \n", file = latexFile, append = T)
  cat("\\begin{minipage}[c]{.40\\linewidth} \n", file = latexFile, append = T)
  cat("\\includegraphics[scale=0.40]{", plot.filename.pdf, "} \n", file = latexFile, append = T, sep = "")
  cat("\\end{minipage} \n", file = latexFile, append = T)
	cat("\\end{center} \n", file = latexFile, append = T)
       
  
  cat("\\newpage \n", file = latexFile, append = T)
  flag.newpage <- FALSE
	cat("\\section*{Feature summary} \n", file = latexFile, append = T)
  
  if(nbinaries(object) > 0 ) {
    if (flag.newpage) {
      cat("\\newpage \n", file = latexFile, append = T)
    }
    cat("\\subsection*{Binary variables} \n", file = latexFile, append = T)
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
    cat("{\\footnotesize \n", file = latexFile, append = T)

  	object.xtable <- xtable(
  		df,
  		label='featureSummary',
  		caption='Binary variables summary',
  		digits = 3,
  		#align = c("l","l","l","c","c"),
  		display = c("d","s","s","d","fg","s")
  	)

	  print(object.xtable, file=latexFile , append=T, tabular.environment='longtable', table.placement = "htb", floating=F) 
    cat("} \n", file = latexFile, append = T)
    flag.newpage <- TRUE
  }
  
  if(nordinals(object) > 0 ) {
    if (flag.newpage) {
      cat("\\newpage \n", file = latexFile, append = T)
    }
    cat("\\subsection*{Ordinal variables} \n", file = latexFile, append = T)
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
  		theNlevels <- c(theNlevels, nvalues(vtemp))
  		theDistrib <- c(theDistrib, distrib(vtemp, percent = T, format = T, chlength = values.chlength))
  		#theSD <- c(theSD, sd(vtemp, na.rm = TRUE))
	  }
  
    nbNA[which(is.na(nbNA))] <- 0
    N <- rep(nTuples, nordinals(object)) - nbNA
    NApourcent <- nbNA / nTuples * 100
  
  	df <- data.frame(scvar.names, descriptions,N, NApourcent, theNlevels, theDistrib)
  	names(df) <- c("Variable", "Description", "N", "NA (%)", "Classes", "Distribution (%)")
    row.names(df) <- index(object, scvar.names)
    cat("{\\footnotesize \n", file = latexFile, append = T)

  	object.xtable <- xtable(
  		df,
  		label='featureSummary',
  		caption='Ordinal variables summary',
  		digits = 3,
  		#align = c("l","l","l","c","c"),
  		display = c("d","s","s","d","fg","d","s")
  	)

	  print(object.xtable, file=latexFile , append=T, tabular.environment='longtable', table.placement = "htb", floating=F) 
    cat("} \n", file = latexFile, append = T)
    flag.newpage <- TRUE
  }
  
  if(nnominals(object) > 0 ) {
    if (flag.newpage) {
      cat("\\newpage \n", file = latexFile, append = T)
    }
    cat("\\subsection*{Nominal variables} \n", file = latexFile, append = T)
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
  		theNlevels <- c(theNlevels, nvalues(vtemp))
  		theDistrib <- c(theDistrib, distrib(vtemp, percent = T, format = T, chlength = values.chlength))
  		#theSD <- c(theSD, sd(vtemp, na.rm = TRUE))
	  }
  
    nbNA[which(is.na(nbNA))] <- 0
    N <- rep(nTuples, nnominals(object)) - nbNA
    NApourcent <- nbNA / nTuples * 100
  
  	df <- data.frame(scvar.names, descriptions,N, NApourcent, theNlevels, theDistrib)
  	names(df) <- c("Variable", "Description", "N", "NA (%)", "Classes", "Distribution (%)")
    row.names(df) <- index(object, scvar.names)
    cat("{\\footnotesize \n", file = latexFile, append = T)

  	object.xtable <- xtable(
  		df,
  		label='featureSummary',
  		caption='Nominal variables summary',
  		digits = 3,
  		#align = c("l","l","l","c","c"),
  		display = c("d","s","s","d","fg","d","s")
  	)

	  print(object.xtable, file=latexFile , append=T, tabular.environment='longtable', table.placement = "htb", floating=F) 
    cat("} \n", file = latexFile, append = T)
    flag.newpage <- TRUE
  }
  
  if(nscales(object) > 0 ) {
    if (flag.newpage) {
      cat("\\newpage \n", file = latexFile, append = T)
    }
    cat("\\subsection*{Scale variables} \n", file = latexFile, append = T)
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
    cat("{\\footnotesize \n", file = latexFile, append = T)

  	object.xtable <- xtable(
  		df,
  		label='featureSummary',
  		caption='Scale variables summary',
  		digits = 3,
  		#align = c("l","l","l","c","c"),
  		display = c("d","s","s","d","fg","fg","fg","fg")
  	)

	  print(object.xtable, file=latexFile , append=T, tabular.environment='longtable', table.placement = "htb", floating=F) 
    cat("} \n", file = latexFile, append = T)
    flag.newpage <- TRUE
  }
     
  if(ntimes(object) > 0 ) {
    cat("\\subsection*{Timestamp variables} \n", file = latexFile, append = T)
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
    cat("{\\footnotesize \n", file = latexFile, append = T)

  	object.xtable <- xtable(
  		df,
  		label='featureSummary',
  		caption='Timestamp variables summary',
  		digits = 3,
  		#align = c("l","l","l","c","c"),
  		display = c("d","s","s","d","fg","fg","fg","fg","fg")
  	)

	  print(object.xtable, file=latexFile , append=T, tabular.environment='longtable', table.placement = "htb", floating=F) 
    cat("} \n", file = latexFile, append = T)
    flag.newpage <- TRUE
  }
    

  cat("\\end{document} \n", file = latexFile, append = T)
	tools::texi2dvi(latexFile,pdf=T)

	# clean directory
	if (keepTex) {
		extensionsToRemove <- ".(log|aux)"
	} else {
		extensionsToRemove <- ".(log|aux|tex)"
	}

	tempTex <- list.files(
		##paste(datadir, wavesFolder, "-SPSS", "/", i, sep = ""),
		getwd(),
		pattern = paste("^", pdfSavingName, extensionsToRemove, sep = "")
	)
	#tempTex <- tempTex[-grep(".pdf$", tempTex)]
	# keepLatex = TRUE (false by default)
	# tempTex <- tempTex[-grep(".tex$", tempTex)]
	
	unlink(tempTex)
  unlink(plot.filename.pdf)
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
    save(object, file = paste(name, ".RData", sep = ''))
    summaryToPDF(object, name)
  }
)