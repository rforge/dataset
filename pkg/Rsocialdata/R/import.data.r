import.data <- function(
  file,
  datadir, 
  variables = NULL,
  tsvar = character(0),
  ovar = character(0),
  wvar = character(0),
  use.coding.order = "increasing",
  savingName = NULL,
  lowernames = TRUE,
  name = NULL,
  description = character(0),
  exportPDF = TRUE,
  spss.max.value.labels = Inf, # only spss
  spss.reencode = "latin1", # only spss
  file.type = 'auto'
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
    
    if (missing(tsvar)) tsvar <- character(0)
    if (missing(ovar)) ovar <- character(0)
    
    if(missing(datadir)) datadir <- getwd()
    path <- file.path(datadir,file)
    path <- sub(paste(.Platform$file.sep,.Platform$file.sep, sep=''), .Platform$file.sep, path)
    
    file.splitted <- strsplit(file, '\\.')[[1]]
    file.extension <- tolower(file.splitted[length(file.splitted)])
    
    file.type <- tolower(file.type)
    file.type.flag <- FALSE
    
    if(file.type == 'stata' || (file.type == 'auto' && file.extension == 'dta')) {
      file.type.flag <- TRUE
      foreigndata <- read.dta(
        file=path,
        convert.factors = FALSE
      )
      variable.labels <- attr(foreigndata, "var.labels")
    }
    if(file.type == 'spss' || (file.type == 'auto' && file.extension == 'sav')) {
      file.type.flag <- TRUE
      foreigndata <- read.spss(
        path,
        use.value.labels = FALSE,
        to.data.frame = TRUE,
        use.missings = FALSE,
        reencode = spss.reencode
      )
      variable.labels <- attr(foreigndata, "variable.labels")
    }
    if(!file.type.flag) stop('Unrecognized file extension. Please used the file.type argument to specify the type of the file.')
    
    
    
    #on converti les noms de variable en noms de variables valides pour R (remplacement des $$ en particulier) (déjà fait par read.spss ?)
    if(!is.null(variables)) {
      variablesExist <- variables %in% tolower(names(foreigndata))
      if(FALSE %in% variablesExist)
        stop(paste("import.data:    ", variables[which(variablesExist == FALSE)], "contain(s) some names not in the file"));
    }
    names(foreigndata) <- make.names(names(foreigndata))
    names(variable.labels) <- make.names(names(variable.labels))
    if (lowernames) {
      names(foreigndata) <- tolower(names(foreigndata))
      names(variable.labels) <- tolower(names(variable.labels))
    }
    
    ## liste qui construira l'objet Rsocialdata
    l <- list();
    
    return('ok')
    
    if(missing(variables)) variables <- names(foreigndata)
    
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
      
      
      vtemp <- foreigndata[[v]]
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
          if (length(uval) > spss.max.value.labels) { # then scale
            
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
