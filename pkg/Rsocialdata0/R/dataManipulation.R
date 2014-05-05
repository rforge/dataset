#####################################################################
## CBIND
#####################################################################

#setMethod(
#  f = "cbind",
#  signature = "ANY", 
#  definition = function (..., deparse.level = 1) {
#    return(2)

#  }
#)

cunion <- function (..., deparse.level = 1) {
	y = list(...)
  stopifnot(length(y) > 0)
  if(!all(unlist(mapply(inherits, y, 'Dataset'))))
    stop("[Dataset::cunion] all object must inherit of class 'Dataset'")
	#verifier que l'union des names est unique
	#verifier si on donne une column
  
	outData <- list()
  count <- 1
	for (i in y) {
		#print(names(i))
		#print(class(i))
		#i <- as.Dataset(i)
    if (count==1)
      r <- row.names(i)
    #print(r)
    if (count > 1){
      stopifnot(row.names(i)==r)
      r <- row.names(i)
    }
		outData = c(outData, slot(i, "variables"))
	}
	#print(names(outData))
  
	out <- dataset(x = outData, row.names = row.names(y[[1]]))
	return(out)
}

# p <- merge(x,y, by = 'idpers')
# p <- merge(x,y)
setMethod(
  f = "merge",
  signature = c("Dataset", "Dataset"), 
  definition = function (x, y, ...) {
    
  	dots <- list(...)
    
    by.flag <- FALSE
    if('by' %in% names(dots)) {
      by.flag <- TRUE
      mergeBy.x <- dots$by
  	  mergeBy.y <- dots$by
    }
    else {
      mergeBy.x <- mergeBy.y <- intersect(names(x), names(y))
    }
    if('by.x' %in% names(dots)) {
      by.flag <- TRUE
      mergeBy.x <- dots$by.x # if by.x is provided, by is ignored, default behavior of the native merge function
    }
    if('by.y' %in% names(dots)) {
      by.flag <- by.flag && TRUE
      mergeBy.y <- dots$by.y # if by.y is provided, by is ignored, default behavior of the native merge function
    }
    
    if(!by.flag)
      warning("You should explicitly provide the variables to used as the primary key.")
    
    nomerging.names.x <- setdiff(names(x), mergeBy.x)
  	nomerging.names.y <- setdiff(names(y), mergeBy.y)
  	nomerging.names.all <- c(nomerging.names.x, nomerging.names.y)
    if(length(nomerging.names.all) == 0) {
      stop(paste(
        "All variables are used as the primary key for merging. There is no variable to merge.\n",
        sep = ' '
      ))
    }
  	if(length(unique(nomerging.names.all)) != length(nomerging.names.all)) {
#   	  stop("The merge method for Dataset objects expect names in x and y are unique. \n Please renames concerned variables.")
      warning(paste("The merge method for Dataset objects expect that names in x and y are unique. \n Names will be make unique, but you better have to make names unique before using the merge method.\n Variable concerned are:", paste(intersect(nomerging.names.x, nomerging.names.y), collapse = ', ')))
      names.in.x <- match(nomerging.names.x, names(x))
      names.in.y <- match(nomerging.names.y, names(y))
      names.unique <- make.unique(c(names(x), names(y)))
      names.unique.x <- names.unique[1:length(names(x))]
      names.unique.y <- names.unique[(length(names(x))+1):length(names.unique)]
      names(x)[names.in.x] <- names.unique.x[names.in.x]
      names(y)[names.in.y] <- names.unique.y[names.in.y]
  	}
       
  	dataMerge <- merge.data.frame(cbind(as.data.frame(x), flagX = 1:nrow(x)), cbind(as.data.frame(y), flagY = 1:nrow(y)), ... = ...)
  	#print(names(dataMerge))
  	#print(dataMerge[1:20,])
    
    
  	tuplesX <- dataMerge$flagX
  	tuplesY <- dataMerge$flagY
    tuplesAll <- data.frame('tX' = tuplesX, "tY" = tuplesY)
    
    tuplesX.n <- length(tuplesX)
    tuplesY.n <- length(tuplesY)
    stopifnot(tuplesX.n == tuplesY.n)

    na.in.tuplesX <- any(is.na(tuplesX))
    na.in.tuplesY <- any(is.na(tuplesY))
    # if there exist a row for which we have to add a new missing value, we then add a new missing code for all variables
    new.miss.code <- NULL
    if(na.in.tuplesX || na.in.tuplesY) {
      new.miss.code <- Dataset:::.missing.gen.candidate.vector(
        valids = unique(c(
          unlist(allvalids(x)),
          unlist(allvalids(y))
        )),
        missings = unique(c(
          unlist(allmissings(x)),
          unlist(allmissings(y))
        ))
      )
    }
    
#     return(list(tuplesX, tuplesY))
    
    newvars.x <- variables(x[,intersect(names(dataMerge), names(x))])
    newvars.y <- variables(y[,setdiff(intersect(names(dataMerge), names(y)), mergeBy.y)])
    
    if(!is.null(new.miss.code)) {
      newvars.x <- mapply(missing.add, newvars.x, "Individual added by a merging operation", new.miss.code)
      newvars.y <- mapply(missing.add, newvars.y, "Individual added by a merging operation", new.miss.code)
    }
    
    for (k in names(newvars.x)) {
      oldcodes <- codes(newvars.x[[k]])
      #         print(oldcodes)
      #         print(tuplesX)
      codestokeep <- which(!is.na(tuplesX)) #FIXME: if there is only NA ?
      
      if(!is.null(new.miss.code)) {
        newcodes <- rep(new.miss.code, tuplesX.n)
        #         print(newcodes)
        #         print(codestokeep)
        newcodes[codestokeep] <- oldcodes[tuplesX[codestokeep]]
      } else {
        newcodes <- oldcodes[tuplesX[codestokeep]]
      }
      
#         print(newcodes)
      if(k %in% mergeBy.x) {
        if (length(which(is.na(tuplesX))) > 0)
          newcodes[which(is.na(tuplesX))] <- tuplesY[which(is.na(tuplesX))]
      }
      newvar <- newvars.x[[k]]
      codes(newvar) <- newcodes
      newvars.x[[k]] <- newvar
      
    }
    for (k in names(newvars.y)) {
      
      oldcodes <- codes(newvars.y[[k]])
      #         print(oldcodes)
      #         print(tuplesX)
      codestokeep <- which(!is.na(tuplesY)) #FIXME: if there is only NA ?
      
      if(!is.null(new.miss.code)) {
        newcodes <- rep(new.miss.code, tuplesY.n)
        #         print(newcodes)
        #         print(codestokeep)
        newcodes[codestokeep] <- oldcodes[tuplesY[codestokeep]]
      } else {
        newcodes <- oldcodes[tuplesY[codestokeep]]
      }
      
      newvar <- newvars.y[[k]]
      codes(newvar) <- newcodes
      newvars.y[[k]] <- newvar
    }


    newvars.all <- c(newvars.x, newvars.y)
    names(newvars.all) <- make.names(names(newvars.all), unique = TRUE)
    
    return(new(
      'Dataset',
      variables = newvars.all,
      row.names = row.names(tuplesAll),
      description = "Created by the merge of X and Y",
      Dataset.version = Dataset:::Dataset.globalenv$Dataset.version
    ))

  	return(out)
  }
)

# x1 <- dataset(data.frame(idx= c(2,3), var1=c("a","b")))
# missings(x1$var1) <- c("No answer" = -1)
# x2 <- dataset(data.frame(idx= c(1,2), var2=c("d","e")))
# missings(x2$var2) <- c("I don't know" = -3)
# x1;x2;
# merge(x1,x2)
# merge(x1,x2, by='idx')
# merge(x1,x2, by='idx', all=T)
# merge(x1,x2, by='idx', all.x=T)
# merge(x1,x2, by='idx', all.y=T)
# 
# 
# x1 <- dataset(data.frame(idx= c(2,3), var1=c("a","b")))
# missings(x1$var1) <- c("No answer" = -1)
# x2 <- dataset(data.frame(idx= c(1,2), var1=c("d","e")))
# missings(x2$var1) <- c("I don't know" = -3)
# x1;x2;
# merge(x1,x2)
# merge(x1,x2, by='idx')
# merge(x1,x2, by='idx', all=T)
# merge(x1,x2, by='idx', all.x=T)
# merge(x1,x2, by='idx', all.y=T)

#merge <- function (x, y, ...) {
	# si deux objets Dataset on lance le merge Dataset, sinon on lance le merge normal
#}

merge.row <- function(x,y) {
  out <- x
  for (i in names(out)) {
    var <- out[[i]]
    codes(var) <- c(codes(x[[i]]),codes(y[[i]]))
    eval(parse(text=paste0("out$", i, ' <- var')))
  }
  row.names(out) <- as.character(1:(nrow(x)+nrow(y)))
  validObject(out)
  return(out)
}