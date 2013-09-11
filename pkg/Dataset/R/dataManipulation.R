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
    #verifier que l'union des names est unique
    #verifier si on donne une column
    allnames <- union(names(x), names(y))
    if(length(unique(allnames)) != length(allnames)) {
      stop("The merge method for Dataset objects expect names in x and y are unique. \n Please renames concerned variables.")
    }
    
  	dots <- list(...)
#     if(is.null(dots$by))
#       mergeBy <- intersect(names(x), names(y))
#     else
      mergeBy <- dots$by
       
#   	if (length(mergeBy) == length(names(x))) # si x ne contient que les variables de jointures : renvoie directement y.
#   		return(y)
#   	if (length(mergeBy) == length(names(y))) # si y ne contient que les variables de jointures : renvoie directement x.
#   		return(x)
  		
  	#print(mergeBy)
#   	dataMerge <- merge.data.frame(cbind(as.data.frame(x[,mergeBy]), flagX = 1:nrow(x)), cbind(as.data.frame(y[,mergeBy]), flagY = 1:nrow(y)), by = mergeBy)
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
    newvars.y <- variables(y[,setdiff(intersect(names(dataMerge), names(y)), mergeBy)])
    
    if(!is.null(new.miss.code)) {
      newvars.x <- mapply(missing.add, newvars.x, "Individual added by a merge operation", new.miss.code)
      newvars.y <- mapply(missing.add, newvars.y, "Individual added by a merge operation", new.miss.code)
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
      if(k %in% mergeBy) {
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
    
#     if() { 
#       missing.gen.candidate(unique(unlist(allmissings(x))))
#     }
    
    
  	#print(tuplesY)
  	## le merge colle au dataset x le dataset y dans l'ordre en modifiant l'ordre des elements pour les faire matcher e la cle primaire de x
  	## je fais donc de meme ici : je reprend les tuples de x conserves apres le merge, et je reprend de y les variables qui n'etaient pas deje dans x et en reprendant l'ordre des tuples matchant avec la cle primaire de x, donne par flagY
#   	out <- cunion(
#   		x[tuplesX,],
#   		y[tuplesY, names(y)[which(!(names(y) %in% mergeBy))]]
#   	)
  	## puis on recupere les variables de y qui ne sont pas dans by (car elles ont ete deje mises)
  	#ybis <- y[tuplesY,names(y)[which(!(names(y) %in% mergeBy))]]
  	#for (i in 1:length(ybis)) { # ceux qui ne sont pas dans le by
  		#print("i")
  		#v <- ybis[,i]
  		#vColumn <- as.Column(v)
  		#print(vColumn)
  		#out <- cUnion(out, vColumn[flagY,])
  	#}
  	#out <- new("Dataset", data = outData)
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

#merge <- function (x, y, ...) {
	# si deux objets Dataset on lance le merge Dataset, sinon on lance le merge normal
#}

runion <- function (...) {
	y = list(...)
	#verifier que l'union des names est unique
	#verifier si on donne une column
	#for (i in y) {
		#print(y)
	#	i <- as.Dataset(i)
	#	outData = c(outData, slot(i, "data"))
	#}
	#out <- new("Dataset", data = outData)
	#return(out)
}