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
  
	out <- dataset(variables = outData, row.names = row.names(y[[1]]))
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
 
  	dots <- list(...)
    if(is.null(dots$by))
      mergeBy <- intersect(names(x), names(y))
    else
      mergeBy <- dots$by
       
  	if (length(mergeBy) == length(names(x))) # si x ne contient que les variables de jointures : renvoie directement y.
  		return(y)
  	if (length(mergeBy) == length(names(y))) # si y ne contient que les variables de jointures : renvoie directement x.
  		return(x)
  		
  	#print(mergeBy)
  	dataMerge <- merge.data.frame(cbind(as.data.frame(x[,mergeBy]), flagX = 1:nrow(x)), cbind(as.data.frame(y[,mergeBy]), flagY = 1:nrow(y)), by = mergeBy)
  	#print(names(dataMerge))
  	#print(dataMerge[1:20,])
    
    
  	tuplesX <- dataMerge$flagX
  	tuplesY <- dataMerge$flagY
    
    #return(list(tuplesX, tuplesY))
  	#print(tuplesY)
  	## le merge colle au dataset x le dataset y dans l'ordre en modifiant l'ordre des elements pour les faire matcher e la cle primaire de x
  	## je fais donc de meme ici : je reprend les tuples de x conserves apres le merge, et je reprend de y les variables qui n'etaient pas deje dans x et en reprendant l'ordre des tuples matchant avec la cle primaire de x, donne par flagY
  	out <- cunion(
  		x[tuplesX,],
  		y[tuplesY, names(y)[which(!(names(y) %in% mergeBy))]]
  	)
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