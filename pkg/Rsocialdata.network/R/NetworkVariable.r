#' The NetworkVariable class
#' 
#' This class allows to store network survey data. Basically, data are stored as matrix of survey codes and a dictionnary containing labels and missing values specification. A network should be read as ROWS \emph{do to} COLUMNS. For instance, if the description field of a network variable is defined as "Gives emotional support." and the first row of the first case is \code{0 1 1 0 0}, we understand that PERSON1 gives emotional support to PERSON2 and PERSON3 in the first network. The class supports a link to a NetworkMetavar object to take into account demographic information about people cited within the networks.
#' @aliases NetworkVariable
#' @export
setClass(
  "NetworkVariable",
  representation(
    networks = 'list', # list des matrices réseau pour chaque individu
    #     valids = 'values', # 0=pas de lien, 1= lien, on peut imaginer d'autres types, 2=lien fort
    #     missings = 'values', # liste des codes pour les valeurs manquantes
    valids = 'numeric', # 0=pas de lien, 1= lien, on peut imaginer d'autres types, 2=lien fort
    missings = 'numeric', # liste des codes pour les valeurs manquantes
    lvl = 'character', # long value label
    description = 'character',
    network.version = 'character',
    directed = 'logical',
    metadata.depends = 'character',
    infos = 'list'
  ),
  validity = function(object) {
    #     if(Rsocialdata.globalenv$print.io) cat (" =>       NetworkVariable: object validity check \n")
    flag = TRUE
    
    if(flag) {
      # on vérifie que chaque élément de la liste est une matrice
    }
    
    if(flag) {
      # pour chaque matrice, on récupère la liste de tous les codes et on vérifie qu'ils sont tous dans values
    }
    
    return(flag)
  }
)

#' Test whether a number is the squared of an integer number
#' 
#' The function returns \code{TRUE} if the root of the number supplied is an \code{integer}.
#' @param x an \code{integer}.
is.squared.integer <- function(x){
  sqrt(x) %% 1 == 0
}
# is.squared.integer(26)





#' Creates a NetworkVariable object
#' 
#' The function transform network data given in a tabular form to a \code{NetworkVariable} object.
#' 
#' @param x A \code{Rsocialdata} object. The variables specifying who cites who within the networks, given in a tabular form.
#' @param n.col.by.network a
#' @param valids A \code{named numeric}. Specify which survey codes correspond to valids cases and label them.
#' @param missings A \code{named numeric}. Specify which survey codes correspond to valids cases and label them.
#' @param description A \code{character} of length 1. A short description for the \code{NetworkMetavar} object.
#' @param directed A logical. Are the networks directed? Default is \code{TRUE}.
#' @param metadata.depends A \code{character} of length 1. Specify the \code{NetworkMetadata} object containing covariates describing individuals of the networks.
#' @param ... Additional parameters. When the number of cited people is not the same for all networks, the \code{n.cited.by.network} (a \code{numeric}) allows to specify them.
#' @export
netvar <- function(
  x,
  n.col.by.network,
  valids = valids(x[[1]]),
  missings = missings(x[[1]]),
  description,
  directed = T,
  metadata.depends,
  ... # ex n.cited.by.network
){
  #prevoir deux cas pour networks
  # soit on donne directement les matrices
  # soit on donne en colone et on calcule les matrices
  # faire les test de validité sur les valids et missings
  if(missing(valids)) valids <- numeric(0)
  if(missing(missings)) missings <- numeric(0)
  #   if(missing(lvl)) lvl <- character(0)
  if(missing(description)) description <- character(0)
  if(missing(metadata.depends)) metadata.depends <- character(0)
  
  args <- list(...)
  
  if(inherits(x,'Rsocialdata')) {
    
    g <- ncol(x) / n.col.by.network # nombre d'individus
    g <- g * (n.col.by.network+1) # nombre d'individus +1
    # ce n'est pas idéal, il faudrait plutot chercher à dire si ncol(x) est un nombre de la forme n*(n-1)
    
    if(!is.squared.integer(g)) {
      stop("ncol(x) should be the square of an integer to build squared matrix")
    }
    # vu que l'on n'a pas les colones ou une personne se cite elle-même (soutien de soi-même sur soi-même), on n'a pas 6*6 colones mais 6*5, on n'a donc pas le carré d'un entier
    
    if('n.cited.by.network' %in% names(args)) {
      n.cited.by.network <- args$n.cited.by.network
      if(!inherits(n.cited.by.network, 'Variable')){
        stop("n.cited.by.network argument should inherit of a Variable object.")
      }
      #       n.cited.by.network <- v(n.cited.by.network)
      if(length(n.cited.by.network) != nrow(x)){
        stop("n.cited.by.network argument should have the lenght of nrow(x).")
      }           
    } else {
      n.cited.by.network <- rep(sqrt(g), nrow(x)) - 1 # we remove ego
    }
    
    #     x.df <- v(x)
    
    networks <- list() # objet de stockage des matrice réseau
    for(p in 1:nrow(x)) { # debut boucle sur les individus
      #       print(paste('indiv',p))
      nIndiv <- v(n.cited.by.network[p]) # on récupère le nombre d'individu que l'individu p a cité
      if (is.na(nIndiv)) { # si le nombre d'individu cité est manquant, la matrice réseau est NA
        m1 <- NA
      } else {
        # on initialise la matrice pour l'individu p avec des 0, en fonction du nombre d'individus cités
        m1 <- matrix(rep(0,(nIndiv+1)^2), ncol=nIndiv+1) # +1 pour ego
        #         dimnames(m1) <- list(
        #          c("ego", "P1", "P2", "P3", "P4", "P5"),
        #          c("ego", "P1", "P2", "P3", "P4", "P5")
        #         )
        
        if(nIndiv > 0) {id.alter <- 1:nIndiv} else {id.alter <- numeric(0)}
        
        dimnames(m1) <- list(
          c("ego", id.alter), # USE ID.ALTER
          c("ego", id.alter)  # USE ID.ALTER
        )
        
        if (nIndiv != 0) {
          # on remplit la matrice avec des 1 aux cases ou il y a un lien
          for (i in 1:(nIndiv+1)) { # on boucle sur les lignes avec le nombre d'individus cités; on ajoute 1 car en plus d'avoir les individus cités on doit aussi compter ego
            #             print(paste('i:',i))
            for(j in 1:(nIndiv)) { # on boucle sur le nombre d'individus cités (ici on n'ajoute pas 1 car on ne considère pas le soutient d'un individu avec lui-même)
              #               print(paste('j:',j))
              k <- j + (i-1)*n.col.by.network # on cherche la variable qui correspond au lien entre l'individu i et l'individu j, on stocke dans k
              #               print(paste('k:',k))
              
              #               if(x.df[p,k] == "oui") {
              if (j < i) { # si on est sous la diagonale de la matrice
                m1[i,j] <- codes(x[[k]])[p]
              } else { # si on est au dessus de la diagonale de la matrice, on décale de 1, car on doit sauter la case du soutient de l'individu à lui-même
                m1[i,j+1] <- codes(x[[k]])[p]
              }
              #               }
              
              
            }
          }
        }
      }
      # on enregistre la matrice dans la liste
      networks[[p]] <- m1
    }
    
    
    return(new('NetworkVariable',
               networks = networks,
               valids = valids,
               missings = missings,
               description = description,
               directed = directed,
               metadata.depends = metadata.depends,
               network.version = '0.0.1',
               infos = list()
    ))
    
  } # end if Rsocialdata
  
  if(inherits(x, 'list')) {
    are.matrix <- mapply(inherits, x, 'matrix')
    all.matrix <- all(mapply(inherits, x, 'matrix'))
    which.no.matrix <- which(!are.matrix)
    
    if(!all.matrix) {
      message("A a list of networks all network have to be matrix object")
      message(paste("following element aren't matrix objects:",
                    paste(which.no.matrix, collapse=', ')
      ))
    } else {
      # faire l'importation
    }
  } # end if list
  
  
} # end function



#' @describeIn networks method for \code{NetworkVariable} objects.
setMethod(
  f = "networks",
  signature = "NetworkVariable", 
  definition = function (object) { 
    return(slot(object, "networks"))
  }
)

#' @describeIn networks method for \code{NetworkVariable} objects.
setMethod(
  f = "networks<-" ,
  signature = "NetworkVariable" ,
  definition = function(object, value){
    object@networks <- value
    validObject(object)
    return(object)
  }
)
setMethod(
  f = "valids",
  signature = "NetworkVariable", 
  definition = function (object) { 
    return(slot(object, "valids"))
  }
)

setMethod(
  f = "valids<-" ,
  signature = "NetworkVariable" ,
  definition = function(object, value){
    object@valids <- value
    validObject(object)
    return(object)
  }
)
setMethod(
  f = "missings",
  signature = "NetworkVariable", 
  definition = function (object) { 
    return(slot(object, "missings"))
  }
)

setMethod(
  f = "missings<-" ,
  signature = "NetworkVariable" ,
  definition = function(object, value){
    object@missings <- value
    validObject(object)
    return(object)
  }
)
setMethod(
  f = "description",
  signature = "NetworkVariable", 
  definition = function (object) { 
    return(slot(object, "description"))
  }
)

setMethod(
  f = "description<-" ,
  signature = "NetworkVariable" ,
  definition = function(object, value){
    object@description <- value
    validObject(object)
    return(object)
  }
)
setMethod(
  f = 'show',
  signature = 'NetworkVariable',
  definition = function(object) {
    print(object@networks) 
  }
)
setMethod(
  f = 'print',
  signature = 'NetworkVariable',
  definition = function(x) {
    show(x) 
  }
)

#' length method for NetworkVariable objects
#'
#' @name length
#' @aliases length,NetworkVariable-method
#' @docType methods
#' @rdname length-methods
NULL
setMethod(
  f= "length",
  signature = "NetworkVariable", 
  definition = function (x) {
    return(length(slot(x, "networks")))
  }
)

#' Extract or replace parts of a NetworkVariable object
#'
#' @name [
#' @aliases [,NetworkVariable-method
#' @docType methods
#' @rdname extract-methods
NULL
setMethod(
  f ="[",
  signature ="NetworkVariable",
  definition = function(x,i){
    networks(x) <- networks(x)[i]
    return(x)
  }
)

#' set parts of NetworkVariable
#'
#' @name [<-
#' @aliases [<-,NetworkVariable-method
#' @docType methods
#' @rdname extract-methods
NULL
setMethod(
  f ="[<-",
  signature =c("NetworkVariable"),
  definition = function(x,i,value){
    networks <- networks(x)
    networks[i] <- value
    networks(x) <- networks
    return(x)
  }
)
