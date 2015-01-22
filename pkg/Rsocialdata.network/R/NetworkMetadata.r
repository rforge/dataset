#' The NetworkMetadata class
#' 
#' This object allows to store covariates data describing individuals within an egocentric networks. This covariates are generally demographic variables.
#' 
#' @export
setClass(
  "NetworkMetadata",
  contains = 'Rsocialdata',
  representation(
    infos = 'list'
  ),
  validity = function(object) {
    #     if(Rsocialdata.globalenv$print.io) cat (" =>       NetworkMetadata: object validity check \n")
    flag = TRUE
    
    if(flag) {
      names <- names(object)
      if (!'id.ego' %in% names) {
        message("A NetworkVariable must contain an 'id.ego' variable")
        flag <- F
      }
      if (!'id.alter' %in% names) {
        message("A NetworkVariable must contain an 'id.alter' variable")
        flag <- F
      }
    }
    
    # id.ego variable shouldn't contain missing values
    if(flag) {
      
    }
    
    # id.alter variable shouldn't contain missing values
    if(flag) {
      
    }
    # pas de variable de poids, pas de variable de controle
    
    return(flag)
  }
)



#' Generates IDs for people cited within an egocentric network
#' 
#' The function generates ID (\code{integer}) for flatten the network in a tabular view: instead of have one ID for ego and then an ID for each people of the network, we generate a unique ID for all people (ego and alters)
#' 
#' @param metavar \code{Rsocialdata} object, demographic variables for all individals of the egocentric network
#' @param nmetavarbyalter number of variable to use for each alter individual
#' @param metavar.id.alter if ids for alters are given en metavar we can use them, else we assume meta variables are given in the same order than id alters
idalter.gen <- function(
  metavar,
  nmetavarbyalter,
  metavar.id.alter = NULL
) {
  
  if(missing(metavar)) {stop("You need to provide metavar argument")}
  if(!inherits(metavar, "Rsocialdata")) {stop("The 'metavar' argument has to be an Rsocialdata object")}
  
#   print(metavar) # ok  
#   print(nrow(metavar)) # RENVOIE NULL...
#   print(getMethod("nrow", "Rsocialdata")(metavar)) # ok
  
  weighting(metavar) <- character(0)
  checkvars(metavar) <- character(0)
  
  n.ego <- nrow(metavar)
  
  if((ncol(metavar) %% nmetavarbyalter) != 0) {
    stop("The number of variables isn't a multiple of the number of variable by alter")
  } else {
    n.alter <- ncol(metavar)/nmetavarbyalter
  }
  
  # creation of the dataset
  id.ego <- svar(rep(1:nrow(metavar), each=n.alter), description="IDs for ego")
  id.alter <- svar(rep(1:n.alter,nrow(metavar)), description="IDs for alter")
  out <- dataset(x=list(id.ego,id.alter))
  #   nrow(out)
  
  # filling the demographic variables
  alldesc <- alldescriptions(metavar)
  alldesc.unique <- unique(alldesc)
  # descriptions are rarely unique, some accents, dots, can generate a non unicity, instead we use only the first variable to get description, values, missings
  alldesc <- alldescriptions(metavar[,1:nmetavarbyalter])
  alldesc <- as.character(alldesc[,1])
  
  # for each metavar
  for(k in 1:nmetavarbyalter) {
    #     j <- numeric(0)
    #     for (l in 1:n.alter) {
    #       j <- c(j, k + (l-1)*nmetavarbyalter)
    #     }
    
    codes <- numeric(0)
    for(i in 1:n.ego) {
      for (l in 1:n.alter) {
        j <- k + (l-1)*nmetavarbyalter
        codes <- c(codes, codes(metavar[[j]])[i])
      }
    }
    #     print(codes)
    #     length(codes)
    if(is.quantitative(metavar[[k]])) { # FIXME tester les unicités missings et values
      temp <- svar(
        x = codes,
        description = alldesc[k],
        values=valids(metavar[[k]]),
        missings=missings(metavar[[k]])
      )
      out <- dataset(x=c(variables(out), temp))
    }
    if(is.qualitative(metavar[[k]])) { # FIXME tester les unicités missings et values
      #       print(alldesc[k])
      #       print(codes)
      #       print(valids(metavar[[k]]))
      #       print(missings(metavar[[k]]))
      temp <- cvar(
        x = codes,
        description = alldesc[k],
        values=valids(metavar[[k]]),
        missings=missings(metavar[[k]])
      )
      out <- dataset(x=c(variables(out), temp))
    }
  }
  return(out)
}


#' Creates NetworkMetavar objects
#' 
#' The function creates a code{NetworkMetavar} from a tabular view of the networks.
#' 
#' @param name A \code{character} of length 1. Name of the \code{NetworkMetavar} object.
#' @param description A \code{character} of length 1. A short description for the \code{NetworkMetavar} object.
#' @param alterdata \code{Rsocialdata} object. The covariates variables.
#' @param names A \code{character}. Define names of the demographic variables describing people within networks.
#' @param nmetavarbyalter An \code{integer}. Number of variable to use for each alter individual.
#' @param nbalter An \code{integer}. Obsolete.
#' @param metavar.id.alter if An \code{integer}. IDs for alters are given en metavar we can use them, else we assume meta variables are given in the same order than id alters
#' @export
netmetavar <- function(
  name,
  description,
  alterdata,
  names,
  nmetavarbyalter,
  nbalter,  
  metavar.id.alter = NULL
){
  if(missing(name)) {name <- "untitled network meta var"}
  if(missing(description)) {description <- "EMPTY"}
  if(missing(alterdata)) {stop("You need to provide alterdata argument")}
  if(missing(nmetavarbyalter)) {}
  if(missing(names)){
    names <- make.names(rep('metavar',nmetavarbyalter), unique=T)
  }
  
  # en ligne dans alterdata on a les données meta du reseau d'un individu.
  # si nbalter existe, on peut créer les colones id.ego et id.alter
  # si nbalter n'est pas donné, on met le nombre de fois que la modalité apparait
  # puis
  # on fait un unique sur names pour récupérer le nombre de variables différentes
  # pour chaque variable
  # on suppose que les individus on été remplis dans l'ordre (qm, qn, qo, ...)
  # on teste si pour chaque occurence de la variable, il y a les meme values et missing
  # on prend alors les values et missing et description de la premiere variable
  # on rempli la nouvelle variable sexe et on l'ajoute au dataset
  # on reitère pour les autres varibles démographiques
  
  alterdata.row <- idalter.gen(
    metavar = alterdata,
    nmetavarbyalter = nmetavarbyalter
  )
  
  names(alterdata.row)[1] <- c('id.ego')
  names(alterdata.row)[2] <- c('id.alter')
  names(alterdata.row)[3:ncol(alterdata.row)] <- names
  
  # dataset: function (x, name, description, row.names, weights, infos, check.rows = FALSE) 
  out <- new(
    'NetworkMetadata',
    variables = variables(alterdata.row),
    name = name,
    description = description,
    row.names = as.character(1:nrow(alterdata.row)),
    Rsocialdata.version = "0.0.1"
  )
}