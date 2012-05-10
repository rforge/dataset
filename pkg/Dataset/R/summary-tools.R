totex <- function(txt){
 return(gsub("_", "\\\\_", txt, perl = T)) 
}
# totex("shp_bon")

str.typevar <- function(x, parenthesis=FALSE) {
  if (parenthesis) {
    txt.before <- '('
    txt.after <- ')'
  } else {
    txt.before <- txt.after <- ''
  }
  
 if(is.nominal(x)) txt <- 'n'
 if(is.ordinal(x)) txt <- 'o'
 if(is.binary(x)) txt <- 'b'
 if(is.scale(x)) txt <- 's'
 if(is.time(x)) txt <- 't'
 if(is.weighting(x)) txt <- 'w'
  
  return(paste(txt.before, txt, txt.after, sep = ''))
}

str.collapse <- function(x,y, sep = ' ') {
  out <- mapply(paste, x, y, sep=sep)
  names(out) <- NULL
  return(out)
}
# str.collapse(c("id", "health"), c("s", "o"))

str.names <- function(x, parenthesis=FALSE, sep = ' ') {
  out <- mapply(str.typevar, variables(x), parenthesis=parenthesis)
  out <- str.collapse(names(x), unlist(out), sep = sep)
  return(out)
}
# str.names(z, parenthesis = T)