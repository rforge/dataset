totex <- function(txt){
 return(gsub("_", "\\\\_", txt, perl = T)) 
}
# totex("shp_bon")

latex.head <- function(latexFile){
  cat("\\usepackage[top=2.5cm, bottom=2.5cm, left=1.5cm, right=1.5cm]{geometry} \n", file = latexFile, append = T)
  cat("\\usepackage[utf8x]{inputenc} \n", file = latexFile, append = T)
  cat("\\usepackage[T1]{fontenc} \n", file = latexFile, append = T)
  cat("\\usepackage{aeguill}  \n", file = latexFile, append = T)
  cat("\\usepackage{longtable} \n", file = latexFile, append = T)
  cat("\\usepackage{graphicx} \n", file = latexFile, append = T)
}

# v a Variable
str.typevar <- function(x, short = FALSE, parenthesis=FALSE) {
  if (parenthesis) {
    txt.before <- '('
    txt.after <- ')'
  } else {
    txt.before <- txt.after <- ''
  }
  
 if(is.nominal(x)) {
   if (short) txt <- 'n'
   else txt <- 'nominal'
 }
 if(is.ordinal(x)) {
   if (short) txt <- 'o'
   else txt <- 'ordinal'
 }
 if(is.binary(x)) {
   if (short) txt <- 'b'
   else txt <- 'binary'
 }
 if(is.scale(x)) {
   if (short) txt <- 's'
   else txt <- 'scale'
 }
 if(is.time(x)) {
   if (short) txt <- 't'
   else txt <- 'time'
 }
 if(is.weighting(x)) {
   if (short) txt <- 'w'
   else txt <- 'weighting'
 }
  
  return(paste(txt.before, txt, txt.after, sep = ''))
}
# str.typevar(svar(c(1)))
# str.typevar(svar(c(1)), short = T)

str.collapse <- function(x,y, sep = ' ') {
  out <- mapply(paste, x, y, sep=sep)
  names(out) <- NULL
  return(out)
}
# str.collapse(c("id", "health"), c("s", "o"))

#x a Dataset
str.names <- function(x, parenthesis=TRUE, sep = ' ') {
  out <- mapply(str.typevar, variables(x), parenthesis=parenthesis)
  out <- str.collapse(names(x), unlist(out), sep = sep)
  return(out)
}
# str.names(z, parenthesis = T)

