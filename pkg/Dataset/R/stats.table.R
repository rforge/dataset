
# PREVOIR LES DEGRES DE LIBERTES
giveStars <- function(pvalue, legend = FALSE) {
  if(!legend) {
    if(is.na(pvalue)) {
      return("   ")
    } else {
      if (pvalue < 0) stop("Dataset::giveStars  -  pvalue have to be > 0")
      if (pvalue < 0.001) return("***")
      if (pvalue < 0.01)  return("** ")
      if (pvalue < 0.05)  return("*  ")
      return("   ") # if nothing matches
    }
  } else {
    return("*** < 0.001  ** < 0.01  * < 0.05")
  }
}

# giveStars(-0.000002)
# giveStars(0.000002)
# giveStars(0.002)
# giveStars(0.02)
# giveStars(0.2)
# giveStars(NA)
# sapply(c(0.000002, 0.002, 0.02, 0.2), giveStars)

giveStars.variable <- function(x, legend = FALSE) {
  return(sapply(x, giveStars))
}
# giveStars.variable(c(0.000002, 0.002, 0.02, 0.2))

giveStars.df <- function(df, legend = FALSE) {
 for (i in 1:ncol(df)) {
   df[[i]] <- giveStars.variable(df[[i]])
 }
 return(df)
}
# df <- data.frame(x = c(0.01,0.2,0.0001), y = c(0.02,0.05,0.00001))
# giveStars.df(df)

even <- function(x) {
  return(x[which((x %% 2) == 0)])
}
# even(1:6)

giveStars.df.even <- function(df, legend = FALSE) {
 for (i in even(1:ncol(df))) {
   df[[i]] <- giveStars.variable(df[[i]])
 }
 return(df)
}
# df <- data.frame(x = c(0.01,0.2,0.0001), y = c(0.02,0.05,0.00001))
# giveStars.df.even(df)

addEvenNames <- function(char.vector, word = " ", use.colnames = FALSE) {
  out <- character(0)
  for (i in 1:length(char.vector)) {
    out <- c(out, char.vector[i])
    if (use.colnames)
      out <- c(out, paste(char.vector[i], word))
    if (!use.colnames)
      out <- c(out,word)
  }
  return(out)
}

# addEvenNames(c("Model 1", "Model 2"))
# addEvenNames(c("Model 1", "Model 2"), word = "signif.")
# addEvenNames(c("Model 1", "Model 2"), word = "signif.", use.colnames = T)

addSignif <- function(char.vector, word = "signif.") {
  out <- character(0)
  for (i in 1:length(char.vector)) {
    out <- c(out, char.vector[i])
    out <- c(out, paste(char.vector[i], word))
  }
  return(out)
}
