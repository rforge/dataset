Dataset.globalenv <- new.env()
Dataset.globalenv$package.infos <- utils:::packageDescription("Dataset")
Dataset.globalenv$object.version <- "0.3-001"
Dataset.globalenv$print.io <- FALSE

Dataset.globalenv.print.io <- function(x){
  stopifnot(is.logical(x))
  Dataset.globalenv$print.io <- x
}
# Dataset.globalenv.print.io(TRUE)
# svar(c(1))
# Dataset.globalenv.print.io(FALSE)
# svar(c(1))
Dataset.version <- function(){
  return(Dataset.globalenv$package.infos$Version)
}