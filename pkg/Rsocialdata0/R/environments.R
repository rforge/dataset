Dataset.globalenv <- new.env()
# read.dcf(file = system.file("DESCRIPTION",package="Dataset"), fields="Version")
Dataset.globalenv$package.infos <- utils:::packageDescription("Rsocialdata0")
# Dataset.globalenv$object.version <- "0.3-001"
# shoud be the same as in utils:::packageVersion("Dataset")
Dataset.globalenv$print.io <- FALSE

Dataset.globalenv$never <- 10
Dataset.globalenv$monitoring <- 8
Dataset.globalenv$important <- 5
Dataset.globalenv$comment <- 3
Dataset.globalenv$all <- 0

Dataset.globalenv$print.comments <- Dataset.globalenv$important

# if the comment is important:
# Dataset.globalenv$print.comments <- Dataset.globalenv$comment
# if(Dataset.globalenv$print.comments <= Dataset.globalenv$important){
#   print('An important comment')
# }
# Dataset.globalenv$print.comments <- Dataset.globalenv$monitoring
# if(Dataset.globalenv$print.comments <= Dataset.globalenv$important){
#   print('An important comment')
# }

Dataset.globalenv.print.io <- function(x){
  stopifnot(is.logical(x))
  Dataset.globalenv$print.io <- x
}
# Dataset.globalenv.print.io(TRUE)
# svar(c(1))
# Dataset.globalenv.print.io(FALSE)
# svar(c(1))
Survey.version <- function(){
#   print('helloe')
#   print(class(getMethod("show", "Variable")))
  return(Dataset.globalenv$package.infos$Version)
}
Dataset.version <- function(){
  return(Dataset.globalenv$package.infos$Version)
}

Dataset.globalenv$Variable.description.default <- "    !!!empty!!!"
Dataset.globalenv$Variable.version <- "0.6"
Dataset.globalenv$Dataset.version  <- "0.4"

Dataset.globalenv$message.operation.success <- "Operation completed successfully."
Dataset.globalenv$message.allocation.rows <- "Here is the allocation of the rows in the different classes."
Dataset.globalenv$message.missing.collision <- "Sorry, a problem occurs, operation aborted. Please report this error by sending an email to 'dataset-requests@lists.r-forge.r-project.org'."