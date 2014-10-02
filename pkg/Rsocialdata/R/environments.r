Rsocialdata.globalenv <- new.env()
# read.dcf(file = system.file("DESCRIPTION",package="Rsocialdata"), fields="Version")
Rsocialdata.globalenv$package.infos <- utils:::packageDescription("Rsocialdata")
# Rsocialdata.globalenv$object.version <- "0.3-001"
# shoud be the same as in utils:::packageVersion("Rsocialdata")
Rsocialdata.globalenv$print.io <- FALSE

Rsocialdata.globalenv$never <- 10
Rsocialdata.globalenv$monitoring <- 8
Rsocialdata.globalenv$important <- 5
Rsocialdata.globalenv$comment <- 3
Rsocialdata.globalenv$all <- 0

Rsocialdata.globalenv$print.comments <- Rsocialdata.globalenv$important

# if the comment is important:
# Rsocialdata.globalenv$print.comments <- Rsocialdata.globalenv$comment
# if(Rsocialdata.globalenv$print.comments <= Rsocialdata.globalenv$important){
#   print('An important comment')
# }
# Rsocialdata.globalenv$print.comments <- Rsocialdata.globalenv$monitoring
# if(Rsocialdata.globalenv$print.comments <= Rsocialdata.globalenv$important){
#   print('An important comment')
# }

Rsocialdata.globalenv.print.io <- function(x){
  stopifnot(is.logical(x))
  Rsocialdata.globalenv$print.io <- x
}
# Rsocialdata.globalenv.print.io(TRUE)
# svar(c(1))
# Rsocialdata.globalenv.print.io(FALSE)
# svar(c(1))
Survey.version <- function(){
#   print('helloe')
#   print(class(getMethod("show", "Variable")))
  return(Rsocialdata.globalenv$package.infos$Version)
}
Rsocialdata.version <- function(){
  return(Rsocialdata.globalenv$package.infos$Version)
}

Rsocialdata.globalenv$Variable.description.default <- "    !!!empty!!!"
Rsocialdata.globalenv$Variable.version <- "0.6"
Rsocialdata.globalenv$Rsocialdata.version  <- "0.4"

Rsocialdata.globalenv$message.operation.success <- "Operation completed successfully."
Rsocialdata.globalenv$message.allocation.rows <- "Here is the allocation of the rows in the different classes."
Rsocialdata.globalenv$message.missing.collision <- "Sorry, a problem occurs, operation aborted. Please report this error by sending an email to 'dataset-requests@lists.r-forge.r-project.org'."