# folder <- '~/SHP 2012/SHP-Data/SPSS'
# folder <- '~/SHP 2012/SHP-Data/SPSS/'
# variables <- c('idpers')
# by <- 'idpers'
# lowernames <- T
# i <- 1
# d1 <- get.spss.folder(
#   folder = folder1,
#   variables = c('idpers'),
#   by = 'idpers'
# )

get.spss.folder <- function(
  folder,
  variables,
  by,
  lowernames = TRUE
) {
  
  folder <- path.expand(folder)
  
  if(lowernames) {
    variables <- tolower(variables)
  }
  
  files <- list.files(
    path = file.path(folder),
    pattern='.\\.sav$',
    recursive = T
  )
  
  out <- NULL
  
  for (i in 1:length(files)) {
    f <- get.spss.file(
      datadir = file.path(folder),
      file = files[10], 
      #features,
      #tsvar,
      #ordinals,
      lowernames = lowernames,
      exportPDF = FALSE
    )
    if(by %in% names(f)) { # else we can't use the file (we can't merge) 
      var <- names(f)[which(names(f) %in% variables)]
      if(length(var) > 0) {
        temp <- f[,c(by,var)]
        if(is.null(out)) {out <- temp}
        else {out <- merge(out, temp)}
        variables <- setdiff(timefixed.variables, var)
      }
    }
    if(length(variables) == 0) break;
  }
  if(length(variables) != 0){
    message("some variables not found:")
    message(variables)
    stop('please check variable names')
  }
  
  return(dataset(out))
}