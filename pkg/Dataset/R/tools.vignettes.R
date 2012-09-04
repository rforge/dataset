openPDF <- function(file) {
#   path <- .find.package("Dataset")
#   vignettes.meta = file.path(path, "Meta", "vignette.rds")
#   v = readRDS(vignettes.meta)
#   # we only take the first vignette
#   v.current <- v[1,"PDF"]
#   v.current <- file.path(path, 'doc', v.current)
  # then we run the viewer
  if(!file.exists(file)) stop("The file doesn't exist")
  
  v.current <- file
  
  sys <- .Platform$OS.type
  match <- FALSE
  if (sys == "unix") {
    match <- TRUE
    viewer <- getOption("pdfviewer")
    system(paste(viewer, v.current, "&"))
  }
  if (sys == "windows") { #FIXME does it works?
    match <- TRUE
    shell.exec(v.current)
  }
  if (!match) {
    message("Sorry, your operating system is not supported.")
  }
}