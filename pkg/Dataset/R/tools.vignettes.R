pdflatex <- function(file) {
  if(!file.exists(file)) stop("The file doesn't exist")
  
  v.current <- file
  pdflatex.path <- Sys.which('pdflatex')
    
  sys <- .Platform$OS.type
  match <- FALSE
  
  if (sys == "unix") {
    match <- TRUE
    if(nchar(pdflatex.path) == 0) {
      message('Unable to find the pdflatex command, please install a LaTeX distribution (TexLive for example), or check your LaTeX distibution.')
      message('Generation of the PDF file aborded.')
    } else {
      system(
#         paste(pdflatex.path, v.current, "&"),
        paste(pdflatex.path, v.current),
        ignore.stdout = T,
        ignore.stderr = T
      )
    }
  }
  if (sys == "windows") { #FIXME does it works?
    match <- TRUE
    if(nchar(pdflatex.path) == 0) {
      message('Unable to find the pdflatex command, please install a LaTeX distribution (Miktex for example), or check your LaTeX distibution.')
      message('Generation of the PDF file aborded.')
    } else {
      system(paste(pdflatex.path, v.current),
             ignore.stdout = T,
             ignore.stderr = T,
             show.output.on.console = F
      )
    }
#     shell(v.current)
  }
  if (!match) {
    message("Sorry, your operating system is not supported. You can ask this feature to the maintainer of the package.")
  }
}

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
    message("Sorry, your operating system is not supported. You can ask this feature to the maintainer of the package")
  }
}