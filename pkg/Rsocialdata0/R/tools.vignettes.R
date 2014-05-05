check.tex <- function() {
  tex.compil <- "pdflatex"
  
  if(nchar(Sys.which(tex.compil)) == 0) {
    message("The generation of the PDF file requires a LaTeX distribution.")
    message("It seems there is no LaTeX distribution on your computer.")
    message("Please install a LaTeX distribution and run the PDF file generation again.")
    message("")
    message("Note: Here are examples of LaTeX distribution you can install on your computer:")
    message("- If you are running on Windows, your can install 'Miktex'.")
    message("- If you are running on Mac OS, you can install 'TeX Live'.")
    message("- If you are running on a Unix system, you can install 'TeX Live'.")
    stop("Unable to locate the 'pdflatex' command.")
  }
}

pdflatex <- function(file, show.log = F) {
  if(!file.exists(file)) stop("The file doesn't exist")
  
  file.pdf <- paste(substring(file, 1, nchar(file) -4), '.pdf', sep = '')
  
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
      if(show.log) {
        system(
          # paste(pdflatex.path, v.current, "&"),
          paste(pdflatex.path, v.current)
        )
      } else {
        system(
  #         paste(pdflatex.path, v.current, "&"),
          paste(pdflatex.path, v.current),
          ignore.stdout = T,
          ignore.stderr = T
        )
      }
    }
  }
  if (sys == "windows") { #FIXME does it works?
    match <- TRUE
    if(nchar(pdflatex.path) == 0) {
      message('Unable to find the pdflatex command, please install a LaTeX distribution (Miktex for example), or check your LaTeX distibution.')
      message('Generation of the PDF file aborded.')
    } else {
      compilation.out <- system(paste(pdflatex.path, v.current),
#              ignore.stdout = T,
#              ignore.stderr = T,
#              show.output.on.console = F
               intern = T
      )
      if(show.log) print(compilation.out)
    }
#     shell(v.current)
  }
  if (!match) {
    message("Sorry, your operating system is not supported. You can ask this feature to the maintainer of the package.")
    stop("Operating system not supported for PDF file generation.")
  }
  
  if(match && !file.exists(file.pdf)) {
    message("Sorry, a problem occurs. The PDF file generation failed.")
    message('')
    message("Maybe the PDF file is open in your PDF viewer which is locking the file. Try to close your PDF viewer.")
    message('')
    message("You can run the command with option 'show.log = TRUE' to try to locate the problem, or directly look at the log file produced.")
    stop("No PDF file produced")
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