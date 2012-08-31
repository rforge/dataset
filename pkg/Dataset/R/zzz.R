
#.onLoad <- function(libname, pkgname) {
#    if (!require(methods)) {
#        stop("Require methods package")
#    }
#}



.onAttach <- function(libname, pkgname) {
	  packageStartupMessage('\n', 'Welcome to Dataset', '.')
	  packageStartupMessage('You are running on version ', Dataset.version(), '.\n')
	  packageStartupMessage(
      'Vignette contains introductory material. To read, type ',
			#"'vignette(package=\"Dataset\")'.\n"
      "‘readVignette()‘.\n"
	  )
	  packageStartupMessage(
      'If you use this package for building a data base, pre-processing data or data analysis, please reward your work by citing the package in your own one. ',
      "Please type ‘citation(\'Dataset\')‘ for citation information.\n"
	  )
	  packageStartupMessage(
	    "Type ‘help(package = \'Dataset\')‘ ",
      "to show the help index.\n"
	  )
    
}

.Last.lib <- function(libpath) {
  message('\n', 'Thank you for using Dataset', '.')
  message('See you soon', '!')
}

.onUnload <- function(libpath) {
  #library.dynam.unload("Dataset", libpath )
}
