
#.onLoad <- function(libname, pkgname) {
#    if (!require(methods)) {
#        stop("Require methods package")
#    }
#}



.onAttach <- function(libname, pkgname) {
	  packageStartupMessage('\n', 'Welcome to Dataset', '.')
	  packageStartupMessage('You are running on version ', Dataset.version(), '.\n')
	  packageStartupMessage(
      'Vignette contains introductory material. To view, type ',
			"'vignette(package=\"Dataset\")'.\n"
	  )
	  packageStartupMessage(
      'If you use this package for building a data base, pre-processing data or data analysis, please reward your work by citing the package in own work. ',
      "Please type 'citation(\"Dataset\")' for citation information.\n"
	  )
		#addVigs2WinMenu("Dataset") 
}

.Last.lib <- function(libpath) {
  message('\n', 'Thank you for using Dataset', '.')
  message('See you soon', '!')
}

.onUnload <- function(libpath) {
  #library.dynam.unload("Dataset", libpath )
}
