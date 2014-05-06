
#.onLoad <- function(libname, pkgname) {
#    if (!require(methods)) {
#        stop("Require methods package")
#    }
#}



.onAttach <- function(libname, pkgname) {
	  packageStartupMessage('\n', 'Welcome to Rsocialdata0', '.')
	  packageStartupMessage('You are running version ', Survey.version(), '.\n')
	  packageStartupMessage(
      'For introductory material, type ',
			#"'vignette(package=\"Rsocialdata0\")'.\n"
      "`vignette('Rsocialdata0-vignette-overview')`.\n"
	  )
	  packageStartupMessage(
      'If you use this package for building a data base, pre-processing data or data analysis, thank you for rewarding our work by citing the package in your own one. ',
      "Please type `citation(\'Rsocialdata0\')` for citation information.\n"
	  )
	  packageStartupMessage(
	    "Type `help(package = \'Rsocialdata0\')` ",
      "to show the help index.\n"
	  )
    
}

.Last.lib <- function(libpath) {
  message('\n', 'Thank you for using Rsocialdata0', '.')
  message('See you soon', '!')
}

.onUnload <- function(libpath) {
  #library.dynam.unload("Rsocialdata0", libpath )
}
