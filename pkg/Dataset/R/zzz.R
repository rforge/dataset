

#.First.lib <- function(libname, pkgname, where) {
#    if (!require(methods)) {
#        stop("Require Methods package")
#    }
#    #if (!require(gplots)) {
#    #    stop("Require gplots package")
#    #}
#    
#    where <- match(paste("package:",pkgname, sep=""), search())
#  
#}

#.onLoad <- function(libname, pkgname) {
#    if (!require(methods)) {
#        stop("Require methods package")
#    }
#}

.onAttach <- function(libname, pkgname) {
	  packageStartupMessage('\n', 'Welcome to Dataset', '.')
	  packageStartupMessage('You are running on version ', Dataset.version(), '.')
	  packageStartupMessage(
      'Vignette contains introductory material. To view, type ',
			"'openVignette()'.\n"
	  )
	  packageStartupMessage(
      'If you use this package for building a data base, pre-processing data or data analysis, please reward your work by citing the package in your paper. ',
      "Please type 'citation(\"Dataset\")' for citation information.\n"
	  )
		#addVigs2WinMenu("Dataset") 
}

.onUnload <- function( libpath ) {
  #library.dynam.unload("Dataset", libpath )
}
