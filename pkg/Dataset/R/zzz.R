

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

#.onAttach <- function(libname, pkgname) {
#	message(paste(
#			"\nWelcome to Dataset\n\n",
#			"You are using the version", version.Dataset(), "\n\n",
#			"Vignette contains introductory material. To view, type",
#			"'openVignette()'.\n\n"
#			#"\n\n To cite Dataset, see",
#			#"'citation(\"Dataset\")' and for packages 'citation(pkgname)'.\n"
#			, sep=" "
#		)
#	)
		#addVigs2WinMenu("Dataset") 
#}

.onUnload <- function( libpath ) {
  #library.dynam.unload("Dataset", libpath )
}
