\name{Variable-class}
\Rdversion{1.1}
\docType{class}
\alias{Variable-class}
\alias{as.Dataset,Variable-method}
\alias{[<-,Variable-method}
\alias{[,Variable-method}
\alias{codes<-,Variable-method}
\alias{codes,Variable-method}
\alias{codes<-,Variable-method}
\alias{codes,Variable-method}
\alias{coerce,Variable,Dataset-method}
\alias{contains,Variable,character-method}
\alias{description<-,Variable-method}
\alias{description,Variable-method}
\alias{length,Variable-method}
\alias{missings<-,Variable-method}
\alias{missings,Variable-method}
\alias{valid,Variable-method}
\alias{values<-,Variable-method}
\alias{values,Variable-method}
\alias{values.permut,Variable,numeric,numeric-method}
\alias{values.reverse,Variable-method}

\title{Class \code{"Variable"}}
\description{
A virtual class for representing an object of type variable.
}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Slots}{
  \describe{
    \item{\code{codes}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{missings}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{values}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{description}:}{Object of class \code{"character"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{as.Dataset}{\code{signature(object = "Variable")}: ... }
    \item{[<-}{\code{signature(x = "Variable")}: ... }
    \item{[}{\code{signature(x = "Variable")}: ... }
    \item{codes<-}{\code{signature(object = "Variable")}: ... }
    \item{codes}{\code{signature(object = "Variable")}: ... }
    \item{codes<-}{\code{signature(object = "Variable")}: ... }
    \item{codes}{\code{signature(object = "Variable")}: ... }
    \item{coerce}{\code{signature(from = "Variable", to = "Dataset")}: ... }
    \item{contains}{\code{signature(object = "Variable", ch = "character")}: ... }
    \item{description<-}{\code{signature(object = "Variable")}: ... }
    \item{description}{\code{signature(object = "Variable")}: ... }
    \item{length}{\code{signature(x = "Variable")}: ... }
    \item{missings<-}{\code{signature(object = "Variable")}: ... }
    \item{missings}{\code{signature(object = "Variable")}: ... }
    \item{valid}{\code{signature(object = "Variable")}: ... }
    \item{values<-}{\code{signature(object = "Variable")}: ... }
    \item{values}{\code{signature(object = "Variable")}: ... }
    \item{values.permut}{\code{signature(object = "Variable", i = "numeric", j = "numeric")}: ... }
    \item{values.reverse}{\code{signature(object = "Variable")}: ... }
	 }
}

\author{
Emmanuel Rousseaux
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("Variable")
}
\keyword{classes}
