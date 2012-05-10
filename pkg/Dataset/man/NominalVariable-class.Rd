\name{NominalVariable-class}
\Rdversion{1.1}
\docType{class}
\alias{NominalVariable-class}
\alias{summary,NominalVariable-method}

\title{Class \code{"NominalVariable"}}
\description{
An object for storing nominal variables.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("NominalVariable", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{codes}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{values}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{missings}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{description}:}{Object of class \code{"character"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{QualitativeVariable}"}, directly.
Class \code{"\linkS4class{Variable}"}, by class "QualitativeVariable", distance 2.
}
\section{Methods}{
  \describe{
    \item{summary}{\code{signature(object = "NominalVariable")}: ... }
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
showClass("NominalVariable")
}
\keyword{classes}
