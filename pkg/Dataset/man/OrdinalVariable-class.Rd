\name{OrdinalVariable-class}
\Rdversion{1.1}
\docType{class}
\alias{OrdinalVariable-class}
\alias{as.factor,OrdinalVariable-method}
\alias{as.vector,OrdinalVariable-method}
\alias{summary,OrdinalVariable-method}

\title{Class \code{"OrdinalVariable"}}
\description{
An object for storing ordinal variables.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("OrdinalVariable", ...)}.
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
    \item{as.factor}{\code{signature(x = "OrdinalVariable")}: ... }
    \item{as.vector}{\code{signature(x = "OrdinalVariable")}: ... }
    \item{summary}{\code{signature(object = "OrdinalVariable")}: ... }
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
showClass("OrdinalVariable")
}
\keyword{classes}
