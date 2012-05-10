\name{ScaleVariable-class}
\Rdversion{1.1}
\docType{class}
\alias{ScaleVariable-class}
\alias{as.data.frame,ScaleVariable-method}
\alias{as.numeric,ScaleVariable-method}
\alias{as.vector,ScaleVariable-method}
\alias{levels,ScaleVariable-method}
\alias{max,ScaleVariable-method}
\alias{mean,ScaleVariable-method}
\alias{min,ScaleVariable-method}
\alias{nmissings,ScaleVariable-method}
\alias{print,ScaleVariable-method}
\alias{sd,ScaleVariable-method}
\alias{show,ScaleVariable-method}
\alias{summary,ScaleVariable-method}
\alias{var,ScaleVariable-method}

\title{Class \code{"ScaleVariable"}}
\description{
An object for storing scale variables.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("ScaleVariable", ...)}.
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
Class \code{"\linkS4class{QuantitativeVariable}"}, directly.
Class \code{"\linkS4class{Variable}"}, by class "QuantitativeVariable", distance 2.
}
\section{Methods}{
  \describe{
    \item{as.data.frame}{\code{signature(x = "ScaleVariable")}: ... }
    \item{as.numeric}{\code{signature(x = "ScaleVariable")}: ... }
    \item{as.vector}{\code{signature(x = "ScaleVariable")}: ... }
    \item{levels}{\code{signature(x = "ScaleVariable")}: ... }
    \item{max}{\code{signature(x = "ScaleVariable")}: ... }
    \item{mean}{\code{signature(x = "ScaleVariable")}: ... }
    \item{min}{\code{signature(x = "ScaleVariable")}: ... }
    \item{nmissings}{\code{signature(object = "ScaleVariable")}: ... }
    \item{print}{\code{signature(x = "ScaleVariable")}: ... }
    \item{sd}{\code{signature(x = "ScaleVariable")}: ... }
    \item{show}{\code{signature(object = "ScaleVariable")}: ... }
    \item{summary}{\code{signature(object = "ScaleVariable")}: ... }
    \item{var}{\code{signature(x = "ScaleVariable")}: ... }
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
showClass("ScaleVariable")
}
\keyword{classes}
