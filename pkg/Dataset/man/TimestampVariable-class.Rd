\name{TimestampVariable-class}
\Rdversion{1.1}
\docType{class}
\alias{TimestampVariable-class}
\alias{as.character,TimestampVariable-method}
\alias{as.data.frame,TimestampVariable-method}
\alias{as.Date,TimestampVariable-method}
\alias{as.numeric,TimestampVariable-method}
\alias{as.vector,TimestampVariable-method}
\alias{format<-,TimestampVariable-method}
\alias{format,TimestampVariable-method}
\alias{max,TimestampVariable-method}
\alias{mean,TimestampVariable-method}
\alias{median,TimestampVariable-method}
\alias{min,TimestampVariable-method}
\alias{nmissings,TimestampVariable-method}
\alias{origin<-,TimestampVariable-method}
\alias{origin,TimestampVariable-method}
\alias{print,TimestampVariable-method}
\alias{show,TimestampVariable-method}
\alias{summary,TimestampVariable-method}

\title{Class \code{"TimestampVariable"}}
\description{
An object for storing timestamp variables.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("TimestampVariable", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{origin}:}{Object of class \code{"character"} ~~ }
    \item{\code{format}:}{Object of class \code{"character"} ~~ }
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
    \item{as.character}{\code{signature(x = "TimestampVariable")}: ... }
    \item{as.data.frame}{\code{signature(x = "TimestampVariable")}: ... }
    \item{as.Date}{\code{signature(x = "TimestampVariable")}: ... }
    \item{as.numeric}{\code{signature(x = "TimestampVariable")}: ... }
    \item{as.vector}{\code{signature(x = "TimestampVariable")}: ... }
    \item{format<-}{\code{signature(object = "TimestampVariable")}: ... }
    \item{format}{\code{signature(object = "TimestampVariable")}: ... }
    \item{max}{\code{signature(x = "TimestampVariable")}: ... }
    \item{mean}{\code{signature(x = "TimestampVariable")}: ... }
    \item{median}{\code{signature(x = "TimestampVariable")}: ... }
    \item{min}{\code{signature(x = "TimestampVariable")}: ... }
    \item{nmissings}{\code{signature(object = "TimestampVariable")}: ... }
    \item{origin<-}{\code{signature(object = "TimestampVariable")}: ... }
    \item{origin}{\code{signature(object = "TimestampVariable")}: ... }
    \item{print}{\code{signature(x = "TimestampVariable")}: ... }
    \item{show}{\code{signature(object = "TimestampVariable")}: ... }
    \item{summary}{\code{signature(object = "TimestampVariable")}: ... }
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
showClass("TimestampVariable")
}
\keyword{classes}
