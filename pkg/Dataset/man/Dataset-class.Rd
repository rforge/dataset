\name{Dataset-class}
\Rdversion{1.1}
\docType{class}
\alias{Dataset-class}
\alias{as.data.frame,Dataset-method}
\alias{[<-,Dataset-method}
\alias{[,Dataset-method}
\alias{[[<-,Dataset-method}
\alias{[[,Dataset-method}
\alias{$,Dataset-method}
\alias{coerce,Dataset,data.frame-method}
\alias{coerce,Variable,Dataset-method}
\alias{head,Dataset-method}
\alias{names<-,Dataset-method}
\alias{names,Dataset-method}
\alias{ncol,Dataset-method}
\alias{nrow,Dataset-method}
\alias{print,Dataset-method}
\alias{row.names<-,Dataset-method}
\alias{row.names,Dataset-method}
\alias{show,Dataset-method}
\alias{summary,Dataset-method}
\alias{tail,Dataset-method}
\alias{weights,Dataset-method}

\title{Class \code{"Dataset"}}
\description{
An object for storing a data set.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Dataset", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{name}:}{Object of class \code{"character"} ~~ }
    \item{\code{description}:}{Object of class \code{"character"} ~~ }
    \item{\code{variables}:}{Object of class \code{"list"} ~~ }
    \item{\code{row.names}:}{Object of class \code{"character"} ~~ }
    \item{\code{weights}:}{Object of class \code{"character"} ~~ }
    \item{\code{infos}:}{Object of class \code{"list"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{as.data.frame}{\code{signature(x = "Dataset")}: ... }
    \item{[<-}{\code{signature(x = "Dataset")}: ... }
    \item{[}{\code{signature(x = "Dataset")}: ... }
    \item{[[<-}{\code{signature(x = "Dataset")}: ... }
    \item{[[}{\code{signature(x = "Dataset")}: ... }
    \item{$}{\code{signature(x = "Dataset")}: ... }
    \item{coerce}{\code{signature(from = "Dataset", to = "data.frame")}: ... }
    \item{coerce}{\code{signature(from = "Variable", to = "Dataset")}: ... }
    \item{head}{\code{signature(x = "Dataset")}: ... }
    \item{names<-}{\code{signature(x = "Dataset")}: ... }
    \item{names}{\code{signature(x = "Dataset")}: ... }
    \item{ncol}{\code{signature(x = "Dataset")}: ... }
    \item{nrow}{\code{signature(x = "Dataset")}: ... }
    \item{print}{\code{signature(x = "Dataset")}: ... }
    \item{row.names<-}{\code{signature(x = "Dataset")}: ... }
    \item{row.names}{\code{signature(x = "Dataset")}: ... }
    \item{show}{\code{signature(object = "Dataset")}: ... }
    \item{summary}{\code{signature(object = "Dataset")}: ... }
    \item{tail}{\code{signature(x = "Dataset")}: ... }
    \item{weights}{\code{signature(object = "Dataset")}: ... }
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
showClass("Dataset")
}
\keyword{classes}
