\name{summary.Statdf-class}
\Rdversion{1.1}
\docType{class}
\alias{summary.Statdf-class}
\alias{as.data.frame,summary.Statdf-method}
\alias{df,summary.Statdf-method}
\alias{df<-,summary.Statdf,data.frame-method}
\alias{print,summary.Statdf-method}
\alias{sdf,summary.Statdf-method}
\alias{sdf<-,summary.Statdf,data.frame-method}
\alias{show,summary.Statdf-method}
\alias{thresholds,summary.Statdf-method}
\alias{thresholds<-,summary.Statdf-method}

\title{Class \code{"summary.Statdf"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("summary.Statdf", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"list"} ~~ }
    \item{\code{thresholds}:}{Object of class \code{"character"} ~~ }
    \item{\code{names}:}{Object of class \code{"character"} ~~ }
    \item{\code{row.names}:}{Object of class \code{"data.frameRowLabels"} ~~ }
    \item{\code{.S3Class}:}{Object of class \code{"character"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{data.frame}"}, directly.
Class \code{"\linkS4class{list}"}, by class "data.frame", distance 2.
Class \code{"\linkS4class{oldClass}"}, by class "data.frame", distance 2.
Class \code{"\linkS4class{vector}"}, by class "data.frame", distance 3.
}
\section{Methods}{
  \describe{
    \item{as.data.frame}{\code{signature(x = "summary.Statdf")}: ... }
    \item{df}{\code{signature(object = "summary.Statdf")}: ... }
    \item{df<-}{\code{signature(object = "summary.Statdf", value = "data.frame")}: ... }
    \item{print}{\code{signature(x = "summary.Statdf")}: ... }
    \item{sdf}{\code{signature(object = "summary.Statdf")}: ... }
    \item{sdf<-}{\code{signature(object = "summary.Statdf", value = "data.frame")}: ... }
    \item{show}{\code{signature(object = "summary.Statdf")}: ... }
    \item{thresholds}{\code{signature(object = "summary.Statdf")}: ... }
    \item{thresholds<-}{\code{signature(object = "summary.Statdf")}: ... }
	 }
}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("summary.Statdf")
}
\keyword{classes}
