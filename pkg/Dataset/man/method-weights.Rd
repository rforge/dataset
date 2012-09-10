\name{weights-methods}
\docType{methods}
\alias{weights}
\alias{weights-methods}
\alias{weights,ANY-method}
\alias{weights,Dataset-method}
\title{Methods for Function \code{weights} in Packages \pkg{stats} and \pkg{Dataset}}
\description{
\code{weights} allows the handling of weights in statistics computations.
}
\section{Methods}{
\describe{

\item{\code{signature(object = "ANY")}}{
See the documentation in the \pkg{stats} package.
}

\item{\code{signature(object = "Dataset")}}{
\code{weights} method for a \code{Dataset} object. Returns the variable containing weights used in the object.
}
}}
\keyword{methods}
\keyword{weighting}
