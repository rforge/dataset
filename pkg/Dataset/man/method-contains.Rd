\name{contains-methods}
\docType{methods}
\alias{contains}
\alias{contains-methods}
\alias{contains,character,Dataset-method}
\alias{contains,character,Variable-method}
\title{Find a specific variable by keywords}
\description{
\code{contains} look at the description of a Variable object and test whether the keywords are in.
Option \code{ignore.case} allows to be case sensitive or not. Default is \code{FALSE}.
Option \code{and} allows to ask for the presence of all keywords (and = \code{TRUE}) or just one or more (and = \code{FALSE}). Default is (and = \code{FALSE}).
}
\section{Methods}{
\describe{

\item{\code{signature(keywords = "character", data = "Dataset")}}{
\code{contains} method for a \code{Variable} object. Value is either a \code{Dataset} containing all the variables satisfying the condition on the existence of the keywords, or \code{NULL}.
}

\item{\code{signature(keywords = "character", data = "Variable")}}{
\code{contains} method for a \code{Variable} object. Value is a \code{logical}
}
}}
\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
