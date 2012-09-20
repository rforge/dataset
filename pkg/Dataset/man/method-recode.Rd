\name{recode-methods}
\docType{methods}
\alias{recode}
\alias{recode-methods}
\alias{recode,CategoricalVariable,list-method}
\title{Perform a recoding of a categorical variable}
\description{
\code{recode} performs a recoding of an object inheriting of a \code{CategoricalVariable} object.
}
\usage{
recode(object, recoding, ...)
}
\section{Methods}{
\describe{

\item{\code{signature(object = "CategoricalVariable", recoding = "list")}}{
\code{recode} method for a \code{CategoricalVariable} object.
}
}}
\arguments{
  \item{object}{
an object inheriting of the class \code{CategoricalVariable}
}
  \item{recoding}{
a \code{list} with the new values as names, and for each element a vector specifying the old values to be merged to create the new value. If a vector of old values is \code{numeric} the function expects codes, and if \code{character} the function expects short value labels.
}
  \item{\dots}{
Not used.
}
}
\details{
A cross table is printed to allow the user checking the recoding performed correctly.
}
\value{
A \code{CategoricalVariable} with the new coding scheme.
}
\author{
Emmanuel Rousseaux
}

\seealso{
\code{\link{cut}}
}
\examples{
data(dds)
#TO WRITE
}
\keyword{methods}
\keyword{categorical}
\keyword{recode}
