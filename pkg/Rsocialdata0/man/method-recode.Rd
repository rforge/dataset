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
recode(object, ...)
}
\section{Methods}{
\describe{

\item{\code{signature(object = "CategoricalVariable")}}{
\code{recode} method for a \code{CategoricalVariable} object.
}
}}
\arguments{
  \item{object}{
an object inheriting of the class \code{CategoricalVariable}
}
  \item{\dots}{
Enter each new value as an option name, and for each element give a vector specifying the old values to be merged to create the new value. If a vector of old values is \code{numeric} the function expects codes, and if \code{character} the function expects short value labels.
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
%data(dds)
%valids(dds$connaissetcomp.acquis.1)
%dds$connaissetcomp.acquis.1.in3 <- recode(
%  dds$connaissetcomp.acquis.1,
%  'no' = 1:2,
%  'average' = 3,
%  'yes' = 4:5
%)
}
\keyword{methods}
\keyword{categorical}
\keyword{recode}
