\name{Rsocialdata0-package}
\alias{Rsocialdata0-package}
\alias{Rsocialdata0}
\docType{package}
\title{
Handling survey data in R. (beta, not for use)
}
\description{
This package provides a class, called Rsocialdata0, and some
methods allowing to handle efficiently survey data: handle variable
labels, values labels, and retrieve information about missing values   
like in a SPSSdata set file.
In particularity we can switch a specific
missing value type from a NA to a level to use it in the
analysis. A function allowing to import data from a SPSS file
to a Rsocialdata0 object is provided.
}
\details{
\tabular{ll}{
Package: \tab Rsocialdata0\cr
Type: \tab Package\cr
Version: \tab 0.2.60.0\cr
Date: \tab 2014-04-20\cr
Depends: \tab R (>= 2.10.1), methods, stats\cr
Suggests: \tab foreign, xtable\cr
LazyLoad: \tab yes\cr
LazyData: \tab yes\cr
ZipData: \tab no\cr
License: \tab GPL (>= 2)\cr
URL: \tab http://rforge.fr/Rsocialdata0\cr
}

Further information is available in the following vignettes:
\tabular{ll}{
\code{Rsocialdata0-vignette-overview} \tab Rsocialdata0 Overview (source, pdf)\cr
}
An overview of how to use the package, including the most important functions.
}
\author{
Emmanuel Rousseaux <emmanuel.rousseaux+Rsocialdata0@gmail.com>, Gilbert Ritschard <gilbert.ritschard@unige.ch>

Maintainer: Emmanuel Rousseaux <emmanuel.rousseaux+Rsocialdata0@gmail.com>
}
\references{
...
}

\keyword{ package }
\keyword{ data }
\keyword{ survey }
\seealso{
\code{\link{dds}}, \code{\link{get.spss.file}}
}
\examples{
# forthcoming
}
