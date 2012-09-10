\name{bivan}
\alias{bivan}
\title{
Compute some bivariate analysis
}
\description{
\code{bivan} returns the results of the bivariate analysis requested by the user.
}
\usage{
  bivan(
  formula,
  data,
  chi2 = TRUE,
  cramer.v = TRUE,
  gk.tau = TRUE,
  somer.d = TRUE,
  stdres = TRUE,
  quiet = FALSE
)
}
\arguments{
  \item{formula}{
a \code{formula}
}
  \item{data}{
a \code{Dataset}
}
  \item{chi2}{
a \code{logical}
}
  \item{cramer.v}{
a \code{logical}
}
  \item{gk.tau}{
a \code{logical}
}
  \item{somer.d}{
a \code{logical}
}
  \item{stdres}{
a \code{logical}
}
  \item{quiet}{
a \code{logical}
}
}
\details{
...
}
\value{
A \code{Bivan} object.
}
\references{
Put papers for GK tau, Somer's D, ...
}
\author{
Emmanuel Rousseaux
}
\note{
...
}

\seealso{
...
}
\examples{
##
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{bivariate}
\keyword{chisq}% __ONLY ONE__ keyword per line
