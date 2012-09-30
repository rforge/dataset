\name{bivan}
\alias{bivan}
\title{
Bivariate analysis measures computation
}
\description{
\code{bivan} computes some main bivariate analysis measures.
}
\usage{
bivan(
  formula,
  data,
  chi2 = T,
  phi = F,
  tschuprow = F,
  cramer.v = T,
  pearson.contingency = F,
  likelihood.ratio = F,
  gk.lambda = F,
  gk.tau = F,
  gk.tau.sqrt = T,
  theil.u = F,
  theil.u.sqrt = F,
  kendall.tau.a = F,
  kendall.tau.b = F,
  stuart.tau.c = F,
  gk.gamma = F,
  somer.d = T,
  wilson.e = F,
  calc.spearman.rho = F,
  stdres = T,
  quiet = F
  )
}
\arguments{
  \item{formula}{
a \code{formula} specifying the target and the predictors. Only first order formula can be supply.
}
  \item{data}{
a \code{Dataset} object.
}
  \item{chi2}{
if \code{TRUE} the Pearson's Khi-squared measure is computed.
}
  \item{phi}{
if \code{TRUE} the Phi measure is computed.
}
  \item{tschuprow}{
if \code{TRUE} the Tschuprow's coefficient is computed.
}
  \item{cramer.v}{
if \code{TRUE} the Cramer's V is computed.
}
  \item{pearson.contingency}{
if \code{TRUE} the Pearson contingency coefficient is computed.
}
  \item{likelihood.ratio}{
if \code{TRUE} the Khi-squared likelihood ratio is computed.
}
  \item{gk.lambda}{
if \code{TRUE} the Goodman and Kruskal's lambda is computed.
}
  \item{gk.tau}{
if \code{TRUE} the Goodman and Kruskal's tau is computed.
}
  \item{gk.tau.sqrt}{
if \code{TRUE} the square root of the Goodman and Kruskal's tau is computed.
}
  \item{theil.u}{
if \code{TRUE} the Theil's u is computed.
}
  \item{theil.u.sqrt}{
if \code{TRUE} the square root Theil's u is computed.
}
  \item{kendall.tau.a}{
if \code{TRUE} the Kendall's tau a is computed.
}
  \item{kendall.tau.b}{
if \code{TRUE} the Kendall's tau b is computed.
}
  \item{stuart.tau.c}{
if \code{TRUE} the Stuart's tau c is computed.
}
  \item{gk.gamma}{
if \code{TRUE} the Goodman and Kruskal's gamma is computed.
}
  \item{somer.d}{
if \code{TRUE} the Somer's D is computed.
}
  \item{wilson.e}{
if \code{TRUE} the Wilson's e is computed.
}
  \item{calc.spearman.rho}{
if \code{TRUE} the Spearman's rho is computed.
}
  \item{stdres}{
if \code{TRUE} the Pearson's Khi-squared standardize residuals are computed.
}
  \item{quiet}{
if \code{TRUE} console messages are turned of.
}
}
\details{
...
}
\value{
A \code{Statdf} object containing all the statistics asked and their approximate p-values (based on asymptotic variance approximation).
}
\references{
Mettre les papiers de toutes les mesures, v de cramer, phi, tschuprow, goodman and kruskal, theil, somer, wilson, ...

Gilbert Ritschard, Jean Kellerhals, Michael Olszak, and Massimo Sardi: Path analysis with partial association measures. International Journal of Methodology, vol 30, number 1, February 1996.
}
\author{
Emmanuel Rousseaux, Gilbert Ritschard.
}

\seealso{
\code{\link{Statdf}}
}
\examples{
# biv1 <- bivan(
#  formula = target ~ predictor1 + predictor2,
#  data = myData
# )
## print biv1
# biv1
## get global measures
# g <- globals(biv1)
## convert them to data.frame
# g.df <- sdf(g)
}
\keyword{bivan}
\keyword{bivariate}
\keyword{analysis}

