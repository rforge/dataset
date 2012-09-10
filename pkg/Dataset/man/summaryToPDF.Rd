\name{summaryToPDF}
\alias{summaryToPDF}
\title{
Produce a summary in a PDF file
}
\description{
\code{summaryToPDF} produces a summary of the object in a PDF file.
}
\usage{
summaryToPDF(
  object,
  pdfSavingName,
  graphics = F,
  description.chlength = 120,
  values.chlength = 6,
  dateformat,
  latexPackages = NULL,
  keepTex = F,
  openPDF = T,
  ...
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
See the help of the method (PUT LINK)
}
  \item{pdfSavingName}{
a \code{character}
}
  \item{graphics}{
a \code{logical}
}
  \item{description.chlength}{
a \code{numeric}
}
  \item{values.chlength}{
a \code{numeric}
}
  \item{dateformat}{
a \code{character}
}
  \item{latexPackages}{
a \code{character}. You can add LaTeX packages you need for the compilation here, especially for supporting other languages than english.
}
  \item{keepTex}{
a \code{logical}
}
  \item{openPDF}{
a \code{logical}
}
  \item{\dots}{
...
}
}
\details{
...
}
\value{
NULL (to check)
}
\author{
Emmanuel Rousseaux
}
\note{
...
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{PDF}
