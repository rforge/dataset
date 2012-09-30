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
  description.chlength = 300,
  valids.chlength = 40,
  valids.cut.percent = 0.5,
  sorting = "decreasing",
  dateformat,
  latexPackages = NULL,
  width.id = 0.5,
  width.varname = 1.5,
  width.description = 10.5,
  width.n = 0.8,
  width.na = 1.2,
  width.valids = 5,
  width.valids.nao.inc = 5,
  width.min = 1.25,
  width.max = 1.25,
  width.mean = 1.25,
  width.stddev = 1.25,
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
  \item{valids.chlength}{
a \code{numeric}
}
  \item{valids.cut.percent}{
a \code{numeric}. The minimal frequency need for a valid case to be reported in the document.
}
  \item{sorting}{
a \code{character}. If "decreasing" valid cases are sorted in decrease order of their frequencies. If "increase" an increasing order is used.
}
  \item{dateformat}{
a \code{character}
}
  \item{latexPackages}{
a \code{character}. You can add LaTeX packages you need for the compilation here, especially for supporting other languages than english.
}
  \item{width.id}{
a \code{numeric}. Width for the variable row index column, in cm.
}
  \item{width.varname}{
a \code{numeric}. Width for variable name column, in cm.
}
  \item{width.description}{
a \code{numeric}. Width for description column, in cm.
}
  \item{width.n}{
a \code{numeric}. Width for number of valid cases column, in cm.
}
  \item{width.na}{
a \code{numeric}. Width for percents of missing values column, in cm.
}
  \item{width.valids}{
a \code{numeric}. For categorical variables, width for the valid cases listing column, in cm.
}
  \item{width.valids.nao.inc}{
a \code{numeric}. For nominal and ordinal variables, allow to increase the width of the valid cases listing column by reducing the description column, in cm.
}
  \item{width.min}{
a \code{numeric}. For quantitative variables, the with of the minimal values column, in cm.
}
  \item{width.max}{
a \code{numeric}. For quantitative variables, the with of the maximal values column, in cm.
}
  \item{width.mean}{
a \code{numeric}. For quantitative variables, the with of the means column, in cm.
}
  \item{width.stddev}{
a \code{numeric}. For quantitative variables, the with of the standard deviations column, in cm.
}
  \item{keepTex}{
a \code{logical}
}
  \item{openPDF}{
a \code{logical}
}
  \item{\dots}{
Other options to pass to the function. For \code{\link{Statdf}} objects the option \code{merge}, with argument 'no', 'left' or 'right' allow to merge or not the p-values columns, either with the next left or right column.
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
