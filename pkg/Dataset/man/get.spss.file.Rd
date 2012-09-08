\name{get.spss.file}
\alias{get.spss.file}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
get.spss.file
}
\description{
Import a SPSS data base to a data base in format \code{Dataset}.
}
\usage{
get.spss.file(
  datadir,
  file, 
	variables = NULL,
	tsvar = character(0),
  ovar = character(0),
  wvar = character(0),
  use.coding.order = "increasing",
	max.value.labels = Inf,
	savingName = NULL,
	lowernames = TRUE,
	name = NULL,
	description = character(0),
	summaryToPDF = TRUE
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datadir}{
the directory where your SPSS file is. If missing the working directory is used.
}
  \item{file}{
the full name of the file you want to import.
}
  \item{variables}{
the list of variables you want to import. If missing all variables in the SPSS file are imported.
}
  \item{tsvar}{
a vector containing the names of variables having to be converted in the \code{TimestampVariable} type. The SPSS timestamp origin "1582-10-14" is used to set the origin of the time variable.
}
  \item{ovar}{
a vector containing the names of variables having to be converted in the \code{OrdinalVariable} type.
}
  \item{wvar}{
a vector containing the names of variables having to be converted in the \code{WeightingVariable} type.
}
  \item{use.coding.order}{
a character, either \code{"increasing"}, \code{"decreasing"}, \code{"default"}. If \code{"increasing"} the coding order is used to sort the order in the values of the Variable. If \code{"decreasing"} the order is in a decreasing way. If \code{"default"} no sorting is performed. Default is \code{"increasing"}.
}
  \item{max.value.labels}{
depreciated. Will be soon removed.
}
  \item{savingName}{
a character. By setting this argument you produce an export in .Rdata of the SPSS file with the name given.
}
  \item{lowernames}{
a logical. If \code{TRUE} all names are set in lower case.
}
  \item{name}{
a character. The name of the data base. If \code{NULL} the name of the file is used.
}
  \item{description}{
a character. A description of the database.
}
  \item{summaryToPDF}{
a logical. If \code{TRUE} the summary in PDF of the data base is created.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A \code{Dataset} object containing your SPSS data base.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Emmanuel Rousseaux
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
%\examples{
%%  ~~examples~~
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ load }
\keyword{ data }
\keyword{ SPSS }% __ONLY ONE__ keyword per line
