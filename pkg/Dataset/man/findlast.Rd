\name{findlast}
\alias{findlast}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find the last valid cases in a \code{data.frame} object.
}
\description{
The \code{findlast} function allows to find the last non-missing value in a set of columns of a \code{data.frame} object, and returns these values. You can then retrieve these last values for others sets of columns of the \code{data.frame} object.
}
\usage{
findlast(data, from, to)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
a \code{data.frame} object.
}
  \item{from}{
a \code{vector} of either column names or column ids from where you want to find the where the last non-missing value is.
}
  \item{to}{
a \code{list} containing in each item the column names or ids of another time-varying variable for which you want to get the last value found in data[,from]. The name of each item is used as column name for the output.
}
}
\details{
This function is useful when working on longitudinal data with attrition problems. It allows to retrieve the last values given by repondants in a specific time-varying variable and then getting the values for other time-varying variables according to the index of last participation of each respondant.
}
\value{
If the \code{to} argument is missing, the function returns the ids where the last non-missing value is for each row of the \code{data argument}.
If the \code{to} argument is given, a \code{data.frame} containing the last values is returned.
}

\author{
Emmanuel Rousseaux
}

\examples{
# data.frame example
fl.data.example <- data.frame(
  'age00' = c(48,NA,34,63,NA,27),
  'age01' = c(NA,27,35,64,NA, NA),
  'age02' = c(50,28,36,NA,NA, NA),
  'sex00' = c('M',NA,'F','M',NA,'M'),
  'sex01' = c(NA,'F','F',NA,NA, NA),
  'sex02' = c('M',NA,'F',NA,'M', NA),
  'nationality00' = factor(c('v1',NA,'v1','v1',NA,'o')),
  'nationality01' = factor(c(NA,'o','v2','o',NA, NA)),
  'nationality02' = factor(c('o',NA,'v2',NA,'o', NA))
)

# we get the column ids where the last non-missing value is
findlast(
  data = fl.data.example,
  from = c('age00', 'age01', 'age02')
)

# we find retrieve the last value for age, and get corresponding values for the variables sex and nationality
findlast(
  data = fl.data.example,
  from = c('age00', 'age01', 'age02'),
  to = list(
    'age' = c('age00', 'age01', 'age02'),
    'sex' = c('sex00', 'sex01', 'sex02'),
    'nationality' = c('nationality00','nationality01','nationality02')
  )
)

# same operation but using column ids instead of column names
findlast(
  data = fl.data.example,
  from = 1:3,
  to = list(
    'age' = 1:3,
    'sex' = 4:6,
    'nationality' = 7:9
  )
)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ find }
\keyword{ last }% __ONLY ONE__ keyword per line
