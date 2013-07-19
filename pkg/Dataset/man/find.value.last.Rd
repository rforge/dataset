\name{find.value.last}
\alias{find.value.last}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find the last occurence of a specific value in a \code{data.frame} object.
}
\description{
The \code{find.value.last} function allows to find the last occurence of a specific value in a set of columns of a \code{data.frame} object, and returns these values. You can then retrieve these last values for others sets of columns of the \code{data.frame} object.
}
\usage{
find.value.last(data, from, to, value = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
a \code{data.frame} object.
}
  \item{from}{
a \code{vector} of either column names or column ids from where you want to find where the last occurence of the value is.
}
  \item{to}{
a \code{list} containing in each item the column names or ids of another time-varying variable for which you want to get the last value found in data[,from]. The name of each item is used as column name for the output.
}
  \item{value}{
an \code{integer}, the value/event you are looking for.
}
}
\details{
This function is useful when working on longitudinal data with attrition problems. It allows to retrieve the last occurence of an event in a specific time-varying variable and then getting the values for other time-varying variables according to the indexs previously reported.
}
\value{
If the \code{to} argument is missing, the function returns the ids where the last occurence of the value is for each row of the \code{data} argument. It also return a column containing the type of the value retrieved : 1 if the value has been found, NA if the sequence contained only NA values, -1 if the sequence contained non-NA values but no occurence of the value.
If the \code{to} argument is given, a \code{data.frame} containing the corresponding last values on covariates is returned.
}

\author{
Emmanuel Rousseaux
}

\seealso{
\code{\link{findlast}}, \code{\link{find.value.fun}}, \code{\link{seq.count}}.
}

\examples{
# data.frame example
fv.data.example <- data.frame(
  'unemployment99' = c(0,NA,NA,NA,NA),
  'unemployment00' = c(0,NA,NA,NA,NA),
  'unemployment01' = c(1,0,NA,NA,NA),
  'unemployment02' = c(0,0,0,NA,NA),
  'unemployment03' = c(0,1,NA,NA,NA),
  'unemployment04' = c(NA,1,1,NA,NA),
  'unemployment05' = c(NA,1,1,NA,NA),
  'unemployment06' = c(NA,NA,1,NA,NA),
  'unemployment07' = c(NA,NA,NA,0,NA),
  'unemployment08' = c(NA,NA,NA,0,NA),
  'unemployment09' = c(NA,NA,NA,0,NA),
  'unemployment10' = c(NA,NA,NA,0,NA),
  'unemployment11' = c(NA,NA,NA,0,NA),
  'age99' = c(15,NA,NA,NA,NA),
  'age00' = c(16,NA,NA,NA,NA),
  'age01' = c(17,20,NA,NA,NA),
  'age02' = c(18,21,23,NA,NA),
  'age03' = c(19,22,NA,NA,NA),
  'age04' = c(NA,23,25,NA,NA),
  'age05' = c(NA,24,26,NA,NA),
  'age06' = c(NA,NA,27,NA,NA),
  'age07' = c(NA,NA,NA,29,NA),
  'age08' = c(NA,NA,NA,30,NA),
  'age09' = c(NA,NA,NA,31,NA),
  'age10' = c(NA,NA,NA,32,NA),
  'age11' = c(NA,NA,NA,33,NA),
  'education99' = c(8,NA,NA,NA,NA),
  'education00' = c(8,NA,NA,NA,NA),
  'education01' = c(8,3,NA,NA,NA),
  'education02' = c(8,3,2,NA,NA),
  'education03' = c(8,4,NA,NA,NA),
  'education04' = c(NA,4,2,NA,NA),
  'education05' = c(NA,4,2,NA,NA),
  'education06' = c(NA,NA,3,NA,NA),
  'education07' = c(NA,NA,NA,7,NA),
  'education08' = c(NA,NA,NA,7,NA),
  'education09' = c(NA,NA,NA,7,NA),
  'education10' = c(NA,NA,NA,8,NA),
  'education11' = c(NA,NA,NA,8,NA)
)
# we get the column ids where the last occurence of the value 1 is
find.value.last(
  data = fv.data.example,
  from = 1:13,
  value = 1
)

# we retrieve the last occurence of the value 1 for unemployment, and get corresponding values for the variables age and education
find.value.last(
  data = fv.data.example,
  from = 1:13,
  to = list(
    'age' = 14:26,
    'education' = 27:39
  ),
  value = 1
)

# same operation but here for the value 0
find.value.last(
  data = fv.data.example,
  from = 1:13,
  to = list(
    'age' = 14:26,
    'education' = 27:39
  ),
  value = 0
)
}
\keyword{ find }
\keyword{ last }
\keyword{ event }
