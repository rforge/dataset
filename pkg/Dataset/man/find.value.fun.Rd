\name{find.value.fun}
\alias{find.value.fun}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find the last occurence of the higher/lower value in a \code{data.frame} object.
}
\description{
The \code{find.value.fun} function allows to find the last occurence of the higher/lower value in a set of columns of a \code{data.frame} object, and returns these values. You can then retrieve these last values for other sets of columns of the \code{data.frame} object.
}
\usage{
find.value.fun(data, from, to, fun = "max")
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
  \item{fun}{
a \code{character}, the name of the operator to use: currently 'max' or 'min'.
}
}
\details{
This function is useful when working on longitudinal data with attrition problems. It allows to retrieve the last occurence of the higher/lower value in a specific time-varying variable and then getting the values for other time-varying variables according to the indexs previously reported.
For numerical time-varying variable, the numeric order is used. For factors, order of levels has to be specified in the factor corresponding to the first year (see example below).
}
\value{
If the \code{to} argument is missing, the function returns the ids where the last occurence of the higher/lower value is for each row of the \code{data} argument. It also return a column containing the value retrieved. NA will be returned for both columns if the sequence contains only NA.
If the \code{to} argument is given, a \code{data.frame} containing the corresponding last values on covariates is returned.
}
\author{
Emmanuel Rousseaux
}

\seealso{
\code{\link{findlast}}, \code{\link{find.value.last}}.
}
\examples{
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
  'education11' = c(NA,NA,NA,8,NA),
  'health99' = factor(c('very well',NA,NA,NA,NA), levels = c("poor", "well", "very well")), # we specify the order of levels to use in the first year of the variable
  'health00' = c('very well',NA,NA,NA,NA),
  'health01' = c('very well','well',NA,NA,NA),
  'health02' = c('very well','well','poor',NA,NA),
  'health03' = c('well','very well',NA,NA,NA),
  'health04' = c(NA,'very well','poor',NA,NA),
  'health05' = c(NA,'very well','poor',NA,NA),
  'health06' = c(NA,NA,'poor',NA,NA),
  'health07' = c(NA,NA,NA,'very well',NA),
  'health08' = c(NA,NA,NA,'very well',NA),
  'health09' = c(NA,NA,NA,'poor',NA),
  'health10' = c(NA,NA,NA,'poor',NA),
  'health11' = c(NA,NA,NA,'well',NA)
)

# we get column ids where the last occurence of the higher value is
find.value.fun(
  data = fv.data.example,
  from = 1:13 # for unemployment
)
find.value.fun(
  data = fv.data.example,
  from = 40:52 # for health
)

# we retrieve the last occurence of the higher value for unemployment, and get corresponding values for the variables age, education and health.
find.value.fun(
  data = fv.data.example,
  from = 1:13,
  to = list(
    'age' = 14:26,
    'education' = 27:39,
    'health' = 40:52
  )
)

# we retrieve the last occurence of the higher value for health, and get corresponding values for the variables unemployment, age and education.
find.value.fun(
  data = fv.data.example,
  from = 40:52,
  to = list(
    'age' = 14:26,
    'education' = 27:39,
    'health' = 40:52
  )
)

# same operation but here for the min
find.value.fun(
  data = fv.data.example,
  from = 40:52,
  to = list(
    'age' = 14:26,
    'education' = 27:39,
    'health' = 40:52
  ),
  fun = 'min'
)

}

\keyword{ find }
\keyword{ value }
\keyword{ max }
\keyword{ min }
\keyword{ higher }
\keyword{ lower }
