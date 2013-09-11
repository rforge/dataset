\name{seq.count}
\alias{seq.count}
\title{
Count the number of occurences of a given value in a channel
}
\description{
The \code{seq.count} function counts the number of occurences of a given value in a \code{channel}. Currently a \code{channel} is represented as a set of column names of a \code{data.frame} and the corresponding \code{data.frame}.
}
\usage{
seq.count(
  x,
  value,
  sequence = names(x),
  origin,
  window.start = 0,
  window.end,
  translate = NULL,
  verbose = FALSE,
  quiet = FALSE
)
}
\arguments{
  \item{x}{
a \code{data.frame}.
}
  \item{value}{
a \code{numeric} of \code{character} of length 1, corresponding to the value you want to count.
}
  \item{sequence}{
a \code{character}. The columns of \code{x} in which you want to count.
}
  \item{origin}{
a \code{character}. The column from which you want to start to count.
}
  \item{window.start}{
a \code{numeric}. Allow to translate the start. Positive or negative values are accepted. Default is 0, meaning we start at the \code{origin}.
}
  \item{window.end}{
a \code{numeric}. Specify how many of column from the origin you want to take. For example, if you set the parameter to 1, you will count on only the column \code{origin}. And if you set the parameter to 10, you will use the column \code{origin} and the 9 following.
}
  \item{translate}{
a \code{numeric} or \code{character} of lenght 1. If a \code{numeric} is used, all the window will be translated according to the value provided: the same translation is applied for each individuals. If a \code{character} is used, the function will look for a column with the corresponding name and will translate the window of each row according to its value on the variable used for translating. Furthermore, the argument can be named with 'left', meaning that the translation will be performed negatively (on the left) instead of positively (on the right, the default).
}
  \item{verbose}{
a \code{logical}. See the Section Value.
}
  \item{quiet}{
a \code{numeric}. If \code{TRUE} information messages won't be displayed. Default is \code{FALSE}.
}
}
\details{
Please take care that if you use a variable for translating windows for each individual, and this variable contains NA, the corresponding rows will be removed. In this case you will be alerted by a warning.
}
\value{
A \code{data.frame}. If \code{verbose} is \code{FALSE} the \code{data.frame} contains only one columns, named \code{COUNT}, corresponding to the count for each individual. If \code{verbose} is \code{TRUE} the \code{data.frame} will also contains columns helping to control how the count was performed: \code{START}, \code{STOP}, \code{RANGE}, \code{TRANSLATE} and the columns corresponding to the \code{channel}.
}
\author{
Emmanuel Rousseaux
}
\seealso{
\code{\link{find.value.fun}}, \code{\link{findlast}}.
}
\examples{
# data.frame example
data.ex <- data.frame(
  "id" = 1:7,
  "age.2010" =  c(5,3,0,1,4,NA,30),
  "vuln.2000" = c(0,1,1,0,NA,1,1),
  "vuln.2001" = c(0,0,1,0,NA,1,1),
  "vuln.2002" = c(0,1,0,1,NA,0,0),
  "vuln.2003" = c(0,0,0,1,NA,1,1),
  "vuln.2004" = c(0,NA,1,1,NA,1,1),
  "vuln.2005" = c(0,0,1,0,NA,0,1),
  "vuln.2006" = c(0,0,1,0,NA,0,1),
  "vuln.2007" = c(0,0,0,1,NA,1,1),
  "vuln.2008" = c(0,0,1,1,NA,1,0),
  "vuln.2009" = c(0,0,0,1,NA,1,0),
  "vuln.2010" = c(0,0,1,1,NA,1,1)
)
data.ex

#### Example 0
# We count only in the year 2006
count0 <- seq.count(
  x = data.ex,
  value = 1,
  sequence = c("vuln.2000","vuln.2001","vuln.2002","vuln.2003","vuln.2004","vuln.2005","vuln.2006","vuln.2007","vuln.2008","vuln.2009","vuln.2010"),
  origin = "vuln.2006",
  window.end = 1 # we take only 1 measure
)
count0

# Same example with the mode 'verbose' on
count0 <- seq.count(
  x = data.ex,
  value = 1,
  sequence = c("vuln.2000","vuln.2001","vuln.2002","vuln.2003","vuln.2004","vuln.2005","vuln.2006","vuln.2007","vuln.2008","vuln.2009","vuln.2010"),
  origin = "vuln.2006",
  window.end = 1, # we take only 1 measure
  verbose = TRUE
)
count0


#### Example 1
# We count between 2006 (included) and 2008 (included)
count1 <- seq.count(
  x = data.ex,
  value = 1,
  sequence = c("vuln.2000","vuln.2001","vuln.2002","vuln.2003","vuln.2004","vuln.2005","vuln.2006","vuln.2007","vuln.2008","vuln.2009","vuln.2010"),
  origin = "vuln.2006",
  window.end = 3
)
count1

# Same example with the mode 'verbose' on
count1 <- seq.count(
  x = data.ex,
  value = 1,
  sequence = c("vuln.2000","vuln.2001","vuln.2002","vuln.2003","vuln.2004","vuln.2005","vuln.2006","vuln.2007","vuln.2008","vuln.2009","vuln.2010"),
  origin = "vuln.2006",
  window.end = 3,
  verbose = TRUE
)
count1


#### Example 2
# For each individual we count occurences of '1' when there where between 0 and 2 years (included)
count2 <- seq.count(
  x = data.ex,
  value = 1,
  sequence = c("vuln.2000","vuln.2001","vuln.2002","vuln.2003","vuln.2004","vuln.2005","vuln.2006","vuln.2007","vuln.2008","vuln.2009","vuln.2010"),
  origin = "vuln.2010", # as we have the age in 2010, we use vuln.2010 as origin
  window.end = 3, # 0, 1 and 2 years
  translate = c('left' = 'age.2010')
)
count2

# Same example with the mode 'verbose' on
count2 <- seq.count(
  x = data.ex,
  value = 1,
  sequence = c("vuln.2000","vuln.2001","vuln.2002","vuln.2003","vuln.2004","vuln.2005","vuln.2006","vuln.2007","vuln.2008","vuln.2009","vuln.2010"),
  origin = "vuln.2010", # as we have the age in 2010, we use vuln.2010 as origin
  window.end = 3, # 0, 1 and 2 years
  translate = c('left' = 'age.2010'),
  verbose = TRUE
)
count2


#### Example 3
# For each individual we count occurences of '1' when there were 2 years
count3 <- seq.count(
  x = data.ex,
  value = 1,
  sequence = c("vuln.2000","vuln.2001","vuln.2002","vuln.2003","vuln.2004","vuln.2005","vuln.2006","vuln.2007","vuln.2008","vuln.2009","vuln.2010"),
  origin = "vuln.2010",
  window.start = 2, # we start two values after origin: corresponding to 2 years old
  window.end = 3, # and we stop three values after origin: we get only the value at 2 years
  translate = c('left' = 'age.2010'),
  verbose = TRUE
)
count3

#### Example 4
# For each individual we count occurences of '1' when there were betweeb 2 years and 5 years
count4 <- seq.count(
  x = data.ex,
  value = 1,
  sequence = c("vuln.2000","vuln.2001","vuln.2002","vuln.2003","vuln.2004","vuln.2005","vuln.2006","vuln.2007","vuln.2008","vuln.2009","vuln.2010"),
  origin = "vuln.2010",
  window.start = 2, # we start two values after origin: corresponding to 2 years old
  window.end = 6,  # and we stop six values after origin: we get the count for the range [2-5]
  translate = c('left' = 'age.2010'),
  verbose = TRUE
)
count4
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ count }
\keyword{ sequence }% __ONLY ONE__ keyword per line
\keyword{ trajectory }
