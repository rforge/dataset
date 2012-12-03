is.not.na <- function(x) {
  return(!is.na(x))
}
length.0.to.na <- function(x) {
  out <- x
  if (length(x) == 0) {
    out <- NA
  }
  return(out)
}
find.last.valid <- function(x) {
  nas <- lapply(as.data.frame(t(x)), is.not.na)
  w <- lapply(nas, which)
  w <- lapply(w,length.0.to.na)
  m <- lapply(w, max)
  m <- unlist(m)
  m <- as.numeric(unlist(m))
  return(m)
}
get.last.valid <- function(x, where) {
  for (i in 1:ncol(x)) {
    if (inherits(x[[i]], 'factor')) {
      x[[i]] <- as.character(x[[i]])
    }
  }
  if(missing(where)) {
    lv <- find.last.valid(x)
  } else {
    lv <- where
  }
  
  nna <- which(is.not.na(lv))
  out <- rep(NA, nrow(x))
  out[nna] <- mapply(get('[.data.frame'), list(x), (1:nrow(x))[nna], lv[nna]) 
  # note: we can also use getS3method('[', 'data.frame')
  
  return(out)  
}

findlast <- function (data, from, to){
  if(missing(data)) stop("'data' argument is missing")
  if(missing(from)) stop("'from' argument is missing")
  
  # we check each index of the list has same number of elements
  # by a try catch to a dataframe object
  # useless because data.frame recycle arguments
  #   if(inherits(
  #     tryCatch(data.frame(c(from,to)),  error = function(e) e),
  #     'error')
  #   ){
  #     stop("Something is wrong in the 'to' argument, maybe there is not the same number of column in each element of the list")
  #   }
  l <- length(from)
  
  if(l <= 1) {
    stop("'from' argument must have at leat 2 elements")
  }
  
  data.from <- data[,from]
  last <- find.last.valid(data.from)
  
  if(missing(to)) {
    return(last)
  } else {
  
    out <- vector(length(names(to)), mode='list')
    names(out) <- names(to)
    for(i in names(to)) {
      names.current <- to[[i]]
      if(length(names.current) != l) {
        message(paste("The element '",i,"' of the 'to' argument has not the same number of column than the 'from' argument.", sep=''))
        message(paste(i, "element of 'to':", paste(names.current, collapse=", ")))
        message(paste("'from' argument:", paste(from,collapse=", ")))
        stop("Bad parameters")
      }
      data.current <- data[,names.current]
      last.current <- get.last.valid(data.current, last)
      out[[i]] <- last.current
    }
    out <- as.data.frame(out)
    return(out)
  }
}


#--------------------------------------------------------------
# Example
# fl.data.example <- data.frame(
#   'age00' = c(48,NA,34,63,NA,27),
#   'age01' = c(NA,27,35,64,NA, NA),
#   'age02' = c(50,28,36,NA,NA, NA),
#   'sex00' = c('M',NA,'F','M',NA,'M'),
#   'sex01' = c(NA,'F','F',NA,NA, NA),
#   'sex02' = c('M',NA,'F',NA,'M', NA),
#   'nationality00' = factor(c('v1',NA,'v1','v1',NA,'o')),
#   'nationality01' = factor(c(NA,'o','v2','o',NA, NA)),
#   'nationality02' = factor(c('o',NA,'v2',NA,'o', NA))
# )
# 
# findlast(
#   data = fl.data.example,
#   from = 1:3
# )
# 
# findlast(
#   data = fl.data.example,
#   from = c('age00', 'age01', 'age02'),
#   to = list(
#     'age' = c('age00', 'age01', 'age02'),
#     'sex' = c('sex00', 'sex01', 'sex02'),
#     'nationality' = c('nationality00','nationality01','nationality02')
#   )
# )
# 
# findlast(
#   data = fl.data.example,
#   from = 1:3,
#   to = list(
#     'age' = 1:3,
#     'sex' = 4:6,
#     'nationality' = 7:9
#   )
# )

