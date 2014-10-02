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

which.this.value <- function(x, value)
{
  equal.to.this.value <- function(e) { e == value }
  
  sapply(x, equal.to.this.value)
}
# fv.test1 <- c(0,0,1,0,0,NA,NA,NA,NA,NA,NA,NA,NA)
# which.this.value(fv.test1, 1)

exists.this.value <- function(x, value) # if the value exists, return the last index of the column where the value is, if the value doesn't exist, return -1, if all NA, return NA
{
  y <- which.this.value(x, value)
  
  if(all(is.na(y))) {
    return(list('value' = NA, 'index' = NA))
  }
  
  if(is.na(any(y))) {
    return(list('value' = -1, 'index' = max(which(!y))))
  }
  
  return(list('value' = 1, 'index' = max(which(y))))
}
# fv.test1 <- c( 0, 0, 1, 0, 0,NA,NA,NA,NA,NA,NA,NA,NA)
# fv.test2 <- c(NA,NA, 0, 0, 1, 1, 1,NA,NA,NA,NA,NA,NA)
# fv.test3 <- c(NA,NA,NA, 0,NA, 1, 1, 1,NA,NA,NA,NA,NA)
# fv.test4 <- c(NA,NA,NA,NA,NA,NA,NA,NA, 0, 0, 0, 0, 0)
# fv.test5 <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
# exists.this.value(fv.test1, 1)
# exists.this.value(fv.test2, 1)
# exists.this.value(fv.test3, 1)
# exists.this.value(fv.test4, 1)
# exists.this.value(fv.test5, 1)

find.last.value <- function (x, value)
{
  return(data.frame(t(mapply(exists.this.value, as.data.frame(t(x)), value)), row.names = 1:nrow(x)))
}
# fv.data.example <- data.frame(
#   'unemployment99' = c(0,NA,NA,NA,NA),
#   'unemployment00' = c(0,NA,NA,NA,NA),
#   'unemployment01' = c(1,0,NA,NA,NA),
#   'unemployment02' = c(0,0,0,NA,NA),
#   'unemployment03' = c(0,1,NA,NA,NA),
#   'unemployment04' = c(NA,1,1,NA,NA),
#   'unemployment05' = c(NA,1,1,NA,NA),
#   'unemployment06' = c(NA,NA,1,NA,NA),
#   'unemployment07' = c(NA,NA,NA,0,NA),
#   'unemployment08' = c(NA,NA,NA,0,NA),
#   'unemployment09' = c(NA,NA,NA,0,NA),
#   'unemployment10' = c(NA,NA,NA,0,NA),
#   'unemployment11' = c(NA,NA,NA,0,NA)
# )
# find.last.value(fv.data.example, 1)

find.value.last <- function (data, from, to, value = 1)
{
  if (missing(data))
    stop("'data' argument is missing")
  if (missing(from))
    stop("'from' argument is missing")
  l <- length(from)
  if (l <= 1) {
    stop("'from' argument must have at leat 2 elements")
  }
  data.from <- data[, from]
  last <- find.last.value(data.from, value)
  if (missing(to)) {
    return(last)
  }
  else {
    out <- vector(length(names(to)), mode = "list")
    names(out) <- names(to)
    for (i in names(to)) {
      names.current <- to[[i]]
      if (length(names.current) != l) {
        message(paste("The element '", i, "' of the 'to' argument has not the same number of column than the 'from' argument.",
                      sep = ""))
        message(paste(i, "element of 'to':", paste(names.current,
                                                   collapse = ", ")))
        message(paste("'from' argument:", paste(from,
                                                collapse = ", ")))
        stop("Bad parameters")
      }
      data.current <- data[, names.current]
      last.current <- get.last.valid(data.current, last[,'index'])
      out[[i]] <- last.current
    }
    out <- as.data.frame(out)
    return(out)
  }
}
# fv.data.example <- data.frame(
#   'unemployment99' = c(0,NA,NA,NA,NA),
#   'unemployment00' = c(0,NA,NA,NA,NA),
#   'unemployment01' = c(1,0,NA,NA,NA),
#   'unemployment02' = c(0,0,0,NA,NA),
#   'unemployment03' = c(0,1,NA,NA,NA),
#   'unemployment04' = c(NA,1,1,NA,NA),
#   'unemployment05' = c(NA,1,1,NA,NA),
#   'unemployment06' = c(NA,NA,1,NA,NA),
#   'unemployment07' = c(NA,NA,NA,0,NA),
#   'unemployment08' = c(NA,NA,NA,0,NA),
#   'unemployment09' = c(NA,NA,NA,0,NA),
#   'unemployment10' = c(NA,NA,NA,0,NA),
#   'unemployment11' = c(NA,NA,NA,0,NA),
#   'age99' = c(15,NA,NA,NA,NA),
#   'age00' = c(16,NA,NA,NA,NA),
#   'age01' = c(17,20,NA,NA,NA),
#   'age02' = c(18,21,23,NA,NA),
#   'age03' = c(19,22,NA,NA,NA),
#   'age04' = c(NA,23,25,NA,NA),
#   'age05' = c(NA,24,26,NA,NA),
#   'age06' = c(NA,NA,27,NA,NA),
#   'age07' = c(NA,NA,NA,29,NA),
#   'age08' = c(NA,NA,NA,30,NA),
#   'age09' = c(NA,NA,NA,31,NA),
#   'age10' = c(NA,NA,NA,32,NA),
#   'age11' = c(NA,NA,NA,33,NA),
#   'education99' = c(8,NA,NA,NA,NA),
#   'education00' = c(8,NA,NA,NA,NA),
#   'education01' = c(8,3,NA,NA,NA),
#   'education02' = c(8,3,2,NA,NA),
#   'education03' = c(8,4,NA,NA,NA),
#   'education04' = c(NA,4,2,NA,NA),
#   'education05' = c(NA,4,2,NA,NA),
#   'education06' = c(NA,NA,3,NA,NA),
#   'education07' = c(NA,NA,NA,7,NA),
#   'education08' = c(NA,NA,NA,7,NA),
#   'education09' = c(NA,NA,NA,7,NA),
#   'education10' = c(NA,NA,NA,8,NA),
#   'education11' = c(NA,NA,NA,8,NA)
# )
# find.value.last(
#   data = fv.data.example,
#   from = 1:13,
#   value = 1
# )
# find.value.last(
#   data = fv.data.example,
#   from = 1:13,
#   to = list(
#     'age' = 14:26,
#     'education' = 27:39
#   ),
#   value = 1
# )
# find.value.last(
#   data = fv.data.example,
#   from = 1:13,
#   to = list(
#     'age' = 14:26,
#     'education' = 27:39
#   ),
#   value = 0
# )

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

# fv.data.example <- data.frame(
#   'unemployment99' = c(0,NA,NA,NA,NA),
#   'unemployment00' = c(0,NA,NA,NA,NA),
#   'unemployment01' = c(1,0,NA,NA,NA),
#   'unemployment02' = c(0,0,0,NA,NA),
#   'unemployment03' = c(0,1,NA,NA,NA),
#   'unemployment04' = c(NA,1,1,NA,NA),
#   'unemployment05' = c(NA,1,1,NA,NA),
#   'unemployment06' = c(NA,NA,1,NA,NA),
#   'unemployment07' = c(NA,NA,NA,0,NA),
#   'unemployment08' = c(NA,NA,NA,0,NA),
#   'unemployment09' = c(NA,NA,NA,0,NA),
#   'unemployment10' = c(NA,NA,NA,0,NA),
#   'unemployment11' = c(NA,NA,NA,0,NA),
#   'age99' = c(15,NA,NA,NA,NA),
#   'age00' = c(16,NA,NA,NA,NA),
#   'age01' = c(17,20,NA,NA,NA),
#   'age02' = c(18,21,23,NA,NA),
#   'age03' = c(19,22,NA,NA,NA),
#   'age04' = c(NA,23,25,NA,NA),
#   'age05' = c(NA,24,26,NA,NA),
#   'age06' = c(NA,NA,27,NA,NA),
#   'age07' = c(NA,NA,NA,29,NA),
#   'age08' = c(NA,NA,NA,30,NA),
#   'age09' = c(NA,NA,NA,31,NA),
#   'age10' = c(NA,NA,NA,32,NA),
#   'age11' = c(NA,NA,NA,33,NA),
#   'education99' = c(8,NA,NA,NA,NA),
#   'education00' = c(8,NA,NA,NA,NA),
#   'education01' = c(8,3,NA,NA,NA),
#   'education02' = c(8,3,2,NA,NA),
#   'education03' = c(8,4,NA,NA,NA),
#   'education04' = c(NA,4,2,NA,NA),
#   'education05' = c(NA,4,2,NA,NA),
#   'education06' = c(NA,NA,3,NA,NA),
#   'education07' = c(NA,NA,NA,7,NA),
#   'education08' = c(NA,NA,NA,7,NA),
#   'education09' = c(NA,NA,NA,7,NA),
#   'education10' = c(NA,NA,NA,8,NA),
#   'education11' = c(NA,NA,NA,8,NA),
#   'health99' = factor(c('very well',NA,NA,NA,NA), levels = c("poor", "well", "very well")),
#   'health00' = c('very well',NA,NA,NA,NA),
#   'health01' = c('very well','well',NA,NA,NA),
#   'health02' = c('very well','well','poor',NA,NA),
#   'health03' = c('well','very well',NA,NA,NA),
#   'health04' = c(NA,'very well','poor',NA,NA),
#   'health05' = c(NA,'very well','poor',NA,NA),
#   'health06' = c(NA,NA,'poor',NA,NA),
#   'health07' = c(NA,NA,NA,'very well',NA),
#   'health08' = c(NA,NA,NA,'very well',NA),
#   'health09' = c(NA,NA,NA,'poor',NA),
#   'health10' = c(NA,NA,NA,'poor',NA),
#   'health11' = c(NA,NA,NA,'well',NA)
# )

transpose.df <- function(x) { # transposition accounting for factor variables
  out <- as.data.frame(t(x))
  
  are.factor <- mapply(inherits, x, 'factor')
  if(any(are.factor)) {
    if(all(are.factor)) {
      lev <- levels(x[,1]) # we use levels of the first wave
      for (i in 1:ncol(out)) {
        out[,i] <- factor(out[,i], levels = lev)
      }
    } else {
      stop("columns don't inherit of the same type, some are factors but some aren't")
    }
  }
  return(out)
}
# transpose.df(fv.data.example[,40:52])

max.cool.num <- function(x) {
  if(missing(x)) return(NA)
  if(length(x) == 0) return(NA)
  if(all(is.na(x))) return(NA)
  else return(max(x, na.rm=T))
}

min.cool.num <- function(x) {
  if(missing(x)) return(NA)
  if(length(x) == 0) return(NA)
  if(all(is.na(x))) return(NA)
  else return(min(x, na.rm=T))
}

higher.level <- function(x) { # get the higher level present in a factor
  x.char <- as.character(x)
  m <- max.cool.num(which(levels(x) %in% unique(x.char)))
  if(is.na(m)) return(list('value' = NA, index = NA))
  else return(list('value' = levels(x)[m], index = max(which(x.char == levels(x)[m]))))
}
# higher.level(factor(c("b","a","a"), levels=c("a","b","c","d")))
# higher.level(factor(c("NA","NA","NA"), levels=c("a","b","c","d")))

lower.level <- function(x) { # get the lower level present in a factor
  x.char <- as.character(x)
  m <- min.cool.num(which(levels(x) %in% unique(x.char)))
  if(is.na(m)) return(list('value' = NA, index = NA))
  else return(list('value' = levels(x)[m], index = max(which(x.char == levels(x)[m]))))
}
# lower.level(factor(c("b","c","d"), levels=c("a","b","c","d"))) 
# lower.level(factor(c("NA","NA","NA"), levels=c("a","b","c","d")))


max.cool <- function(x) {
  if(!(inherits(x, 'numeric') || inherits(x, 'factor')))
    stop("x has to be a numeric or a factor.")
  
#   print('x')
#   print(x)
  if(inherits(x, 'numeric')) {
    m <- max.cool.num(x)
    if (is.na(m)) {index <- NA} else {index <- max(which(x == m))}
    return(list('value' = m, 'index' = index))
  }
  if(inherits(x, 'factor')) {
    return(higher.level(x))
  }
}
# mc <- transpose.df(fv.data.example[,40:52])
# mc[,1]
# mc[,2]

min.cool <- function(x) {
  if(!(inherits(x, 'numeric') || inherits(x, 'factor')))
    stop("x has to be a numeric or a factor.")
  
  #   print('x')
  #   print(x)
  if(inherits(x, 'numeric')) {
    m <- min.cool.num(x)
    if (is.na(m)) {index <- NA} else {index <- max(which(x == m))}
    return(list('value' = m, 'index' = index))
  }
  if(inherits(x, 'factor')) {
    return(lower.level(x))
  }
}


find.fun.value <- function (x, fun = 'max')
{
  stopifnot(fun %in% c('min','max'))
  
  if(fun == 'max') {
    return(data.frame(t(mapply(max.cool, as.data.frame(transpose.df(x)))), row.names = 1:nrow(x)))
  }
  if(fun == 'min') {
    return(data.frame(t(mapply(min.cool, as.data.frame(transpose.df(x)))), row.names = 1:nrow(x)))
  }
}
# find.fun.value(fv.data.example[,40:52])
# find.fun.value(fv.data.example[,1:13])
# find.fun.value(fv.data.example[,40:52], fun = 'min')
# find.fun.value(fv.data.example[,1:13], fun = 'min')
# data.frame(t(mapply(max.cool, as.data.frame(transpose.df(fv.data.example[,40:52])))), row.names = 1:nrow(fv.data.example[,1:13]))
# data.frame(t(mapply(max.cool, as.data.frame(transpose.df(fv.data.example[,1:13])))), row.names = 1:nrow(fv.data.example[,1:13]))

find.value.fun <- function (data, from, to, fun = 'max')
{
  if (missing(data))
    stop("'data' argument is missing")
  if (missing(from))
    stop("'from' argument is missing")
  l <- length(from)
  if (l <= 1) {
    stop("'from' argument must have at leat 2 elements")
  }
  stopifnot(fun %in% c('min', 'max'))
  
  data.from <- data[, from]
  last <- find.fun.value(data.from, fun)
  if (missing(to)) {
    return(last)
  }
  else {
    out <- vector(length(names(to)), mode = "list")
    names(out) <- names(to)
    for (i in names(to)) {
      names.current <- to[[i]]
      if (length(names.current) != l) {
        message(paste("The element '", i, "' of the 'to' argument has not the same number of column than the 'from' argument.",
                      sep = ""))
        message(paste(i, "element of 'to':", paste(names.current,
                                                   collapse = ", ")))
        message(paste("'from' argument:", paste(from,
                                                collapse = ", ")))
        stop("Bad parameters")
      }
      data.current <- data[, names.current]
      last.current <- get.last.valid(data.current, last[,'index'])
      out[[i]] <- last.current
    }
    out <- as.data.frame(out)
    return(out)
  }
}
# ?find.value.last
# # we get the column ids where the last occurence of the value 1 is
# find.value.fun(
#   data = fv.data.example,
#   from = 1:13
# )
# find.value.fun(
#   data = fv.data.example,
#   from = 40:52
# )
# 
# # we retrieve the last occurence of the value 1 for unemployment, and get corresponding values for the variables age and education
# find.value.fun(
#   data = fv.data.example,
#   from = 1:13,
#   to = list(
#     'age' = 14:26,
#     'education' = 27:39,
#     'health' = 40:52
#   )
# )
# find.value.fun(
#   data = fv.data.example,
#   from = 27:39,
#   to = list(
#     'age' = 14:26,
#     'education' = 27:39,
#     'health' = 40:52
#   )
# )
# find.value.fun(
#   data = fv.data.example,
#   from = 40:52,
#   to = list(
#     'age' = 14:26,
#     'education' = 27:39,
#     'health' = 40:52
#   )
# )
# find.value.fun(
#   data = fv.data.example,
#   from = 40:52,
#   to = list(
#     'unemployment' = 1:13,
#     'age' = 14:26,
#     'education' = 27:39
#   )
# )
# 
# # same operation but here for the min
