seq.count.aux <- function(x, value, start, stop) { # x: a vector, length > 0
  stopifnot(length(x) > 0)
  
  if(is.na(start) || is.na(stop)) return(NA)
  
  if(!stop>=start) {
#     print(start)
#     print(stop)
#     stop("stop >= start is not TRUE")
    return(NA)
  }
  if(start < 1)
    return(NA)
  if(stop > length(x))
    return(NA)
  
  out <- x[(start):(stop)] == value
  if(all(is.na(out))) return(NA)
  else return(sum(na.omit(out)))
}
# seq.count.aux.data <- c(0,1,1,0,0,1,NA,NA,NA)
# seq.count.aux(seq.count.aux.data, 1, 1, 3)
# seq.count.aux(seq.count.aux.data, 1, 4, 7)
# seq.count.aux(seq.count.aux.data, 1, 7, 9)
# length(seq.count.aux.data) # 
# seq.count.aux(seq.count.aux.data, 1, 12, 18)
# seq.count.aux(seq.count.aux.data, 1, -6, -3)

seq.count.aux2 <- function(x, threshold, bound){
#   stopifnot(length(x)>0)
#   stopifnot(bound %in% c('lower', 'upper'))
  if(bound == 'lower'){
    if(x < threshold) return(NA)
    else return(x)
  }
  if(bound == 'upper') {
    if(x > threshold) return(NA)
    else return(x)
  }
}
# seq.count.aux2(3, 1, 'lower')
# seq.count.aux2(0, 1, 'lower')
# seq.count.aux2(3, 11, 'upper')
# seq.count.aux2(14, 11, 'upper')

seq.count <- function(
  x,
  value,
  sequence = names(x),
  origin,
  window.start = 0,
  window.end,
  translate = NULL,
#   truncate.right = FALSE,
#   truncate.left = FALSE,
  verbose = FALSE,
  quiet = FALSE
){
  stopifnot(inherits(x, 'data.frame'))
  stopifnot(nrow(x) > 0)
  stopifnot(length(value) == 1)
  nam <- names(x)
  stopifnot(all(sequence %in% nam))
  stopifnot(length(origin) == 1)
  stopifnot(origin %in% nam)
  stopifnot(inherits(window.start, 'numeric'))
  stopifnot(length(window.start) == 1)
  stopifnot(inherits(window.end, 'numeric'))
  stopifnot(length(window.end) == 1)
  
  translate.flag <- FALSE
  translate.name <- NULL
  if(!is.null(translate)) {
    translate.flag <- TRUE
    translate.name <- names(translate)
    stopifnot(inherits(translate, 'numeric') || inherits(translate, 'character'))
    stopifnot(length(translate) == 1)
    if(is.character(translate)) {
      stopifnot(translate %in% nam)
      stopifnot(inherits(x[,translate], 'numeric'))
      translate.na <- is.na(x[,translate])
      if(all(translate.na))
        stop("All values of the variable used for translating are NA... No computation can be performed")
      if(any(translate.na)) {
        nrow.before <- nrow(x)
        x <- x[which(!translate.na),]
        if(!quiet) {
          warning(paste(
            "The variable used for translating contains NA... The corresponding individuals were removed", "\n",
            "    Number of row(s) before subsetting:", nrow.before, "\n",
            "    Number of row(s) after subsetting:", nrow(x), "\n"
          ))
        }
      }
    }      
    if(!is.null(translate.name)) {
      stopifnot(translate.name %in% c('left', 'right'))
    }
  }
  
  stopifnot(window.start >= 0)
  stopifnot(window.end >= 1)
  stopifnot(window.start < window.end)
  
  seqs <- x[, sequence]
  seq.window.origin <- rep(which(names(seqs) == origin), nrow(seqs))
  
  translate.var <- rep(0, nrow(seqs))
  if(!is.null(translate)) {
    if(is.numeric(translate)) {
      if(is.null(translate.name) || translate.name == 'right') {
        translate.var <- rep(translate, nrow(seqs))
      } else {
        translate.var <- rep(-translate, nrow(seqs))
      }
      seq.window.origin <- seq.window.origin + translate.var
    }
    if(is.character(translate)) {
      if(is.null(translate.name) || translate.name == 'right') {
        translate.var <- x[,translate]
      } else {
        translate.var <-  -x[,translate]
      }
      seq.window.origin <- seq.window.origin + translate.var
    }
  }
  
  seq.window.start <- seq.window.origin + window.start
  seq.window.end <- seq.window.origin + window.end - 1
  #   print(c('start' = seq.window.start, 'stop' = seq.window.end))
  
#   if(truncate.right) {
    seq.window.end <- mapply('min', seq.window.end, ncol(seqs))
#   }
#   
#   if(truncate.left) {
    seq.window.start <- mapply('max', seq.window.start, 1)
#   }
  
  seq.window.start <- mapply('seq.count.aux2', seq.window.start, ncol(seqs), 'upper')
  seq.window.end <- mapply('seq.count.aux2', seq.window.end, 1, 'lower')
  
#   if(any(seq.window.start < 1)){
#     message("Some index are out of the lower bound (<= 0):")
#     print(seq.window.start)
#     message('Index:')
#     stop("Out of bounds")
#   }
#   if(any(seq.window.end > ncol(seqs))){
#     message("Some index are out of the upper bound:")
#     print(head(seqs))
#     message('Upper bound:', ncol(seqs))
#     message('Index:')
#     print(seq.window.end)
#     stop("Out of bounds")
#   }
  
  out <- mapply(
    seq.count.aux,
    as.list(as.data.frame(t(seqs))),
    value,
    seq.window.start,
    seq.window.end
  )
  
  out.df <- data.frame(
    'COUNT' = out,
    row.names = row.names(x)
  )
  
  if(verbose) {
    out.df <- cbind(
      out.df,
      data.frame(
        'START' = seq.window.start,
        'STOP' = seq.window.end,
        'RANGE' = seq.window.end - seq.window.start + 1,
        'TRANSLATE' = translate.var
      ),
      seqs
    )
  }
  
  
  return(out.df)
}



# # data.frame example
# data.ex <- data.frame(
#   "id" = 1:7,
#   "age.2010" =  c(5,3,0,1,4,NA,30),
#   "vuln.2000" = c(0,1,1,0,NA,1,1),
#   "vuln.2001" = c(0,0,1,0,NA,1,1),
#   "vuln.2002" = c(0,1,0,1,NA,0,0),
#   "vuln.2003" = c(0,0,0,1,NA,1,1),
#   "vuln.2004" = c(0,NA,1,1,NA,1,1),
#   "vuln.2005" = c(0,0,1,0,NA,0,1),
#   "vuln.2006" = c(0,0,1,0,NA,0,1),
#   "vuln.2007" = c(0,0,0,1,NA,1,1),
#   "vuln.2008" = c(0,0,1,1,NA,1,0),
#   "vuln.2009" = c(0,0,0,1,NA,1,0),
#   "vuln.2010" = c(0,0,1,1,NA,1,1)
# )
# data.ex
# 
# #### Example 0
# # We count only in the year 2006
# count0 <- seq.count(
#   x = data.ex,
#   value = 1,
#   sequence = c("vuln.2000","vuln.2001","vuln.2002","vuln.2003","vuln.2004","vuln.2005","vuln.2006","vuln.2007","vuln.2008","vuln.2009","vuln.2010"),
#   origin = "vuln.2006",
#   window.end = 1 # we take only 1 measure
# )
# count0
# 
# # Same example with the mode 'verbose' on
# count0 <- seq.count(
#   x = data.ex,
#   value = 1,
#   sequence = c("vuln.2000","vuln.2001","vuln.2002","vuln.2003","vuln.2004","vuln.2005","vuln.2006","vuln.2007","vuln.2008","vuln.2009","vuln.2010"),
#   origin = "vuln.2006",
#   window.end = 1, # we take only 1 measure
#   verbose = TRUE
# )
# count0
# 
# 
# #### Example 1
# # We count between 2006 (included) and 2008 (included)
# count1 <- seq.count(
#   x = data.ex,
#   value = 1,
#   sequence = c("vuln.2000","vuln.2001","vuln.2002","vuln.2003","vuln.2004","vuln.2005","vuln.2006","vuln.2007","vuln.2008","vuln.2009","vuln.2010"),
#   origin = "vuln.2006",
#   window.end = 3
# )
# count1
# 
# # Same example with the mode 'verbose' on
# count1 <- seq.count(
#   x = data.ex,
#   value = 1,
#   sequence = c("vuln.2000","vuln.2001","vuln.2002","vuln.2003","vuln.2004","vuln.2005","vuln.2006","vuln.2007","vuln.2008","vuln.2009","vuln.2010"),
#   origin = "vuln.2006",
#   window.end = 3,
#   verbose = TRUE
# )
# count1
# 
# 
# #### Example 2
# # For each individual we count occurences of '1' when there where between 0 and 2 years (included)
# count2 <- seq.count(
#   x = data.ex,
#   value = 1,
#   sequence = c("vuln.2000","vuln.2001","vuln.2002","vuln.2003","vuln.2004","vuln.2005","vuln.2006","vuln.2007","vuln.2008","vuln.2009","vuln.2010"),
#   origin = "vuln.2010", # as we have the age in 2010, we use vuln.2010 as origin
#   window.end = 3, # 0, 1 and 2 years
#   translate = c('left' = 'age.2010')
# )
# count2
# 
# # Same example with the mode 'verbose' on
# count2 <- seq.count(
#   x = data.ex,
#   value = 1,
#   sequence = c("vuln.2000","vuln.2001","vuln.2002","vuln.2003","vuln.2004","vuln.2005","vuln.2006","vuln.2007","vuln.2008","vuln.2009","vuln.2010"),
#   origin = "vuln.2010", # as we have the age in 2010, we use vuln.2010 as origin
#   window.end = 3, # 0, 1 and 2 years
#   translate = c('left' = 'age.2010'),
#   verbose = TRUE
# )
# count2
# 
# 
# #### Example 3
# # For each individual we count occurences of '1' when there were 2 years
# count3 <- seq.count(
#   x = data.ex,
#   value = 1,
#   sequence = c("vuln.2000","vuln.2001","vuln.2002","vuln.2003","vuln.2004","vuln.2005","vuln.2006","vuln.2007","vuln.2008","vuln.2009","vuln.2010"),
#   origin = "vuln.2010",
#   window.start = 2, # we start two values after origin: corresponding to 2 years old
#   window.end = 3, # and we stop three values after origin: we get only the value at 2 years
#   translate = c('left' = 'age.2010'),
#   verbose = TRUE
# )
# count3
# 
# #### Example 4
# count4 <- seq.count(
#   x = data.ex,
#   value = 1,
#   sequence = c("vuln.2000","vuln.2001","vuln.2002","vuln.2003","vuln.2004","vuln.2005","vuln.2006","vuln.2007","vuln.2008","vuln.2009","vuln.2010"),
#   origin = "vuln.2010",
#   window.start = 2, # we start two values after origin: corresponding to 2 years old
#   window.end = 6,  # and we stop six values after origin: we get the count for the range [2-5]
#   translate = c('left' = 'age.2010'),
#   verbose = TRUE
# )
# count4

# count5 <- seq.count(
#   x = data.ex,
#   value = 1,
#   sequence = c("vuln.2000","vuln.2001","vuln.2002","vuln.2003","vuln.2004","vuln.2005","vuln.2006","vuln.2007","vuln.2008","vuln.2009","vuln.2010"),
#   origin = "vuln.2010",
#   window.start = 2, # we start at 2 years
#   window.end = 6, # and we stop 4 values after: we get the count for the range [2-5]
#   translate = c('left' = 30),
#   verbose = TRUE
# )
# count5