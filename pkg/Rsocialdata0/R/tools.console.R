# tools for console outputs

cons.clearline <- function(n=255) {
  cat("\r", paste(rep(' ', n), collapse=''), sep='')
  flush.console()
}

cons.counter.new <- function(txt.before, txt.after, txt.finish = 'done!', value = 0) {
  return(list(
    'txt.before' = txt.before,
    'txt.after' = txt.after,
    'txt.finish' = txt.finish,
    'value' = value
  ))
}

cons.counter.change.value <- function(value, counter) {
  counter$value <- value
  return(counter)
}

cons.counter.add.value <- function(value, counter) {
  counter$value <- counter$value + value
  return(counter)
}
cons.counter.add.one <- function(counter) {
  return(cons.counter.add.value(1,counter))
}

cons.counter.print <- function(counter) {
  cons.clearline()
  cat("\r", counter$txt.before, counter$value, counter$txt.after, sep='')
  flush.console()
}

cons.counter.print.finish <- function(counter) {
  cons.clearline()
  cat("\r", counter$txt.before, counter$txt.finish, sep='')
  flush.console()
}



## ex
# cons.counter.test <- function() {
#   cons.counter <- cons.counter.new(
#     txt.before = 'Importing variables... ',
#     txt.after = '/12'
#   )
#   cons.counter.print(cons.counter)
#   cons.counter <- cons.counter.add.one(cons.counter)
#   cons.counter.print(cons.counter)
#   cons.counter.print.finish(cons.counter)
# }
# cons.counter.test()






# progress.counter <- function(x){
#   
#   txt <- 'Importing variables... '
#   
#   for (i in 1:length(x)) {
#     cons.clearline()
#     cat("\r", txt, x[i], '/12', sep='')
#     flush.console()
#     Sys.sleep(0.5)
#   }
#   cons.clearline()
#   cat("\r", txt, 'done!', sep='')
# }
# progress.counter(1:12)
