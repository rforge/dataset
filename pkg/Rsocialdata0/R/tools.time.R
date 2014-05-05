giveDate <- function(date=T, time=T) {
  if(date && time)
    format(Sys.time(), "%Y.%m.%d %H:%M:%S")
  else if (date)
    format(Sys.time(), "%Y.%m.%d")
  else if (time)
    format(Sys.time(), "%H:%M:%S")
  else
    ''
}