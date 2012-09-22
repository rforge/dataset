# generic functions
as.missing.gnl.char <- function(x,i) {
  t <- which(names(values(x)) == i)
  temp.mis <- c(missings(x), values(x)[t])
  temp.val <- values(x)[-t]
  #print(temp.mis)
  #print(temp.val)
  return(list(
    'val' = temp.val,
    'mis' = temp.mis
  ))
}

as.missing.gnl.num <- function(x,i) {
  t <- which(values(x) == i)
  temp.mis <- c(missings(x), values(x)[t])
  temp.val <- values(x)[-t]
  #print(temp.mis)
  #print(temp.val)
  return(list(
    'val' = temp.val,
    'mis' = temp.mis
  ))
}

#methods
setMethod(
  f ="as.missing",
  signature =c("NominalVariable", "numeric"),
    definition = function(x,i){
      l <- as.missing.gnl.num(x,i)
      return(nvar(
        x = codes(x),
        missings = l$mis,
        values = l$val,
        description = description(x)
      ))
  }
)
setMethod(
  f ="as.missing",
  signature =c("NominalVariable", "character"),
    definition = function(x,i){
      l <- as.missing.gnl.char(x,i)
      return(nvar(
        x = codes(x),
        missings = l$mis,
        values = l$val,
        description = description(x)
      ))
  }
)



setMethod(
  f ="as.missing",
  signature =c("BinaryVariable", "numeric"),
    definition = function(x,i){
      l <- as.missing.gnl.num(x,i)
      return(bvar(
        x = codes(x),
        missings = l$mis,
        values = l$val,
        description = description(x)
      ))
  }
)
setMethod(
  f ="as.missing",
  signature =c("BinaryVariable", "character"),
    definition = function(x,i){
      l <- as.missing.gnl.char(x,i)
      return(bvar(
        x = codes(x),
        missings = l$mis,
        values = l$val,
        description = description(x)
      ))
  }
)



setMethod(
  f ="as.missing",
  signature =c("OrdinalVariable", "numeric"),
    definition = function(x,i){
      l <- as.missing.gnl.num(x,i)
      return(ovar(
        x = codes(x),
        missings = l$mis,
        values = l$val,
        description = description(x)
      ))
  }
)
setMethod(
  f ="as.missing",
  signature =c("OrdinalVariable", "character"),
    definition = function(x,i){
      l <- as.missing.gnl.char(x,i)
      return(ovar(
        x = codes(x),
        missings = l$mis,
        values = l$val,
        description = description(x)
      ))
  }
)



setMethod(
  f ="as.missing",
  signature =c("ScaleVariable", "numeric"),
    definition = function(x,i){
      l <- as.missing.gnl.num(x,i)
      return(svar(
        x = codes(x),
        missings = l$mis,
        values = l$val,
        description = description(x)
      ))
  }
)
setMethod(
  f ="as.missing",
  signature =c("ScaleVariable", "character"),
    definition = function(x,i){
      l <- as.missing.gnl.char(x,i)
      return(svar(
        x = codes(x),
        missings = l$mis,
        values = l$val,
        description = description(x)
      ))
  }
)


setMethod(
  f ="as.missing",
  signature =c("TimestampVariable", "numeric"),
    definition = function(x,i){
      l <- as.missing.gnl.num(x,i)
      return(tvar(
        x = codes(x),
        missings = l$mis,
        values = l$val,
        description = description(x)
      ))
  }
)
setMethod(
  f ="as.missing",
  signature =c("TimestampVariable", "character"),
    definition = function(x,i){
      l <- as.missing.gnl.char(x,i)
      return(tvar(
        x = codes(x),
        missings = l$mis,
        values = l$val,
        description = description(x)
      ))
  }
)