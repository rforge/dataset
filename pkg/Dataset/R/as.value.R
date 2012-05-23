# generic functions
as.value.gnl.char <- function(x,i) {
  t <- which(names(missings(x)) == i)
  temp.val <- c(values(x), missings(x)[t])
  temp.mis <- missings(x)[-t]
  return(list(
    'val' = temp.val,
    'mis' = temp.mis
  ))
}

as.value.gnl.num <- function(x,i) {
  t <- which(missings(x) == i)
  temp.val <- c(values(x), missings(x)[t])
  temp.mis <- missings(x)[-t]
  return(list(
    'val' = temp.val,
    'mis' = temp.mis
  ))
}

#methods
setMethod(
  f ="as.value",
  signature =c("NominalVariable", "numeric"),
    definition = function(x,i){
      l <- as.value.gnl.num(x,i)
      return(nvar(
        x = codes(x),
        missings = l$mis,
        values = l$val,
        description = description(x),
        weights = weights(x)
      ))
	}
)
setMethod(
  f ="as.value",
  signature =c("NominalVariable", "character"),
    definition = function(x,i){
      l <- as.value.gnl.char(x,i)
      return(nvar(
        x = codes(x),
        missings = l$mis,
        values = l$val,
        description = description(x),
        weights = weights(x)
      ))
  }
)



setMethod(
  f ="as.value",
  signature =c("BinaryVariable", "numeric"),
    definition = function(x,i){
      l <- as.value.gnl.num(x,i)
      return(bvar(
        x = codes(x),
        missings = l$mis,
        values = l$val,
        description = description(x),
        weights = weights(x)
      ))
  }
)
setMethod(
  f ="as.value",
  signature =c("BinaryVariable", "character"),
    definition = function(x,i){
      l <- as.value.gnl.char(x,i)
      return(bvar(
        x = codes(x),
        missings = l$mis,
        values = l$val,
        description = description(x),
        weights = weights(x)
      ))
  }
)



setMethod(
  f ="as.value",
  signature =c("OrdinalVariable", "numeric"),
    definition = function(x,i){
      l <- as.value.gnl.num(x,i)
      return(ovar(
        x = codes(x),
        missings = l$mis,
        values = l$val,
        description = description(x),
        weights = weights(x)
      ))
  }
)
setMethod(
  f ="as.value",
  signature =c("OrdinalVariable", "character"),
    definition = function(x,i){
      l <- as.value.gnl.char(x,i)
      return(ovar(
        x = codes(x),
        missings = l$mis,
        values = l$val,
        description = description(x),
        weights = weights(x)
      ))
  }
)



setMethod(
  f ="as.value",
  signature =c("ScaleVariable", "numeric"),
    definition = function(x,i){
      l <- as.value.gnl.num(x,i)
      return(svar(
        x = codes(x),
        missings = l$mis,
        values = l$val,
        description = description(x),
        weights = weights(x)
      ))
  }
)
setMethod(
  f ="as.value",
  signature =c("ScaleVariable", "character"),
    definition = function(x,i){
      l <- as.value.gnl.char(x,i)
      return(svar(
        x = codes(x),
        missings = l$mis,
        values = l$val,
        description = description(x),
        weights = weights(x)
      ))
  }
)


setMethod(
  f ="as.value",
  signature =c("TimestampVariable", "numeric"),
    definition = function(x,i){
      l <- as.value.gnl.num(x,i)
      return(tvar(
        x = codes(x),
        missings = l$mis,
        values = l$val,
        description = description(x),
        weights = weights(x)
      ))
  }
)
setMethod(
  f ="as.value",
  signature =c("TimestampVariable", "character"),
    definition = function(x,i){
      l <- as.value.gnl.char(x,i)
      return(tvar(
        x = codes(x),
        missings = l$mis,
        values = l$val,
        description = description(x),
        weights = weights(x)
      ))
  }
)