# generic functions
as.value.gnl.char <- function(x,i) {
  t <- which(names(missings(x)) == i)
  temp.val <- c(valids(x), missings(x)[t])
  temp.mis <- missings(x)[-t]
  return(list(
    'val' = temp.val,
    'mis' = temp.mis
  ))
}

as.value.gnl.num <- function(x,i) {
  t <- which(missings(x) == i)
  temp.val <- c(valids(x), missings(x)[t])
  temp.mis <- missings(x)[-t]
  return(list(
    'val' = temp.val,
    'mis' = temp.mis
  ))
}

#methods
setMethod(
  f ="as.valid",
  signature =c("numeric", "NominalVariable"),
    definition = function(i,x){
      l <- as.value.gnl.num(x,i)
      return(cvar(
        x = codes(x),
        missings = l$mis,
        values = l$val,
        description = description(x)
      ))
	}
)
setMethod(
  f ="as.valid",
  signature =c("character", "NominalVariable"),
    definition = function(i,x){
      l <- as.value.gnl.char(x,i)
      return(cvar(
        x = codes(x),
        missings = l$mis,
        values = l$val,
        description = description(x)
      ))
  }
)



setMethod(
  f ="as.valid",
  signature =c("numeric", "BinaryVariable"),
    definition = function(i,x){
      l <- as.value.gnl.num(x,i)
      return(cvar(
        x = codes(x),
        missings = l$mis,
        values = l$val,
        description = description(x)
      ))
  }
)
setMethod(
  f ="as.valid",
  signature =c("character", "BinaryVariable"),
    definition = function(i,x){
      l <- as.value.gnl.char(x,i)
      return(cvar(
        x = codes(x),
        missings = l$mis,
        values = l$val,
        description = description(x)
      ))
  }
)



setMethod(
  f ="as.valid",
  signature =c("numeric", "OrdinalVariable"),
    definition = function(i,x){
      l <- as.value.gnl.num(x,i)
      if(length(l$val) != 2) {
        return(ovar(
          x = codes(x),
          missings = l$mis,
          values = l$val,
          description = description(x)
        ))
      } else {
        return(bvar(
          x = codes(x),
          missings = l$mis,
          values = l$val,
          description = description(x)
        ))
      }
  }
)
setMethod(
  f ="as.valid",
  signature =c("character", "OrdinalVariable"),
    definition = function(i,x){
      l <- as.value.gnl.char(x,i)
      if(length(l$val) != 2) {
        return(ovar(
          x = codes(x),
          missings = l$mis,
          values = l$val,
          description = description(x)
        ))
      } else {
        return(bvar(
          x = codes(x),
          missings = l$mis,
          values = l$val,
          description = description(x)
        ))
      }
  }
)



setMethod(
  f ="as.valid",
  signature =c("numeric", "ScaleVariable"),
    definition = function(i,x){
      l <- as.value.gnl.num(x,i)
      return(svar(
        x = codes(x),
        missings = l$mis,
        values = l$val,
        description = description(x)
      ))
  }
)
setMethod(
  f ="as.valid",
  signature =c("character", "ScaleVariable"),
    definition = function(i,x){
      l <- as.value.gnl.char(x,i)
      return(svar(
        x = codes(x),
        missings = l$mis,
        values = l$val,
        description = description(x)
      ))
  }
)


setMethod(
  f ="as.valid",
  signature =c("numeric", "TimestampVariable"),
    definition = function(i,x){
      l <- as.value.gnl.num(x,i)
      return(tvar(
        x = codes(x),
        missings = l$mis,
        values = l$val,
        description = description(x)
      ))
  }
)
setMethod(
  f ="as.valid",
  signature =c("character", "TimestampVariable"),
    definition = function(i,x){
      l <- as.value.gnl.char(x,i)
      return(tvar(
        x = codes(x),
        missings = l$mis,
        values = l$val,
        description = description(x)
      ))
  }
)

# setMethod(
#   f ="as.valid",
#   signature =c("numeric", "WeightingVariable"),
#   definition = function(i,x){
#     l <- as.value.gnl.num(x,i)
#     return(wvar(
#       x = codes(x),
#       missings = l$mis,
#       values = l$val,
#       description = description(x)
#     ))
#   }
# )
# setMethod(
#   f ="as.valid",
#   signature =c("character", "WeightingVariable"),
#   definition = function(i,x){
#     l <- as.value.gnl.char(x,i)
#     return(wvar(
#       x = codes(x),
#       missings = l$mis,
#       values = l$val,
#       description = description(x)
#     ))
#   }
# )