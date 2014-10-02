# data(iris)
# ir <- dataset(iris)
# a <- ir$Species
# b <- ir$Sepal.Lentgh
# a1 <- as.missing('setosa', a)



# generic functions
as.missing.gnl.char <- function(x,i) {
  t <- which(names(valids(x)) == i)
  temp.mis <- c(missings(x), valids(x)[t])
  temp.val <- valids(x)[-t]
  #print(temp.mis)
  #print(temp.val)
  return(list(
    'val' = temp.val,
    'mis' = temp.mis
  ))
}

as.missing.gnl.num <- function(x,i) {
  t <- which(valids(x) == i)
  temp.mis <- c(missings(x), valids(x)[t])
  temp.val <- valids(x)[-t]
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
  signature =c("numeric", "NominalVariable"),
    definition = function(i,x){
      l <- as.missing.gnl.num(x,i)
      return(cvar(
        x = codes(x),
        missings = l$mis,
        values = l$val,
        description = description(x)
      ))
  }
)
setMethod(
  f ="as.missing",
  signature =c("character", "NominalVariable"),
    definition = function(i,x){
      l <- as.missing.gnl.char(x,i)
      return(cvar(
        x = codes(x),
        missings = l$mis,
        values = l$val,
        description = description(x)
      ))
  }
)



setMethod(
  f ="as.missing",
  signature =c("numeric", "BinaryVariable"),
    definition = function(i,x){
      l <- as.missing.gnl.num(x,i)
      return(cvar(
        x = codes(x),
        missings = l$mis,
        values = l$val,
        description = description(x)
      ))
  }
)
setMethod(
  f ="as.missing",
  signature =c("character", "BinaryVariable"),
    definition = function(i,x){
      l <- as.missing.gnl.char(x,i)
      return(cvar(
        x = codes(x),
        missings = l$mis,
        values = l$val,
        description = description(x)
      ))
  }
)



setMethod(
  f ="as.missing",
  signature =c("numeric", "OrdinalVariable"),
    definition = function(i,x){
      l <- as.missing.gnl.num(x,i)
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
  f ="as.missing",
  signature =c("character", "OrdinalVariable"),
    definition = function(i,x){
      l <- as.missing.gnl.char(x,i)
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
  f ="as.missing",
  signature =c("numeric", "ScaleVariable"),
    definition = function(i,x){
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
  signature =c("character", "ScaleVariable"),
    definition = function(i,x){
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
  signature =c("numeric", "TimestampVariable"),
    definition = function(i,x){
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
  signature =c("character", "TimestampVariable"),
    definition = function(i,x){
      l <- as.missing.gnl.char(x,i)
      return(tvar(
        x = codes(x),
        missings = l$mis,
        values = l$val,
        description = description(x)
      ))
  }
)


## doesn't make sens for a weighting variable, as it can't contain missing
# setMethod(
#   f ="as.missing",
#   signature =c("numeric", "WeightingVariable"),
#   definition = function(i,x){
#     l <- as.missing.gnl.num(x,i)
#     return(wvar(
#       x = codes(x),
#       missings = l$mis,
#       values = l$val,
#       description = description(x)
#     ))
#   }
# )
# setMethod(
#   f ="as.missing",
#   signature =c("character", "WeightingVariable"),
#   definition = function(i,x){
#     l <- as.missing.gnl.char(x,i)
#     return(wvar(
#       x = codes(x),
#       missings = l$mis,
#       values = l$val,
#       description = description(x)
#     ))
#   }
# )