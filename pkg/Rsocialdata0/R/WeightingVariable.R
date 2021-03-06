#####################################################################
## WeightingVariable class
#####################################################################


setClass(
  Class = "WeightingVariable",
  contains = c("ScaleVariable"),
  validity = function(object) {
    if(Dataset.globalenv$print.io) cat (" =>       WeightingVariable: object validity check \n")
    flag <- TRUE
    #if(length(slot(object, 'Weighting')) > 0)
    #  stop("A WeightingVariable can't have a non-empty Weighting slot")
    
    if(any(is.na(v(object)))) {
      flag <- FALSE
      print(v(object))
      print(codes(object))
      print(missings(object))
      print(description(object))
      message("A WeightingVariable can't have missings values")
    }
    
  	return(flag)
	}
)


#=================================================================================================
# Class initializer
#=================================================================================================

  
#=================================================================================================
# Class standard constructeur
#=================================================================================================

# x <- c(10,53,24,96,85,77,12,NA)
# y <- svar(x)
wvar <- function(
  x,
  missings,
  values,
  description
) {
  if(Dataset.globalenv$print.io) cat(" => (in)  WeightingVariable: builder \n")
  if(missing(missings)) missings <- numeric(0)
  if(missing(values)) values <- numeric(0)
  if(missing(description)) description <- Dataset.globalenv$Variable.description.default
  if(missing(x)) x <- numeric(0)
  
  if(inherits(x, "ScaleVariable")) {
    return(new(
        Class = "WeightingVariable",
        codes = codes(x),
        missings = missings(x),
        values = valids(x),
        description = description(x),
        Variable.version = slot(x, 'Variable.version')
      )
    )
  }
  
  # we apply special treatment for scale variable
  variable <- quantitativeVariable( #FIXME: SHOULD BE SCALE
    x = x,
    missings = missings,
    values = values,
    description = description
  )
  
  # then we apply special treatment for a Weighting variable
  # (nothing)
  
  if(Dataset.globalenv$print.io) cat(" => (out) WeightingVariable: builder \n")
  out <- new(
    Class = "WeightingVariable",
    codes = variable$x,
    missings = variable$missings,
    values = variable$values,
    description = variable$description,
    Variable.version = variable$Variable.version
  )
  
  if(Dataset.globalenv$print.comments <= Dataset.globalenv$important){
    message(paste(
      'number of missings:',
      nmissings(out),
      '(',
      round(nmissings(out)/length(out)*100,2),
      '%)'
    ))
  }
  
  return(out)
}

# ww <- wvar(c(1,2,3))
# str.typevar(ww, parenthesis = T)

# setMethod(
#   f = "show",
#   signature = "WeightingVariable", 
#   definition = function (object) {
#     txt.desc <- 'Description: no'
#     if(length(description(object)) > 0)
#       txt.desc <- paste('Description:', description(object))
#       
#     message(txt.desc)
# 	  print(as.numeric(object))
#   }
# )
# 
# # print
# setMethod("print", "WeightingVariable", 
#   definition = function (x, ...) {
# 	show(x)
#   }
# )

is.weighting <- function(x){
  if(inherits(x, "WeightingVariable")){
    return(TRUE)
  } else {
    return(FALSE)
  }
}