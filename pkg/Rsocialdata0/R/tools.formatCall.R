formatCall.create.spaces <- function(nspaces, output = 'console') {
  stopifnot(output %in% c('console', 'tex'))
  if(output == 'console') symb <- ' '
  if(output == 'tex') symb <- '~'
  paste(rep(symb, nspaces), collapse = '')
}
# formatCall.create.spaces(0)
# formatCall.create.spaces(1)
# formatCall.create.spaces(2)

formatCall <- function(cl, space.indent = 2, output = 'console', display = F){
  
  if(inherits(cl, "call")) {
    cl <- paste(deparse(cl), collapse='')
    cl <- gsub('[[:space:]]+', ' ', cl)
  }
  
  n <- nchar(cl)
  
  out <- ''
  indent <- 0
  special <- c('(', ')', ',')
  
  if(nzchar(cl)) {
    for(i in 1:n) {
      current <- substr(cl,i,i)
      if(!(current %in% special)) {
        if(i>=1){
          past <- substr(cl,i-1,i-1)
          if(past == ',' || current == ' ') {}
          else {
            out <- paste(out, current, sep = '')
          }
        } else {
          out <- paste(out, current, sep = '')
        }
      } else if(current == '(') {
        indent <- indent + space.indent
        out <- paste(out, current, '\n', formatCall.create.spaces(space.indent, output = output), sep = '')
      } else if(current == ',') {
        out <- paste(out, current, '\n', formatCall.create.spaces(space.indent, output = output), sep = '')
      } else if(current == ')'){
        indent <- indent - space.indent
        out <- paste(out, '\n', current, '\n', sep = '')
      } else {
        stop('Unexpected character.')
      }
    }
  }
  
  if(display) cat(out)
  
  return(out)
}

# formatCall(call("round", 10.5), display=T)