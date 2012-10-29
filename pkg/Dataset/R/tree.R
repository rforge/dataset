##

setClass(
  'Treecontrol',
  contains = c('list'),
  validity = function(object) {
    flag = TRUE
    
    return(flag)
  }
)

treecontrol <- function(
  minsplit = 20,
  minbucket = 7,
  minprob = 0.01,
  maxheight = -1,
  alpha2 = 0.05,
  alpha3 = -1,
  alpha4 = 0.05,
  stump = FALSE
) {
  
  l <- list(
    'minsplit' = minsplit,
    'minbucket' = minbucket,
    'minprob' = minprob,
    'maxheight' = maxheight,
    'alpha2' = alpha2,
    'alpha3' = alpha3,
    'alpha4' = alpha4,
    'stump' = stump
  )
  
  out <- new('Treecontrol', '.Data' = l)

  return(out)
}

setMethod(
  f = "show",
  signature = "Treecontrol", 
  definition = function (object) {
#     l <-  object@.Data
#     names(l) <- names(object)
#     print(l)
    df <- data.frame(t(as.data.frame(object)))
    names(df) <- " "
    print(df)
  }
)

# print
setMethod("print", "Treecontrol", 
  definition = function (x, ...) {
    show(x)
  }
)









# 
# setClass(
#   'Tree',
#   contains = c('constparty', 'party'),
#   validity = function(object) {
#     flag = TRUE
#     
#     return(flag)
#   }
# )



addWhere <- function(tree, data){
#   where <- as.numeric(names(predict(tree)))
#   names(where) <- 1:length(where)
  where <- predict(tree, v(data), type='node')
  tree$where <- cvar(as.factor(where))
  description(tree$where) <- "Terminal node IDs"
  return(tree)
}

tree.chaid <- function(formula, data, control = treecontrol(), subset, na.action) {
  
  stopifnot(inherits(data, 'Dataset'))
  
  if(!is.installed.pkg('CHAID')) {
    exit.by.uninstalled.pkg('CHAID')
  } else {
    require(CHAID)
    setOldClass(c("constparty", "party"), where = .GlobalEnv)
    
    ctrl <- chaid_control(
      minsplit = control$minsplit,
      minbucket = control$minbucket,
      minprob = control$minprob,
      maxheight = control$maxheight,
      alpha2 = control$alpha2,
      alpha3 = control$alpha3,
      alpha4 = control$alpha4,
      stump = control$stump
    )
    
    out <-  do.call("chaid", list(
      'formula' = formula, 
      'data' = v(data),
      'control' = ctrl,
      'weights' = as.vector(weights(data))
    ))
          
    out <- addWhere(out, data)
#     out <- chaid(formula, data = v(data), control = ctrl, weights = weights)
    return(out)
  }
}

# ctr <- treecontrol()
# data(iris)
# iris$Sepal.Length <- cut(iris$Sepal.Length, breaks = 4)
# iris$Sepal.Width <- cut(iris$Sepal.Width, breaks = 4)
# iris$Petal.Length <- cut(iris$Petal.Length, breaks = 4)
# iris$Petal.Width <- cut(iris$Petal.Width, breaks = 4)
# ir <- dataset(iris)
# a <- tree.chaid(Species ~ ., data = ir)
# plot(a)

tree.cart <- function(formula, data, control = treecontrol(), subset, na.action) {
  
  stopifnot(inherits(data, 'Dataset'))
  
  if(!is.installed.pkg('rpart')) {
    exit.by.uninstalled.pkg('rpart')
  } else {
    require(rpart)
    if(!is.installed.pkg('CHAID')) {
      exit.by.uninstalled.pkg('CHAID')
    } else {
      require(CHAID)
      setOldClass(c("constparty", "party"), where = .GlobalEnv)
      
      ctrl <- rpart.control()
      ctrl$minsplit = control$minsplit
      ctrl$minbucket = control$minbucket
  #     ctrl$maxdepth = control$maxheight
      
      out <-  do.call("rpart", list(
        'formula' = formula, 
        'data' = v(data),
        'control' = ctrl,
        'weights' = as.vector(weights(data))
      ))
      
      out <- as.party(out)
      
      out <- addWhere(out, data)
                      
      return(out)
    }
  }
}

# ctr <- treecontrol()
# data(iris)
# iris$Sepal.Length <- cut(iris$Sepal.Length, breaks = 4)
# iris$Sepal.Width <- cut(iris$Sepal.Width, breaks = 4)
# iris$Petal.Length <- cut(iris$Petal.Length, breaks = 4)
# iris$Petal.Width <- cut(iris$Petal.Width, breaks = 4)
# ir <- dataset(iris)
# do.call("rpart", list(
#   'formula' = Species ~ ., 
#   'data' = iris,
#   'control' = rpart.control()
# #   'weights' = as.vector(weights(data))
# ))
# b1 <- as.party(rpart(Species ~ ., data = iris, weights = as.vector(weights(ir))))
# plot(b1)


# b <- tree.cart(Species ~ ., data = ir)
# plot(b)
# text(fit, use.n=TRUE)
# 
# rpart(Species ~ ., data = iris)

# setMethod("where", "party", 
#   definition = function (x, ...) {
#     show(x)
#   }
# )

# where.constparty <- function(x, ...) {
#   out <- as.numeric(names(predict(x)))
#   names(out) <- 1:length(out) #FIXME: use row.names
#   return(out)
# }
# where.party <- function(x, ...) {
#   out <- as.numeric(names(predict(x)))
#   names(out) <- 1:length(out) #FIXME: use row.names
#   return(out)
# }
# where.chaid <- function(x, ...) {
#   out <- as.numeric(names(predict(x)))
#   names(out) <- 1:length(out) #FIXME: use row.names
#   return(out)
# }

# setMethod("where", "rpart", 
#   definition = function (x, ...) {
#     return(x$where)
#   }
# )

# where.rpart <- function(x, ...) {
#   return(x$where)
# }


# summaryToPDF(a)

setMethod(
  f = 'summaryToPDF',
  signature = c('constparty'),
  definition = function (
    object,
    pdfSavingName,
    graphics = F,
    description.chlength,
    valids.chlength,
    valids.cut.percent,
    sorting,
    dateformat,
    latexPackages,
    width.id,
    width.varname,
    width.description,
    width.n,
    width.na,
    width.valids,
    width.valids.nao.inc,
    width.min,
    width.max,
    width.mean,
    width.stddev,
    keepTex,
    openPDF,
    merge = 'no'
  ) {
    
    if(!is.installed.pkg('xtable')) {
      exit.by.uninstalled.pkg('xtable')
    } else {
      
      require(xtable)
            
#       outName <- name(object)
      outName <- "Tree"
      
      outName.pdf <- make.names(outName) # no spaces for Unix/Texlive compilation ?
      
      if(missing(pdfSavingName)) {  	
        pdfSavingName <- paste("Summary-", outName.pdf, sep = "") # no spaces for Unix/Texlive compilation ?
      }
      
      latexFile <- paste(pdfSavingName, ".tex", sep="")
      
      outFileCon <- file(latexFile, "w", encoding="UTF-8")
      
      latex.head(title = paste("Summary of the", totex(outName)), latexPackages, outFileCon)
      
      #cat("\\section*{Overview} \n", file = outFileCon, append = T)
      
#       object.xtable <- xtable(
#         sdf(s),
#         #label='validCasesSummary',
#         #caption='Number of variables by percent of valid cases',
#         caption=thresholds(s),
#         #digits = 3,
#         align = c("l", rep('c', ncol(sdf(s)))),
#         #display = c("d","d","d")
#       )
      
      plot.filename <- paste(pdfSavingName, ".tree", sep = "")
      plot.filename <- gsub("\\.", "-", plot.filename)
      plot.filename.pdf <- paste(plot.filename, ".pdf", sep = "")
      pdf(file = plot.filename.pdf)
      plot(
        object
#         val,
#         type = "l",
#         main = "Number of variables by percent of valid cases",
#         xlab = "Percentage of valid cases",
#         ylab = "Number of variables"
      )
      dev.off()
      
      cat("\\begin{center} \n", file = outFileCon, append = T)
      cat("\\begin{minipage}[c]{.60\\linewidth} \n", file = outFileCon, append = T)
      cat("\\includegraphics[scale=0.60]{", plot.filename.pdf, "} \n", file = outFileCon, append = T, sep = "")
     
      cat("\\end{minipage} \n", file = outFileCon, append = T)
      cat("\\begin{minipage}[c]{.35\\linewidth} \n", file = outFileCon, append = T)
#       print(object.xtable, file=outFileCon , append=T, include.rownames = F, table.placement = "htb", floating=F)
      cat("\\subsection*{Information} \n", file = outFileCon, append = T)
      cat("\\begin{itemize*} \n", file = outFileCon, append = T)
      cat("\\item Depth:", depth(object), " \n", file = outFileCon, append = T)
      cat("\\item Width:", width(object), " \n", file = outFileCon, append = T)
      cat("\\item Length:", length(object), " \n", file = outFileCon, append = T)
      cat("\\end{itemize*} \n", file = outFileCon, append = T)
      cat("\\subsection*{Statistics} \n", file = outFileCon, append = T)
      where <- object$where
      cat("\\end{minipage} \n", file = outFileCon, append = T)
      cat("\\end{center} \n", file = outFileCon, append = T)
      
      
      close.and.clean(outFileCon, pdfSavingName, keepTex, openPDF)
    }
  }
)
