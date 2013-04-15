
setClass(
  'Treecontrol',
  contains = c('list'),
  validity = function(object) {
    flag = TRUE
    
    return(flag)
  }
)


tree.control <- function(
  min.for.splitting = 20, #chaid, rpart
  min.terminal.node.n = 7, #chaid, rpart
  min.terminal.node.percent = 1, #chaid
  min.complexity.reduction = 0.001,
  max.height = 3, #chaid, rpart (not used for rpart)
  max.pvalue.merge = 0.05, # chaid
  alpha3 = -1, # chaid
  max.pvalue.split = 0.05, # chaid
  stump = FALSE # chaid
) {
  
  l <- list(
    'min.for.splitting' = min.for.splitting,
    'min.terminal.node.n' = min.terminal.node.n,
    'min.terminal.node.percent' = min.terminal.node.percent,
    'min.complexity.reduction' = min.complexity.reduction,
    'max.height' = max.height,
    'max.pvalue.merge' = max.pvalue.merge,
    'alpha3' = alpha3,
    'max.pvalue.split' = max.pvalue.split,
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
#     df <- cbind(df, data.frame(
#       'CHAID' = c(T,T,T,F,T,T,T,T,T),
#       'CART'  = c(T,T,F,T,T,F,F,F,F)
#       ))
    df <- cbind(df, data.frame(
      'CHAID' = c('x','x','x','','x','x','x','x','x'),
      'CART'  = c('x','x','','x','x','','','','')
    ))
    names(df) <- c(' ', 'CHAID', 'CART')
    print(df)
  }
)

# print
setMethod("print", "Treecontrol", 
  definition = function (x, ...) {
    show(x)
  }
)

specific.control.TO.tree.control <- function(ctrl){ # as a list
  nam <- names(ctrl)
  if("alpha2" %in% nam)
    nam[which(nam == "alpha2")] <- 'max.pvalue.merge'
  if("alpha3" %in% nam)
    nam[which(nam == "alpha3")] <- 'alpha3'
  if("alpha4" %in% nam)
    nam[which(nam == "alpha4")] <- 'max.pvalue.split'
  if("minsplit" %in% nam)
    nam[which(nam == "minsplit")] <- 'min.for.splitting'
  if("minbucket" %in% nam)
    nam[which(nam == "minbucket")] <- 'min.terminal.node.n'
  if("minprob" %in% nam)
    nam[which(nam == "minprob")] <- 'min.terminal.node.percent'
  if("stump" %in% nam)
    nam[which(nam == "stump")] <- 'stump'
  if("maxheight" %in% nam)
    nam[which(nam == "maxheight")] <- 'max.height'
  if("maxdepth" %in% nam)
    nam[which(nam == "maxdepth")] <- 'max.height'
  if("cp" %in% nam)
    nam[which(nam == "cp")] <- 'min.complexity.reduction'
  
  names(ctrl) <- nam
  return(ctrl)
}
# specific.control.TO.tree.control(a@control)




setClass(
  'Tree',
#   contains = c('constparty', 'party'),
  representation(
    method.name = 'character',
    description = 'character',
    date = 'character',
    call = 'call',
    formula = 'formula',
    data.model = 'Dataset',
    control = 'list',
    tree = 'list'
  ),
  validity = function(object) {
    flag = TRUE
    
    return(flag)
  }
)

setMethod(
  f = "show",
  signature = "Tree", 
  definition = function (object) {
    print(object@tree[[1]])
  }
)


setMethod(
  f = "print",
  signature = "Tree", 
  definition = function (x, ...) {
    show(x)
  }
)

setMethod(
  f = "description",
  signature = "Tree", 
  definition = function (object) {
    object@description
  }
)

tree.height <- function(x) {
  stopifnot(inherits(x, 'Tree'))
  return(depth(x@tree[[1]]))
}
tree.width <- function(x) {
  stopifnot(inherits(x, 'Tree'))
  return(width(x@tree[[1]]))
}
tree.length <- function(x) {
  stopifnot(inherits(x, 'Tree'))
  return(length(x@tree[[1]]))
}
tree.where <- function(x) {
  stopifnot(inherits(x, 'Tree'))
  return(x@tree[[1]]$where)
}
tree.predict <- function(x) {
  stopifnot(inherits(x, 'Tree'))
  return(predict(x@tree[[1]]))
}
tree.target <- function(x) {
  stopifnot(inherits(x, 'Tree'))
  return(a@data.model[[all.vars(a@formula)[1]]])
}

# predicted <- as.character(tree.predict(a))
# real <- as.character(v(tree.target(a)))
# unique.p <- unique(predicted)
# unique.r <- unique(real)
# n.unique.p <- length(unique.p)
# n.unique.r <- length(unique.r)
# stopifnot(n.unique.p == n.unique.r)
# stopifnot(unique.p == unique.r)
# if(n.unique.r > 2) {
#   real <- replace(real, which(real %in% unique.r[2:n.unique.r]), 'other')
#   predicted <- replace(predicted, which(predicted %in% unique.r[2:n.unique.r]), 'other')
# }
# pred <- prediction(factor(predicted), factor(real))
# perf <- performance(pred, measure = "tpr", x.measure = "fpr")
# plot(perf, col=rainbow(10))
# library(ROCR)
# data(ROCR.simple)
# pred <- prediction(ROCR.simple$predictions,ROCR.simple$labels)

setMethod(
  'plot',
  'Tree', 
  definition = function(
    x,
    main = NULL,
    terminal_panel = node_barplot_round,
    tp_args = list(), 
    inner_panel = node_barplot_round_inner,
    ip_args = list(),
    edge_panel = edge_simple, 
    ep_args = list(),
    type = c("extended", "simple"),
    drop_terminal = NULL,
    tnex = NULL,
    newpage = TRUE, 
    pop = TRUE,
    gp = gpar(),
    ...
    ) { 
    if(!is.installed.pkg('partykit')) {
      exit.by.uninstalled.pkg('partykit')
    } else {
      require(partykit)
      
      tree <- x@tree[[1]]

      partykit:::plot.constparty( # will select plot method for constparty objects
        x = tree,
        main = main,
        terminal_panel = terminal_panel,
        tp_args = tp_args, 
        inner_panel = inner_panel,
        ip_args = ip_args,
        edge_panel = edge_panel, 
        ep_args = ep_args,
        type = type,
        drop_terminal = drop_terminal,
        tnex = tnex,
        newpage = newpage, 
        pop = pop,
        gp = gp,
        ... = ...
      )
    }
  }
)


addWhere <- function(tree, data){
#   where <- as.numeric(names(predict(tree)))
#   names(where) <- 1:length(where)
  where <- predict(tree, v(data), type='node')
  tree$where <- cvar(as.factor(where))
  description(tree$where) <- "Terminal node IDs"
  return(tree)
}

tree.learn.chaid <- function(
  formula,
  data,
  control = tree.control(),
  subset,
  na.action,
  valid.lab.trunc = '15',
  interactive = F
) {
  
  stopifnot(inherits(data, 'Dataset'))
  
  if(!is.installed.pkg('CHAID')) {
    exit.by.uninstalled.pkg('CHAID', 'http://r-forge.r-project.org')
  } else {
    require(CHAID)
    setOldClass(c("constparty", "party"), where = .GlobalEnv)
    
    cl <- match.call()
    
    ctrl <- chaid_control(
      minsplit = control$min.for.splitting,
      minbucket = control$min.terminal.node.n,
      minprob = control$min.terminal.node.percent,
      maxheight = control$max.height,
      alpha2 = control$max.pvalue.merge,
      alpha3 = control$alpha3,
      alpha4 = control$max.pvalue.split,
      stump = control$stump
    )
      
    if(!interactive){
      
      data.df <- v(data)
      
      mf <- model.frame(formula, data = data.df)
      
      out <-  do.call("chaid", list(
        'formula' = formula, 
        'data' = data.df,
        'control' = ctrl,
        'weights' = as.vector(weights(data))
      ))
      
      if(depth(out) == 0) {
        tree.out.no.split()
        return(NULL)
      }
            
      out <- addWhere(out, data)
  #     out <- chaid(formula, data = v(data), control = ctrl, weights = weights)
      attr(ctrl, 'class') <- NULL
      return(new(
        Class = "Tree",
        method.name = 'CHAID',
        description = 'None',
        date = giveDate(),
        call = cl,
        formula = formula(terms(out)),
        data.model = data[,names(mf)],
        control = ctrl,
        tree = list(out)
      ))
    } else {
      path <- getwd()
      app.name <- 'shiny-tree.learn.chaid'
      packages.ui <- ''
      packages.server <- c('Dataset', 'partykit', 'CHAID')
      
      
      shiny.app <- shiny.write.app(
        folder = path,
        app = app.name
      )
      
      setwd(file.path(path,app.name))
      export(data, 'data', openPDF=F)
      setwd(path)
      
      var.descriptors <- attr(terms(formula),"term.labels")
      checkboxs <- list()
      for (i in 1:length(var.descriptors)) {
        checkboxs[[i]] <- paste('checkboxInput(\'',var.descriptors[i],"', '",var.descriptors[i],"', TRUE)", sep='')
      }
      
      shiny.write.ui(
        title = 'Tree analysis - CHAID method',
        sidebarPanel =  c(list(
          "h4('Tree growing settings')",
          "h4('‾‾‾‾')",
          paste("sliderInput('min.terminal.node.percent', 'min.terminal.node.percent:', min = 0, max = 100, value = ", control$min.terminal.node.percent,")", sep=''),          
          paste("sliderInput('min.for.splitting', 'min.for.splitting:', min = 0, max = 100, value = ",control$min.for.splitting,")", sep=''),          
          paste("sliderInput('max.pvalue.merge', 'max.pvalue.merge:', min = 0, max = 1, value = ",control$max.pvalue.merge,", step = 0.01)", sep=''),          
          paste("sliderInput('max.pvalue.split', 'max.pvalue.split:', min = 0, max = 1, value = ",control$max.pvalue.split,", step = 0.01)", sep=''),          
          paste("sliderInput('max.height', 'max.height:', min = 0, max = 10, value = ",control$max.height,")", sep=''),          
          "h4('____')",
          "h4('Explanatory variables involved:')"), checkboxs
        ),
        mainPanel = list(
          "plotOutput('treePlot')"  
        ),
        app = shiny.app,
        packages = packages.ui
      )
      
      shiny.write.server.intro(
        app = shiny.app,
        packages = packages.server
      )
      
      
      cat("output$treePlot <- renderPlot({", ' \n', file = shiny.app$server, append=T)
      cat("  load('data.RData')", ' \n', file = shiny.app$server, append=T)
      cat("  formula <- ", deparse(formula), ' \n', file = shiny.app$server, append=T)
      
      var.descriptors <- attr(terms(formula),"term.labels")
      for (i in var.descriptors) {
        cat("  if(!input[['", i, "']]) formula <- update(formula, . ~ . - ", i,")\n", file = shiny.app$server, append=T, sep='')
      }
      cat("\n", file = shiny.app$server, append=T)
      cat("  tr <- tree.learn.chaid(", "\n", file = shiny.app$server, append=T)
      cat("    formula = formula,", "\n", file = shiny.app$server, append=T)
      cat("    data = data,", "\n", file = shiny.app$server, append=T)
      cat("    control = tree.control(", "\n", file = shiny.app$server, append=T)
      cat("      min.terminal.node.percent = input$min.terminal.node.percent,", file = shiny.app$server, "\n", append=T)
      cat("      min.for.splitting = input$min.for.splitting,", "\n", file = shiny.app$server, append=T)
      cat("      max.pvalue.merge = input$max.pvalue.merge,", "\n", file = shiny.app$server, append=T)
      cat("      max.pvalue.split = input$max.pvalue.split,", "\n", file = shiny.app$server, append=T)
      cat("      max.height = input$max.height", "\n", file = shiny.app$server, append=T)
      cat("    )", "\n", file = shiny.app$server, append=T)
      cat("  )", "\n", file = shiny.app$server, append=T)
      cat("\n", file = shiny.app$server, append=T)
      
      cat("  plot(tr) \n", "\n", file = shiny.app$server, append=T)
      cat("})", "\n", file = shiny.app$server, append=T)
      
      shiny.write.server.outro(
        app = shiny.app
      )
      
      shiny.run(shiny.app)
    }
  }
}

# ctr <- tree.control()
# data(iris)
# iris$Sepal.Length <- cut(iris$Sepal.Length, breaks = 4)
# iris$Sepal.Width <- cut(iris$Sepal.Width, breaks = 4)
# iris$Petal.Length <- cut(iris$Petal.Length, breaks = 4)
# iris$Petal.Width <- cut(iris$Petal.Width, breaks = 4)
# ir <- dataset(iris)
# a <- tree.learn.chaid(
#   Species ~ .,
#   data = ir
# )
# plot(a@tree[[1]])
# plot(a)
# exportPDF(a, plot.tree.ratio = 1.5)
# b1 <- bivan(
#   formula = a@formula,
#   data = a@data.model
# )
# tree.where(a)
# fm <- a@formula
# dm <- a@data.model
# dm$where <- tree.where(a)
# fm <- update(fm, . ~ . + where)
# b1 <- bivan(
#   formula = fm,
#   data = dm
# )

tree.learn.cart <- function(
  formula,
  data,
  control = tree.control(),
  subset,
  na.action,
  valid.lab.trunc = '15',
  interactive = F
) {
  
  stopifnot(inherits(data, 'Dataset'))
  
  if(!is.installed.pkg('rpart')) {
    exit.by.uninstalled.pkg('rpart')
  } else {
    require(rpart)
#     if(!is.installed.pkg('CHAID')) {
#       exit.by.uninstalled.pkg('CHAID', 'http://r-forge.r-project.org')
#     } else {
#       require(CHAID)
    if(!is.installed.pkg('partykit')) {
      exit.by.uninstalled.pkg('partykit')
    } else {
      require(partykit)
      setOldClass(c("constparty", "party"), where = .GlobalEnv)
      
      cl <- match.call()
      
      ctrl <- rpart.control()
      ctrl$minsplit <- control$min.for.splitting
      ctrl$minbucket <- control$min.terminal.node.n
      ctrl$cp <- control$min.complexity.reduction
#       ctrl$maxdepth <- control$max.height ##FIXME, return a root tree
      
      if(!interactive){
        
        
        data.df <- v(data)      
        
        mf <- model.frame(formula, data = data.df)
        
        
        
        out <-  do.call("rpart", list(
          'formula' = formula, 
          'data' = v(data),
          'control' = ctrl,
          'weights' = as.vector(weights(data))
        ))
        
  
        out <- as.party(out)
        
        if(depth(out) == 0) {
          tree.out.no.split()
          return(NULL)
        }
        
        # if we got a root node, as.party coerce to a partynode instead of a party object
        # then we have to coerce manually
        if(inherits(out, "partynode"))
          out <- party(out, v(data))
        
  #       out.formula <- formula(terms(out))
        out.formula <- as.formula("y ~ x")
        
        out <- addWhere(out, data)
        
        attr(ctrl, 'class') <- NULL
                        
        return(new(
          Class = "Tree",
          method.name = 'CART',
          description = 'None',
          date = giveDate(),
          call = cl,
          formula = out.formula,
          data.model = data[,names(mf)],
          control = ctrl,
          tree = list(out)
        ))
      } else {
        path <- getwd()
        app.name <- 'shiny-tree.learn.cart'
        packages.ui <- ''
        packages.server <- c('Dataset', 'partykit')
        
        
        shiny.app <- shiny.write.app(
          folder = path,
          app = app.name
        )
        
        setwd(file.path(path,app.name))
        export(data, 'data', openPDF=F)
        setwd(path)
        
        var.descriptors <- attr(terms(formula),"term.labels")
        checkboxs <- list()
        for (i in 1:length(var.descriptors)) {
          checkboxs[[i]] <- paste('checkboxInput(\'',var.descriptors[i],"', '",var.descriptors[i],"', TRUE)", sep='')
        }

        
        shiny.write.ui(
          title = 'Tree analysis - CART method',
          sidebarPanel =  c(list(
            "h4('Tree growing settings')",
            "h4('‾‾‾‾')",
            paste("sliderInput('min.terminal.node.percent', 'min.terminal.node.percent:', min = 0, max = 100, value = ", control$min.terminal.node.percent,")", sep=''),          
            paste("sliderInput('min.for.splitting', 'min.for.splitting:', min = 0, max = 100, value = ",control$min.for.splitting,")", sep=''),          
            paste("sliderInput('min.complexity.reduction', 'min.complexity.reduction:', min = 0, max = 1, value = ",control$min.complexity.reduction,", step = 0.001)", sep=''),                  
            paste("sliderInput('max.height', 'max.height:', min = 0, max = 10, value = ",control$max.height,")", sep=''),          
            "h4('____')",
            "h4('Explanatory variables involved:')"), checkboxs
          ),
          mainPanel = list(
            "plotOutput('treePlot')"  
          ),
          app = shiny.app,
          packages = packages.ui
        )
        
        shiny.write.server.intro(
          app = shiny.app,
          packages = packages.server
        )
        
        
        cat("output$treePlot <- renderPlot({", ' \n', file = shiny.app$server, append=T)
        cat("  load('data.RData')", ' \n', file = shiny.app$server, append=T)
        cat("  formula <- ", deparse(formula), ' \n', file = shiny.app$server, append=T)
        
        var.descriptors <- attr(terms(formula),"term.labels")
        for (i in var.descriptors) {
          cat("  if(!input[['", i, "']]) formula <- update(formula, . ~ . - ", i,")\n", file = shiny.app$server, append=T, sep='')
        }
        cat("\n", file = shiny.app$server, append=T)
        cat("  tr <- tree.learn.cart(", "\n", file = shiny.app$server, append=T)
        cat("    formula = formula,", "\n", file = shiny.app$server, append=T)
        cat("    data = data,", "\n", file = shiny.app$server, append=T)
        cat("    control = tree.control(", "\n", file = shiny.app$server, append=T)
        cat("      min.terminal.node.percent = input$min.terminal.node.percent,", file = shiny.app$server, "\n", append=T)
        cat("      min.for.splitting = input$min.for.splitting,", "\n", file = shiny.app$server, append=T)
        cat("      min.complexity.reduction = input$min.complexity.reduction,", "\n", file = shiny.app$server, append=T)
        cat("      max.height = input$max.height", "\n", file = shiny.app$server, append=T)
        cat("    )", "\n", file = shiny.app$server, append=T)
        cat("  )", "\n", file = shiny.app$server, append=T)
        cat("\n", file = shiny.app$server, append=T)
        
        cat("  plot(tr) \n", "\n", file = shiny.app$server, append=T)
        cat("})", "\n", file = shiny.app$server, append=T)
        
        shiny.write.server.outro(
          app = shiny.app
        )
        
        shiny.run(shiny.app)
      }
    }
  }
}

# ctr <- tree.control()
# data(iris)
# iris$Sepal.Length <- cut(iris$Sepal.Length, breaks = 4)
# iris$Sepal.Width <- cut(iris$Sepal.Width, breaks = 4)
# iris$Petal.Length <- cut(iris$Petal.Length, breaks = 4)
# iris$Petal.Width <- cut(iris$Petal.Width, breaks = 4)
# ir <- dataset(iris)
# 
# b <- tree.learn.cart(Species ~ ., data = ir)
# plot(b)
# b <- tree.learn.chaid(Species ~ ., data = ir)
# plot(b)
# exportPDF(b)

## do.call("rpart", list(
##   'formula' = Species ~ ., 
##   'data' = iris,
##   'control' = rpart.control()
## #   'weights' = as.vector(weights(data))
## ))
## b1 <- as.party(rpart(Species ~ ., data = iris, weights = as.vector(weights(ir))))
## plot(b1)



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


# exportPDF(a)

setMethod(
  f = 'exportPDF',
  signature = c('Tree'),
  definition = function (
    object,
    pdfSavingName,
    graphics = F,
    description.chlength,
    valids.chlength,
    valids.cut.percent,
    sorting,
    dateformat,
    page.orientation,
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
    plot.tree.ratio = 1,
    keepTex,
    openPDF,
    merge = 'no'
  ) {
    
    check.tex()
    
    if(!is.installed.pkg('xtable')) {
      exit.by.uninstalled.pkg('xtable')
    } else {
      
      require(xtable)
            
#       outName <- name(object)
      outName <- paste("Tree-", object@method.name, sep='')
      
      outName.pdf <- make.names(outName) # no spaces for Unix/Texlive compilation ?
      
      if(missing(pdfSavingName)) {  	
        pdfSavingName <- paste("Summary-", outName.pdf, sep = "") # no spaces for Unix/Texlive compilation ?
      }
      
      latexFile <- paste(pdfSavingName, ".tex", sep="")
      
      is.writable(pdfSavingName, path = getwd())
      
      outFileCon <- file(latexFile, "w", encoding="UTF-8")
      
      latex.head(title = paste("Summary of the", totex(outName)),
                 page.orientation, latexPackages, outFileCon)
      
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
#       pdf(file = plot.filename.pdf, paper='a4r')
#       pdf(file = plot.filename.pdf, width=11.7, height=8.3) # a4 in inches 11.7 x 8.3
      pdf(file = plot.filename.pdf, width=11.7*plot.tree.ratio, height=8.3*plot.tree.ratio)
#       par(pin=c(10, 4)) # width, height
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
      cat("\\begin{minipage}[t]{.50\\linewidth} \n", file = outFileCon, append = T)
#       cat("\\section*{Input} \n", file = outFileCon, append = T)
      cat("\\subsection*{Call} \n", file = outFileCon, append = T)
      cat("\\begin{verbatim}", " \n", file = outFileCon, append = T)
#       capture.output(formatCall(object@call), file = outFileCon, append = T)
      sink(file = outFileCon, append = T)
      formatCall(object@call, display=T)
      sink(NULL)
      cat("\\end{verbatim}", " \n", file = outFileCon, append = T)
      
      cat("\\subsection*{Model formula} \n", file = outFileCon, append = T)
      
      cat("\\begin{verbatim} \n", file = outFileCon, append = T)
      cat(deparse(object@formula), " \n", file = outFileCon, append = T)
      cat("\\end{verbatim} \n", file = outFileCon, append = T)
      
      cat("\\subsection*{Settings} \n", file = outFileCon, append = T)
      
      cat(list.to.tex(specific.control.TO.tree.control(object@control)), file = outFileCon, append = T)
      
     
      cat("\\end{minipage} \n", file = outFileCon, append = T)
      cat("\\begin{minipage}[t]{.45\\linewidth} \n", file = outFileCon, append = T)
#       print(object.xtable, file=outFileCon , append=T, include.rownames = F, table.placement = "htb", floating=F)
#       cat("\\section*{Output} \n", file = outFileCon, append = T)
            
      cat("\\subsection*{Variables} \n", file = outFileCon, append = T)
      
      allv <- all.vars(object@formula)
      allvlist <- vector(length(allv), mode = 'list')
      names(allvlist) <- allv
      for(i in 1:length(allv)) {
        allvlist[i] <- description(object@data.model[[allv[i]]])
      }
      cat(list.to.tex(allvlist), file = outFileCon, append = T)
      
      cat("\\subsection*{Date of the analysis} \n", file = outFileCon, append = T)
      cat(totex(object@date), " \n", file = outFileCon, append = T)
      
      cat("\\subsection*{Description} \n", file = outFileCon, append = T)
      cat(totex(description(object)), " \n", file = outFileCon, append = T)
      
      
      
      cat("
        \\end{minipage} \n
        \\end{center} \n
      ", file = outFileCon, append = T)
      
      
      cat("\\newpage \n", file = outFileCon, append = T)
      
      cat("\\begin{center} \n", file = outFileCon, append = T)
      cat("\\begin{minipage}[t]{.50\\linewidth} \n", file = outFileCon, append = T)
      cat("\\subsection*{Tree metrics} \n", file = outFileCon, append = T)
      cat("\\begin{itemize*} \n", file = outFileCon, append = T)
      cat("\\item Height:", tree.height(object), " \n", file = outFileCon, append = T)
      cat("\\item Width:", tree.width(object), " \n", file = outFileCon, append = T)
      cat("\\item Length:", tree.length(object), " \n", file = outFileCon, append = T)
      cat("\\end{itemize*} \n", file = outFileCon, append = T)
      cat("\\subsection*{Bivariate analysis} \n", file = outFileCon, append = T)
      
      fm <- object@formula
      dm <- object@data.model
      dm$nodes <- tree.where(object)
      fm <- update(fm, . ~ . + nodes)
      biv1 <- bivan(
        formula = fm,
        data = dm
      )
      
      s <- summary(global(biv1), merge = 'left')
      object.xtable <- xtable(
        sdf(s),
        align = c("l", rep('c', ncol(sdf(s)))),
        caption=paste('Comparative bivariate analysis between the node ids and the target. Legend:', thresholds(s))
      )
      print(object.xtable, file=outFileCon , append=T,
            table.placement = "htb",
            floating=F
      )
      
      cat("\n \\noindent Comparative bivariate analysis between the node ids and the target. \n", file = outFileCon, append = T)
      cat("\\newline Legend:", thresholds(s),"\n", file = outFileCon, append = T)
      
      cat("\\end{minipage} \n", file = outFileCon, append = T)
      cat("\\begin{minipage}[t]{.45\\linewidth} \n", file = outFileCon, append = T)
      
      cat("\\subsection*{Tree quality assessment} \n", file = outFileCon, append = T)
      cat("\\emph{Forthcoming} \n", file = outFileCon, append = T)
      
      
      cat("
          \\end{minipage} \n
          \\end{center} \n
          ", file = outFileCon, append = T)
      
      cat("\\newpage \n", file = outFileCon, append = T)
#       cat("\\thispagestyle{empty} \n", file = outFileCon, append = T)
      cat("\\begin{center} \n", file = outFileCon, append = T)
#       cat("\\includegraphics[width=0.9\\textwidth]{", plot.filename.pdf, "} \n", file = outFileCon, append = T, sep = "")
      cat("\\includegraphics[scale=", 0.8/plot.tree.ratio, "]{", plot.filename.pdf, "} \n", file = outFileCon, append = T, sep = "")
      cat("\\end{center} \n", file = outFileCon, append = T)
      
      close.and.clean(outFileCon, pdfSavingName, keepTex, openPDF)
    }
  }
)

node_barplot_round <- function(
  obj,
  col = "black",
  fill = NULL,
  beside = NULL,
  ymax = NULL,
  ylines = NULL,
  widths = 1,
  gap = NULL,
  reverse = NULL,
  id = TRUE,
  gp = gpar()
){
  y <- obj$fitted[["(response)"]]
  stopifnot(is.factor(y) || isTRUE(all.equal(round(y), y)))
  probs_and_n <- function(x) {
    y1 <- x$fitted[["(response)"]]
    if (!is.factor(y1)) 
      y1 <- factor(y1, levels = min(y):max(y))
    w <- x$fitted[["(weights)"]]
    if (is.null(w)) 
      w <- rep.int(1L, length(y1))
    sumw <- tapply(w, y1, sum)
    sumw[is.na(sumw)] <- 0
    prob <- c(sumw/sum(w), sum(w))
    names(prob) <- c(levels(y1), "nobs")
    prob
  }
  probs <- do.call("rbind", nodeapply(obj, nodeids(obj), probs_and_n, 
                                      by_node = FALSE))
  nobs <- probs[, "nobs"]
  probs <- probs[, -ncol(probs)]
  if (is.factor(y)) {
    ylevels <- levels(y)
    if (is.null(beside)) 
      beside <- if (length(ylevels) < 3) 
        FALSE
    else TRUE
    if (is.null(ymax)) 
      ymax <- if (beside) 
        1.1
    else 1
    if (is.null(gap)) 
      gap <- if (beside) 
        0.1
    else 0
  }
  else {
    if (is.null(beside)) 
      beside <- FALSE
    if (is.null(ymax)) 
      ymax <- max(probs) * 1.1
    ylevels <- seq(1:NCOL(probs))
    if (length(ylevels) < 2) 
      ylevels <- ""
    if (is.null(gap)) 
      gap <- 1
  }
  if (is.null(reverse)) 
    reverse <- !beside
  if (is.null(fill)) 
    fill <- gray.colors(length(ylevels))
  if (is.null(ylines)) 
    ylines <- if (beside) 
      c(3, 2)
  else c(1.5, 2.5)
  rval <- function(node) {
    nid <- id_node(node)
    pred <- probs[nid, ]
    if (reverse) {
      pred <- rev(pred)
      ylevels <- rev(ylevels)
    }
    np <- length(pred)
    nc <- if (beside) 
      np
    else 1
    fill <- rep(fill, length.out = np)
    widths <- rep(widths, length.out = nc)
    col <- rep(col, length.out = nc)
    ylines <- rep(ylines, length.out = 2)
    gap <- gap * sum(widths)
    yscale <- c(0, ymax)
    xscale <- c(0, sum(widths) + (nc + 1) * gap)
    top_vp <- viewport(layout = grid.layout(nrow = 2, ncol = 3, 
                                            widths = unit(c(ylines[1], 1, ylines[2]), c("lines", 
                                                                                        "null", "lines")), heights = unit(c(1, 1), c("lines", 
                                                                                                                                     "null"))), width = unit(1, "npc"), height = unit(1, 
                                                                                                                                                                                      "npc") - unit(2, "lines"), name = paste("node_barplot", 
                                                                                                                                                                                                                              nid, sep = ""), gp = gp)
    pushViewport(top_vp)
    grid.rect(gp = gpar(fill = "white", col = 0))
    top <- viewport(layout.pos.col = 2, layout.pos.row = 1)
    pushViewport(top)
    mainlab <- paste(ifelse(id, paste("Node", names(obj)[nid], 
                                      "(n = "), "n = "), round(nobs[nid]), ifelse(id, ")", ""), 
                     sep = "")
    grid.text(mainlab)
    popViewport()
    plot <- viewport(layout.pos.col = 2, layout.pos.row = 2, 
                     xscale = xscale, yscale = yscale, name = paste("node_barplot", 
                                                                    node$nodeID, "plot", sep = ""))
    pushViewport(plot)
    if (beside) {
      xcenter <- cumsum(widths + gap) - widths/2
      for (i in 1:np) {
        grid.rect(x = xcenter[i], y = 0, height = pred[i], 
                  width = widths[i], just = c("center", "bottom"), 
                  default.units = "native", gp = gpar(col = col[i], 
                                                      fill = fill[i]))
      }
      if (length(xcenter) > 1) 
        grid.xaxis(at = xcenter, label = FALSE)
      grid.text(ylevels, x = xcenter, y = unit(-1, "lines"), 
                just = c("center", "top"), default.units = "native", 
                check.overlap = TRUE)
      grid.yaxis()
    }
    else {
      ycenter <- cumsum(pred) - pred
      for (i in 1:np) {
        grid.rect(x = xscale[2]/2, y = ycenter[i], height = min(pred[i], 
                                                                ymax - ycenter[i]), width = widths[1], just = c("center", 
                                                                                                                "bottom"), default.units = "native", gp = gpar(col = col[i], 
                                                                                                                                                               fill = fill[i]))
      }
      if (np > 1) {
        grid.text(ylevels[1], x = unit(-1, "lines"), 
                  y = 0, just = c("left", "center"), rot = 90, 
                  default.units = "native", check.overlap = TRUE)
        grid.text(ylevels[np], x = unit(-1, "lines"), 
                  y = ymax, just = c("right", "center"), rot = 90, 
                  default.units = "native", check.overlap = TRUE)
      }
      if (np > 2) {
        grid.text(ylevels[-c(1, np)], x = unit(-1, "lines"), 
                  y = ycenter[-c(1, np)], just = "center", rot = 90, 
                  default.units = "native", check.overlap = TRUE)
      }
      grid.yaxis(main = FALSE)
    }
    grid.rect(gp = gpar(fill = "transparent"))
    upViewport(2)
  }
  return(rval)
}

class(node_barplot_round) <- "grapcon_generator"


node_barplot_round_inner <- function(
  obj,
  col = "black",
  fill = NULL,
  beside = NULL,
  ymax = NULL,
  ylines = NULL,
  widths = 1,
  gap = NULL,
  reverse = NULL,
  id = TRUE,
  gp = gpar(),
  abbreviate = FALSE
){
  y <- obj$fitted[["(response)"]]
  stopifnot(is.factor(y) || isTRUE(all.equal(round(y), y)))
  probs_and_n <- function(x) {
    y1 <- x$fitted[["(response)"]]
    if (!is.factor(y1)) 
      y1 <- factor(y1, levels = min(y):max(y))
    w <- x$fitted[["(weights)"]]
    if (is.null(w)) 
      w <- rep.int(1L, length(y1))
    sumw <- tapply(w, y1, sum)
    sumw[is.na(sumw)] <- 0
    prob <- c(sumw/sum(w), sum(w))
    names(prob) <- c(levels(y1), "nobs")
    prob
  }
  
  #------
  meta <- obj$data
  nam <- names(obj)
  extract_label <- function(node) {
    if (is.terminal(node)) 
      return(rep.int("", 2))
    varlab <- character_split(split_node(node), meta)$name
    if (abbreviate > 0) 
      varlab <- abbreviate(varlab, as.numeric(abbreviate))
    plab <- ""
    return(c(varlab, plab))
  }
  #------
  
  probs <- do.call("rbind", nodeapply(obj, nodeids(obj), probs_and_n, 
                                      by_node = FALSE))
  nobs <- probs[, "nobs"]
  probs <- probs[, -ncol(probs)]
  if (is.factor(y)) {
    ylevels <- levels(y)
    if (is.null(beside)) 
      beside <- if (length(ylevels) < 3) 
        FALSE
    else TRUE
    if (is.null(ymax)) 
      ymax <- if (beside) 
        1.1
    else 1
    if (is.null(gap)) 
      gap <- if (beside) 
        0.1
    else 0
  }
  else {
    if (is.null(beside)) 
      beside <- FALSE
    if (is.null(ymax)) 
      ymax <- max(probs) * 1.1
    ylevels <- seq(1:NCOL(probs))
    if (length(ylevels) < 2) 
      ylevels <- ""
    if (is.null(gap)) 
      gap <- 1
  }
  if (is.null(reverse)) 
    reverse <- !beside
  if (is.null(fill)) 
    fill <- gray.colors(length(ylevels))
  if (is.null(ylines)) 
    ylines <- if (beside) 
      c(3, 2)
  else c(1.5, 2.5)
  rval <- function(node) {
    nid <- id_node(node)
    pred <- probs[nid, ]
    if (reverse) {
      pred <- rev(pred)
      ylevels <- rev(ylevels)
    }
    np <- length(pred)
    nc <- if (beside) 
      np
    else 1
    fill <- rep(fill, length.out = np)
    widths <- rep(widths, length.out = nc)
    col <- rep(col, length.out = nc)
    ylines <- rep(ylines, length.out = 2)
    gap <- gap * sum(widths)
    yscale <- c(0, ymax)
    xscale <- c(0, sum(widths) + (nc + 1) * gap)
    top_vp <- viewport(layout = grid.layout(nrow = 2, ncol = 3, 
                                            widths = unit(c(ylines[1], 1, ylines[2]), c("lines", 
                                                                                        "null", "lines")), heights = unit(c(1, 1), c("lines", 
                                                                                                                                     "null"))), width = unit(1, "npc"), height = unit(1, 
                                                                                                                                                                                      "npc") - unit(2, "lines"), name = paste("node_barplot", 
                                                                                                                                                                                                                              nid, sep = ""), gp = gp)
    pushViewport(top_vp)
    grid.rect(gp = gpar(fill = "white", col = 0))
    top <- viewport(layout.pos.col = 2, layout.pos.row = 1)
    pushViewport(top)
    mainlab <- paste(
      ifelse(
        id,
        paste(
          "Node",
          names(obj)[nid],
          "(n = "
        ),
        "n = "
      ),
      round(nobs[nid]),
      ifelse(
        id, ")",
        ""
      ),
      ", split: ",
      extract_label(node),
      sep = ""
    )
    grid.text(mainlab)
    popViewport()
    plot <- viewport(layout.pos.col = 2, layout.pos.row = 2, 
                     xscale = xscale, yscale = yscale, name = paste("node_barplot", 
                                                                    node$nodeID, "plot", sep = ""))
    pushViewport(plot)
    if (beside) {
      xcenter <- cumsum(widths + gap) - widths/2
      for (i in 1:np) {
        grid.rect(x = xcenter[i], y = 0, height = pred[i], 
                  width = widths[i], just = c("center", "bottom"), 
                  default.units = "native", gp = gpar(col = col[i], 
                                                      fill = fill[i]))
      }
      if (length(xcenter) > 1) 
        grid.xaxis(at = xcenter, label = FALSE)
      grid.text(ylevels, x = xcenter, y = unit(-1, "lines"), 
                just = c("center", "top"), default.units = "native", 
                check.overlap = TRUE)
      grid.yaxis()
    }
    else {
      ycenter <- cumsum(pred) - pred
      for (i in 1:np) {
        grid.rect(x = xscale[2]/2, y = ycenter[i], height = min(pred[i], 
                                                                ymax - ycenter[i]), width = widths[1], just = c("center", 
                                                                                                                "bottom"), default.units = "native", gp = gpar(col = col[i], 
                                                                                                                                                               fill = fill[i]))
      }
      if (np > 1) {
        grid.text(ylevels[1], x = unit(-1, "lines"), 
                  y = 0, just = c("left", "center"), rot = 90, 
                  default.units = "native", check.overlap = TRUE)
        grid.text(ylevels[np], x = unit(-1, "lines"), 
                  y = ymax, just = c("right", "center"), rot = 90, 
                  default.units = "native", check.overlap = TRUE)
      }
      if (np > 2) {
        grid.text(ylevels[-c(1, np)], x = unit(-1, "lines"), 
                  y = ycenter[-c(1, np)], just = "center", rot = 90, 
                  default.units = "native", check.overlap = TRUE)
      }
      grid.yaxis(main = FALSE)
    }
    grid.rect(gp = gpar(fill = "transparent"))
    upViewport(2)
  }
  return(rval)
}

class(node_barplot_round_inner) <- "grapcon_generator"





tree.out.no.split <- function() {
  message("No split performed.")
  message("Maybe your control parameters are too constraining.")
  message("NULL is returned")
}
