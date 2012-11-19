# TODO : methode S4 ?
# formula est de classe formula
# imbric est une liste, chaque élément est une list de termes
# exemple : formula = santébin ~ sexe + age, imbric = list(~ education + occupation, ~ partenaire)
#

setClass(
  'RegLog',
  contains = c('list'),
  validity = function(object) {
    flag = TRUE
    
    return(flag)
  }
)

constrasts.indicator <- c(
  'unordered' = "contr.treatment",
  'ordered' = "contr.treatment"
)

reglog <- function(
  formula,
  target,
  imbric,
  data,
  model.type = NULL,
  contrasts = 'indicator',
  subset,
  na.action,
  ...
) {


  data.Dataset <- data
  data <- v(data)
  
  contrasts.user <- options("contrasts")$contrasts
  constrasts.indicator <- c(
    'unordered' = "contr.treatment",
    'ordered' = "contr.treatment"
  )
  if (contrasts == 'indicator') {
    options(contrasts = constrasts.indicator)
  }
  
  # for taking weights into account
  data <- cbind(data, data.frame('....weights125678' = as.vector(weights(data.Dataset))))
  
  if(missing(imbric)) n.models <- 1
  else n.models <- length(imbric) + 1

  # formula must be complete
  if (!(inherits(formula, 'formula') && length(formula) == 3)) stop("Please check formula: either not a formula or incomplete")
  # we build formulas for all models
  formulas <- list(formula)
  if(!missing(imbric)) {
    for (i in 1:(n.models-1)) {
      tmp <- imbric[[i]]
      formulas[[i+1]] <- as.formula(
        paste(c(deparse(formulas[[i]][[2]]), '~', deparse(formulas[[i]][[3]]), '+', deparse(tmp[[length(tmp)]])), collapse='')
      )
    }
  }
  #print(formulas)

  full.formula <- formulas[[n.models]] 
  t <- terms(full.formula)
  variables <- as.character(setdiff(strsplit(as.character(attr(t, "variables")), split="\\("), "list"))
  response <- variables[[attr(t, "response")]]
  lev <- levels(data[[response]])
  if(!(target %in% lev)) stop(paste(target, "isn't a class of the response variable"))
  
  olev <- setdiff(lev, target)
  
  target.Variable <- data.Dataset[[response]]
  target.recoded <- recode(
    target.Variable,
    '1' = target,
    '0' = olev
  )
  model.type <- 'binary'
      
  target.bin <- as.numeric(as.character(v(target.recoded)))
  data[[response]] <- target.bin
  
  # we keep only ful complete cases
#   data <- data[complete.cases(data),] #FIXME
  # and check for representativness changes
  only.complete(variables, data.Dataset)
#   data[[1]] <- factor(data[[1]])
#   return(data)

  out <- list()
  for (i in 1:n.models) {
    formula <- formulas[[i]] 
    t <- terms(formula)
    model.variables <- as.character(setdiff(strsplit(as.character(attr(t, "variables")), split="\\("), "list"))


#     print(formula)
#     print(data)
    rl <- glm(
      formula = formula,
      data = data,
      family=binomial,
      weights = ....weights125678
    )
#     rl <- do.call('glm', list(
#       'formula' = formula,
#       'data' = data,
#       'family' = 'binomial',
#       'weights' = 'weights1'
#       )
#     )
#     rl.call <- call('glm',list(
#         'formula' = formula,
#         'data' = data,
#         'family' = 'binomial',
#         'weights' = 'weights1'
#       )
#     )
#     print(rl.call)
#     rl <- eval(rl.call)
    #print(rl)
    rl.s <- summary(rl)
  
    deviance <- rl$deviance
    null.deviance <- rl$null.deviance
    n <- rl$df.null + 1
  
    b <- exp(coef(rl))
    p <- (rl$aic - deviance)/2
    bic <- deviance + p * log(n)
    
    r2.cs <- 1 - exp((deviance - null.deviance)/n)
    r2.nag <- r2.cs/(1-exp(-null.deviance/n))

    out[[paste("bloc", i, sep='')]] <- list(
      vari = model.variables,
      params = length(coef(rl)),
      df = length(coef(rl)) - 1,
      rl = rl,
      rl.s = rl.s,
      bic = bic,
      r2.cs = r2.cs,
      r2.nag = r2.nag
    )
  }

  
  options(contrasts = contrasts.user)
  
  out[["variables"]] <- variables
  out[["response"]] <- response
  out[["target"]] <- target
  out[["data"]] <- data.Dataset
  out[["model.type"]] <- model.type
  out[["nmodels"]] <- n.models
  out[["contrasts"]] <- contrasts

  out <- new('RegLog', out)
  return(out)
}

summary2 <- function(
  x,
  odd.ratio = T,
  global.measures = c('deviance', 'deviance.null', 'chi2.model', 'chi2.bloc', 'r2.cs', 'r2.nag', 'df', 'parameters', 'aic', 'bic')
){

  sx <- summary(x[[x$nmodels]]$rl)$coefficients
  coef.names <- rownames(sx)
  coef.est <- sx[,'Estimate']
  coef.pval <- sx[,'Pr(>|z|)']
  # we put the intercept at the end
  coef.names <- c(coef.names[2:length(coef.names)],coef.names[1])
  coef.est <- c(coef.est[2:length(coef.est)],coef.est[1])
  coef.pval <- c(coef.pval[2:length(coef.pval)],coef.pval[1])

  ncol <- 2 * x$nmodels
  nrow <- length(coef.names)
  
  coef.df <- data.frame(matrix(rep(NA, ncol*nrow), ncol=ncol), stringsAsFactors = F)
  rownames(coef.df) <- coef.names
#   print(coef.df)
  
#   colnames <- addEvenNames(paste('Model', 1:x$nmodels))
#   colnames <- paste('Model', 1:x$nmodels)
#   colnames2 <- addSignif(col
#   colnames <- c(colnames, colnames)
  colnames <- addSignif(paste('Model', 1:x$nmodels))
  names(coef.df) <- colnames
#   print(coef.df)

  for(i in 1:x$nmodels) {
    sx <- summary(x[[i]]$rl)$coefficients
    coef.est <- sx[,'Estimate']
    coef.pval <- sx[,'Pr(>|z|)']
    for (k in names(coef.est)){
      coef.df[k,2*(i-1)+1] <- coef.est[k]
      coef.df[k,2*i] <- coef.pval[k]
    }
    if(odd.ratio) {
      coef.df[,2*(i-1)+1] <- exp(coef.df[,2*(i-1)+1])
    }
  }
#   stdf <- statdf(coef.df, pvalues = 'even', na = '',formatc = list('digits' = 4, 'format' = 'f'))
#   print(stdf)
#   print(summary(stdf), merge='left')
  #print(coef.df)


  # quality assessment
  gm <- intersect(
    global.measures,
    c('deviance', 'deviance.null', 'chi2.model', 'chi2.bloc', 'r2.cs', 'r2.nag', 'df', 'parameters', 'aic', 'bic') # available measures
  )
  ngm <- length(gm)
  gm.df <- data.frame(matrix(rep(NA, ncol*ngm), ncol=ncol), stringsAsFactors = F)
  row.names(gm.df) <- gm
  names(gm.df) <- colnames
  rcount <- 0
  if (is.element("deviance", gm)) {
    rcount <- rcount + 1
    for(i in 1:x$nmodels) {
      gm.df[rcount,2*i-1] <- x[[i]]$rl$deviance
    }
  }
  if (is.element("deviance.null", gm)) {
    rcount <- rcount + 1
    for(i in 1:x$nmodels) {
      gm.df[rcount,2*i-1] <- x[[i]]$rl$null.deviance
    }
  }
  if (is.element("chi2.model", gm)) {
    rcount <- rcount + 1
    deviance.null <- x[[1]]$rl$null.deviance
    for(i in 1:x$nmodels) {
      chi <- deviance.null - x[[i]]$rl$deviance # deviance reduction => chi2 gain
      chi.pval <- pchisq(chi, x[[i]]$df, lower.tail = F)
      gm.df[rcount,2*i-1] <- chi
      gm.df[rcount,2*i] <- chi.pval
    }
  }
  if (is.element("chi2.bloc", gm)) {
    rcount <- rcount + 1
    deviance.previous <- x[[1]]$rl$null.deviance
    df.previous <-  x[[1]]$rl$df.null
    for(i in 1:x$nmodels) {
      chi <- deviance.previous - x[[i]]$rl$deviance
      chi.df <- df.previous - x[[i]]$rl$df.residual
      chi.pval <- pchisq(chi, chi.df, lower.tail = F)
      gm.df[rcount,2*i-1] <- chi
      gm.df[rcount,2*i] <- chi.pval
      deviance.previous <- x[[i]]$rl$deviance
      df.previous <- x[[i]]$rl$df.residual
    }
  }
  if (is.element("r2.cs", gm)) {
    rcount <- rcount + 1
    for(i in 1:x$nmodels) {
      gm.df[rcount,2*i-1] <- x[[i]]$r2.cs
    }
  }
  if (is.element("r2.nag", gm)) {
    rcount <- rcount + 1
    for(i in 1:x$nmodels) {
      gm.df[rcount,2*i-1] <- x[[i]]$r2.nag
    }
  }
  if (is.element("df", gm)) {
    rcount <- rcount + 1
    for(i in 1:x$nmodels) {
      gm.df[rcount,2*i-1] <- x[[i]]$df
    }
  }
  if (is.element("parameters", gm)) {
    rcount <- rcount + 1
    for(i in 1:x$nmodels) {
      gm.df[rcount,2*i-1] <- x[[i]]$params
    }
  }
  if (is.element("aic", gm)) {
    rcount <- rcount + 1
    for(i in 1:x$nmodels) {
      gm.df[rcount,2*i-1] <- x[[i]]$rl$aic
    }
  }
  if (is.element("bic", gm)) {
    rcount <- rcount + 1
    for(i in 1:x$nmodels) {
      gm.df[rcount,2*i-1] <- x[[i]]$bic
    }
  }
  #print(gm.df)
  coef.name <- 'Estimated coefficients'
  if(odd.ratio) {
    coef.name <- paste(coef.name, '(odd ratios)')
  }
  coef.statdf <- statdf(
    coef.df,
    pvalues = 'even',
    name = coef.name,
    na = '',
    formatc = list('digits' = 4, 'format' = 'f')
  )
  gm.statdf <- statdf(
    gm.df,
    pvalues = 'even',
    name = 'Quality measures',
    na = '',
    formatc = list('digits' = 2, 'format' = 'f')
  )
  out <- list(
    coeffs = coef.statdf,
    gm = gm.statdf
  )
  class(out) <- 'summary.reglog'
  return(out)
  #print("target versus ...")
  #print(x$bic)
}
 


setMethod(
  f = 'print',
  signature = c('RegLog'),
  definition = function(x, ...) {

    s <- summary2(x)
    
    message('Table 1:')
    print(summary(s[[1]], merge='left'))
    
    message('')
    message('Table 2: ')
    print(summary(s[[2]], merge='left'))
  }
)

setMethod(
  f = 'show',
  signature = c('RegLog'),
  definition = function(object) {
    print(object)
  }
)



# summaryToPDF.reglog <- function(x){
setMethod(
  f = 'summaryToPDF',
  signature = c('RegLog'),
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
    ...
  ) {
    if(!is.installed.pkg('xtable')) {
      exit.by.uninstalled.pkg('xtable')
    } else {
      
      if(!missing(pdfSavingName)) {
        outName <- pdfSavingName
      } else {
        outName <- 'Logistic Regression'
      }
      
      outName.pdf <- make.names(outName) # no spaces for Unix/Texlive compilation ?
      
      if(missing(pdfSavingName)) {    
        pdfSavingName <- paste("Summary-", outName.pdf, sep = "") # no spaces for Unix/Texlive compilation ?
      }
      
      latexFile <- paste(pdfSavingName, ".tex", sep="")
      
      is.writable(pdfSavingName, path = getwd())
      
      outFileCon <- file(latexFile, "w", encoding="UTF-8")
      
      latex.head(title = paste("Summary of logistic regression", '', ""), latexPackages, outFileCon)
    
      args <- list(...)
      if(is.logical(args$odd.ratio)) {
        oddr <- args$odd.ratio
      } else {
        oddr <- T
      }
      s <- summary2(object, odd.ratio = oddr)
      require(xtable)
      
      
      cat("\\section*{Settings} \n", file = outFileCon, append = T)
      
      cat("\\textbf{Model fitted:}",
          totex(paste(object[["model.type"]], 'logistic regression model')),
          " \n",
          file = outFileCon, append = T
      )
      cat("\\newline \n", file = outFileCon, append = T)
      cat("\\textbf{Contrasts used for nominal and ordinal variables:}",
          totex(paste(object[["contrasts"]])),
          " \n",
          file = outFileCon, append = T
      )
      cat("\\newline \n", file = outFileCon, append = T)
      cat("\\textbf{Target variable}",
          totex(object[["response"]]),
          " \n",
          file = outFileCon, append = T
      )
      
      if (length(description(object[["data"]][[object[["response"]]]])) > 0) {
        cat(paste(":",
                  totex(description(object[["data"]][[object[["response"]]]]))), " \n", file = outFileCon, append = T)
      }
      cat("\\newline \n", file = outFileCon, append = T)
      cat("\\textbf{Logit estimated for:}",
          totex(paste(object[["target"]])),
          file = outFileCon, append = T
      )
      
      cat("\\newline \n", file = outFileCon, append = T)
      cat("\\textbf{Predictor(s)}", " \n", file = outFileCon, append = T)
      cat("\\begin{itemize*}", " \n", file = outFileCon, append = T)
      predictors <- object[['variables']]
      predictors <- predictors[-which(predictors == object[['response']])]
      stopifnot(length(predictors) > 0)
      for (i in predictors) {
        cat("\\item ", totex(i), file = outFileCon, append = T)
        if(length(description(object[["data"]][[i]])) > 0) {
          cat(paste(":",  totex(description(object[["data"]][[i]]))), file = outFileCon, append = T)
        }
        cat(" \n", file = outFileCon, append = T)
      }
      cat("\\end{itemize*}", " \n", file = outFileCon, append = T)
      
      
      cat("\\textbf{Weighting} ", file = outFileCon, append = T)
      object.data <- object[["data"]]
      wvarname <- weighting(object.data)
      if(length(wvarname) > 0) {
        cat(totex(wvarname), file = outFileCon, append = T)
        if (length(description(object.data[[wvarname]])) > 0) {
          cat(paste(":", totex(description(object.data[[wvarname]]))), " \n", file = outFileCon, append = T)
        }
      } else {
        cat("No weighting variable defined, equi-weighting is used", " \n", file = outFileCon, append = T)
      }
      
      
      cat("\\section*{Modeling} \n", file = outFileCon, append = T)
      
      
      
  #     print(
  #       xtable(
  #         giveStars.df.even(s[[1]]),
  #         digits = 3,
  #         display = c('d','f','e','f','f','f','f')
  #         ),
  #       file=latexFile , append=T
  #     )
      s1 <- summary(s[[1]], merge = 'left')
      object.xtable <- xtable(
        sdf(s1),
        align = c("l", rep('c', ncol(s1))),
        caption=paste(name(s1), ', ', thresholds(s1)),
      )
      cat("\\begin{center} \n", file = outFileCon, append = T)
      print(object.xtable, file=outFileCon , append=T,
            table.placement = "htb",
            floating=T
      )
#       cat("\\newline ", " \n", file = outFileCon, append = T)
#       cat(thresholds(s1), " \n", file = outFileCon, append = T)
      cat("\\end{center} \n", file = outFileCon, append = T)
      
      
      
      
      s2 <- summary(s[[2]], merge = 'left')
      object.xtable <- xtable(
        sdf(s2),
        align = c("l", rep('c', ncol(s2))),
        caption=paste(name(s2), ', ', thresholds(s2)),
      )
      cat("\\begin{center} \n", file = outFileCon, append = T)
      print(object.xtable, file=outFileCon , append=T,
            table.placement = "htb",
            floating=T
      )
      cat("\\end{center} \n", file = outFileCon, append = T)
  #      print(
  #       xtable(
  #         giveStars.df.even(s[[2]]),
  #         digits = 3,
  #         display = c('d','f','e','f','f','f','f')
  #         ),
  #       file=latexFile , append=T
  #     )
      
      close.and.clean(outFileCon, pdfSavingName, keepTex, openPDF)
    }
  }
)

#11155
#l <- mysubset.recoded[c("santé", "sexe","age",'education')]
#l[which(complete.cases(l)),]
#7114

#coefficients 
#residuals
#fitted.values
#effects
#R
#rank
#qr
#family
#deviance
#aic
#null.deviance
#iter
#weights
#prior.weights
#df.residual
#df.null