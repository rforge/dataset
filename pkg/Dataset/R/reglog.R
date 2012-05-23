# methode S4
# formula est de classe formula
# imbric est une liste, chaque élément est une list de termes
# exemple : formula = santébin ~ sexe + age, imbric = list(~ education + occupation, ~ partenaire)
# 
reglog <- function(
  formula,
  target,
  imbric,
  data,
  subset,
  weights,
  na.action
) {

  data.Dataset <- data
  data <- v(data)
  
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

  big.f <- formulas[[n.models]] 
  t <- terms(big.f)
  variables <- as.character(setdiff(strsplit(as.character(attr(t, "variables")), split="\\("), "list"))
  response <- variables[[attr(t, "response")]]
  lev <- levels(data[[response]])
  if(!(target %in% lev)) stop(paste(target, "isn't a class of the response variable"))
  
  olev <- setdiff(lev, target)
  
  target.Variable <- data.Dataset[[response]]
  target.recoded <- recode(
    target.Variable,
    list(
      '1' = target,
      '0' = olev
    )
  )
      
  #data <- v(data.Dataset)
  #data <- data[,variables]
  data[[response]] <- v(target.recoded)
  data <- data[complete.cases(data),]
  data[[1]] <- factor(data[[1]])
  
  #recodage1 <- paste("c('", target, "')=c('1')", sep='')
  #recodage2 <- paste("c('",paste(olev, collapse="','"),"')=c('0')", sep="")
    
  #data[[response]] <- recode(
  #  data[[response]],
  #  paste(recodage1,recodage2,sep=";")
  #)

  #mf <- model.frame(big.f, data = data)
  #return(mf)
 
  # on récupère le noms de toutes les variables utilisées, et on construit le jeu de données n'ayant aucun missing sur ces variables là
  # 

  #mapp <- mapply(inherits, formulas, "formula")
  #if(!all(mapp)) stop("reglog: all objects in argument 'formulas' must inherits of class 'formula'")

  # we compute all regression

  out <- list()
  for (i in 1:n.models) {
    formula <- formulas[[i]] 
    t <- terms(formula)
    model.variables <- as.character(setdiff(strsplit(as.character(attr(t, "variables")), split="\\("), "list"))

    #print(head(data))
    rl <- glm(
      formula = formula,
      data = data,
      family=binomial
    )
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

  out[["variables"]] <- variables
  out[["response"]] <- response
  out[["nmodels"]] <- n.models

  class(out) <- "reglog"
  return(out)
}

summary.reglog <- function(
  x,
  global.measures = c('deviance', 'deviance.null', 'chi2.model', 'chi2.bloc', 'r2.cs', 'r2.nag', 'df', 'parameters', 'aic', 'bic')
){

  sx <- summary(x[[x$nmodels]]$rl)$coefficients
  coef.names <- rownames(sx)
  coef.est <- sx[,'Estimate']
  coef.pval <- sx[,'Pr(>|z|)']
  coef.names <- c(coef.names[2:length(coef.names)],coef.names[1])
  coef.est <- c(coef.est[2:length(coef.est)],coef.est[1])
  coef.pval <- c(coef.pval[2:length(coef.pval)],coef.pval[1])

  ncol <- 2 * x$nmodels
  nrow <- length(coef.names)
  
  coef.df <- data.frame(matrix(rep(NA, ncol*nrow), ncol=ncol), stringsAsFactors = F)
  rownames(coef.df) <- coef.names
  
  colnames <- addEvenNames(paste('Model', 1:x$nmodels))
  names(coef.df) <- colnames

  for(i in 1:x$nmodels) {
    sx <- summary(x[[i]]$rl)$coefficients
    coef.est <- sx[,'Estimate']
    coef.pval <- sx[,'Pr(>|z|)']
    for (k in names(coef.est)){
      coef.df[k,2*(i-1)+1] <- exp(coef.est[k])
      coef.df[k,2*i] <- coef.pval[k]
    }
  }

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
  if (is.element("chi2.bloc", gm)) {
    rcount <- rcount + 1
    deviance.previous <- x[[1]]$rl$null.deviance
    df.previous <- 0
    for(i in 1:x$nmodels) {
      chi <- deviance.previous - x[[i]]$rl$deviance
      chi.df <- x[[i]]$df - df.previous
      #chi.print <- paste(chi, ' (dl=', chi.df, ')', sep='')
      chi.pval <- pchisq(chi, chi.df, lower.tail = F)
      #gm.df[rcount,2*i-1] <- chi.print
      gm.df[rcount,2*i-1] <-chi.df
      gm.df[rcount,2*i] <- chi.pval
      deviance.previous <- x[[i]]$rl$deviance
      df.previous <- x[[i]]$df
    }
  }
  if (is.element("chi2.model", gm)) {
    rcount <- rcount + 1
    deviance.null <- x[[1]]$rl$null.deviance
    for(i in 1:x$nmodels) {
      chi <- deviance.null - x[[i]]$rl$deviance
      #chi.print <- paste(chi, ' (dl=', x[[i]]$df, ')', sep='')
      chi.pval <- pchisq(chi, x[[i]]$df, lower.tail = F)
      #gm.df[rcount,2*i-1] <- chi.print
      gm.df[rcount,2*i-1] <- chi
      gm.df[rcount,2*i] <- chi.pval
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
  out <- list(
    coeffs = coef.df,
    gm <- gm.df
  )
  class(out) <- 'summary.reglog'
  return(out)
  #print("target versus ...")
  #print(x$bic)
}
  
print.reglog <- function(x) {
  print("reglog object")
}

print.summary.reglog <- function(x) {
  message("Coefficients")
  message('')
  print(giveStars.df.even(x[[1]]))
  message('')
  message('')
  print(giveStars.df.even(x[[2]]))
}
  
summaryToPDF.reglog <- function(x){
  pdfSavingName <- "reglog"
  latexFile <- paste(pdfSavingName, ".tex", sep="")

  cat("\\documentclass[landscape]{article} \n" , file = latexFile, append = F)
  cat("\\usepackage[top=2.5cm, bottom=2.5cm, left=1.5cm, right=1.5cm]{geometry} \n", file = latexFile, append = T)
  cat("\\usepackage{longtable} \n", file = latexFile, append = T)
  cat("\\usepackage{graphicx} \n", file = latexFile, append = T)
  cat("\\author{Generated by the R Dataset package} \n", file = latexFile, append = T)
  cat("\\title{Summary of logistic regression} \n", file = latexFile, append = T)
  cat("\\begin{document} \n", file = latexFile, append = T)
  cat("\\maketitle \n", file = latexFile, append = T)

  #cat("AIC: ", x$rl$aic, ", BIC: ", x$bic, " \n", file = latexFile, append = T)
  
  s <- summary(x)
  
  require(xtable)
  print(
    xtable(
      giveStars.df.even(s[[1]]),
      digits = 3,
      display = c('d','f','e','f','f','f','f')
      ),
    file=latexFile , append=T
  )
   print(
    xtable(
      giveStars.df.even(s[[2]]),
      digits = 3,
      display = c('d','f','e','f','f','f','f')
      ),
    file=latexFile , append=T
  )
  
  cat("\\end{document} \n", file = latexFile, append = T)
  tools::texi2dvi(latexFile,pdf=T)
  # clean directory
  keepTex <- T
  if (keepTex) {
  extensionsToRemove <- ".(log|aux)"
  } else {
  extensionsToRemove <- ".(log|aux|tex)"
  }

  tempTex <- list.files(
	##paste(datadir, wavesFolder, "-SPSS", "/", i, sep = ""),
	getwd(),
	pattern = paste("^", pdfSavingName, extensionsToRemove, sep = "")
  )
  # tempTex <- tempTex[-grep(".pdf$", tempTex)]
  # keepLatex = TRUE (false by default)
  # tempTex <- tempTex[-grep(".tex$", tempTex)]

  unlink(tempTex)
}

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