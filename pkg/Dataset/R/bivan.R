# debug/test examples
# data(mtcars)
# mtc.dataset <- dataset(mtcars, ordinal = 'am')
# mtc.dataset <- dataset(mtcars)
# mtc.dataset$am <- bvar(v(mtc.dataset[['am']]), values=c("auto" = 0, "manual" = 1))
# mtc.dataset$vs <- bvar(v(mtc.dataset[['vs']]), values=c("V" = 0, "S" = 1))
# mtc.dataset$cyl <- ovar(v(mtc.dataset[['cyl']]), values=c("4" = 4, "6" = 6, "8"=8), description = 'number of cylinders')
# mtc.dataset$gear <- ovar(v(mtc.dataset[['gear']]), values=c("3" = 3, "4" = 4, "5"=5), description = 'gear type')
# b1 <- bivan(cyl ~ am + gear, mtc.dataset)
# summaryToPDF(b1, pdf = 'bivan1')
#target(b1)
#description(target(b1))

bivan.tests <- function(){
 out <- data.frame(
   'chi2' = c("Chi^2", 'global', 'symmetric', 'nominal', 'nominal'),
   'cramer.v' = c("Cramer's V", 'global', 'symmetric', 'nominal', 'nominal'),
   'gk.tau' = c("GK's Tau", 'global', 'symmetric', 'nominal', 'nominal'),
   'somer.d' = c("Somer's D", 'global', 'symmetric', 'nominal', 'nominal'),
   'stdres' = c("Std. Res.", 'global', 'symmetric', 'nominal', 'nominal')
 )
 row.names(out) <- c('name', 'type', 'symmetry', 'dependant', 'predictor')
 return(out)
}
# getting which test the user want to perform
#allTests <- c(
  # == dependant feature: nominal ==
  # === predictor feature: nominal ===
  # nominal crit.global
  ## symmetric
#  "Chi^2", "Phi", "Cramer's V",
  ### Rand, kappa
  ## directional, error reduction in prediction type
#  "GK's Lambda",
#  "GK's Tau", "GK's Tau sqrt",
  ### Theil u index (and sqrt)
#  "Theil's u", "Theil's u sqrt",
  ## ordinal, based on concordance/discordance, symmetric
#  "Kendall's A Tau", "Kendall's B Tau",  "Stuart's C Tau", "GK's gamma",
  ## ordinal, based on conc/disc, directional
#  "Somer's D"
  ## ordinal, based on the rank
  ### rho de Spearman
  # == dependant feature: numeric ==
  # === predictor feature: nominal ===
  ### eta coefficient
  # === predictor feature: numeric ===
  ### Pearson linear correlation

setClass(
  'Bivan',
  representation(
    target = 'Dataset',
    predictors = 'Dataset',
    stdres = 'Statdf',
    global = 'Statdf'
  ),
  validity = function(object) {
    flag = TRUE
    
    # only one target
    if (flag && ncol(target(object)) != 1) {
      message("bivan function currently accepts only one target")
      message(names(target(object)))
      flag <- FALSE
    }
    
    return(flag)
  }
)

setMethod('target', 'Bivan', 
          definition = function (object) { 
            return(slot(object, 'target'))
          }
)
setReplaceMethod(
  f = 'target' ,
  signature = 'Bivan' ,
  definition = function(object, value){
    object@target <- value
    validObject(object)
    return(object)
  }
)
setMethod('predictors', 'Bivan', 
          definition = function (object) { 
            return(slot(object, 'predictors'))
          }
)
setReplaceMethod(
  f = 'predictors' ,
  signature = 'Bivan' ,
  definition = function(object, value){
    object@predictors <- value
    validObject(object)
    return(object)
  }
)
setMethod('stdres', 'Bivan', 
          definition = function (object) { 
            return(slot(object, 'stdres'))
          }
)
setReplaceMethod(
  f = 'stdres' ,
  signature = 'Bivan' ,
  definition = function(object, value){
    object@stdres <- value
    validObject(object)
    return(object)
  }
)
setMethod('global', 'Bivan', 
          definition = function (object) { 
            return(slot(object, 'global'))
          }
)
setReplaceMethod(
  f = 'global' ,
  signature = 'Bivan' ,
  definition = function(object, value){
    object@global <- value
    validObject(object)
    return(object)
  }
)

setMethod(
  f = 'print',
  signature = c('Bivan'),
  definition = function(x, ...) {
    message("Target")
    message(names(target(x))[1], appendLF = F)
    if (length(description(target(x)[[1]])) > 0) {
      message(paste(":", description(target(x)[[1]])), appendLF = F)
    }
    message("")
    #message(paste(yname, " is ", str.typevar(data[[yname]]), ".", sep = ""))
    message("")
    message("Predictor(s)")
    #message(paste(names(predictors(x)), collapse = ', '))
    preds <- names(predictors(x))
    for (i in preds) {
      message(i, appendLF = F)
      if (length(description(predictors(x)[[i]])) > 0) {
        message(paste(":", description(predictors(x)[[i]])), appendLF = F)
      }
      message("")
    }
    message("")
    #for (i in xnames) {
    #  message(paste(i, " is ", str.typevar(data[[i]]), ".", sep = ""))
    #}
    
    if(ncol(stdres(x)) > 0) {
      print(summary(stdres(x), merge = 'left'))
      message("");message("")
    }
    
    if(ncol(global(x)) > 0) {
      print(summary(global(x), merge = 'left'))
      message("");message("")
    }
  }
)

setMethod(
  f = 'show',
  signature = c('Bivan'),
  definition = function(object) {
    print(object)
  }
)

# summaryToPDF(b1)
setMethod(
  f = 'summaryToPDF',
  signature = c('Bivan'),
  definition = function(object, pdfSavingName, graphics = FALSE, description.chlength = 120, values.chlength = 6, dateformat, latexPackages = NULL, keepTex = FALSE, openPDF) {
    
    require(xtable)
    
    outName <- 'Bivariate analysis'
    
    outName.pdf <- make.names(outName) # no spaces for Unix/Texlive compilation ?
    
    if(missing(pdfSavingName)) {  	
      pdfSavingName <- paste("Summary-", outName.pdf, sep = "") # no spaces for Unix/Texlive compilation ?
    }
    
    latexFile <- paste(pdfSavingName, ".tex", sep="")
    
    outFileCon <- file(latexFile, "w", encoding="UTF-8")
    
    latex.head(title = paste("Summary of the", totex(outName)), latexPackages, outFileCon)
    
    cat("\\section*{Variables} \n", file = outFileCon, append = T)
    
    cat("\\textbf{Target}", names(target(object))[1], file = outFileCon, append = T)
    if (length(description(target(object)[[1]])) > 0) {
      cat(paste(":", description(target(object)[[1]])), " \n", file = outFileCon, append = T)
    }
    
    cat("\\newline \n", file = outFileCon, append = T)
    cat("\\textbf{Predictor(s)}", " \n", file = outFileCon, append = T)
    cat("\\begin{itemize*}", " \n", file = outFileCon, append = T)
    preds <- names(predictors(object))
    for (i in preds) {
      cat("\\item ", i, file = outFileCon, append = T)
      if(length(description(predictors(object)[[i]])) > 0) {
        cat(paste(":", description(predictors(object)[[i]])), file = outFileCon, append = T)
      }
      cat(" \n", file = outFileCon, append = T)
    }
    cat("\\end{itemize*}", " \n", file = outFileCon, append = T)
    
    # stdres --------------------------------------
    if(ncol(stdres(object)) > 0) {
      s <- summary(stdres(object), merge = 'left')
      object.xtable <- xtable(
        sdf(s),
        align = c("l", rep('c', ncol(sdf(s)))),
        caption=thresholds(s),
      )
      cat("\\section*{", name(s), "} \n", file = outFileCon, append = T)
      cat("\\begin{center} \n", file = outFileCon, append = T)
      print(object.xtable, file=outFileCon , append=T,
            table.placement = "htb",
            floating=F
      )
      cat("\\newline ", " \n", file = outFileCon, append = T)
      cat(thresholds(s), " \n", file = outFileCon, append = T)
      cat("\\end{center} \n", file = outFileCon, append = T)
    }
    
    # global --------------------------------------
    if(ncol(global(object)) > 0) {
      s <- summary(global(object), merge = 'left')
      object.xtable <- xtable(
        sdf(s),
        align = c("l", rep('c', ncol(sdf(s)))),
        caption=thresholds(s),
      )
      cat("\\section*{", name(s), "} \n", file = outFileCon, append = T)
      cat("\\begin{center} \n", file = outFileCon, append = T)
      print(object.xtable, file=outFileCon , append=T,
            table.placement = "htb",
            floating=F
      )
      cat("\\newline ", " \n", file = outFileCon, append = T)
      cat(thresholds(s), " \n", file = outFileCon, append = T)
      cat("\\end{center} \n", file = outFileCon, append = T)
    }
    
    close.and.clean(outFileCon, pdfSavingName, keepTex, openPDF)
    
  }
)




calc.pval <- function(x) {
  return(pnorm(-abs(x))*2)
}

setMethod(
  f = 'bivan',
  signature = c(
    'formula', 
    'Dataset'
  ),
  definition = function (
    formula,
    data,
    chi2,
    cramer.v,
    gk.tau,
    somer.d,
    stdres,
    quiet
  ) {
    
    #FIXME: type date non géré
    #data.Dataset <- data
    data.df <- v(data)
    
    if (any(attr(terms(formula, data = data.df), "order") > 1)) 
      stop("interactions are not allowed")
    
    if (length(formula) < 3L) {
      stop("You have to specify a response variable")
    }
    
    t <- terms(formula)
    variables <- as.character(setdiff(strsplit(as.character(attr(t, "variables")), split="\\("), "list"))
    yname <- variables[attr(t, "response")]
    xnames <- variables[-attr(t, "response")]
    nbxnames <- length(xnames)
    
    y <- data.df[[yname]]
    ncat <- nlevels(y)
    x <- data.df[xnames]
    
    alltests <- bivan.tests()
    
    # -------------------------
    # stdres
    out.stdres <- statdf()
    if(stdres) {
      res <- NULL
      for (i in xnames) {
        #i <- 1
        tablexy <- table(x[,i], y)
        chisq <- chisq.test(tablexy, correct=FALSE)
        res.temp <- chisq$stdres
        row.names(res.temp) <- paste(paste(i, '/'), row.names(res.temp))
        if(is.null(res)) {
          res <- res.temp
        } else {
          res <- rbind(res, res.temp)
        }
      }

      out.stdres <- data.frame(matrix(rep(0, ncol(res)*2*nrow(res)), nrow = nrow(res)))
      names(out.stdres) <- addSignif(dimnames(res)[[2]])
      row.names(out.stdres) <- row.names(res)
      for (i in 1:ncol(res)){
         out.stdres[,i*2-1] <- res[,i]
         out.stdres[,i*2] <- calc.pval(res[,i])
      }
      out.stdres <- statdf(
        out.stdres,
        pvalues = 'even',
        name = paste(alltests['name', 'stdres'], 'table')
      )
    }
    
    # -------------------------
    # global
    userTests <- character(0)
    # SPOT1
    if (chi2) userTests <- c(userTests, 'chi2')
    if (cramer.v) userTests <- c(userTests, 'cramer.v')
    if (gk.tau) userTests <- c(userTests, 'gk.tau')
    if (somer.d) userTests <- c(userTests, 'somer.d')
    
    userTestsSignif <- addSignif(userTests)
    
    nbTests <- length(userTests)
    
    # creating an blank dataframe for storing results (with p-values)
    mes <- as.data.frame(matrix(rep(0, nbxnames * nbTests * 2), nrow = nbxnames))
    row.names(mes) <- xnames
    names(mes) <- userTestsSignif
    
    for (i in xnames) {
      tablexy <- table(x[,i], y)
      chisq <- chisq.test(tablexy, correct=FALSE)
      
      j <- 0
      
      # KEEP SAME ORDER THAN IN 'SPOT1'
      if (is.element("chi2", userTests)) {
        j <- j+1; mes[i, j] <- chisq$statistic
        j <- j+1; mes[i, j] <- chisq$p.value
      }
      
      if (is.element("Phi", userTests)) {
        j <- j+1; mes[i, j] <- sqrt(chisq$statistic / sum(tablexy));
        j <- j+1; mes[i, j] <- chisq$p.value
      }
      
      if (is.element("cramer.v", userTests)) {
        j <- j+1; mes[i, j] <- sqrt(chisq$statistic / (sum(tablexy) * min(dim(tablexy) - 1 )))
        j <- j+1; mes[i, j] <- chisq$p.value
      }
      
      if (is.element("GK's Lambda", userTests)) {
        temp <- calc.GK.lambda(tablexy)
        j <- j+1; mes[i, j] <- temp$statistic;
        # j <- j+1; mes[i, j] <- GK.tau(tablexy)$tau.CR
        j <- j+1; mes[i, j] <- temp$pvalue;
        # j <- j+1; mes[i, j] <- GK.tau(tablexy)$p.tau.CR
      }
      
      if (is.element("gk.tau", userTests)) {
        temp <- GK.tau(tablexy)
        j <- j+1; mes[i, j] <- temp$tau.CR;
        # j <- j+1; mes[i, j] <- GK.tau(tablexy)$tau.CR
        j <- j+1; mes[i, j] <- temp$p.tau.CR;
        # j <- j+1; mes[i, j] <- GK.tau(tablexy)$p.tau.CR
      }
      if (is.element("GK's Tau sqrt", userTests)) {
        temp <- GK.tau(tablexy)
        j <- j+1; mes[i, j] <- sqrt(temp$tau.CR);
        # j <- j+1; mes[i, j] <- GK.tau(tablexy)$tau.CR
        j <- j+1; mes[i, j] <- temp$p.tau.CR;
        # j <- j+1; mes[i, j] <- GK.tau(tablexy)$p.tau.CR
      }
      
      if (is.element("Theil's u", userTests)) {
        temp <- calc.Theil.u(tablexy)
        j <- j+1; mes[i, j] <- temp$statistic;
        # j <- j+1; mes[i, j] <- GK.tau(tablexy)$tau.CR
        j <- j+1; mes[i, j] <- temp$pvalue;
        # j <- j+1; mes[i, j] <- GK.tau(tablexy)$p.tau.CR
      }
      if (is.element("Theil's u sqrt", userTests)) {
        temp <- calc.Theil.u(tablexy)
        j <- j+1; mes[i, j] <- sqrt(temp$statistic);
        # j <- j+1; mes[i, j] <- GK.tau(tablexy)$tau.CR
        j <- j+1; mes[i, j] <- temp$pvalue;
        # j <- j+1; mes[i, j] <- GK.tau(tablexy)$p.tau.CR
      }
      
      if (is.element("Kendall's A Tau", userTests)) {
        temp <- calc.Kendall.tauA(tablexy)
        j <- j+1; mes[i, j] <- temp$statistic;
        j <- j+1; mes[i, j] <- temp$pvalue;
      }
      
      if (is.element("Stuart's C Tau", userTests)) {
        temp <- calc.Stuart.tauC(tablexy)
        j <- j+1; mes[i, j] <- temp$statistic;
        j <- j+1; mes[i, j] <- temp$pvalue;
      }
      
      if (is.element("GK's gamma", userTests)) {
        temp <- calc.GK.gamma(tablexy)
        j <- j+1; mes[i, j] <- temp$statistic;
        j <- j+1; mes[i, j] <- temp$pvalue;
      }
      
      if (is.element("somer.d", userTests)) {
        temp <- calc.Sd(tablexy)
        j <- j+1; mes[i, j] <- temp$Sd.CR;
        j <- j+1; mes[i, j] <- temp$pvalue;
        # j <- j+1; mes[i, j] <- calc.Sd(tablexy)$Sd.CR
      }
    }

    out.global <- statdf(
      mes,
      pvalues = 'even',
      name = 'Global association measures'
    )
    # -------------------------
    # out
          
    out <- new(
      'Bivan',
      target = data[,yname],
      predictors = data[,xnames],
      stdres = out.stdres,
      global = out.global
    )
    
    return(out)
    
  }
)







# =========================================================================================================
# Statistics
# =========================================================================================================

nij.under.H0 <- function(x) { # x : table of contingency
  tbl.x <- margin.table(x, 1)
  tbl.y <- margin.table(x, 2)
  n <- margin.table(x)
  
  out <- x
  for (i in 1:nrow(x)){
    for (j in 1:ncol(x)){
      out[i,j] <- tbl.x[i]*tbl.y[j]/n
    }
  }
  return(out)
}

statistic.chisq.likelihood.ratio <- function(x){ # x : table of contingency
  e <- nij.under.H0(x)
  G <- 0
  for (i in 1:nrow(x)){
    for (j in 1:ncol(x)){
      G <- G + x[i,j] * log(x[i,j]/e[i,j])
    }
  }
  G <- 2 * G
  return(G)
}

statistic.chisq.pearson <- function(x){ # x : table of contingency
  e <- nij.under.H0(x)
  G <- 0
  for (i in 1:nrow(x)){
    for (j in 1:ncol(x)){
      if(e[i,j] > 0)
        G <- G + ((x[i,j] - e[i,j])^2)/e[i,j]
    }
  }
  G <- G
  return(G)
}
# =========================================================================================================
# Phi
# =========================================================================================================
calc.phi <- function(x)
{
  x <- matrix(as.numeric(x), dim(x))
  
  c <- concordant(x)
  d <- discordant(x)
  n <- sum(x)
  SumR <- rowSums(x)
  SumC <- colSums(x)

  Sd.CR <- (2 * (c - d)) / ((n ^ 2) - (sum(SumR ^ 2)))
  Sd.RC <- (2 * (c - d)) / ((n ^ 2) - (sum(SumC ^ 2)))
  Sd.S <- (2 * (c - d)) / ((n ^ 2) - (((sum(SumR ^ 2)) + (sum(SumC ^ 2))) / 2))

  Sdlist <- list(Sd.CR, Sd.RC, Sd.S)
  names(Sdlist) <- c("Sd.CR", "Sd.RC", "Sd.S")

  Sdlist
}

# =========================================================================================================
# Cramer's V
# =========================================================================================================

# http://www.r-bloggers.com/example-8-39-calculating-cramers-v/
cv.test1 = function(x,y) {
  CV = sqrt(chisq.test(x, y, correct=FALSE)$statistic /
		(length(x) * (min(length(unique(x)),length(unique(y))) - 1)))
	print.noquote("Cramér V / Phi:")
	return(as.numeric(CV))
}
# http://home.hib.no/ansatte/gbj/cramer_v.htm
cv.test2 <- function(x) {
	CV <- sqrt(chisq.test(x, correct = FALSE)$statistic / 
	(sum(x) * min(dim(x) - 1 )))
	### The result of the Pearson chi-square (without the Yates correction) is divided by the sum of table cells and...
	### ...multiplied by the smalles number of (row or column) cells minus 1.
	### The $statistic sends the correct value (the X^2 only) into the sqrt function
	print.noquote("Cramér V / Phi:")
	return(as.numeric(CV))
}
# =========================================================================================================

# =========================================================================================================
# Theil's u
# =========================================================================================================
calc.GK.lambda <- function(x)
{
  x <- t(matrix(as.numeric(x), dim(x)))
  #print(x)
  
  m <- nrow(x)
  p <- ncol(x)
  
  mtable.x <- as.vector(margin.table(x, 1))
  #print(mtable.x)
  mtable.y <- as.vector(margin.table(x, 2))
  #print(mtable.y)
  n <- margin.table(x)
  #print(n)
  
  nm <- max(mtable.x)
  numerator <- 0
  denominator <- n - nm
  
  for (j in 1:p) {
    numerator <- numerator + max(x[,j])
  }

  numerator <- numerator - nm

  statistic <- numerator/denominator
  
  #pvalue using comparing lambda/asymptotic standard error to Norm(0,1)
  sum1 <- 0
  sum2 <- 0
  Mrow <- numeric(0)
  
  for (j in 1:p) {
    sum1 <- sum1 + max(x[,j])/n
    Mrow <- c(Mrow, which.max(x[,j])) # for determining Jplus
  }
  
  M <- which.max(mtable.x) # get the row where the max is
  Jplus <- which(Mrow == M)

  for (j in Jplus) {
    sum2 <- sum2 + max(x[,j])/n
  }
  asymptotic.variance <- 
    (1 - sum1) * (nm/n + sum1 - 2*sum2) / (n * (1 - nm/n)^3)
    
  pvalue <- pnorm(statistic/sqrt(asymptotic.variance), mean = 0, sd = 1, lower.tail = F)
  
  return(list(
    statistic = statistic,
    pvalue = pvalue
  ))
}
# =========================================================================================================
# Theil's u
# =========================================================================================================
calc.Theil.u <- function(x)
{
  x <- t(matrix(as.numeric(x), dim(x)))
  #print(x)
  
  m <- nrow(x)
  p <- ncol(x)
  
  mtable.x <- as.vector(margin.table(x, 1))
  #print(mtable.x)
  mtable.y <- as.vector(margin.table(x, 2))
  #print(mtable.y)
  n <- margin.table(x)
  #print(n)
  
  numerator <- 0
  denominator <- 0
  
  for (i in 1:m) {
    for (j in 1:p) {
      numerator <- numerator + x[i,j]*log2(mtable.x[i] * mtable.y[j] / x[i,j])
    }
    denominator <- denominator + mtable.x[i] * log2(mtable.x[i])
  }
  
  numerator <- numerator - n*log2(n)
  denominator <- denominator - n*log2(n)

  statistic <- numerator/denominator
  
  #pvalue using the likelihood ratio statistic
  G <- statistic.chisq.likelihood.ratio(x)
  pvalue <- pchisq(G, df = (m-1)*(p-1), lower.tail = F)
  
  return(list(
    statistic = statistic,
    pvalue = pvalue
  ))
}

# =========================================================================================================
# Goodman & Kruskal Tau
# Antti Arppe
# https://stat.ethz.ch/pipermail/r-help/2007-August/138098.html
# =========================================================================================================
GK.tau <- function(dat) {
	N <- sum(dat);
	dat.rows <- nrow(dat);
	dat.cols <- ncol(dat);
	max.col <- sum.col <- L.col <- matrix(,dat.cols);
	max.row <- sum.row <- L.row <- matrix(,dat.rows);
	for(i in 1:dat.cols)
		{ sum.col[i] <- sum(dat[,i]); max.col[i] <- max(dat[,i]); }
	for(i in 1:dat.rows)
		{ sum.row[i] <- sum(dat[i,]); max.row[i] <- max(dat[i,]); }
	max.row.margin <- max(apply(dat,1,sum));
	max.col.margin <- max(apply(dat,2,sum));

	# Goodman-Kruskal tau
	# Tau Column|Row
	n.err.unconditional <- N^2;
	for(i in 1:dat.rows)
		n.err.unconditional <- n.err.unconditional-N*sum(dat[i,]^2/sum.row[i]);
	n.err.conditional <- N^2-sum(sum.col^2);
	tau.CR <- 1-(n.err.unconditional/n.err.conditional);
	v <- n.err.unconditional/(N^2);
	d <- n.err.conditional/(N^2);
	f <- d*(v+1)-2*v;
	var.tau.CR <- 0;
	for(i in 1:dat.rows)
		for(j in 1:dat.cols)
			var.tau.CR <- var.tau.CR + dat[i,j]*(-2*v*(sum.col[j]/N)+d*((2*dat[i,j]/sum.row[i])-sum((dat[i,]/sum.row[i])^2))-f)^2/(N^2*d^4);
	ASE.tau.CR <- sqrt(var.tau.CR);
	z.tau.CR <- tau.CR/ASE.tau.CR;
	U.tau.CR <- (N-1)*(dat.cols-1)*tau.CR; # Chi-squared approximation for H0 according to Margolin & Light 1974, see also Liebetrau 1983
	p.tau.CR <- pchisq(U.tau.CR,df=(dat.rows-1)*(dat.cols-1),lower=FALSE);
	# Tau Row|Column
	n.err.unconditional <- N^2;
	for(j in 1:dat.cols)
	 n.err.unconditional <- n.err.unconditional-N*sum(dat[,j]^2/sum.col[j]);
	n.err.conditional <- N^2-sum(sum.row^2);
	tau.RC <- 1-(n.err.unconditional/n.err.conditional);
	v <- n.err.unconditional/(N^2);
	d <- n.err.conditional/(N^2);
	f <- d*(v+1)-2*v;
	var.tau.RC <- 0;
	for(i in 1:dat.rows)
		for(j in 1:dat.cols)
			var.tau.RC <- var.tau.RC + dat[i,j]*(-2*v*(sum.row[i]/N)+d*((2*dat[i,j]/sum.col[j])-sum((dat[,j]/sum.col[j])^2))-f)^2/(N^2*d^4);
	ASE.tau.RC <- sqrt(var.tau.RC);
	z.tau.RC <- tau.CR/ASE.tau.RC;
	U.tau.RC <- (N-1)*(dat.rows-1)*tau.RC; # Chi-squared approximation for H0 according to Margolin & Light 1974, see also Liebetrau 1983
	p.tau.RC <- pchisq(U.tau.RC,df=(dat.rows-1)*(dat.cols-1),lower=FALSE);
	results <- data.frame(tau.RC, tau.CR, p.tau.RC, p.tau.CR, var.tau.RC, ASE.tau.RC, ASE.tau.CR);
	return(results);
}

# source("GK.tau.R")
# x
     # [,1] [,2] [,3]
# [1,]   30   10    1
# [2,]    2   20    5
# [3,]    1   10   15
# GK.tau(x)
     # tau.RC    tau.CR     p.tau.RC     p.tau.CR  var.tau.RC ASE.tau.RC
# 1 0.3522439 0.3219675 2.002024e-13 3.065436e-12 0.004584542 0.06770924
  # ASE.tau.CR
# 1 0.07004566

# =========================================================================================================


# =========================================================================================================
# Somer's D
# =========================================================================================================
# mt <- attr(mf, "terms")
# x <- model.matrix(mt, mf, contrasts)

#' Calculate Somers' d for the constructs. d is an 
#' assymetric association measure as it depends on which 
#' variable is set as dependent and independent.
#' The direction of dependency needs to be specified.
#'
#' @param x           \code{repgrid} object
#' @param dependent   A string denoting the direction of dependency in the output 
#'                    table (as d is assymetrical). Possible values are \code{"c"}
#'                    (the default) for setting the columns as dependent, \code{"r"} 
#'                    for setting the rows as the dependent variable and \code{"s"} for the 
#'                    symmetrical Somers' d measure (the mean of the two directional 
#'                    values for code{"c"} and \code{"r"}).
#' @param trim        The number of characters a construct is trimmed to (default is
#'                    \code{30}). If \code{NA} no trimming occurs. Trimming
#'                    simply saves space when displaying correlation of constructs
#'                    with long names.
#' @param index       Whether to print the number of the construct 
#'                    (default is \code{TRUE}). 
#' @param col.index   Logical. Wether to add an extra index column so the 
#'                    column names are indexes instead of construct names. This option 
#'                    renders a neater output as long construct names will stretch 
#'                    the output (default is \code{FALSE}).
#' @param digits      Numeric. Number of digits to round to (default is 
#'                    \code{2}).
#' @param output      The type of output printed to the console. \code{output=0}
#'                    will supress printing of the output. \code{output=1} (default) will print
#'                    results to the screen. 
#' @return            \code{matrix} of construct correlations.
#' @note              Thanks to Marc Schwartz for supplying the code to calculate
#'                    Somers' d.
#' @references        Somers, R. H. (1962). A New Asymmetric Measure of Association
#'                    for Ordinal Variables. \emph{American Sociological Review, 27}(6),
#'                    799-811.
#'
#' @author        Mark Heckmann
#' @export
#'
#' @examples \dontrun{
#'
#'    constructD(fbb2003)       # columns as dependent (default)
#'    constructD(fbb2003, "c")  # row as dependent
#'    constructD(fbb2003, "s")  # symmetrical index
#'  
#'    # surpress printing
#'    d <- constructD(fbb2003, out=0, trim=5)
#'    d
#'    
#'    # more digits
#'    constructD(fbb2003, dig=3)
#'
#'    # add index column, no trimming
#'    constructD(fbb2003, col.index=TRUE, index=F, trim=NA)  
#'
#' }
#'
constructD <- function(x, dependent = "c", 
                       trim=30, index=T, col.index=F, digits=1, output=1){
  if (!inherits(x, "repgrid")) 							    # check if x is repgrid object
  	stop("Object x must be of class 'repgrid'")
  scores <- getRatingLayer(x)
  l <- lapply(as.data.frame(t(scores)),  I)     # put each row into a list
    
  somersd <- function(x, y, dependent, smin, smax){
    na.index <- is.na(x) | is.na(y)
    x <- x[!na.index]
    y <- y[!na.index]
    x <- factor(unlist(x), levels=seq(smin, smax))
    y <- factor(unlist(y), levels=seq(smin, smax))
    m <-  as.matrix(table(x,y))
    
    if (dependent == "r")
      i <- 1 else 
    if (dependent == "c")
      i <- 2 else i <- 3
    calc.Sd(m)[[i]]
  }  
  
  nc <- length(l)
  smin <- x@scale$min
  smax <- x@scale$max
  sds <- mapply(somersd, rep(l,each=nc), rep(l, nc), 
                MoreArgs=list(dependent=dependent, 
                              smin=smin, smax=smax))
  res <- matrix(sds, nc)
  res <- addNamesToMatrix2(x, res, index=index, trim=trim, along=1)
  res <- round(res, digits)
  out <- res
  invisible(out)
}

### Thanks to Marc Schwartz for supplying the code for the Somer's d measure

# Calculate Concordant Pairs in a table
# cycle through x[r, c] and multiply by
# sum(x elements below and to the right of x[r, c])
# x = table
concordant <- function(x)
{
  x <- matrix(as.numeric(x), dim(x))
  
  # get sum(matrix values > r AND > c)
  # for each matrix[r, c]
  mat.lr <- function(r, c)
  { 
    lr <- x[(r.x > r) & (c.x > c)]
    sum(lr)
  }

  # get row and column index for each
  # matrix element
  r.x <- row(x)
  c.x <- col(x)

  # return the sum of each matrix[r, c] * sums
  # using mapply to sequence thru each matrix[r, c]
  sum(x * mapply(mat.lr, r = r.x, c = c.x))
}

# Calculate DIScordant Pairs in a table
# cycle through x[r, c] and multiply by
# sum(x elements below and to the left of x[r, c])
# x = table
discordant <- function(x)
{
  x <- matrix(as.numeric(x), dim(x))
  
  # get sum(matrix values > r AND < c)
  # for each matrix[r, c]
  mat.ll <- function(r, c)
  { 
    ll <- x[(r.x > r) & (c.x < c)]
    sum(ll)
  }

  # get row and column index for each
  # matrix element
  r.x <- row(x)
  c.x <- col(x)

  # return the sum of each matrix[r, c] * sums
  # using mapply to sequence thru each matrix[r, c]
  sum(x * mapply(mat.ll, r = r.x, c = c.x))
}


# Calculate Somers' d
# Return 3 values:
# 1. Sd C~R
# 2. Sd R~C
# 3. Sd Symmetric (Mean of above)
# x = table
calc.Sd <- function(x)
{
  x <- matrix(as.numeric(x), dim(x))
  
  c <- concordant(x)
  d <- discordant(x)
  n <- sum(x)
  SumR <- rowSums(x)
  SumC <- colSums(x)

  Sd.CR <- (2 * (c - d)) / ((n ^ 2) - (sum(SumR ^ 2)))
  Sd.RC <- (2 * (c - d)) / ((n ^ 2) - (sum(SumC ^ 2)))
  Sd.S <- (2 * (c - d)) / ((n ^ 2) - (((sum(SumR ^ 2)) + (sum(SumC ^ 2))) / 2))

  Sdlist <- list(Sd.CR, Sd.RC, Sd.S, NA)
  names(Sdlist) <- c("Sd.CR", "Sd.RC", "Sd.S", "pvalue")

  Sdlist
}

## example from Kaehler book, p.123 table, p.129 results
# m <- matrix(c(4,6,0,11,146,22,2,20,39), 3)
# calc.Sd(m)    # correct


# =========================================================================================================
# Kendall's A tau
# =========================================================================================================
calc.Kendall.tauA <- function(x)
{
  x <- matrix(as.numeric(x), dim(x))
  
  c <- concordant(x)
  d <- discordant(x)
  n <- sum(x)
  
  return(list(
    statistic = (c - d)/n^2,
    pvalue = NA
  ))
}
# =========================================================================================================
# Kendall's B tau
# =========================================================================================================
calc.Kendall.tauB <- function(x)
{
  x <- matrix(as.numeric(x), dim(x))
  
  c <- concordant(x)
  d <- discordant(x)
  
  return(0)
}
# =========================================================================================================
# Stuart's C tau
# =========================================================================================================
calc.Stuart.tauC <- function(x)
{
  x <- matrix(as.numeric(x), dim(x))
  
  c <- concordant(x)
  d <- discordant(x)
  
  statistic <- (c - d)/(1 - 1/min(dim(x)))
    
  return(list(
    statistic = statistic,
    pvalue = NA
  ))

}
# =========================================================================================================
# GK's gamma
# =========================================================================================================
calc.GK.gamma <- function(x)
{
  x <- matrix(as.numeric(x), dim(x))
  
  c <- concordant(x)
  d <- discordant(x)
  
  statistic <- (c - d)/(c + d)
  
  return(list(
    statistic = statistic,
    pvalue = NA
  ))
}