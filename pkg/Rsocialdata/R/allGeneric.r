setGeneric("name", function(object){ standardGeneric("name") })
setGeneric("name<-", function(object, value){ standardGeneric("name<-") })
setGeneric("nroww", function(object, ...){ standardGeneric("nroww") })
setGeneric("nindividual", function(object, ...){ standardGeneric("nindividual") })
setGeneric("nvariable", function(x, ...){ standardGeneric("nvariable") })
setGeneric("missings", function(object){ standardGeneric("missings") })
setGeneric("nmissings", function(object){ standardGeneric("nmissings") })
setGeneric("nmissingsw", function(object, weights){ standardGeneric("nmissingsw") })
setGeneric("missings<-", function(object, value){ standardGeneric("missings<-" ) })
setGeneric("description", function(object){ standardGeneric("description") })
setGeneric("description<-", function(object, value){ standardGeneric("description<-" ) })

setGeneric("db.author", function(object){ standardGeneric("db.author") })
setGeneric("db.author<-", function(object, value){ standardGeneric("db.author<-" ) })
setGeneric("db.manager", function(object){ standardGeneric("db.manager") })
setGeneric("db.manager<-", function(object, value){ standardGeneric("db.manager<-" ) })
setGeneric("db.contact.email", function(object){ standardGeneric("db.contact.email") })
setGeneric("db.contact.email<-", function(object, value){ standardGeneric("db.contact.email<-" ) })
setGeneric("db.license", function(object){ standardGeneric("db.license") })
setGeneric("db.license<-", function(object, value){ standardGeneric("db.license<-" ) })
setGeneric("db.release.date", function(object){ standardGeneric("db.release.date") })
setGeneric("db.release.date<-", function(object, value){ standardGeneric("db.release.date<-" ) })
setGeneric("db.citation", function(object){ standardGeneric("db.citation") })
setGeneric("db.citation<-", function(object, value){ standardGeneric("db.citation<-" ) })
setGeneric("db.website", function(object){ standardGeneric("db.website") })
setGeneric("db.website<-", function(object, value){ standardGeneric("db.website<-" ) })
setGeneric("db.details", function(object){ standardGeneric("db.details") })
setGeneric("db.details<-", function(object, value){ standardGeneric("db.details<-" ) })


setGeneric("descriptions", function(object){ standardGeneric("descriptions") })
setGeneric("values", function(object){ standardGeneric("values") })
setGeneric("valids", function(object){ standardGeneric("valids") })
setGeneric("valids<-", function(object, value){ standardGeneric("valids<-" ) })
setGeneric("nvalids", function(object){ standardGeneric("nvalids") })
setGeneric("nvalues", function(object){ standardGeneric("nvalues") })
setGeneric("value", function(value, object){ standardGeneric("value") })
setGeneric("valids.reverse", function(object){ standardGeneric("valids.reverse") })
setGeneric("valids.permut", function(object, i, j){ standardGeneric("valids.permut") })
setGeneric("is.ordered", function(x){ is.ordered })
setGeneric("codes", function(object){ standardGeneric("codes") })
setGeneric("codes<-", function(object, value){ standardGeneric("codes<-" ) })
setGeneric("checkvars", function(object){ standardGeneric("checkvars") })
setGeneric("checkvars<-", function(object, value){ standardGeneric("checkvars<-" ) })
setGeneric("spatial", function(object){ standardGeneric("spatial") })
setGeneric("spatial<-", function(object, value){ standardGeneric("spatial<-" ) })
setGeneric("spatial.country", function(object){ standardGeneric("spatial.country") })
setGeneric("spatial.country<-", function(object, value){ standardGeneric("spatial.country<-" ) })
setGeneric("spatial.variable", function(object){ standardGeneric("spatial.variable") })
setGeneric("spatial.variable<-", function(object, value){ standardGeneric("spatial.variable<-" ) })
setGeneric("origin", function(object){ standardGeneric("origin") })
setGeneric("origin<-", function(object, value){ standardGeneric("origin<-" ) })
setGeneric("format")
#setGeneric("format", function(object){ standardGeneric("format") })
setGeneric("format<-", function(object, value){ standardGeneric("format<-" ) })
setGeneric("as.valid", function(i, x){ standardGeneric("as.valid") })
setGeneric("as.missing", function(i, x){ standardGeneric("as.missing") })
setGeneric("distrib", function(object, weights, missings.omit = TRUE, percent = FALSE, sorting, format = FALSE, digits = 2, chlength = 6, sep, cut = 40, cut.percent = 0){ standardGeneric("distrib") })
setGeneric("sd")
setGeneric("sum")
setGeneric("variables", function(object){ standardGeneric("variables") })
setGeneric("variables<-", function(object, value){ standardGeneric("variables<-" ) })
setGeneric("Variable.version", function(object, ...){ standardGeneric("Variable.version") })
setGeneric("Rsocialdata.version", function(object, ...){ standardGeneric("Rsocialdata.version") })
setGeneric("weights", function(object, ...){ stats::weights })
setGeneric("weights<-", function(object, value){ standardGeneric("weights<-" ) })



setGeneric("is.weighted", function(object){ standardGeneric("is.weighted") })
setGeneric("infos", function(object){ standardGeneric("infos") })
setGeneric("infos<-", function(object, value){ standardGeneric("infos<-" ) })
setGeneric("quantitatives", function(object){ standardGeneric("quantitatives") })
setGeneric("nquantitatives", function(object){ standardGeneric("nquantitatives") })
setGeneric("scales", function(object){ standardGeneric("scales") })
setGeneric("nscales", function(object){ standardGeneric("nscales") })
setGeneric("scales.exact", function(object){ standardGeneric("scales.exact") })
setGeneric("nscales.exact", function(object){ standardGeneric("nscales.exact") })
setGeneric("qualitatives", function(object){ standardGeneric("qualitatives") })
setGeneric("nqualitatives", function(object){ standardGeneric("nqualitatives") })
setGeneric("nominals.exact", function(object){ standardGeneric("nominals.exact") })
setGeneric("nnominals.exact", function(object){ standardGeneric("nnominals.exact") })
setGeneric("nominals", function(object){ standardGeneric("nominals") })
setGeneric("nnominals", function(object){ standardGeneric("nnominals") })
setGeneric("ordinals", function(object){ standardGeneric("ordinals") })
setGeneric("nordinals", function(object){ standardGeneric("nordinals") })
setGeneric("times", function(object){ standardGeneric("times") })
setGeneric("ntimes", function(object){ standardGeneric("ntimes") })
setGeneric("binaries", function(object){ standardGeneric("binaries") })
setGeneric("nbinaries", function(object){ standardGeneric("nbinaries") })
setGeneric("weightings", function(object){ standardGeneric("weightings" ) })
setGeneric("nweightings", function(object){ standardGeneric("nweightings" ) })
setGeneric("index", function(object, names){ standardGeneric("index") })
setGeneric("rename", function(x, ...){ standardGeneric("rename") })

#' Produce a summary of an object in a PDF file
#' 
#' This generic method intends to produce a summary of the object in a PDF file. You can also keep the tex source to reuse it in your own documents.
#' 
#' @param object the object for which you to write the summary in PDF.
#' @param pdfSavingName a \code{character}
#' @param dateformat a \code{character}
#' @param description.chlength a \code{numeric}
#' @param graphics a \code{logical}
#' @param keepTex a \code{logical} keep the source of the tex file?
#' @param latexPackages a \code{character}. You can add LaTeX packages you need for the compilation here, especially for supporting other languages than english.
#' @param page.orientation the orientation of the page, either "portrait" or "landscape".
# @param plot.tree.ratio a \code{numeric} in between 0 and 1, used to reduce the scale of the tree plot in methods for tree analyses.
#' @param sorting a \code{character}. If "decreasing" valid cases are sorted in decrease order of their frequencies. If "increase" an increasing order is used.
#' @param valids.chlength a \code{numeric}
#' @param valids.cut.percent a \code{numeric}. The minimal frequency need for a valid case to be reported in the document.
#' @param width.id a \code{numeric}. Width for the variable row index column, in cm.
#' @param width.varname a \code{numeric}. Width for variable name column, in cm.
#' @param width.description a \code{numeric}. Width for description column, in cm.
#' @param width.n a \code{numeric}. Width for number of valid cases column, in cm.
#' @param width.na a \code{numeric}. Width for percents of missing values column, in cm.
#' @param width.valids a \code{numeric}. For categorical variables, width for the valid cases listing column, in cm.
#' @param width.valids.nao.inc a \code{numeric}. For nominal and ordinal variables, allow to increase the width of the valid cases listing column by reducing the description column, in cm.
#' @param width.min a \code{numeric}. For quantitative variables, the with of the minimal values column, in cm.
#' @param width.max a \code{numeric}. For quantitative variables, the with of the maximal values column, in cm.
#' @param width.mean a \code{numeric}. For quantitative variables, the with of the means column, in cm.
#' @param width.stddev a \code{numeric}. For quantitative variables, the with of the standard deviations column, in cm.
#' @param openPDF a \code{logical} open the PDF after generation completed?
#' @param ... Other options to pass to the function. For \code{\link{Statdf}} objects the option \code{merge}, with argument 'no', 'left' or 'right' allow to merge or not the p-values columns, either with the next left or right column.
#' @export
setGeneric("exportPDF", function(
  object,
  pdfSavingName,
  dateformat,
  page.orientation = "portrait",
  latexPackages = NULL,
  keepTex = F,
  openPDF = T,
  ...
){ standardGeneric("exportPDF") })


setGeneric("exportTAB", function(object, ...){ standardGeneric("exportTAB") })
setGeneric("as.Rsocialdata", function(object){ standardGeneric("as.Rsocialdata") })
setGeneric("contains", function(keywords, data, ignore.case = T, and = F){ standardGeneric("contains") })
setGeneric("valid", function(object, percent = 70){ standardGeneric("valid") })
setGeneric("alldescriptions", function(object){ standardGeneric("alldescriptions") })
setGeneric("allvalues", function(object){ standardGeneric("allvalues") })
setGeneric("v", function(x){ standardGeneric("v") })
setGeneric("export", function(object, name, ...){ standardGeneric("export") })

# for Statdf
#setGeneric("df")
setGeneric("sdf", function(object){ standardGeneric("sdf") })
setGeneric("sdf<-", function(object, value){ standardGeneric("sdf<-" ) })
setGeneric("pvalues", function(object){ standardGeneric("pvalues") })
setGeneric("pvalues<-", function(object, value){ standardGeneric("pvalues<-" ) })
setGeneric("thresholds", function(object){ standardGeneric("thresholds") })
setGeneric("thresholds<-", function(object, value){ standardGeneric("thresholds<-" ) })
setGeneric("na", function(object){ standardGeneric("na") })
setGeneric("na<-", function(object, value){ standardGeneric("na<-" ) })
setGeneric("nan", function(object){ standardGeneric("nan") })
setGeneric("nan<-", function(object, value){ standardGeneric("nan<-" ) })
setGeneric("formatc", function(object){ standardGeneric("formatc") })
setGeneric("formatc<-", function(object, value){ standardGeneric("formatc<-" ) })
#setGeneric("legend") #package:graphics
#setGeneric("legend", function(object){ standardGeneric("legend") })
#setGeneric("legend<-", function(object, value){ standardGeneric("legend<-" ) })

setGeneric("bivan", function(
  formula,
  data,
  chi2 = T,
  phi = F,
  tschuprow = F, # 1918
  cramer.v = T, # 1946
  pearson.contingency = F, # 1948
  likelihood.ratio = F,
  gk.lambda = F, # 1954
  gk.tau = F, # 1954
  gk.tau.sqrt = T,
  theil.u = F,
  theil.u.sqrt = F,
  kendall.tau.a = F, # 1938
  kendall.tau.b = F, # 1945
  stuart.tau.c = F, # 1953
  gk.gamma = F, # 1954
  somers.d = T, # 1962
  wilson.e = F, # 1974
  spearman.rho = F,
  std.res = T,
  quiet = F
){ standardGeneric("bivan") })
setGeneric("target", function(object){ standardGeneric("target") })
setGeneric("target<-", function(object, value){ standardGeneric("target<-" ) })
setGeneric("predictors", function(object){ standardGeneric("predictors") })
setGeneric("predictors<-", function(object, value){ standardGeneric("predictors<-" ) })
setGeneric("observed", function(object){ standardGeneric("observed") })
setGeneric("observed<-", function(object, value){ standardGeneric("observed<-" ) })
setGeneric("expected", function(object){ standardGeneric("expected") })
setGeneric("expected<-", function(object, value){ standardGeneric("expected<-" ) })
setGeneric("std.res", function(object){ standardGeneric("std.res") })
setGeneric("std.res<-", function(object, value){ standardGeneric("std.res<-" ) })
setGeneric("global", function(object){ standardGeneric("global") })
setGeneric("global<-", function(object, value){ standardGeneric("global<-" ) })



setGeneric("varid", function(names, object){ standardGeneric("varid" ) })

setGeneric("frequencies", function(x, data, sort = 'decreasing', sort.ordinal = F, ...){ standardGeneric("frequencies" ) })

#setGeneric("table", function(..., exclude = if (useNA == "no") c(NA, NaN), useNA = c("no", "ifany", "always"), dnn = list.names(...), deparse.level = 1){ table })

# setGeneric("sd", function(object, na.rm = FALSE){ standardGeneric("sd") })
#setGeneric("applyToColumn", function(fun, x, ...){ standardGeneric("applyToColumn") })
#setGeneric("longitudinalSummary", function(object){ standardGeneric("longitudinalSummary") })
#setGeneric("longitudinalSummaryToPDF", function(object, features, pdfSavingName, rate = 0.60, keepTex = FALSE){ standardGeneric("longitudinalSummaryToPDF") })
##setGeneric("merge",  function(x, y, by){	standardGeneric("merge") })
#setGeneric("as.Column", function(object){ standardGeneric("as.Column") })
#setGeneric("as.Rsocialdata", function(object){ standardGeneric("as.Rsocialdata") })
#setGeneric("applyToColumns", function(object){ standardGeneric("applyToColumns") })
#setGeneric("categorize", function(object){ standardGeneric("categorize") })
##setGeneric("cUnion", function(...){ standardGeneric("cUnion") })
##setClassUnion
setGeneric("merge")
setGeneric("table")
# setGeneric("freq")
#setGeneric("merge",  function(x, y, ...){  standardGeneric("merge") })
setGeneric("cbind",  function(..., deparse.level = 1){  standardGeneric("cbind") })
setGeneric("rbind",  function(..., deparse.level = 1){  standardGeneric("rbind") })

# setGeneric("recode", function(object, recoding, ...){ standardGeneric("recode") })
setGeneric("recode", function(object, ...){ standardGeneric("recode") })
#setGeneric("cut", function(x, ...){ standardGeneric("cut") })
#setGeneric("as.Variable", function(x, ...){ standardGeneric("as.Variable") })
# setGeneric("cut", function(x, ...){ cut })
setGeneric("cut")
# setGeneric("Ops")
setGeneric("subset")
setGeneric("only.complete", function(variables, data, ...){ standardGeneric("only.complete") })
# if (!isGeneric("sd")) {
	# if (is.function("sd"))
	# fun <- sd
	# else fun <- function(x, na.rm = FALSE) standardGeneric("sd")
	# setGeneric("sd", fun)
# }
## attention il faut vraiment donner les mêmes arguments !
# Creating a generic for "sd" in package ".GlobalEnv"
# (the supplied definition differs from and overrides the implicit generic
# in package "stats": Formal arguments differ: (object, na.rm), (x, na.rm))
###setGeneric("sd", stats::sd)

# setGeneric("where", function(x, ...){ standardGeneric("where") })
# lockBinding("summaryToPDF" ,. GlobalEnv )