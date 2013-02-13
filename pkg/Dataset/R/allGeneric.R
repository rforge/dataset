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
setGeneric("db.population", function(object){ standardGeneric("db.population") })
setGeneric("db.population<-", function(object, value){ standardGeneric("db.population<-" ) })


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
setGeneric("Dataset.version", function(object, ...){ standardGeneric("Dataset.version") })
setGeneric("weights", function(object, ...){ stats::weights })
setGeneric("weights<-", function(object, value){ standardGeneric("weights<-" ) })
setGeneric("weighting", function(object, ...){ standardGeneric("weighting" ) })
setGeneric("weighting<-", function(object, value){ standardGeneric("weighting<-" ) })
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
setGeneric("exportPDF", function(
  object,
  pdfSavingName,
  graphics = F,
  description.chlength = 300,
  valids.chlength = 40,
  valids.cut.percent = 0.5,
  sorting = "decreasing",
  dateformat,
  page.orientation = "landscape",
  latexPackages = NULL,
  width.id = 0.5,
  width.varname = 1.5,
  width.description = 10.5,
  width.n = 0.8,
  width.na = 1.2,
  width.valids = 5,
  width.valids.nao.inc = 5,
  width.min = 1.25,
  width.max = 1.25,
  width.mean = 1.25,
  width.stddev = 1.25,
  keepTex = F,
  openPDF = T,
  ...
){ standardGeneric("exportPDF") })
setGeneric("as.Dataset", function(object){ standardGeneric("as.Dataset") })
setGeneric("contains", function(keywords, data, ignore.case = T, and = F){ standardGeneric("contains") })
setGeneric("valid", function(object, percent = 70){ standardGeneric("valid") })
setGeneric("alldescriptions", function(object){ standardGeneric("alldescriptions") })
setGeneric("allvalues", function(object){ standardGeneric("allvalues") })
setGeneric("v", function(x){ standardGeneric("v") })
setGeneric("export", function(object, name){ standardGeneric("export") })

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
  somer.d = T, # 1962
  wilson.e = F, # 1974
  calc.spearman.rho = F,
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
#setGeneric("as.Dataset", function(object){ standardGeneric("as.Dataset") })
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
setGeneric("Ops")
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