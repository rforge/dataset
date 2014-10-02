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
#' @param graphics a \code{logical}
#' @param description.chlength a \code{numeric}
#' @param valids.chlength a \code{numeric}
#' @param valids.cut.percent a \code{numeric}. The minimal frequency need for a valid case to be reported in the document.
#' @param sorting a \code{character}. If "decreasing" valid cases are sorted in decrease order of their frequencies. If "increase" an increasing order is used.
#' @param dateformat a \code{character}
#' @param latexPackages a \code{character}. You can add LaTeX packages you need for the compilation here, especially for supporting other languages than english.
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
#' @param keepTex a \code{logical} keep the source of the tex file?
#' @param openPDF a \code{logical} open the PDF after generation completed?
#' @param dots Other options to pass to the function. For \code{\link{Statdf}} objects the option \code{merge}, with argument 'no', 'left' or 'right' allow to merge or not the p-values columns, either with the next left or right column.
#' @export
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
  plot.tree.ratio = 1,
  keepTex = F,
  openPDF = T,
  ...
){ standardGeneric("exportPDF") })


#' @describeIn exportPDF method for \code{Rsocialdata} objects. Generate a codebook of the database.
setMethod("exportPDF", "Rsocialdata", 
          definition = function (
            object,
            pdfSavingName,
            graphics,
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
            keepTex,
            openPDF
          ) {
            
            check.tex()
            
            if(!is.installed.pkg('xtable')) {
              exit.by.uninstalled.pkg('xtable')
            } else {
              require(xtable)
              
              
              #TAILLE IDEALE 19.5cm
              align.common <-  c(
                paste("p{",width.id,"cm}",sep=''),
                paste("p{",width.varname,"cm}",sep=''),
                paste("p{",width.description,"cm}",sep=''),
                paste("p{",width.n,"cm}",sep=''),
                paste("p{",width.na,"cm}",sep='')
              ) # 14.5cm
              align.bin    <-  c(
                align.common,
                paste("p{",width.valids+0.5,"cm}",sep='')
              )
              align.nom    <-  c(
                align.common,
                paste("p{",0.6,"cm}",sep=''),
                paste("p{",width.valids-0.6,"cm}",sep='')
              )
              align.nom[3] <- paste("p{",width.description - width.valids.nao.inc,"cm}",sep='')
              align.nom[7] <- paste("p{",width.valids + width.valids.nao.inc,"cm}",sep='')
              align.ord    <-  align.nom
              align.scale  <-  c(
                align.common,
                paste("p{",width.min,"cm}",sep=''),
                paste("p{",width.max,"cm}",sep=''),
                paste("p{",width.mean,"cm}",sep=''),
                paste("p{",width.stddev,"cm}",sep='')
              ) 
              align.wvar   <-  align.scale
              align.ts   <-  align.scale
              #     align.ts     <-  c(
              #       align.common,
              #       paste("p{"width.min"cm}",sep=''),
              #       paste("p{"width.min"cm}",sep=''),
              #       paste("p{"width.min"cm}",sep=''),
              #       paste("p{"1.5"cm}",sep='')
              #     )
              
              nTuples <- nroww(object)
              weights <- weights(object)
              
              if (length(name(object)) == 0) { outName <- "Untitled Rsocialdata" } else { outName <- name(object) }
              outName <- make.names(outName) # no spaces for Unix/Texlive compilation ?
              
              if(missing(pdfSavingName)) {		
                pdfSavingName <- paste("Summary-", outName, sep = "") # no spaces for Unix/Texlive compilation ?
              }
              
              
              latexFile <- paste(pdfSavingName, ".tex", sep="")
              
              is.writable(pdfSavingName, path = getwd()) 	
              
              outFileCon <- file(latexFile, "w", encoding="UTF-8")
              
              #     print('hello-beforelatexhead')
              #     print(class(getMethod("show", "Variable")))
              
              latex.head(title = paste("Summary of the", totex(name(object)), "dataset"),
                         page.orientation, latexPackages, outFileCon)
              
              #     print('hello-afterlatexhead')
              #     print(class(getMethod("show", "Variable")))
              
              cat("\\section*{Overview} \n", file = outFileCon, append = T)
              cat("\\begin{minipage}[t]{.46\\linewidth} \n", file = outFileCon, append = T)
              cat("\\begin{itemize*} \n", file = outFileCon, append = T)
              cat("\\item \\textbf{Name:}", totex(name(object)), "\n", file = outFileCon, append = T)
              cat("\\item \\textbf{Description:}", description(object), "\n", file = outFileCon, append = T)
              #cat("\\item Object version:", oversion(object), "\n", file = outFileCon, append = T)
              #cat("\\item Created by Rsocialdata version:", pversion(object), "\n", file = outFileCon, append = T)
              cat("\\item \\textbf{Number of variables:} ", ncol(object), " (",
                  nbinaries(object), " binaries, ",
                  nordinals(object), " ordinals, ",
                  nnominals.exact(object), " nominals, ",
                  nscales.exact(object), " scales, ",
                  ntimes(object), " timestamps, ",
                  nweightings(object), " weightings",
                  ")", "\n", sep = "", file = outFileCon, append = T)
              cat("\\item \\textbf{Number of individuals:}", nTuples, "(for ", nrow(object), " rows)","\n", file = outFileCon, append = T)
              
              
              
              cat("\\item \\textbf{Percent of missing values:}", missings(object)["nmissingspercent.cha"], "\\%", "\n", file = outFileCon, append = T)
              if(length(weighting(object)) > 0) {
                cat("\\item \\textbf{Weighting variable:} ", totex(weighting(object)), ', ', totex(description(object[[weighting(object)]])), ".", "\n", file = outFileCon, append = T, sep='')
              } else {
                cat("\\item \\textbf{Weighting variable:} none.", "\n", file = outFileCon, append = T, sep='')
              }
              if(length(checkvars(object)) > 0) {
                cat("\\item \\textbf{Control variable(s):} ", file = outFileCon, append = T, sep='')
                cv.temp <- checkvars(object)
                count <- 0
                count.last <- length(cv.temp)
                for (cv in cv.temp) {
                  count <- count + 1
                  cat(totex(cv), ' (', totex(description(object[[cv]])), ")", file = outFileCon, append = T, sep='')
                  if (count == count.last) {
                    cat('. ', "\n",  file = outFileCon, append = T, sep='')
                  } else {
                    cat(', ', file = outFileCon, append = T, sep='')
                  }
                }
              } else {
                cat("\\item \\textbf{Control variable(s):} none.", "\n", file = outFileCon, append = T, sep='')
              }
              if(nchar(spatial(object)) > 0) {
                cat("\\item \\textbf{Spatial variable:} ", totex(spatial(object)), ', ', totex(description(spatial[[weighting(object)]])), ".", "\n", file = outFileCon, append = T, sep='')
              } else {
                cat("\\item \\textbf{Spatial variable:} none.", "\n", file = outFileCon, append = T, sep='')
              }
              cat("\\end{itemize*} \n", file = outFileCon, append = T)
              
              cat("\\end{minipage} \\hfill \n", file = outFileCon, append = T)
              cat("\\begin{minipage}[t]{.46\\linewidth} \n", file = outFileCon, append = T)
              cat("\\begin{itemize*} \n", file = outFileCon, append = T)
              cat("\\item \\textbf{Author(s):} ", totex(db.author(object)), "\n", file = outFileCon, append = T, sep='')
              cat("\\item \\textbf{Database manager(s):} ", totex(db.manager(object)), "\n", file = outFileCon, append = T, sep='')
              cat("\\item \\textbf{Contact e-mail(s):} ", totex(db.contact.email(object)), "\n", file = outFileCon, append = T, sep='')
              cat("\\item \\textbf{License:} ", totex(db.license(object)), "\n", file = outFileCon, append = T, sep='')
              cat("\\item \\textbf{Release date:} ", totex(db.release.date(object)), "\n", file = outFileCon, append = T, sep='')
              cat("\\item \\textbf{Citation:} ", totex(db.citation(object)), "\n", file = outFileCon, append = T, sep='')
              cat("\\item \\textbf{Website:} ", totex(db.website(object)), "\n", file = outFileCon, append = T, sep='')
              #     cat("\\item \\textbf{Population:} ", totex(db.details(object)), "\n", file = outFileCon, append = T, sep='')
              cat("\\end{itemize*} \n", file = outFileCon, append = T)
              cat("\\end{minipage} \\hfill \n", file = outFileCon, append = T)
              
              cat("\\newpage \n", file = outFileCon, append = T)
              cat("\\section*{Detailed description} \n", totex(db.details(object)), "\n", file = outFileCon, append = T, sep='')
              
              cat("\\newpage \n", file = outFileCon, append = T)
              cat("\\section*{Distribution of variables by percent of valid cases} \n", file = outFileCon, append = T)
              
              percents <- seq(from = 0, to = 100, by = 10)
              val <- c()
              for (i in percents) {
                val <- c(val, ncol(valid(object, percent = i)))
              }
              valdf <- data.frame(percents,val)
              names(valdf) <- c("Percents of valid cases", "Number of variables")
              
              object.xtable <- xtable(
                valdf,
                label='validCasesSummary',
                caption='Number of variables by percent of valid cases',
                digits = 3,
                #     		align = c("l","p{4cm}","l"),
                display = c("d","d","d")
              )
              
              plot.filename <- paste(pdfSavingName, ".validcasesRplot", sep = "")
              plot.filename <- gsub("\\.", "-", plot.filename)
              plot.filename.pdf <- paste(plot.filename, ".pdf", sep = "")
              pdf(file = plot.filename.pdf)
              plot(
                percents,
                val,
                type = "l",
                main = "Number of variables by percent of valid cases",
                xlab = "Percentage of valid cases",
                ylab = "Number of variables"
              )
              polygon(c(0,percents,100),c(0,val,0), col="gray")
              dev.off()
              
              cat("\\begin{center} \n", file = outFileCon, append = T)
              cat("\\begin{minipage}[c]{.35\\linewidth} \n", file = outFileCon, append = T)
              print(object.xtable, file=outFileCon , append=T, include.rownames = F, table.placement = "htb", floating=F) 
              cat("\\end{minipage} \n", file = outFileCon, append = T)
              cat("\\begin{minipage}[c]{.40\\linewidth} \n", file = outFileCon, append = T)
              cat("\\includegraphics[scale=0.40]{", plot.filename.pdf, "} \n", file = outFileCon, append = T, sep = "")
              cat("\\end{minipage} \n", file = outFileCon, append = T)
              cat("\\end{center} \n", file = outFileCon, append = T)
              
              
              cat("\\newpage \n", file = outFileCon, append = T)
              cat("\\section*{Configuration} \n", file = outFileCon, append = T)
              cat("\\begin{itemize} \n", file = outFileCon, append = T)
              cat("\\item Variable descriptions are cut above", description.chlength, "characters. \n", file = outFileCon, append = T)
              cat("\\item Valid case descriptions are cut above", valids.chlength, "characters. \n", file = outFileCon, append = T)
              if (valids.cut.percent > 0) {
                cat("\\item For categorical variables, valid cases are cut when under", valids.cut.percent, "\\% of representativity. \n", file = outFileCon, append = T)
              }
              cat("\\item For nominal variables, valid cases are listed in", sorting, "order. \n", file = outFileCon, append = T)
              cat("\\item For ordinal variables, valid cases are listed in", sorting, "order. \n", file = outFileCon, append = T)
              cat("\\end{itemize} \n", file = outFileCon, append = T)
              cat("\\newpage \n", file = outFileCon, append = T)
              flag.newpage <- FALSE
              cat("\\section*{Variable summary} \n", file = outFileCon, append = T)
              
              
              if(nweightings(object) > 0 ) {
                if (flag.newpage) {
                  #         cat("\\newpage \n", file = outFileCon, append = T)
                  cat("\\vspace*{2cm} \n", file = outFileCon, append = T)
                }
                cat("\\subsection*{Weighting variable(s)} \n", file = outFileCon, append = T)
                scvar <- weightings(object)
                scvar.names <- names(scvar)
                
                
                descriptions <- c()
                nbNA <- c()
                theMin <- c()
                theMax <- c()
                theMean <- c()
                theSD <- c()
                
                for (i in scvar.names ) {
                  vtemp <- scvar[[i]]
                  
                  desc.temp <- description(vtemp)
                  if (nchar(desc.temp) > description.chlength)
                    desc.temp <- paste(substr(desc.temp, 0, description.chlength - 3), "...", sep = "")
                  
                  descriptions <- c(descriptions, desc.temp)
                  nbNA <- c(nbNA, nmissingsw(vtemp, weights))
                  theMin <- c(theMin, min(vtemp, na.rm = TRUE))
                  theMax <- c(theMax, max(vtemp, na.rm = TRUE))
                  theMean <- c(theMean, mean(vtemp, na.rm = TRUE))
                  theSD <- c(theSD, sd(vtemp, na.rm = TRUE))
                }
                
                nbNA[which(is.na(nbNA))] <- 0
                N <- rep(nTuples, nweightings(object)) - nbNA
                NApourcent <- nbNA / nTuples * 100
                
                df <- data.frame(scvar.names, descriptions,N, NApourcent, theMin, theMax, theMean, theSD)
                
                names(df) <- c("Variable", "Description", "N", "NA (%)", "Min", "Max", "Mean", "Std dev.")
                # row.names(df) <- 1:nrow(df)
                
                
                row.names(df) <- varid(scvar.names, object)
                cat("{\\footnotesize \n", file = outFileCon, append = T)
                
                object.xtable <- xtable(
                  df,
                  label='featureSummary',
                  caption='Weighting variable(s) summary',
                  digits = 3,
                  align = align.wvar,
                  display = c("d","s","s","d","fg","fg","fg","fg","fg")
                )
                
                print(object.xtable, file=outFileCon , append=T, tabular.environment='longtable', table.placement = "htb", floating=F) 
                cat("} \n", file = outFileCon, append = T)
                flag.newpage <- TRUE
              }
              
              
              if(nbinaries(object) > 0 ) {
                if (flag.newpage) {
                  #         cat("\\newpage \n", file = outFileCon, append = T)
                  cat("\\vspace*{2cm} \n", file = outFileCon, append = T)
                }
                cat("\\subsection*{Binary variables} \n", file = outFileCon, append = T)
                scvar <- binaries(object)
                scvar.names <- names(scvar)
                
                descriptions <- c()
                nbNA <- c()
                theDistrib <- c()   
                
                for (i in scvar.names ) {
                  vtemp <- scvar[[i]]
                  
                  desc.temp <- description(vtemp)
                  if (nchar(desc.temp) > description.chlength)
                    desc.temp <- paste(substr(desc.temp, 0, description.chlength - 3), "...", sep = "")
                  
                  descriptions <- c(descriptions, desc.temp)
                  nbNA <- c(nbNA, nmissingsw(vtemp, weights))
                  theDistrib <- c(theDistrib, distrib(vtemp, weights, percent = T, format = T, chlength = valids.chlength, sorting=sorting, cut.percent=valids.cut.percent))
                  #theSD <- c(theSD, sd(vtemp, na.rm = TRUE))
                }
                
                nbNA[which(is.na(nbNA))] <- 0
                N <- rep(nTuples, nbinaries(object)) - nbNA
                NApourcent <- nbNA / nTuples * 100
                
                df <- data.frame(scvar.names, descriptions,N, NApourcent, theDistrib)
                names(df) <- c("Variable", "Description", "N", "NA (%)", "Distribution (%)")
                row.names(df) <- varid(scvar.names, object)
                cat("{\\footnotesize \n", file = outFileCon, append = T)
                
                object.xtable <- xtable(
                  df,
                  label='featureSummary',
                  caption='Binary variables summary',
                  digits = 3,
                  align = align.bin,
                  display = c("d","s","s","d","fg","s")
                )
                
                print(object.xtable, file=outFileCon , append=T, tabular.environment='longtable', table.placement = "htb", floating=F) 
                cat("} \n", file = outFileCon, append = T)
                flag.newpage <- TRUE
              }
              
              if(nordinals(object) > 0 ) {
                if (flag.newpage) {
                  #         cat("\\newpage \n", file = outFileCon, append = T)
                  cat("\\vspace*{2cm} \n", file = outFileCon, append = T)
                }
                cat("\\subsection*{Ordinal variables} \n", file = outFileCon, append = T)
                scvar <- ordinals(object)
                scvar.names <- names(scvar)
                
                descriptions <- c()
                nbNA <- c()
                theNlevels <- c()
                theDistrib <- c()   
                
                for (i in scvar.names ) {
                  vtemp <- scvar[[i]]
                  
                  desc.temp <- description(vtemp)
                  if (nchar(desc.temp) > description.chlength)
                    desc.temp <- paste(substr(desc.temp, 0, description.chlength - 3), "...", sep = "")
                  
                  descriptions <- c(descriptions, desc.temp)
                  nbNA <- c(nbNA, nmissingsw(vtemp, weights))
                  theNlevels <- c(theNlevels, nvalids(vtemp))
                  theDistrib <- c(theDistrib, distrib(vtemp, weights, percent = T, format = T, chlength = valids.chlength, sorting=sorting, cut.percent=valids.cut.percent))
                  #theSD <- c(theSD, sd(vtemp, na.rm = TRUE))
                }
                
                nbNA[which(is.na(nbNA))] <- 0
                N <- rep(nTuples, nordinals(object)) - nbNA
                NApourcent <- nbNA / nTuples * 100
                
                df <- data.frame(scvar.names, descriptions,N, NApourcent, theNlevels, theDistrib)
                names(df) <- c("Variable", "Description", "N", "NA (%)", "Classes", "Distribution (%)")
                row.names(df) <- varid(scvar.names, object)
                cat("{\\footnotesize \n", file = outFileCon, append = T)
                
                object.xtable <- xtable(
                  df,
                  label='featureSummary',
                  caption='Ordinal variables summary',
                  digits = 3,
                  align = align.ord,
                  display = c("d","s","s","d","fg","d","s")
                )
                
                print(object.xtable, file=outFileCon , append=T, tabular.environment='longtable', table.placement = "htb", floating=F) 
                cat("} \n", file = outFileCon, append = T)
                flag.newpage <- TRUE
              }
              
              if(nnominals.exact(object) > 0 ) {
                if (flag.newpage) {
                  #         cat("\\newpage \n", file = outFileCon, append = T)
                  cat("\\vspace*{2cm} \n", file = outFileCon, append = T)
                }
                cat("\\subsection*{Nominal variables} \n", file = outFileCon, append = T)
                scvar <- nominals.exact(object)
                scvar.names <- names(scvar)
                
                descriptions <- c()
                nbNA <- c()
                theNlevels <- c()
                theDistrib <- c()   
                
                for (i in scvar.names ) {
                  #print(i)
                  vtemp <- scvar[[i]]
                  
                  desc.temp <- description(vtemp)
                  #print(desc.temp)
                  if (nchar(desc.temp) > description.chlength)
                    desc.temp <- paste(substr(desc.temp, 0, description.chlength - 3), "...", sep = "")
                  
                  descriptions <- c(descriptions, desc.temp)
                  nbNA <- c(nbNA, nmissingsw(vtemp, weights))
                  theNlevels <- c(theNlevels, nvalids(vtemp))
                  theDistrib <- c(theDistrib, distrib(vtemp, weights, percent = T, format = T, chlength = valids.chlength, sorting=sorting, cut.percent=valids.cut.percent))
                  #theSD <- c(theSD, sd(vtemp, na.rm = TRUE))
                }
                
                nbNA[which(is.na(nbNA))] <- 0
                N <- rep(nTuples, nnominals.exact(object)) - nbNA
                NApourcent <- nbNA / nTuples * 100
                
                df <- data.frame(scvar.names, descriptions,N, NApourcent, theNlevels, theDistrib)
                names(df) <- c("Variable", "Description", "N", "NA (%)", "Classes", "Distribution (%)")
                row.names(df) <- varid(scvar.names, object)
                cat("{\\footnotesize \n", file = outFileCon, append = T)
                
                object.xtable <- xtable(
                  df,
                  label='featureSummary',
                  caption='Nominal variables summary',
                  digits = 3,
                  align = align.nom,
                  display = c("d","s","s","d","fg","d","s")
                )
                
                print(object.xtable, file=outFileCon , append=T, tabular.environment='longtable', table.placement = "htb", floating=F) 
                cat("} \n", file = outFileCon, append = T)
                flag.newpage <- TRUE
              }
              
              if(nscales.exact(object) > 0 ) {
                if (flag.newpage) {
                  #         cat("\\newpage \n", file = outFileCon, append = T)
                  cat("\\vspace*{2cm} \n", file = outFileCon, append = T)
                }
                cat("\\subsection*{Scale variables} \n", file = outFileCon, append = T)
                scvar <- scales.exact(object)
                scvar.names <- names(scvar)
                
                descriptions <- c()
                nbNA <- c()
                theMin <- c()
                theMax <- c()
                theMean <- c()
                theSD <- c()
                
                for (i in scvar.names ) {
                  vtemp <- scvar[[i]]
                  
                  desc.temp <- description(vtemp)
                  if (nchar(desc.temp) > description.chlength)
                    desc.temp <- paste(substr(desc.temp, 0, description.chlength - 3), "...", sep = "")
                  
                  descriptions <- c(descriptions, desc.temp)
                  nbNA <- c(nbNA, nmissingsw(vtemp, weights))
                  theMin <- c(theMin, min(vtemp, na.rm = TRUE))
                  theMax <- c(theMax, max(vtemp, na.rm = TRUE))
                  theMean <- c(theMean, mean(vtemp, na.rm = TRUE))
                  theSD <- c(theSD, sd(vtemp, na.rm = TRUE))
                }
                
                nbNA[which(is.na(nbNA))] <- 0
                N <- rep(nTuples, nscales.exact(object)) - nbNA
                NApourcent <- nbNA / nTuples * 100
                
                df <- data.frame(scvar.names, descriptions,N, NApourcent, theMin, theMax, theMean, theSD)
                names(df) <- c("Variable", "Description", "N", "NA (%)", "Min", "Max", "Mean", "Std dev.")
                # row.names(df) <- 1:nrow(df)
                row.names(df) <- varid(scvar.names, object)
                cat("{\\footnotesize \n", file = outFileCon, append = T)
                
                object.xtable <- xtable(
                  df,
                  label='featureSummary',
                  caption='Scale variables summary',
                  digits = 3,
                  align = align.scale,
                  display = c("d","s","s","d","fg","fg","fg","fg","fg")
                )
                
                print(object.xtable, file=outFileCon , append=T, tabular.environment='longtable', table.placement = "htb", floating=F) 
                cat("} \n", file = outFileCon, append = T)
                flag.newpage <- TRUE
              }
              
              if(ntimes(object) > 0 ) {
                if (flag.newpage) {
                  #         cat("\\newpage \n", file = outFileCon, append = T)
                  cat("\\vspace*{2cm} \n", file = outFileCon, append = T)
                }
                cat("\\subsection*{Timestamp variables} \n", file = outFileCon, append = T)
                scvar <- times(object)
                scvar.names <- names(scvar)
                
                descriptions <- c()
                nbNA <- c()
                theMin <- c()
                theMax <- c()
                theMed <- c()
                theMean <- c()  	
                
                for (i in scvar.names ) {
                  vtemp <- scvar[[i]]
                  if(!missing(dateformat)){
                    format(vtemp) <- dateformat
                  }
                  
                  desc.temp <- description(vtemp)
                  if (nchar(desc.temp) > description.chlength)
                    desc.temp <- paste(substr(desc.temp, 0, description.chlength - 3), "...", sep = "")
                  
                  descriptions <- c(descriptions, desc.temp)
                  nbNA <- c(nbNA, nmissingsw(vtemp, weights))
                  theMin <- c(theMin, min(vtemp, na.rm = TRUE))
                  theMax <- c(theMax, max(vtemp, na.rm = TRUE))
                  theMed <- c(theMed, median(vtemp, na.rm = TRUE))
                  theMean <- c(theMean, mean(vtemp, na.rm = TRUE))
                  #theSD <- c(theSD, sd(vtemp, na.rm = TRUE))
                }
                
                nbNA[which(is.na(nbNA))] <- 0
                N <- rep(nTuples, ntimes(object)) - nbNA
                NApourcent <- nbNA / nTuples * 100
                
                df <- data.frame(scvar.names, descriptions,N, NApourcent, theMin, theMax, theMed, theMean)
                names(df) <- c("Variable", "Description", "N", "NA (%)", "Min", "Max", "Median", "Mean")
                row.names(df) <- varid(scvar.names, object)
                cat("{\\footnotesize \n", file = outFileCon, append = T)
                
                object.xtable <- xtable(
                  df,
                  label='featureSummary',
                  caption='Timestamp variables summary',
                  digits = 3,
                  align = align.ts,
                  display = c("d","s","s","d","fg","fg","fg","fg","fg")
                )
                
                print(object.xtable, file=outFileCon , append=T, tabular.environment='longtable', table.placement = "htb", floating=F) 
                cat("} \n", file = outFileCon, append = T)
                flag.newpage <- TRUE
              }
              
              
              close.and.clean(outFileCon, pdfSavingName, keepTex, openPDF)
              
              if(!keepTex) {
                unlink(plot.filename.pdf)
              }
            }
          }
)


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