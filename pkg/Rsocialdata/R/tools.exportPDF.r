totex <- function(txt){
  out <- txt
  out <- gsub("_", "\\\\_", out, perl = T)
  out <- gsub("\\$", "\\\\$", out, perl = T)
  return(out) 
}
# totex("shp_bon")
# totex("shp$$bon")

# FIXME : acrobat ne vérouille pas le fichier en écriture, donc le test writable passe mais la compilation plante ensuite... 
is.writable <- function(pdfSavingName, extensions, path) {
  # FIXME first we test the folder (for all latex aux files)
  
  if(missing(extensions)) extensions <- c('.tex','.pdf')
  
  for (i in extensions) {
    currentfile <- paste(pdfSavingName, i, sep = '')
    if(file.access(currentfile, mode = 0) == 0){ #exist
      if(file.access(currentfile, mode = 2) == -1){ #writable
        message("The file '", currentfile, "' exists and is not writable.")
        message('Maybe a software is currently using it, your PDF viewer for example.')
        message('Try to close any software which could lock the file.')
        message('')
        stop('Unable to get write permission.')
      }
    }
  }
  return(TRUE)
}

latex.head <- function(title, page.orientation, latexPackages, outFileCon){
  message('')
  message('Your file will be save in ', getwd())
  filenameTex <- summary(outFileCon)$description
  filename <- substr(filenameTex, 0, nchar(filenameTex)-4)
  filename.pdf <- paste(filename, '.pdf', sep='')
  message('Name of your file: ', filename.pdf)
  message('')
  
  message('Writing tex file...', appendLF=F)
  cat("\\documentclass[", page.orientation, "]{article} \n" , file = outFileCon, append = F)
  cat("\\usepackage[top=2.5cm, bottom=2.5cm, left=1.5cm, right=1.5cm]{geometry} \n", file = outFileCon, append = T)
  
  cat("\\usepackage[utf8]{inputenc} \n", file = outFileCon, append = T)
  
#   print('hello-beforelatexwritepackage')
#   print(class(getMethod("show", "Variable")))
  
  latex.write.packages(latexPackages, outFileCon)
  
  #cat("\\usepackage[utf8x]{inputenc} \n", file = outFileCon, append = T)
  cat("\\usepackage[T1]{fontenc} \n", file = outFileCon, append = T)
  #cat("\\usepackage{aeguill}  \n", file = outFileCon, append = T)
  cat("\\usepackage{longtable} \n", file = outFileCon, append = T)
  cat("\\usepackage{graphicx} \n", file = outFileCon, append = T)
  
 
  
  # itemize environment with no space
  cat("\\newenvironment{itemize*}% \n", file = outFileCon, append = T)
  cat("{\\begin{itemize}% \n", file = outFileCon, append = T)
  cat("\\setlength{\\itemsep}{0pt}% \n", file = outFileCon, append = T)
  cat("\\setlength{\\parskip}{0pt}}% \n", file = outFileCon, append = T)
  cat("{\\end{itemize}} \n", file = outFileCon, append = T)
  
#   print('helloc')
#   print(class(getMethod("show", "Variable")))
  
  cat("\\author{Generated by the R Rsocialdata package \\\\ \\emph{version ", Rsocialdata:::Survey.version(), "}} \n", file = outFileCon, append = T)
  
#   print('hellod')
#   print(class(getMethod("show", "Variable")))
  
	cat("\\title{", title, "} \n", file = outFileCon, append = T)
	cat("\\begin{document} \n", file = outFileCon, append = T)
#   cat("\\vspace*{-1cm} \n", file = outFileCon, append = T)
	cat("\\maketitle \n", file = outFileCon, append = T)
  
#   print('hello-endlatexhead')
#   print(class(getMethod("show", "Variable")))
}

latex.write.packages <- function(packages, outFileCon){
  if(!is.null(packages)) {
    stopifnot(inherits(packages, "character"))
    packages <- unique(packages)
    for (i in 1:length(packages)) {
      cat("\\usepackage", packages[i], " \n", file = outFileCon, append = T, sep="")
    }
  }
}


# close.and.clean <- function(outFileCon, pdfSavingName, keepTex, openPDF){
#   cat("\\end{document} \n", file = outFileCon, append = T)
#   close(outFileCon)
#   message(' done', appendLF=T)
#   
#   message('Processing tex to pdf...', appendLF=F)
#   tools::texi2pdf(paste(pdfSavingName, '.tex', sep = ''))
#   message(' done', appendLF=T)
#   
#   # clean directory
#   message('Cleaning auxiliary files...', appendLF=F)
#   if (keepTex) {
#     extensionsToRemove <- ".(log|aux)"
#   } else {
#     extensionsToRemove <- ".(log|aux|tex)"
#   }
#   
#   tempTex <- list.files(
#     ##paste(datadir, wavesFolder, "-SPSS", "/", i, sep = ""),
#     getwd(),
#     pattern = paste("^", pdfSavingName, extensionsToRemove, sep = "")
#   )
#   
#   unlink(tempTex)
#   message(' done', appendLF=T)
#   
#   # opening of the pdf file
#   if (openPDF) {
#     message('')
#     message('Launching PDF file...', appendLF=T)
#     path <- file.path(getwd(), paste(pdfSavingName, '.pdf', sep = ''))
# #     print(path)
#     openPDF(path)
# #     message(' done', appendLF=T)
#   }
#   
# }

close.and.clean <- function(outFileCon, pdfSavingName, keepTex, openPDF){
  cat("\\end{document} \n", file = outFileCon, append = T)
  close(outFileCon)
  message(' done', appendLF=T)
  
  message('Processing tex to pdf...', appendLF=F)
  pdflatex(paste(pdfSavingName, '.tex', sep = ''))
  message(' done', appendLF=T)
  
  # clean directory
  message('Cleaning auxiliary files...', appendLF=F)
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
  
  unlink(tempTex)
  message(' done', appendLF=T)
  
  # opening of the pdf file
  if (openPDF) {
    message('')
    message('Launching PDF file...', appendLF=T)
    path <- file.path(getwd(), paste(pdfSavingName, '.pdf', sep = ''))
    #     print(path)
    openPDF(path)
    #     message(' done', appendLF=T)
  }
  
}

# v a Variable
str.typevar <- function(x, short = FALSE, parenthesis=FALSE) {
  if (parenthesis) {
    txt.before <- '('
    txt.after <- ')'
  } else {
    txt.before <- txt.after <- ''
  }
  
 if(is.nominal(x)) {
   if (short) txt <- 'n'
   else txt <- 'nominal'
 }
 if(is.ordinal(x)) {
   if (short) txt <- 'o'
   else txt <- 'ordinal'
 }
 if(is.binary(x)) {
   if (short) txt <- 'b'
   else txt <- 'binary'
 }
 if(is.scale(x)) {
   if (short) txt <- 's'
   else txt <- 'scale'
 }
 if(is.time(x)) {
   if (short) txt <- 't'
   else txt <- 'time'
 }
 if(is.weighting(x)) {
   if (short) txt <- 'w'
   else txt <- 'weighting'
 }
  
  return(paste(txt.before, txt, txt.after, sep = ''))
}
# str.typevar(svar(c(1)))
# str.typevar(svar(c(1)), short = T)

str.collapse <- function(x,y, sep = ' ') {
  out <- mapply(paste, x, y, sep=sep)
  names(out) <- NULL
  return(out)
}
# str.collapse(c("id", "health"), c("s", "o"))

str.capitalize <- function(x, all = FALSE) {
  if(all) {
    return(gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", x, perl=TRUE))
  } else {
    return(sub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", x, perl=TRUE))
  }
}

#x a Rsocialdata
str.names <- function(x, parenthesis=TRUE, sep = ' ') {
  out <- mapply(str.typevar, variables(x), parenthesis=parenthesis)
  out <- str.collapse(names(x), unlist(out), sep = sep)
  return(out)
}
# str.names(z, parenthesis = T)

addSignif <- function(char.vector, word = "signif.") {
  out <- character(0)
  for (i in 1:length(char.vector)) {
    out <- c(out, char.vector[i])
    out <- c(out, paste(char.vector[i], word))
  }
  return(out)
}

list.to.tex <- function(l, star = T) {
  stopifnot(inherits(l, 'list'))
  out <- ''
  n <- length(l)
  nam <- names(l)
  if (n == 0) return(out)
  else {
    out <- paste(out, '\\begin{itemize*}\n')
    
    for (i in 1:n) {
      out <- paste(
        out,
        '\\item \\textbf{',
        nam[i],
        ':} ',
        l[i],
        ' \n', sep = ''
      )
    }
    
    out <- paste(out, '\n', '\\end{itemize*} \n')
    
    return(out)
  }
}
# list.to.tex(list('bon' = 0.4, 'jour' = 'soir'))