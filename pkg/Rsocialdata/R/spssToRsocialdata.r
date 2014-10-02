# pathToFolder <- "~/SHP 2012/SHP-Data/SPSS"
# setwd(pathToFolder)
# setwd('~/Documents/')
# spssToRsocialdata(pathToFolder)
spssToRsocialdata <- function(pathToFolder, recursive = TRUE, max.value.labels = Inf) {
  
  ptm <- proc.time()
  
  path.current <- getwd()
  pathToFolder <- path.expand(pathToFolder)
  
  dirs <- list.dirs(
    path = pathToFolder,
    recursive = recursive
  )
  dirs <- dirs[-1]
  dirs <- sub(
    paste('^',pathToFolder, .Platform$file.sep, sep=''),
    '',
    dirs
  )
  dirs <- gsub(
    'spss',
    'R',
    dirs,
    ignore.case=T
  )
  Rfolder <- 'R-Rsocialdata-files'
  #dir.create(paste(pathToFolder, '/', Rfolder, sep=''))
  dirs <- paste(
    pathToFolder,
    Rfolder,
    dirs,
    sep=.Platform$file.sep
  )
  mapply(dir.create, dirs, recursive=T)
  
  files <- list.files(
    path = pathToFolder,
    pattern='.\\.sav$',
    include.dirs = F,
    recursive = recursive
  )
  files.RData <- sub('\\.sav$', '.RData', files, ignore.case = TRUE)
  files.RData <- gsub('spss', 'R', files.RData, ignore.case = TRUE)
  
  for(i in 1:length(files)) {
    s <- unlist(strsplit(files[i], .Platform$file.sep))
    filename <- s[length(s)]
    file.dir <- paste(s[-length(s)], collapse=.Platform$file.sep)
    
    data <- get.spss.file(
      paste(pathToFolder, .Platform$file.sep, file.dir,.Platform$file.sep, sep=''),
      filename,
      max.value.labels = max.value.labels
    )
    
    assign(filename, data)
    s <- file.path(pathToFolder, Rfolder, files.RData[i])
    #save(filename, file = s)
    eval(call('save', filename, file = s))
    #do.call('save', list(get(filename), file = s))
    #setwd('~/Documents/')
    #save(s, file='bob.RData')
    
    s <- unlist(strsplit(s, .Platform$file.sep))
    file.dir <- paste(s[-length(s)], collapse=.Platform$file.sep)
    
    setwd(file.dir)
    exportPDF(get(filename), dateformat = "%Y/%m/%d", description.chlength = 160, values.chlength = 12)
    
  }
  setwd(path.current)
  
  duration <- proc.time() - ptm
  message(paste("Duration:", duration[1]))
}