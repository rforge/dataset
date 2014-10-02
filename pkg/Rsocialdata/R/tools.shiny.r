# path <- '/home/emmanuelrdataset/r-forge/svn-working-copy/dataset/pkg/Rsocialdata'
# app.name <- 'shiny-test-1'
# packages.ui <- c('rpart', 'party')
# packages.server <- c('rpart', 'party')
# 
# shiny.app <- shiny.write.app(
#   folder = path,
#   app = app.name
# )
# 
# shiny.write.ui(
#   title = 'Tree with party!',
#   sidebarPanel =  list(
#     "checkboxInput('Wind', 'Wind', TRUE)",
#     "sliderInput('obs', 'Number of observations:', min = 0, max = 100, value = 5)",
#     "sliderInput('depth', 'Maximal depth:', min = 0, max = 10, value = 3)"
#   ),
#   mainPanel = list(
#     "plotOutput('distPlot')"  
#   ),
#   app = shiny.app,
#   packages = packages.ui
# )
# 
# shiny.write.server.intro(
#   app = shiny.app,
#   packages = packages.server
# )
# 
# cat("output$distPlot <- renderPlot({", ' \n', file = shiny.app$server, append=T)
# cat("  airq <- subset(airquality, !is.na(Ozone))", ' \n', file = shiny.app$server, append=T)
# cat("  var <- names(airq)", ' \n', file = shiny.app$server, append=T)
# cat("  var <- var[-which(var == 'Ozone')]", ' \n', file = shiny.app$server, append=T)
#   
# cat("  if(!input[['Wind']]) var <- var[-which(var == 'Wind')]", ' \n', file = shiny.app$server, append=T)
#   
# cat("  formula.descriptors <- paste(var, collapse = ' + ')", ' \n', file = shiny.app$server, append=T)
#   #     cat(formula.descriptors)
# cat("  tr <- ctree(", ' \n', file = shiny.app$server, append=T)
# cat("    as.formula(paste('Ozone', ' ~ ', formula.descriptors)),", ' \n', file = shiny.app$server, append=T)
# cat("    data = airq, ", ' \n', file = shiny.app$server, append=T)
# cat("    controls = ctree_control(", ' \n', file = shiny.app$server, append=T)
# cat("      maxsurrogate = 3,", ' \n', file = shiny.app$server, append=T)
# cat("      minbucket = input$obs,", ' \n', file = shiny.app$server, append=T)
# cat("      maxdepth = input$depth", ' \n', file = shiny.app$server, append=T)
# cat("    )", ' \n', file = shiny.app$server, append=T)
# cat("  )", ' \n', file = shiny.app$server, append=T)
# cat("  plot(tr)", ' \n', file = shiny.app$server, append=T)
# cat("})", ' \n', file = shiny.app$server, append=T)
# 
# shiny.write.server.outro(
#   app = shiny.app
# )
# 
# shiny.run(shiny.app)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

shiny.write.app <- function(folder, app) {
  path <- file.path(folder, app)
  dir.create(path, showWarnings=F)
  
  launcher <- 'app.launcher.R'
  launcher.path <- file.path(path, launcher)
  
  cat('setwd(\"', path, '\")', '\n', file = launcher.path, sep='')
  cat('library(shiny)', '\n', file = launcher.path, append=T)
  cat("runApp('./')", '\n', file = launcher.path, append=T)
  
  return(list(
    'path' = path,
    'server' = file.path(path, 'server.R'),
    'ui' = file.path(path, 'ui.R')
  ))
}


shiny.write.ui <- function(
  title = "Untitled",
  sidebarPanel =  list(),
  mainPanel = list(),
  app,
  packages
) {
#   cat('', ' \n', file = file)
  if(missing(app)) stop("You have to provide a shiny-app folder")
  
  file <- app$ui
  
  cat('library(shiny)', ' \n\n',  file = file)
  if(!missing(packages)) {
    for (i in packages)
      cat('library(', i, ')', ' \n', file = file, append=T, sep = '')
    
    cat(' \n', file = file, append=T, sep = '')
  }
  
  cat('shinyUI(', ' \n', file = file, append=T)
  cat('  pageWithSidebar(', ' \n\n', file = file, append=T)
  cat('    headerPanel(\"', title, '\"),', ' \n\n', file = file, append=T, sep = '')
  
  s <- length(sidebarPanel)
  if(s > 0) {
    cat('    sidebarPanel(', ' \n', file = file, append=T)
    for (i in 1:s) {
      cat('    ', sidebarPanel[[i]], file = file, append=T)
      if (i==s) cat(' \n', file = file, append=T)
      else cat(', \n\n', file = file, append=T)
    }
    cat('    ),', ' \n', file = file, append=T)
  }

  
  m <- length(mainPanel)
  if(m > 0) {
    cat('    mainPanel(', ' \n', file = file, append=T)
    for (i in 1:m) {
      cat('    ', mainPanel[[i]], file = file, append=T)
      if (i==m) cat(' \n', file = file, append=T)
      else cat(', \n\n', file = file, append=T)
    }
    cat('    )', ' \n', file = file, append=T)
  }
  
  
  cat('  )', ' \n', file = file, append=T)
  cat(')', ' \n', file = file, append=T)
}

shiny.write.server.intro <- function(
  app,
  packages
) {
  #   cat('', ' \n', file = file)
  if(missing(app)) stop("You have to provide a shiny-app folder")
  
  file <- app$server
  
  cat('setwd(\"', app$path, '\")', '\n\n', file = file, sep='')
  cat('library(shiny)', ' \n\n',  file = file)
  if(!missing(packages)) {
    for (i in packages)
      cat('library(', i, ')', ' \n', file = file, append=T, sep = '')
    
    cat(' \n', file = file, append=T, sep = '')
  }
  
  cat('shinyServer(function(input, output) {', ' \n', file = file, append=T)

}

shiny.write.server.outro <- function(
  app
) {
  #   cat('', ' \n', file = file)
  if(missing(app)) stop("You have to provide a shiny-app folder")
  
  file <- app$server
    
  cat('})', ' \n', file = file, append=T)
  
}

shiny.run <- function(app) {
 path <- app$path
 source(file.path(app$path, 'app.launcher.R'))
}