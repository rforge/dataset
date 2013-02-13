# gchart.map
# 
# example
# shpp <- get.spss.file(
#   file = 'SHP10_P_USER.sav',
#   datadir = '/home/emmanuelrdataset/SHP 2012/SHP-Data/SPSS/SHP-Data-W1-W12-SPSS/W12_2010/',
#   name = 'SHP wave 2010'
# )
# contains("canton", shpp)
# shph <- get.spss.file(
#   file = 'SHP10_H_USER.sav',
#   datadir = '/home/emmanuelrdataset/SHP 2012/SHP-Data/SPSS/SHP-Data-W1-W12-SPSS/W12_2010/',
#   name = 'SHP wave 2010'
# )
# exportPDF(shph)
# 
# c <- missings(shph)
# b <- lapply(variables(shph), nmissings)
# a <- variables(shph)
# length(a)
# length(b)
# shph$canton10
# shph[['canton10']]
# do.call(getMethod("show", "Variable"), list(shph$canton10))
# class(getMethod("show", "Variable"))
# a <- shp
# 
# 
# a <- contains("canton", shph)
# 
# shp <- merge(shpp, shph, by='idhous10')
# 
# shp$wp10t1s <- wvar(shp$wp10t1s)
# weighting(shp) <- 'wp10t1s'
# 
# # exportPDF(shp)
# 
# spatial.country(shp) <- 'CH'
# spatial.country(shp)
# 
# # unique(v(shp$canton10))
# 
# shp$canton10.short <- rename(
#   shp$canton10,
#   'GE  Geneva' = 'Geneva',
#   'VD  Vaud' = 'Vaud',
#   'VS  Valais' = 'Valais',
#   'FR  Fribourg' = 'Fribourg',
#   'NE  Neuchatel' = 'Neuchatel',
#   'AG  Argovia' = 'Argovia',
#   'SO  Solothurn' = 'Solothurn',
#   'BE  Berne' = 'Berne',
#   'TI  Ticino' = 'Ticino',
#   'LU  Lucerne' = 'Lucerne',
#   'BS  Basle-Town' = 'Basle-Town',
#   'BL  Basle-Country' = 'Basle-Country',
#   'ZH  Zurich' = 'Zurich',
#   'SG  St. Gall' = 'St. Gall',
#   'ZG  Zug' = 'Zug',
#   'SZ  Schwyz' = 'Schwyz',
#   'TG Thurgovia' = 'Thurgovia',
#   'GR  Grisons' = 'Grisons',
#   'AR  Appenzell Outer-Rhodes' = 'Appenzell Outer-Rhodes',
#   'SH  Schaffhausen' = 'Schaffhausen',
#   'GL  Glarus' = 'Glarus',
#   'AI  Appenzell Inner-Rhodes' = 'Appenzell Inner-Rhodes',
#   'NW  Nidwalden' = 'Nidwalden',
#   'UR  Uri' = 'Uri',
#   'OW  Obwalden' = 'Obwalden',
#   'JU  Jura' = 'Jura'
# )
#                              
# spatial.variable(shp) <- 'canton10.short'
# spatial.variable(shp)
# 
# gchart.map('age10', shp)
# gchart.map('age10', shp, dataMode = 'markers') #not run
# 
# 


gchart.map <- function(
  varname,
  data,
  filename = 'gchart.map',
  legend = varname,
  dataMode = 'regions'
) { # varname: Scale variable

  if(missing(varname)) stop("You have to provide the name of the variable you want to plot")
  if(missing(data)) stop("You have to provide the database")
  
  if(missing(legend))
    legend <- varname
  
  if(missing(dataMode))
    legend <- 'regions'
  
  sp.v <- spatial.variable(data)
  stopifnot(spatial.variable(data) %in% names(data))
  
  df <- v(data[, c(varname, sp.v)])
  df <- df[complete.cases(df),]
  agg <- do.call('aggregate', list(
    'formula' = as.formula(paste(varname, ' ~ ', sp.v)),
    'data' = df,
    'FUN' = mean))
  
  #----------------------------------------------------------------
  # creating the file
  #----------------------------------------------------------------
  nameh <- paste(filename, ".html", sep = "")
  #----------------------------------------------------------------
  # head of Google file
  #----------------------------------------------------------------
  
  cat("<!DOCTYPE html> \n" , file = nameh, append = F)
  cat("<html> \n" , file = nameh, append = T)
  cat("<head> \n" , file = nameh, append = T)
  cat("  <script type='text/javascript' src='https://www.google.com/jsapi'></script> \n" , file = nameh, append = T)
  cat("  <script type='text/javascript'> \n" , file = nameh, append = T)
  cat("   google.load('visualization', '1', {'packages': ['geomap']}); \n" , file = nameh, append = T)
  cat("   google.setOnLoadCallback(drawMap); \n" , file = nameh, append = T)
  cat("\n" , file = nameh, append = T)
  cat("    function drawMap() { \n" , file = nameh, append = T)
  cat("      var data = google.visualization.arrayToDataTable([ \n" , file = nameh, append = T)
  
  
  
  #----------------------------------------------------------------
  # DATA
  #----------------------------------------------------------------
  cat("        ['field.used?', '", legend ,"'],   \n" , file = nameh, append = T, sep = "")
  
  for (i in 1:nrow(agg)){
    cat("        ['", as.character(agg[i,1]) ,"', ", agg[i,2], "],   \n" , file = nameh, append = T, sep='')
  }
#   cat("        ['Geneva', 200],   \n" , file = nameh, append = T)
#   cat("        ['St. Gall', 700]   \n" , file = nameh, append = T)

  cat("      ]); \n" , file = nameh, append = T)
  #----------------------------------------------------------------
  # data type
  #----------------------------------------------------------------
  sp.c <- spatial.country(data)
  if(length(sp.c) != 1) stop("Bad spatial.country() output, please check the manual.")
  cat("\n" , file = nameh, append = T)
  cat("      var options = {}; \n" , file = nameh, append = T)
  cat("      options['region'] = '", sp.c ,"'; \n" , file = nameh, append = T, sep='')
  cat("      options['dataMode'] = '", dataMode ,"'; \n" , file = nameh, append = T, sep='')
  
  #----------------------------------------------------------------
  # colors
  #----------------------------------------------------------------
  cat("      //options['colors'] = [0xFF8747, 0xFFB581, 0xc06000]; //orange colors \n" , file = nameh, append = T)
  cat("\n" , file = nameh, append = T)
  
  
  
  
  #----------------------------------------------------------------
  # tail of Google file
  #----------------------------------------------------------------
  cat("      var container = document.getElementById('map_canvas'); \n" , file = nameh, append = T)
  cat("      var geomap = new google.visualization.GeoMap(container); \n" , file = nameh, append = T)
  cat("      geomap.draw(data, options); \n" , file = nameh, append = T)
  cat("    }; \n" , file = nameh, append = T)
  
  cat("  </script> \n" , file = nameh, append = T)
  cat("</head> \n" , file = nameh, append = T)
  cat("\n" , file = nameh, append = T)
  cat("<body> \n" , file = nameh, append = T)
  cat("    <div id='map_canvas'></div> \n" , file = nameh, append = T)
  cat("</body> \n" , file = nameh, append = T)
  cat("\n" , file = nameh, append = T)
  cat("</html> \n" , file = nameh, append = T)
  
}