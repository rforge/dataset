# gchart.map

gchart.map <- function(x, name) {
  
  n <- length(x)
  na <- names(x)
  
  nameh <- paste(name, ".html", sep = "")
  cat("<html> \n" , file = nameh, append = F)
  cat("<head> \n" , file = nameh, append = T)
  cat("  <script type='text/javascript' src='https://www.google.com/jsapi'></script> \n" , file = nameh, append = T)
  cat("  <script type='text/javascript'> \n" , file = nameh, append = T)
  cat("   google.load('visualization', '1', {'packages': ['geomap']}); \n" , file = nameh, append = T)
  cat("   google.setOnLoadCallback(drawMap); \n" , file = nameh, append = T)
  cat("\n" , file = nameh, append = T)
  cat("    function drawMap() { \n" , file = nameh, append = T)
  cat("      var data = new google.visualization.DataTable(); \n" , file = nameh, append = T)
  cat("      data.addRows(", n, "); \n" , file = nameh, append = T)
  cat("      data.addColumn('string', 'Canton'); \n" , file = nameh, append = T)
  cat("      data.addColumn('number', 'Percent'); \n" , file = nameh, append = T)
  
  for(i in 1:n) {
   cat("      data.setValue(", i-1, ", 0, '", na[i], "'); \n" , file = nameh, append = T, sep = "")
   cat("      data.setValue(", i-1, ", 1, ", x[i], "); \n" , file = nameh, append = T, sep = "")
  }
  #cat("      data.setValue(0, 0, 'Geneva'); \n" , file = nameh, append = T)
  #cat("      data.setValue(0, 1, 20.2); \n" , file = nameh, append = T)
  #cat("      data.setValue(1, 0, 'Zurich'); \n" , file = nameh, append = T)
  #cat("      data.setValue(1, 1, 300); \n" , file = nameh, append = T)
  #cat("      data.setValue(2, 0, 'Valais'); \n" , file = nameh, append = T)
  #cat("      data.setValue(2, 1, 200); \n" , file = nameh, append = T)
  
  cat("\n" , file = nameh, append = T)
  cat("      var options = {}; \n" , file = nameh, append = T)
  cat("      options['region'] = 'CH'; \n" , file = nameh, append = T)
  cat("      options['dataMode'] = 'regions'; \n" , file = nameh, append = T)
  cat("      //options['colors'] = [0xFF8747, 0xFFB581, 0xc06000]; //orange colors \n" , file = nameh, append = T)
  cat("\n" , file = nameh, append = T)
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