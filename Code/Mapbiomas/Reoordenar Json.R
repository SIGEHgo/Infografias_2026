for (i in vector) {
  
  jason = "descargas_mapbiomas/37-3-400.json" |>  jsonlite::read_json(simplifyVector = T) 
  interes = jason[["statistic"]]
  
  for (j in 1:nrow(interes)) {
    j = 1
    valores = interes[[3]][[j]]
  }
  
  
  
  
  
  
  
}




