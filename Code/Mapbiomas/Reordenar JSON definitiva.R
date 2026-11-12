archivos = list.files(
  path = "descargas_mapbiomas/",
  full.names = TRUE
)

archivos = archivos[!grepl("información", basename(archivos))]
archivos = archivos[!grepl("diccionario", basename(archivos))]




diccionario = "descargas_mapbiomas/diccionario_hidalgo.json" |> 
  jsonlite::read_json(simplifyVector = T)

diccionario = diccionario |> 
  dplyr::select(archivo, CVEGEO, MUNICIPIO) |> 
  unique()


hidalgo = paste0("descargas_mapbiomas/", diccionario$archivo, ".json") 
archivos = archivos[archivos%in% hidalgo]


guardar =  NULL

for (i in 1:length(archivos)) {
  
  datos = archivos[i] |>  jsonlite::read_json(simplifyVector = T)
  
  df = datos[["statistic"]]
  
  municipio = NULL
  anio = NULL
  
  for (j in 1:nrow(df)) {
    df_valores = df[[3]][[j]]
    
    df_valores = df_valores |> 
      dplyr::mutate(
        Año = df[[1]][[j]],
        Total = df[[2]][[j]]
      )
    
    anio = anio |> 
      dplyr::bind_rows(df_valores)
  }
  
  municipio = municipio |> 
    dplyr::bind_cols(anio) |> 
    dplyr::mutate(
      id_mapbiomas = archivos[i] |>  
        basename() |>  
        gsub(pattern = ".json", replacement = "") |> 
        stringr::str_squish()
    ) 
  
  
  guardar = guardar |>  
    dplyr::bind_rows(municipio)
  
}



guardar = guardar |> 
  dplyr::left_join(
    y = diccionario,
    by = c("id_mapbiomas" = "archivo")
  )

guardar = guardar |> 
  dplyr::mutate(
    CVEGEO = CVEGEO |>  as.numeric()
  ) |> 
  dplyr::arrange(
    CVEGEO, Año
  ) |> 
  dplyr::mutate(
    CVEGEO = CVEGEO |>  as.character()
  )

guardar = guardar |> 
  dplyr::select(
    CVEGEO, MUNICIPIO, Año, Total, pixelValue, value, id_mapbiomas
  ) |> 
  dplyr::rename(
    Clase_id = pixelValue
  )



guardar |>  jsonlite::write_json(
  "descargas_mapbiomas/datos_hidalgo.json", 
  pretty = T, 
  auto_unbox = T
  )








