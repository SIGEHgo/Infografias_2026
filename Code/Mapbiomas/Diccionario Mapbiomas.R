archivos = list.files(
  path = "descargas_mapbiomas/",
  pattern = "información",
  full.names = TRUE
)

diccionario = NULL


for (i in 1:length(archivos)) {
  
  nombre_archivo = archivos[i] |>  
    basename() |>  
    gsub(pattern = "información.json", replacement = "") |>  
    stringr::str_squish()
  
  
  informacion = archivos[i] |> 
    jsonlite::read_json(simplifyVector = T)
  
  informacion = informacion[["territory"]]
  
  dataf = data.frame(
    archivo = nombre_archivo,
    informacion = informacion[["nameFormatted"]]
  )
  
  
  diccionario = diccionario |> 
    dplyr::bind_rows(dataf)
  
}


diccionario = diccionario |> 
  tidyr::separate_wider_delim(
    cols = informacion,
    delim = " - ",
    names = c("MUNICIPIO", "ESTADO", "TIPO", "CVEGEO")
  )


diccionario = diccionario |> 
  dplyr::select(-TIPO) |> 
  dplyr::relocate(
    ESTADO, .before = MUNICIPIO
  ) |> 
  dplyr::relocate(
    CVEGEO, .before = ESTADO
  )
 
hidalgo = diccionario |> 
  dplyr::filter(ESTADO == "Hidalgo") |> 
  dplyr::mutate(
    CVEGEO = CVEGEO |> as.numeric()
  ) |> 
  dplyr::arrange(CVEGEO) |> 
  dplyr::mutate(
    CVEGEO = CVEGEO |>  as.character()
  )


hidalgo |>  jsonlite::write_json("descargas_mapbiomas/diccionario_hidalgo.json", pretty = T, auto_unbox = T)


