#### Clases ####


diccionario = "descargas_mapbiomas/clases_cobertura_terrestre.xlsx" |> 
  readxl::read_excel()


diccionario = diccionario |> 
  dplyr::filter(!is.na(Group))

diccionario = diccionario |> 
  dplyr::mutate(
    Group = Group |>  gsub(pattern = "^[^.]*\\.", replacement = "") |>  stringr::str_squish()
  )




datos = "descargas_mapbiomas/datos_hidalgo.json" |>  
  jsonlite::read_json(simplifyVector = T)




clases = datos |> 
  dplyr::filter(pixelValue %in% diccionario$ID) |> 
  dplyr::left_join(
    y = diccionario |>  dplyr::select(ID, Class),
    by = c("pixelValue" = "ID")
  )


clases = clases |> 
  dplyr::select(
    CVEGEO, MUNICIPIO, Año,  Class, value, Total
  ) |> 
  dplyr::rename(
    AÑO = Año,
    `AREA TOTAL` = Total
  )



clases = clases |> 
  tidyr::pivot_wider(
    names_from = Class,
    values_from = value
  )


clases = clases |> 
  dplyr::relocate(`AREA TOTAL`, .after = dplyr::last_col())

clases = clases |> 
  dplyr::mutate(
    CVEGEO = CVEGEO |>  as.numeric()
  ) |> 
  dplyr::arrange(CVEGEO) |> 
  dplyr::mutate(
    CVEGEO = CVEGEO |>  as.character()
  )


clases |>  openxlsx::write.xlsx("Outputs/Mapbiomas/MAPBIOMAS Cobertura por clase 1985-2025.xlsx")



































#### Grupo ####


diccionario = "descargas_mapbiomas/clases_cobertura_terrestre.xlsx" |> 
  readxl::read_excel()


diccionario = diccionario |> 
  dplyr::filter(!is.na(Group))

diccionario = diccionario |> 
  dplyr::mutate(
    Group = Group |>  gsub(pattern = "^[^.]*\\.", replacement = "") |>  stringr::str_squish()
  )




datos = "descargas_mapbiomas/datos_hidalgo.json" |>  
  jsonlite::read_json(simplifyVector = T)




grupo = datos |> 
  dplyr::filter(pixelValue %in% diccionario$ID) |> 
  dplyr::left_join(
    y = diccionario,
    by = c("pixelValue" = "ID")
  )


grupo = grupo |> 
  dplyr::select(
    CVEGEO, MUNICIPIO, Año,  Class, Group, value, Total
  ) |> 
  dplyr::rename(
    AÑO = Año,
    `AREA TOTAL` = Total
  )


grupo = grupo |> 
  dplyr::select(-Class) |> 
  dplyr::rename(Class = Group)


grupo = grupo |> 
  dplyr::group_by(CVEGEO, MUNICIPIO, AÑO, Class) |> 
  dplyr::summarise(
    value = value |>  sum(na.rm = T)
  ) |> 
  dplyr::ungroup()

grupo = grupo |> 
  tidyr::pivot_wider(
    names_from = Class,
    values_from = value
  )


grupo = grupo |> 
  dplyr::mutate(
    CVEGEO = CVEGEO |>  as.numeric()
  ) |> 
  dplyr::arrange(CVEGEO) |> 
  dplyr::mutate(
    CVEGEO = CVEGEO |>  as.character()
  )


grupo = grupo |> 
  dplyr::mutate(
    ID = paste0(CVEGEO, "_",AÑO)
  )


total = "Outputs/Mapbiomas/MAPBIOMAS Cobertura por clase 1985-2025.xlsx" |> 
  readxl::read_excel()

total = total |> 
  dplyr::mutate(
    ID = paste0(CVEGEO, "_", AÑO)
  ) |> 
  dplyr::select(ID, `AREA TOTAL`)


grupo = grupo  |> 
  dplyr::left_join(
    y = total,
    by = "ID"
  )

grupo = grupo |> 
  dplyr::select(-ID)


grupo = grupo |> 
  dplyr::mutate(
    `Unidad de medida` = "ha"
  )


grupo |>  openxlsx::write.xlsx("Outputs/Mapbiomas/MAPBIOMAS Cobertura por grupo 1985-2025.xlsx")
