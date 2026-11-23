pecuaria = "Inputs/Page 5/Cierre de la Producción Pecuaria 2025.csv" |> 
  read.csv()


pecuaria = pecuaria |> 
  dplyr::filter(Cveestado == 13)


pecuaria = pecuaria |> 
  dplyr::mutate(
    CVEGEO = paste0(Cveestado, Cvempio |>  stringr::str_pad(width = 3, side = "left", pad = "0")) 
  ) |> 
  dplyr::select(
    CVEGEO, Nomespecie, Nomproducto, Volumen:Asacrificado
    )


pecuaria = pecuaria |> 
  dplyr::mutate(
    Producto = paste(Nomespecie, Nomproducto) |>  stringr::str_squish()
  ) |> 
  dplyr::select(-Nomespecie, -Nomproducto) |> 
  dplyr::relocate(Producto, .after = CVEGEO)


pecuaria = pecuaria |> 
  dplyr::mutate(CVEGEO = CVEGEO |> as.numeric()) |> 
  dplyr::arrange(CVEGEO) |> 
  dplyr::mutate(CVEGEO = CVEGEO |>  as.character())



pecuaria = pecuaria |> 
  tidyr::pivot_wider(
    id_cols = CVEGEO,
    names_from = Producto,
    values_from = c(Volumen, Peso, Precio, Valor, Asacrificado),
    names_glue = "{Producto} { .value }"
  )


agricola = "Inputs/Page 5/Cierre_agricola_mun_2025.csv" |> 
  read.csv(fileEncoding = "latin1")

agricola = agricola |> 
  dplyr::filter(Idestado == 13) |> 
  dplyr::mutate(
    CVEGEO = paste0(Idestado, Idmunicipio |> stringr::str_pad(width = 3, side = "left", pad = "0"))
  ) |> 
  dplyr::select(
    CVEGEO, Nomcicloproductivo, Nommodalidad, Nomunidad, Nomcultivo:Valorproduccion 
    )



agricola = agricola |> 
  dplyr::group_by(CVEGEO, Nomcultivo) |> 
  dplyr::summarise(
    Sembrada = Sembrada |>  sum(na.rm = T),
    Cosechada = Cosechada |>  sum(na.rm = T),
    Volumenproduccion = Volumenproduccion |>  sum(na.rm = T),
    Valorproduccion = Valorproduccion |>  sum(na.rm = T)
  ) |> 
  dplyr::ungroup()


agricola = agricola |> 
  dplyr::mutate(CVEGEO = CVEGEO |>  as.numeric()) |> 
  dplyr::arrange(CVEGEO, Nomcultivo) |> 
  dplyr::mutate(CVEGEO = CVEGEO |>  as.character())
