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


agricola_modalidad = agricola |> 
  dplyr::select(CVEGEO, Nommodalidad, Sembrada, Cosechada, Valorproduccion)|> 
  dplyr::group_by(CVEGEO, Nommodalidad) |> 
  dplyr::summarise_all(.funs = sum) |> 
  dplyr::ungroup()

agricola_modalidad = agricola_modalidad |> 
  tidyr::pivot_wider(
    id_cols = CVEGEO,
    names_from = Nommodalidad,
    values_from = Sembrada:Valorproduccion,
    names_glue = "{Nommodalidad} { .value}",
    names_vary = "slowest",
    values_fill = 0
  )



agricola = agricola |> 
  dplyr::mutate(
    Nomcultivo = Nomcultivo |>  
      gsub(pattern = "\\(.*", replacement = "") |> 
      stringr::str_squish() |> 
      paste0(" (", Nomunidad, ")") |> 
      stringr::str_squish()
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


agricola_total = agricola |> 
  dplyr::group_by(CVEGEO) |> 
  dplyr::summarise(
    `Sembrada Total` = Sembrada |>  sum(na.rm = T),
    `Cosechada Total` = Cosechada |>  sum(na.rm = T),
    `Volumen produccion Total` = Volumenproduccion |>  sum(na.rm = T), 
    `Valor produccion Total` = Valorproduccion |>  sum(na.rm = T)
  ) |> 
  dplyr::ungroup()

agricola = agricola |> 
  tidyr::pivot_wider(
     id_cols = CVEGEO,
     names_from = Nomcultivo,
     values_from = Sembrada:Valorproduccion,
     names_glue = "{Nomcultivo} { .value}",
     names_vary = "slowest",
     values_fill = 0
   )



cielo_abierto = "Inputs/Page 5/Cielo abierto.xlsx" |>  readxl::read_excel()

cielo_abierto = cielo_abierto |> 
  dplyr::filter(!is.na(...2)) |> 
  dplyr::select(-`Año: 2025`, -...2) |> 
  dplyr::slice(-1)

cielo_abierto |>  names() = c("Municipio", "Cielo Sembrada", "Cielo Cosechada", "Cielo Valor Produccion")


mun = "../../Importantes_documentos_usar/Municipios/municipiosjair.shp" |>  
  sf::read_sf() |> 
  sf::st_drop_geometry() |>  
  dplyr::select(CVEGEO, NOM_MUN)


mun$NOM_MUN[which(!mun$NOM_MUN %in% cielo_abierto$Municipio)] 

cielo_abierto = cielo_abierto |> 
  dplyr::mutate(
    Municipio = dplyr::case_when(
      Municipio == "Atotonilco El Grande" ~ "Atotonilco el Grande",
      Municipio == "Mineral de La Reforma" ~ "Mineral de la Reforma",
      T ~ Municipio
    )
  )

cielo_abierto = cielo_abierto |> 
  dplyr::inner_join(
      y = mun,
      by = c("Municipio" = "NOM_MUN")
    )

cielo_abierto = cielo_abierto |> 
  dplyr::relocate(CVEGEO, .before = Municipio) |> 
  dplyr::select(-Municipio)




protegida = "Inputs/Page 5/Agricultura protegida.xlsx" |>  
  readxl::read_excel()

protegida = protegida |> 
  dplyr::filter(!is.na(...2))|> 
  dplyr::select(-`Año: 2025`, -...2) |> 
  dplyr::slice(-1)

protegida |>  names() = c("Municipio", "Protegida Sembrada", "Protegida Cosechada", "Protegida Valor Produccion")

protegida = protegida |> 
  dplyr::mutate(
    Municipio = dplyr::case_when(
      Municipio == "Atotonilco El Grande" ~ "Atotonilco el Grande",
      Municipio == "Mineral de La Reforma" ~ "Mineral de la Reforma",
      T ~ Municipio
    )
  )

protegida = protegida |> 
  dplyr::inner_join(
    y = mun,
    by = c("Municipio" = "NOM_MUN")
  )

protegida = protegida |> 
  dplyr::relocate(
    CVEGEO, .before = Municipio
  ) |> 
  dplyr::select(-Municipio)



leche = "Inputs/Page 5/Leche bovino.xlsx" |>  
  readxl::read_excel()

leche = leche |> 
  dplyr::filter(!is.na(...2)) |> 
  dplyr::filter(`Leche Bovino` != "Total") |> 
  dplyr::slice(-c(1,2))

leche = leche |> 
  dplyr::select(`Leche Bovino`, ...2, ...4) |> 
  dplyr::rename(
    Municipio = `Leche Bovino`,
    `Leche produccion` = ...2,
    `Leche valor de produccion` = ...4
  ) 


leche = leche |> 
  dplyr::mutate(
    Municipio = dplyr::case_when(
      Municipio == "Atotonilco El Grande" ~ "Atotonilco el Grande",
      Municipio == "Mineral de La Reforma" ~ "Mineral de la Reforma",
      T ~ Municipio
    )
  )



leche = leche |> 
  dplyr::left_join(
    y = mun,
    by = c("Municipio" = "NOM_MUN")
  ) |> 
  dplyr::select(CVEGEO, `Leche produccion`, `Leche valor de produccion`)



huevos = "Inputs/Page 5/Huevo para plato.xlsx" |>  
  readxl::read_excel()

huevos = huevos |> 
  dplyr::filter(!is.na(...2 )) |> 
  dplyr::filter(`Huevo para plato` != "Total")

huevos = huevos |> 
  dplyr::slice(-1) |> 
  dplyr::select(`Huevo para plato`,...2, ...4) |> 
  dplyr::rename(
    Municipio = `Huevo para plato`,
    `Huevo produccion` = ...2,
    `Huevo valor de produccion` = ...4
  )

huevos = huevos |> 
  dplyr::mutate(
    Municipio = dplyr::case_when(
      Municipio == "Atotonilco El Grande" ~ "Atotonilco el Grande",
      Municipio == "Mineral de La Reforma" ~ "Mineral de la Reforma",
      T ~ Municipio
    )
  )

huevos = huevos |> 
  dplyr::left_join(
    y = mun,
    by = c("Municipio" = "NOM_MUN")
  ) |> 
  dplyr::relocate(CVEGEO, .before = Municipio)



explotacion = "Inputs/Page 5/Unidades y viviendas y explotacion.xlsx" |> 
  readxl::read_excel()

explotacion = explotacion |> 
  dplyr::filter(`INEGI. Censo Agropecuario 2022` == "13 HGO")

explotacion = explotacion |> 
  dplyr::filter(
    ! ...4 %in% c("Unidades de producción con cría de animales", "Viviendas con cría de animales")
  )

explotacion = explotacion |> 
  dplyr::select(...4, ...5)


explotacion = explotacion |> 
  dplyr::rename(
    Municipio = ...4,
    `Unidades de producción y viviendas con cría y explotación de animales`= ...5
  )


tidyr::pivo
