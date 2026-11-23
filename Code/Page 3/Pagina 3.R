#### Pagina 3 ####

#### Seguridad ####




#### Secretariado ####

source("../../Importantes_documentos_usar/Conexion_Postgres_CITNOVA.R")

secretariado = DBI::dbReadTable(
  conn = buig, 
  name = "Incidencia del fuero común Secretariado Hidalgo 2025"
  )


secretariado = secretariado |> 
  dplyr::mutate(
    Tipo.de.delito = dplyr::if_else(
      condition = Tipo.de.delito == "Robo",
      true = Subtipo.de.delito,
      false = Tipo.de.delito
    )
  )


secretariado = secretariado |> 
  dplyr::group_by(Cve..Municipio, Municipio, Tipo.de.delito) |> 
  dplyr::summarise(
    Total = Total |> sum(na.rm = T)
  ) |> 
  dplyr::ungroup()


secretariado = secretariado |> 
  tidyr::pivot_wider(
    names_from = Tipo.de.delito,
    values_from = Total
  )


secretariado = secretariado |> 
  dplyr::mutate(
    `Total de delitos` =  rowSums(dplyr::across(Aborto:`Violencia familiar`), na.rm = TRUE)
  ) |>  
  dplyr::relocate(`Total de delitos`, .after = Municipio)



secretariado = secretariado |> 
  dplyr::relocate(Homicidio, .after = `Total de delitos`) |> 
  dplyr::relocate(Secuestro, .after = Homicidio) |> 
  dplyr::relocate(Lesiones, .after = Secuestro) |> 
  dplyr::relocate(Extorsión, .after = Lesiones) |> 
  dplyr::relocate(Feminicidio, .after = Extorsión) |> 
  dplyr::relocate(Narcomenudeo, .after = Feminicidio) |> 
  dplyr::relocate(`Robo a casa habitación`, .after = Narcomenudeo) |> 
  dplyr::relocate(`Robo de vehículo automotor`, .after = `Robo a casa habitación`) |> 
  dplyr::relocate(`Robo a negocio`, .after = `Robo de vehículo automotor`) |>
  dplyr::relocate(`Violación simple`, .after = `Robo a negocio`) |> 
  dplyr::relocate(`Violación equiparada`, .after = `Violación simple`) |> 
  dplyr::relocate(`Violencia familiar`, .after = `Violación equiparada`)



secretariado = secretariado |> 
  dplyr::mutate(
    `Otros delitos` = rowSums(dplyr::across(Aborto:`Violencia de género en todas sus modalidades distinta a la violencia familiar`), na.rm = TRUE)
  ) |> 
  dplyr::relocate(`Otros delitos`, .after = `Violencia familiar`)


secretariado = secretariado |> 
  dplyr::select(`Cve..Municipio`:`Otros delitos`)

secretariado = secretariado |> 
  dplyr::rename(Violación = `Violación simple`) |> 
  dplyr::mutate(
    Violación = Violación + `Violación equiparada`
  ) |> 
  dplyr::select(-`Violación equiparada`) |> 
  dplyr::relocate(Violación, .after = `Robo a negocio`)|> 
  dplyr::rename(CVEGEO = Cve..Municipio)|> 
  dplyr::select(-Municipio)


secretariado = secretariado |> 
  dplyr::mutate(
    CVEGEO = CVEGEO |>  as.numeric()
  ) |> 
  dplyr::arrange(CVEGEO) |> 
  dplyr::mutate(
    CVEGEO = CVEGEO |>  as.character()
  )




#### Personal de seguridad activo ####}

personal_seguridad = "Inputs/Page 3/Personal de seguridad activo/conjunto_de_datos_rec_huma_sp_cngmd2025_csv/conjunto_de_datos/m3s1p33_cngmd2025.csv" |> 
  read.csv()

personal_seguridad = personal_seguridad |> 
  dplyr::filter(cve_ent == 13) |> 
  dplyr::group_by(cvegeo) |> 
  dplyr::summarise(
    `Personal de seguridad` = sexostt |> as.numeric() |>  sum(na.rm = T),
    `Personal de seguridad Hombres` = sexos1 |>  as.numeric() |>  sum(na.rm = T),
    `Personal de seguridad Mujeres` = sexos2 |>  as.numeric() |>  sum(na.rm = T),
    
  )

personal_seguridad = personal_seguridad |> 
  dplyr::rename(CVEGEO = cvegeo)

personal_seguridad = personal_seguridad |> 
  dplyr::mutate(CVEGEO = CVEGEO |> as.numeric()) |> 
  dplyr::arrange(CVEGEO) |> 
  dplyr::mutate(CVEGEO = CVEGEO |>  as.character())




#### Puestas a disposicion de personas ####

disposicion = "Inputs/Page 3/Puestas a disposicion/conjunto_de_datos_puesta_disposic_cngmd2025_csv/conjunto_de_datos/m3s2p1_cngmd2025.csv" |> 
  read.csv()

disposicion = disposicion |> 
  dplyr::filter(cve_ent == 13)  |> 
  dplyr::filter(aniorefe_d == 2)

disposicion = disposicion |> 
  dplyr::select(cvegeo, pudisptt, pudisp1, pudisp2) |> 
  dplyr::rename(
    CVEGEO = cvegeo,
    `Puestas a disposicion total`= pudisptt,
    `Puestas a disposicion ante la autoridad de justicia civica` = pudisp1,
    `Puestas a disposicion ante el ministerio publico` = pudisp2
  ) |> 
  dplyr::mutate(CVEGEO = CVEGEO |>  as.numeric()) |> 
  dplyr::arrange(CVEGEO) |> 
  dplyr::mutate(CVEGEO = CVEGEO |>  as.character())

#### Llamadas recibidas al 911 durante 2025 ####

llamadas = "Inputs/Page 3/Llamadas 911/conjunto_de_datos_centro_aten_sist_telef_cngmd2025_csv/conjunto_de_datos/m3s1p145b_cngmd2025.csv" |> 
  read.csv()

llamadas = llamadas |> 
  dplyr::filter(cve_ent == 13) |>
  dplyr::mutate(cvegeo = cvegeo |>  as.numeric()) |> 
  dplyr::arrange(cvegeo) |> 
  dplyr::mutate(cvegeo = cvegeo |>  as.character())


llamadas = llamadas |> 
  dplyr::mutate(llamad1 = llamad1 |>  as.numeric()) |> 
  dplyr::group_by(cvegeo)  |> 
  dplyr::summarise(`Llamadas 911 procedentes`= llamad1 |>  sum(na.rm = T))


#### Justicia ####

justicia_dispocision = "Inputs/Page 3/Justicia/Infografías_nov_2026.xlsx" |> 
  readxl::read_excel(sheet = 5)

justicia_dispocision = justicia_dispocision |> 
  dplyr::select(
    `Clave INEGI`, 
    `Procedimientos \r\nadministrativos`, 
    `A disposición de la \r\npolicía municipal`,
    `A disposición de la \r\npolicía estatal`,
    `A disposición de la \r\nGuardia Nacional`
  ) |> 
  dplyr::mutate(`Clave INEGI` = `Clave INEGI` |>  as.numeric()) |> 
  dplyr::arrange(`Clave INEGI`) |> 
  dplyr::mutate(`Clave INEGI` = `Clave INEGI` |> as.character()) |> 
  dplyr::rename(
    `CVEGEO` = `Clave INEGI`,
    `Procedimientos administrativos` = `Procedimientos \r\nadministrativos`,
    `A disposición de la policía municipal` = `A disposición de la \r\npolicía municipal`,
    `A disposición de la policía estatal` = `A disposición de la \r\npolicía estatal`,
    `A disposición de la Guardia Nacional` = `A disposición de la \r\nGuardia Nacional`
  )


#### Medio ambiente ####

#### Areas Naturales ####

areas = "Inputs/Page 3/Areas naturales/Tablas_paginas_15_y_16_ROP_PCEANP_2026.xlsx" |> 
  readxl::read_excel()

areas |>  names() = areas[2,]

areas = areas |> 
  dplyr::filter(!is.na(`Categoría y Nombre`)) |> 
  dplyr::slice(-1) |> 
  dplyr::mutate(
    `Categoría y Nombre` = `Categoría y Nombre` |> 
      gsub(pattern = "^ZPE\\b", replacement = "Zona de Preservación Ecológica") |> 
      gsub(pattern = "^REE\\b", replacement = "Reserva Ecológica Estatal") |>
      gsub(pattern = "^PE\\b", replacement = "Parque Estatal") |>
      stringr::str_squish()
  )


mun = "../../Importantes_documentos_usar/Municipios/municipiosjair.shp" |> 
  sf::read_sf() |>  
  sf::st_drop_geometry() |>  
  dplyr::select(CVEGEO, NOM_MUN) |> 
  dplyr::mutate(CVEGEO = CVEGEO |>  as.numeric()) |> 
  dplyr::arrange(CVEGEO) |> 
  dplyr::mutate(CVEGEO = CVEGEO |>  as.character())

homologar_areas = fuzzyjoin::stringdist_join(
    x = areas |>  dplyr::select(Municipio),
    y = mun |>  dplyr::select(NOM_MUN, CVEGEO),
    by = c("Municipio" = "NOM_MUN"),
    mode = "left",
    ignore_case = F,
    method = "jw",
    max_dist = 99,
    distance_col = "distancia"
  ) |> 
  dplyr::group_by(Municipio) |> 
  dplyr::slice_min(order_by = distancia, n = 1) |> 
  unique() |> 
  dplyr::ungroup()
  

homologar_areas = homologar_areas |> 
  dplyr::mutate(CVEGEO = CVEGEO |>  as.numeric()) |> 
  dplyr::arrange(CVEGEO) |> 
  dplyr::mutate(CVEGEO = CVEGEO |>  as.character()) |> 
  dplyr::select(-distancia)

areas = areas |> 
  dplyr::left_join(
    y = homologar_areas,
    by = "Municipio"
  )

areas = areas |> 
  dplyr::select(CVEGEO, `Categoría y Nombre`, `Sup. (Ha)`) |> 
  dplyr::mutate(`Sup. (Ha)` = `Sup. (Ha)` |>  as.numeric()) |> 
  dplyr::rename(`Area natural protegida`= `Categoría y Nombre`) |> 
  dplyr::group_by(CVEGEO) |> 
  dplyr::summarise(
    `Area natural protegida` = paste0(`Area natural protegida`, collapse = ", "),
    `Superficie en Ha` = paste0(`Sup. (Ha)`, collapse = ", "),
    `Numero de areas naturales protegidas` = dplyr::n(),
    `Superficie en Ha total` = sum(`Sup. (Ha)`, na.rm = T)
  )
 

#### Recoleccion de residuos solidos ####

residuos = "Inputs/Page 3/Residuos solidos urbanos/cngmd2025_resid_solid.xlsx" |> 
  readxl::read_excel(sheet = 6) |> 
  dplyr::select(dplyr::where(~ !all(is.na(.))))  #Borrar las columnas que son todo NA



residuos |>  names() = paste(
  residuos[4,] |>  
    as.vector() |>  
    zoo::na.locf(na.rm = T) |>  
    unlist() |>  
    as.character(),
  residuos[5,]
) |> 
  gsub(pattern = "NA", replacement = "") |>
  gsub(pattern = "\r\n|\r|\n", replacement = " ") |> 
  stringr::str_squish()
  
  

residuos = residuos |> 
  dplyr::filter(CVE_ENT == "13") |> 
  dplyr::select(CVEGEO, `Cantidad promedio diaria de residuos sólidos urbanos recolectados`:`Sistema de recolección Sistema de contenedores`) |> 
  dplyr::mutate(CVEGEO = CVEGEO |>  as.numeric()) |> 
  dplyr::arrange(CVEGEO) |> 
  dplyr::mutate(CVEGEO = CVEGEO |> as.character()) |> 
  dplyr::filter(CVEGEO != "13000")
  




  







