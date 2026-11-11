source("../../Importantes_documentos_usar/Conexion_Postgres_CITNOVA.R")

#### Hoja 2 ####

datos = DBI::dbReadTable(conn = buig,name = "encuesta_intercensal_2025")
dicc_eic_2025=get_col_descriptions(connection = buig,table_name = "encuesta_intercensal_2025")

preparar_apartado_vivienda = function(
  columnas,
  datos_base = datos,
  diccionario_base = dicc_eic_2025
) {
  if (
    !is.character(columnas) ||
    length(columnas) == 0 ||
    anyNA(columnas) ||
    any(!nzchar(columnas))
  ) {
    stop("`columnas` debe ser un vector de nombres de columnas no vacío.")
  }

  if (anyDuplicated(columnas)) {
    stop("`columnas` no debe contener nombres repetidos.")
  }

  columnas_datos = c("CVEGEO", columnas)
  columnas_faltantes = setdiff(columnas_datos, names(datos_base))

  if (length(columnas_faltantes) > 0) {
    stop(
      "No se encontraron en `datos` estas columnas: ",
      paste(columnas_faltantes, collapse = ", ")
    )
  }

  columnas_diccionario = c("column_name", "column_description")
  if (!all(columnas_diccionario %in% names(diccionario_base))) {
    stop("`diccionario` debe contener `column_name` y `column_description`.")
  }

  datos_apartado = datos_base |> 
    dplyr::select(CVEGEO, dplyr::all_of(columnas))

  diccionario_seleccionado = diccionario_base |> 
    dplyr::filter(column_name %in% columnas)

  nombres_diccionario = as.character(diccionario_seleccionado$column_name)
  if (anyDuplicated(nombres_diccionario)) {
    stop("El diccionario contiene más de una descripción para alguna columna solicitada.")
  }

  columnas_sin_descripcion = setdiff(columnas, nombres_diccionario)
  if (length(columnas_sin_descripcion) > 0) {
    stop(
      "No se encontraron descripciones en el diccionario para: ",
      paste(columnas_sin_descripcion, collapse = ", ")
    )
  }

  diccionario_ordenado = diccionario_seleccionado[
    match(columnas, nombres_diccionario),
    ,
    drop = FALSE
  ]
  descripciones = as.character(diccionario_ordenado$column_description)

  if (anyNA(descripciones) || any(!nzchar(stringr::str_trim(descripciones)))) {
    stop("El diccionario contiene descripciones vacías para alguna columna solicitada.")
  }

  nombres_salida = c("CVEGEO", descripciones)
  if (anyDuplicated(nombres_salida)) {
    stop("Las descripciones deben ser únicas para evitar nombres de columna duplicados.")
  }

  names(datos_apartado) = nombres_salida

  datos_apartado |> 
    dplyr::mutate(
      CVEGEO = CVEGEO |> 
        as.character() |> 
        substr(start = 1, stop = 5) |> 
        stringr::str_squish()
    )
}

#### Hoja 2  - Vivienda ####

#### H2 - Bienes y tecnologias ####

bienes = preparar_apartado_vivienda(
  columnas = c(
    "PCN_VPH_PC",
    "PCN_VPH_INTER",
    "PCN_VPH_TELEF",
    "PCN_VPH_TV",
    "PCN_VPH_CEL",
    "PCN_VPH_TVP"
  )
)



#### H2 - Déficit de servicios básicos ####

#Nota: Hizo falta Calidad y espacios de la vivienda

ser_basicos = preparar_apartado_vivienda(
  columnas = c(
    "PCN_VPH_AGUADV",
    "PCN_VPH_C_ELEC",
    "PCN_VPH_C_SERV",
    "PCN_VPH_DRENAJ"
  )
)




ser_basicos = ser_basicos |> 
  dplyr::mutate(
    dplyr::across(
      .cols = -CVEGEO,
      .fns = ~ .x |>  as.numeric()
    ),
    dplyr::across(
      .cols = -CVEGEO,
      .fns = ~ round(100 - .x, digits = 2)
    )
  )


ser_basicos |>  names() = ser_basicos |>  names() |>  
  gsub(pattern = " dispone", replacement = " no dispone") |>  stringr::str_squish() |> 
  gsub(pattern = "se no", replacement = "no") |>  stringr::str_squish()


 
#### H2 - Salud ####

#### Personas afiliadas a servicios de salud ####

salud = preparar_apartado_vivienda(
  columnas = c(
    "PDER_SS",
    "PCN_PDER_SS"
  )
)


#### Establecimientos de salud ####

establecimientos_salud = DBI::dbReadTable(conn = buig,name = "Establecimientos_salud_Operacion_Hidalgo")

establecimientos_salud = establecimientos_salud |>
  dplyr::group_by(`CLAVE.DEL.MUNICIPIO`) |> 
  dplyr::count(`NIVEL.ATENCION`) |> 
  dplyr::rename(
    `CLAVE DEL MUNICIPIO` = CLAVE.DEL.MUNICIPIO,
    `NIVEL ATENCION` = NIVEL.ATENCION
  )


establecimientos_salud = establecimientos_salud |>
  tidyr::pivot_wider(
    id_cols = `CLAVE DEL MUNICIPIO`,
    names_from = `NIVEL ATENCION`,
    values_from = n,
    values_fill = 0
  )

establecimientos_salud = establecimientos_salud |> 
  dplyr::mutate(
    `CLAVE DEL MUNICIPIO` = paste0("13", `CLAVE DEL MUNICIPIO`) |>  stringr::str_squish()
  )


#### Personal medico por institución ####

personal_medico = DBI::dbReadTable(
  conn = buig,
  name = "Personal Medico Dependencias Sector Público 2025"
  )

personal_medico = personal_medico |> 
  dplyr::filter(!is.na(CVEGEO)) |> 
  dplyr::select(-Municipio)


personal_medico = personal_medico |> 
  dplyr::mutate(
    CVEGEO = CVEGEO |>  as.numeric()
  ) |> 
  dplyr::arrange(CVEGEO) |> 
  dplyr::mutate(
    CVEGEO = CVEGEO |>  as.character()
  )


#### Camas para hospitalización ####

camas = "Inputs/Page 2/Recursos_Salud_Sectorial_2025/BD ABIERTOS SECTORIAL 2025.csv" |>  
  read.csv(fileEncoding = "Latin1")


camas = camas |> 
  dplyr::filter(
    Clave.Estado == 13
  ) |> 
  dplyr::select(
    Clave.Municipio, TOTAL.CAMAS.AREA.HOSPITALIZACIÓN
  ) |> 
  dplyr::rename(
    `CVEGEO` = Clave.Municipio,
    `Total Camas` = TOTAL.CAMAS.AREA.HOSPITALIZACIÓN
  ) 
