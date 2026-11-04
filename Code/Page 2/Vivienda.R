source("../../Importantes_documentos_usar/Conexion_Postgres_CITNOVA.R")

#### Hoja 2 ####

datos = DBI::dbReadTable(conn = buig,name = "encuesta_intercensal_2025")
dicc_eic_2025=get_col_descriptions(connection = buig,table_name = "encuesta_intercensal_2025")

#### Hoja 2  - Vivienda ####

#### H2 - Bienes y tecnologias ####

bienes = datos |> 
  dplyr::select(CVEGEO, PCN_VPH_PC, PCN_VPH_INTER, PCN_VPH_TELEF, PCN_VPH_TV, PCN_VPH_CEL, PCN_VPH_TVP)

bienes_diccionario = dicc_eic_2025 |> 
  dplyr::filter(column_name %in% names(bienes)) |> 
  dplyr::mutate(
    column_name = column_name |>  factor(levels = bienes |>  names())
  ) |> 
  dplyr::arrange(column_name)

bienes |>  names() = bienes_diccionario$column_description

bienes = bienes |> 
  dplyr::rename(CVEGEO = `Clave geoestadística integrada`)|> 
  dplyr::mutate(
    CVEGEO = CVEGEO |>  substr(start = 1, stop = 5) |>  stringr::str_squish()
  )



#### H2 - Déficit de servicios básicos ####

#Nota: Hizo falta Calidad y espacios de la vivienda

ser_basicos = datos |> 
  dplyr::select(CVEGEO, PCN_VPH_AGUADV, PCN_VPH_C_ELEC, PCN_VPH_C_SERV, PCN_VPH_DRENAJ)


ser_basicos_diccionario = dicc_eic_2025 |> 
  dplyr::filter(column_name %in% names(ser_basicos)) |> 
  dplyr::mutate(
    column_name = column_name |>  factor(levels = ser_basicos |>  names())
  ) |> 
  dplyr::arrange(column_name)


ser_basicos |>  names() = ser_basicos_diccionario$column_description

ser_basicos = ser_basicos |> 
  dplyr::rename(CVEGEO = `Clave geoestadística integrada`)|> 
  dplyr::mutate(
    CVEGEO = CVEGEO |>  substr(start = 1, stop = 5) |>  stringr::str_squish()
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

salud = datos |> 
  dplyr::select(CVEGEO, PDER_SS, PCN_PDER_SS)



clues = DBI::dbReadTable(conn = buig,name = "unidades_salud_clues_2025") 
