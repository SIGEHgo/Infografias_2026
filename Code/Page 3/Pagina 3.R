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





