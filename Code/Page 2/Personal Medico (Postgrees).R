personal_medico = "Inputs/Page 2/Personal medico por institución.xlsx" |>  
  readxl::read_excel()


personal_medico = personal_medico |> 
  dplyr::select(-c(...2, ...3, ...4, ...11, ...13))

personal_medico |>  names() = personal_medico[4,]

personal_medico = personal_medico |> 
  dplyr::filter(
    !is.na(Total) 
  )

personal_medico = personal_medico |> 
  dplyr::rename(
    `IMSS BIENESTAR`= `IMSS\r\nBIENESTAR\r\nOrganismo\r\nPúblico Des-\r\nconcentrado`
  ) |> 
  dplyr::slice(-1)


mun = "../../Importantes_documentos_usar/Municipios/municipiosjair.shp" |>  
  sf::read_sf() |> 
  sf::st_drop_geometry()

mun = mun |> 
  dplyr::select(CVEGEO, NOM_MUN)


personal_medico = personal_medico |> 
  dplyr::left_join(
    y = mun,
    by = c("Municipio" = "NOM_MUN")
  )


personal_medico = personal_medico |> 
  dplyr::relocate(CVEGEO, .before = Municipio)

source("../../Importantes_documentos_usar/Conexion_Postgres_CITNOVA.R")

DBI::dbWriteTable(
  conn = buig,
  name = "Personal Medico Dependencias Sector Público 2025",
  value = personal_medico,
  overwrite = T
)