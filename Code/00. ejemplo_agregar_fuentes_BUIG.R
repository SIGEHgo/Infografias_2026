##Fuentes Locales
"../../Reutilizables/INEGI/conjunto_de_datos_eic2025_105_csv/conjunto_de_datos/conjunto_datos_eic2025_105.csv" |> 
  read.csv()->eic_2025
"../../Reutilizables/INEGI/microdatos de prueba/eic2025_micro_00_csv/personas00.csv" |> 
  data.table::fread()->micro_eic_2025
"../../Reutilizables/INEGI/microdatos de prueba/eic2025_micro_00_csv/viviendas00.csv" |> 
  data.table::fread()->micro_eic_2025



micro_eic_2025=micro_eic_2025 |> 
  dplyr::filter(CVE_ENT==13)##Solo Hidalgo


DBI::dbWriteTable(conn = buig,name = "microdatos_encuesta_intercensal_2025",
                  value = micro_eic_2025,chunk_size=5000 )##Usamos chunk_size porque son muchos registros (1M)
"../../Reutilizables/INEGI/microdatos de prueba/eic2025_micro_00_csv/eic2025_micro_fd.xlsx" |> 
  readxl::read_excel(sheet='PERSONAS')->dicc_micro_eic_2025
DBI::dbWriteTable(conn = buig,name = "microdatos_diccionario_personas_2025",
                  value = dicc_micro_eic_2025 )
"../../Reutilizables/INEGI/microdatos de prueba/eic2025_micro_00_csv/eic2025_micro_fd.xlsx" |> 
  readxl::read_excel(sheet='VIVIENDAS')->dicc_micro_eic_2025
DBI::dbWriteTable(conn = buig,name = "microdatos_diccionario_viviendas_2025",
                  value = dicc_micro_eic_2025 )
"../../Reutilizables/INEGI/microdatos de prueba/eic2025_micro_00_csv/eic2025_micro_fd.xlsx" |> 
  readxl::read_excel(sheet='MIGRANTES')->dicc_micro_eic_2025
DBI::dbWriteTable(conn = buig,name = "microdatos_diccionario_migrantes_2025",
                  value = dicc_micro_eic_2025 )



"../../Reutilizables/INEGI/conjunto_de_datos_enr2025_csv/conjunto_de_datos/conjunto_de_datos_enr2025.csv" |> 
  data.table::fread()->enr_2025
enr_2025=enr_2025 |> 
  dplyr::filter(ent_regis==13)
DBI::dbWriteTable(conn = buig,name = "estadistica_nacimientos_2025",
                  value = enr_2025,chunk_size=5000 )##Usamos chunk_size porque son muchos registros
